"""Merges weekly.py's scrape (box score + DST) with extra_dash.py's
big-play/target-share report into td.main.weekly's full 41-column schema.

Ports TRUFFLEdashOmni.R's `omni_weekly` transform (lines 24-70) plus
global.R's cleanWeekly()'s PosRk logic - see
truffledash.com/R to Python Scripts/ for context on other tables' equivalent
docs (no such doc exists yet for weekly, hence porting directly from the R).

One (Season, Week) batch at a time, matching how weekly_to_motherduck.py
upserts. The original R pipeline sourced DST rows from the `fantasy` table
(only ever covering DSTs in a starting lineup that week) and unioned them in
before this transform ran - weekly.py now scrapes every DST directly, so
that union step is gone entirely here.
"""

import datetime

import pandas as pd

SUNDATES_CSV = "TRUFFLEdashOmni/data/sundates.csv"

TARGET_COLUMNS = [
    "SznWkPlPos", "SznPlPos", "PlPos", "Season", "Week", "sunDate",
    "Player", "Pos", "NFL", "Opp", "OpRk",
    "PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt",
    "RuAtt", "RuYd", "RuTD", "RuFD",
    "Tar", "Rec", "ReYd", "ReTD", "ReFD",
    "FL", "FPts", "PosRk", "PPFD", "PPR", "hPPR", "STD",
    "Pa20", "Pa40", "Ru20", "Re20", "Re40", "TmTar", "TotYd",
    "ScrimYd", "PaRuYd",
]


def _get_sun_date(season: int, week: int, sundates_csv: str) -> datetime.date:
    """Returns a real date object, not the raw "M/D/YY" string - td.main.weekly's
    sunDate column is DATE-typed (fixed 2026-08-11 after it was discovered to have
    been stored as VARCHAR since the table was first created back in the R
    pipeline, which never cast it either). Keep it that way going forward."""
    dates = pd.read_csv(sundates_csv)
    match = dates[(dates["Season"] == season) & (dates["Week"] == week)]
    if match.empty:
        raise ValueError(f"No sunDate found for Season={season} Week={week} in {sundates_csv}")
    return datetime.datetime.strptime(match.iloc[0]["sunDate"], "%m/%d/%y").date()


def _composite_keys(df: pd.DataFrame) -> tuple:
    player_nospace = df["Player"].str.replace(" ", "", regex=False)
    season_str = df["Season"].astype(str)
    szn_wk_pl_pos = season_str + "_" + df["Week"].astype(str) + "_" + player_nospace + "_" + df["Pos"]
    szn_pl_pos = season_str + "_" + player_nospace + "_" + df["Pos"]
    pl_pos = player_nospace + "_" + df["Pos"]
    return szn_wk_pl_pos, szn_pl_pos, pl_pos


def build_weekly_table(weekly_df: pd.DataFrame, extradash_df: pd.DataFrame, season: int, week: int,
                        sundates_csv: str = SUNDATES_CSV) -> pd.DataFrame:
    df = weekly_df.drop(columns=["TRUFFLE"], errors="ignore").copy()

    # Taysom-Hill-style dual-eligibility fix, ported from TRUFFLEdashOmni.R -
    # CBS occasionally tags a player with two positions (e.g. "QB,TE"); this
    # league always counts them at the second.
    df.loc[df["Pos"] == "QB,TE", "Pos"] = "TE"

    df["SznWkPlPos"], df["SznPlPos"], df["PlPos"] = _composite_keys(df)
    df["sunDate"] = _get_sun_date(season, week, sundates_csv)

    df["PPFD"] = df["FPts"]
    df["PPR"] = df["FPts"] - df["RuFD"] - df["ReFD"] + df["Rec"]
    df["hPPR"] = df["FPts"] - df["RuFD"] - df["ReFD"] + 0.5 * df["Rec"]
    df["STD"] = df["FPts"] - df["RuFD"] - df["ReFD"]

    ed = extradash_df.copy()
    ed.loc[ed["Pos"] == "QB,TE", "Pos"] = "TE"
    ed["SznWkPlPos"], _, _ = _composite_keys(ed)
    ed = ed[["SznWkPlPos", "Pa20", "Pa40", "Ru20", "Re20", "Re40", "TotYd"]]

    df = df.merge(ed, on="SznWkPlPos", how="left")

    # skipna=True (pandas' default) deliberately, unlike the R original's
    # bare sum() (no na.rm=TRUE) - a team with a Tar-less DST row in the same
    # Season/Week/NFL group would otherwise turn TmTar NA for that team's
    # skill players too, which isn't the intent.
    df["TmTar"] = df.groupby("NFL")["Tar"].transform("sum")
    df["ScrimYd"] = df["RuYd"] + df["ReYd"]
    df["PaRuYd"] = df["PaYd"] + df["RuYd"]

    df = df.sort_values("FPts", ascending=False)
    df["PosRk"] = df.groupby("Pos")["FPts"].rank(method="first", ascending=False).astype(int)

    return df[TARGET_COLUMNS].reset_index(drop=True)

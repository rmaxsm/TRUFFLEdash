"""Scrapes the current week's box-score stats from CBS Sports (the TRUFFLE
league's CBS site is the URL host, but the data itself is every real NFL
player's stats that week - not TRUFFLE/KERFUFFLE fantasy-roster data).

This is the single most important scrape in the whole pipeline - it's the
source for td.main.weekly, which holds real per-week NFL player stats dating
back to 2020. Like ids.py, this is deliberately fantasy-ownership-agnostic:
it never resolves the page's "Avail"/TRUFFLE-owner column (dropped
unread, same as extra_dash.py) and never touches omni_client/teams_df at all.

Two separate CBS reports are combined here:
  - scrape_weekly_skill(): the usual QB/RB/WR/TE box score (all individual
    stat columns).
  - scrape_weekly_dst(): a DST-filtered view of the same TRUFFLEoffense
    report - CBS doesn't break defensive stats (sacks/INTs/fumble
    recoveries/points allowed) out into columns here, so only Opp/OpRk/FPts
    are populated and every offense-stat column is left NaN. This replaces
    relying on the `fantasy` table for DST rows, which only ever covered
    DSTs that were in a starting lineup that week - scraping this report
    directly gets every DST, every week, regardless of ownership.

Players (or DSTs) who didn't play that week show Avg == "-" on CBS's page
and are dropped - except during the pre-season, when every row on the page
shows Avg == "-" (no games have been played yet), which would otherwise
scrape as an empty result. See scrape_weekly()'s allow_preseason_placeholder
param.
"""

import argparse
import datetime
import re

import pandas as pd
from bs4 import BeautifulSoup

from cbs_auth import get_session, verify_session
from cbs_scrape_utils import clean_player_name, get_calendar_week

LEAGUE = "TRUFFLE"

RAW_COLUMNS = [
    "Season", "Week", "Pos", "NFL", "Player", "Opp", "OpRk",
    "PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt", "RuAtt", "RuYd", "RuTD", "RuFD",
    "Tar", "Rec", "ReYd", "ReTD", "ReFD", "FL", "Avg", "FPts",
]

# Every one of these is a DOUBLE in td.main.weekly - OpRk is deliberately
# excluded (kept as a string) since it's a VARCHAR column there, unlike the
# original script which cast it to float along with everything else.
NUMERIC_COLUMNS = [
    "PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt", "RuAtt", "RuYd", "RuTD", "RuFD",
    "Tar", "Rec", "ReYd", "ReTD", "ReFD", "FL", "FPts",
]

# CBS's raw header row still has a "TRUFFLE" (Avail/ownership) column at
# index 1 - separateColumns() labels it that only so it can be dropped by
# name below, unread, alongside Action/Bye/Rost/Start.
DROP_RAW_COLUMNS = ["Action", "TRUFFLE", "Bye", "Rost", "Start"]


def separateColumns(row):
    allCols = [cell.getText() for cell in row]
    allCols[1] = "TRUFFLE"
    return allCols


def separatePlayers(row):
    return [cell.getText() for cell in row]


def _apply_preseason_placeholder(df: pd.DataFrame, season: int, week: int, allow: bool) -> pd.DataFrame:
    """See module docstring - only fires when explicitly requested AND every
    scraped row is confirmed blank, so a real scraping bug mid-season (which
    would also show all "-") can't silently masquerade as "just preseason"."""
    all_blank = len(df) > 0 and (df["Avg"] == "-").all()
    if not allow:
        return df
    if not all_blank:
        raise RuntimeError(
            f"--allow-preseason-placeholder was set for Season={season} Week={week}, but real "
            f"stats are already present (not every scraped row is Avg='-') - refusing to "
            f"fabricate placeholder data over what looks like real, in-progress season data. "
            f"Drop the flag and re-run normally."
        )
    print(
        f"PRESEASON TEST MODE: every scraped row for Season={season} Week={week} shows Avg='-' "
        f"(no games played yet) - forcing Avg to a placeholder value so this fake week can "
        f"flow through the pipeline for testing. All other stats are left at their real "
        f"scraped values (0s). This must only ever be written to a non-production database."
    )
    df = df.copy()
    df["Avg"] = "1"
    return df


def scrape_weekly_skill(season: int, week: int, allow_preseason_placeholder: bool = False) -> pd.DataFrame:
    """Scrapes the box-score stats page for a single week.

    Returns one row per skill-position player with real stats that week,
    columns matching td.main.weekly's schema minus the derived/joined columns
    (composite keys, sunDate, PPFD/PPR/hPPR/STD, PosRk, TmTar, ScrimYd,
    PaRuYd) that weekly_transform.py adds after merging in extra_dash.py.
    """
    session = get_session(LEAGUE)
    verify_session(session, LEAGUE)

    url = (
        "https://theradicalultimatefflexperience.football.cbssports.com/"
        f"stats/stats-main/all:FLEX/period-{week}:p/TRUFFLEoffense/?print_rows=9999"
    )
    response = session.get(url, timeout=30)
    soup = BeautifulSoup(response.content, "html.parser")

    tbl = soup.find("table", {"class": "data pinHeader"})
    combined = tbl.find_all("tr", class_="label")
    cols = separateColumns(combined[1])

    allRows = tbl.find_all("tr", class_=re.compile(r"row\d"))
    puffinsRows = tbl.find_all("tr", class_=re.compile("bgFan"))

    allPlayers = [separatePlayers(row) for row in allRows]
    allPlayers += [separatePlayers(row) for row in puffinsRows]

    df = pd.DataFrame(allPlayers, columns=cols)
    df = df.drop(columns=DROP_RAW_COLUMNS, errors="ignore")

    playerTeam = df["Player"].apply(lambda x: pd.Series([i.strip() for i in x.split("•")]))
    position = playerTeam[0].apply(lambda y: pd.Series([i for i in y.split(" ")][-1]))
    player = playerTeam[0].apply(lambda z: pd.Series(" ".join([i for i in z.split(" ")][:-1])))
    nfl = pd.Series(playerTeam[1])

    df["Player"] = player
    df.insert(0, "Pos", position[0])
    df.insert(1, "NFL", nfl)
    df.insert(0, "Season", season)
    df.insert(1, "Week", week)

    df.columns = RAW_COLUMNS
    df["OpRk"] = df["OpRk"].replace("---", "33")
    df[NUMERIC_COLUMNS] = df[NUMERIC_COLUMNS].apply(pd.to_numeric, errors="coerce")

    df["Player"] = clean_player_name(df["Player"])
    df["Player"] = df["Player"].str.replace("Will Fuller V", "Will Fuller", regex=False)

    df = _apply_preseason_placeholder(df, season, week, allow_preseason_placeholder)

    df = df[df["Avg"] != "-"].copy()
    df = df.drop(columns=["Avg"])
    return df.sort_values(by="FPts", ascending=False).reset_index(drop=True)


DST_EXPECTED_COLUMNS = ["Action", "TRUFFLE", "Player", "Opp", "OVP", "Bye", "Rost", "Start", "Avg", "Total"]


def scrape_weekly_dst(season: int, week: int, allow_preseason_placeholder: bool = False) -> pd.DataFrame:
    """Scrapes the DST-filtered TRUFFLEoffense report for a single week.

    This report has no individual defensive stat columns - only Opp,
    OpRk (CBS labels it "OVP" here), and FPts are populated. Every
    offense-stat column (PaCmp..FL) is simply absent from this frame, so
    scrape_weekly()'s pd.concat with scrape_weekly_skill()'s output fills
    them with NaN automatically - matching td.main.weekly's existing DST
    rows, which are already NULL in those columns.
    """
    session = get_session(LEAGUE)
    verify_session(session, LEAGUE)

    url = (
        "https://theradicalultimatefflexperience.football.cbssports.com/"
        f"stats/stats-main/all:DST/period-{week}:p/TRUFFLEoffense/?print_rows=9999"
    )
    response = session.get(url, timeout=30)
    soup = BeautifulSoup(response.content, "html.parser")

    tbl = soup.find("table", {"class": "data pinHeader"})
    combined = tbl.find_all("tr", class_="label")
    cols = separateColumns(combined[1])
    if cols != DST_EXPECTED_COLUMNS:
        raise RuntimeError(
            f"CBS's DST report columns changed - expected {DST_EXPECTED_COLUMNS}, got {cols}. "
            f"Update scrape_weekly_dst() before trusting this scrape."
        )

    allRows = tbl.find_all("tr", class_=re.compile(r"row\d"))
    puffinsRows = tbl.find_all("tr", class_=re.compile("bgFan"))
    allTeams = [separatePlayers(row) for row in allRows]
    allTeams += [separatePlayers(row) for row in puffinsRows]

    df = pd.DataFrame(allTeams, columns=cols)
    df = df.drop(columns=["Action", "TRUFFLE", "Bye", "Rost", "Start"], errors="ignore")

    playerTeam = df["Player"].apply(lambda x: pd.Series([i.strip() for i in x.split("•")]))
    mascot = playerTeam[0].apply(lambda y: pd.Series(" ".join([i for i in y.split(" ")][:-1])))
    nfl = pd.Series(playerTeam[1])

    df["Player"] = mascot
    df.insert(0, "Pos", "DST")
    df.insert(1, "NFL", nfl)
    df.insert(0, "Season", season)
    df.insert(1, "Week", week)

    df = df.rename(columns={"OVP": "OpRk", "Total": "FPts"})
    df["OpRk"] = df["OpRk"].replace("---", "33")
    df["FPts"] = pd.to_numeric(df["FPts"], errors="coerce")

    df = _apply_preseason_placeholder(df, season, week, allow_preseason_placeholder)

    df = df[df["Avg"] != "-"].copy()
    return df[["Season", "Week", "Pos", "NFL", "Player", "Opp", "OpRk", "FPts"]]


def scrape_weekly(season: int, week: int, allow_preseason_placeholder: bool = False) -> pd.DataFrame:
    """Scrapes both the skill-position box score and the DST report for a
    single week and combines them into one frame - the real entry point for
    everything downstream (weekly_transform.py, the CLI below)."""
    skill = scrape_weekly_skill(season, week, allow_preseason_placeholder)
    dst = scrape_weekly_dst(season, week, allow_preseason_placeholder)
    combined = pd.concat([skill, dst], ignore_index=True, sort=False)
    return combined.sort_values(by="FPts", ascending=False).reset_index(drop=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--season", type=int, default=None,
                         help="Season to scrape (default: auto-detect from today's date via the ESPN calendar)")
    parser.add_argument("--week", type=int, default=None,
                         help="Week to scrape (default: auto-detect from today's date via the ESPN calendar)")
    parser.add_argument("--which", choices=["current", "previous"], default="current",
                         help="Which calendar week to auto-detect when --season/--week aren't given - see "
                              "get_calendar_week()'s docstring. 'current' is correct for every day of the "
                              "normal cadence, including the Wednesday correction run; 'previous' is a "
                              "manual escape hatch only.")
    parser.add_argument("--allow-preseason-placeholder", action="store_true",
                         help="If every scraped row is Avg='-' (no games played yet this season), write "
                              "synthetic placeholder data instead of an empty result. For pipeline testing "
                              "only - weekly_to_motherduck.py refuses to combine this with --database td.")
    args = parser.parse_args()

    season, week = args.season, args.week
    if season is None or week is None:
        detected_season, detected_week = get_calendar_week(which=args.which)
        season = season or detected_season
        week = week or detected_week

    begin_time = datetime.datetime.now()
    df = scrape_weekly(season, week, allow_preseason_placeholder=args.allow_preseason_placeholder)

    filepath = "data/weekly_scraperesult.csv"
    df.to_csv(filepath, index=False)
    print(df)
    print(f"\nstored scrape result at {filepath}")
    print(f"execution time: {datetime.datetime.now() - begin_time}")

"""Scrapes the current week's CBS "ExtraDash" report for TRUFFLE - big-play
and target-share stats (Pa20/Pa40/Ru20/Re20/Re40/TotYd) that aren't on the
main box-score page weekly.py scrapes.

Feeds weekly_transform.py's merge with weekly.py's output, joined on
Season+Week+Player+Pos. Per TRUFFLEdashOmni.R's merge, only Pa20/Pa40/Ru20/
Re20/Re40/TotYd are actually used downstream - team ownership ("Avail") isn't
resolved or kept here at all, same as weekly.py.

Same Avg == "-" inactive-player gate and preseason-placeholder handling as
weekly.py - see that module's docstring for why. Replaces the ancient
dre/CBS/extraDash.py, whose hardcoded 2022 cookies are long dead; this uses
cbs_auth.py's session like every other reworked scraper.
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
    "Season", "Week", "Player", "Pos", "NFL",
    "Cmp%", "Pa20", "Pa40", "RuYPC", "Ru20", "Tar", "Tar%", "ReYPC", "Re20", "Re40", "ReFD%",
    "TotYd", "Avg", "FPts",
]

# The only columns TRUFFLEdashOmni.R's merge actually pulls from this report.
NUMERIC_COLUMNS = ["Pa20", "Pa40", "Ru20", "Re20", "Re40", "TotYd"]
OUTPUT_COLUMNS = ["Season", "Week", "Player", "Pos", "NFL"] + NUMERIC_COLUMNS


def separateColumns(row):
    return [cell.getText() for cell in row]


def separatePlayers(row):
    return [cell.getText() for cell in row]


def _apply_preseason_placeholder(df: pd.DataFrame, season: int, week: int, allow: bool) -> pd.DataFrame:
    """Mirrors weekly.py's failsafe - only fires when explicitly requested AND
    every scraped row is confirmed blank."""
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


def scrape_extra_dash(season: int, week: int, allow_preseason_placeholder: bool = False) -> pd.DataFrame:
    session = get_session(LEAGUE)
    verify_session(session, LEAGUE)

    url = (
        "https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main/"
        f"all:QB:RB:WR:TE:RB-WR-TE:FLEX/period-{week}:p/ExtraDash/?print_rows=9999"
    )
    response = session.get(url, timeout=30)
    soup = BeautifulSoup(response.content, "html.parser")

    complete = soup.find("div", {"id": "sortableStats"})
    tbl = complete.find("table")
    colHeaders = separateColumns(tbl.find_all("th"))

    allRows = tbl.find_all("tr", class_=re.compile(r"row\d"))
    puffinsRows = tbl.find_all("tr", class_="bgFan")
    allPlayers = [separatePlayers(row) for row in allRows]
    allPlayers += [separatePlayers(row) for row in puffinsRows]

    df = pd.DataFrame(allPlayers, columns=colHeaders)
    df = df.drop(columns=["Action", "Avail", "Opp", "OVP", "Bye", "Rost", "Start"], errors="ignore")

    playerTeam = df["Player"].apply(lambda x: pd.Series([i.strip() for i in x.split("•")]))
    position = playerTeam[0].apply(lambda y: pd.Series([i for i in y.split(" ")][-1]))
    player = playerTeam[0].apply(lambda z: pd.Series(" ".join([i for i in z.split(" ")][:-1])))
    nfl = pd.Series(playerTeam[1])

    df["Player"] = player
    df.insert(1, "Pos", position[0])
    df.insert(2, "NFL", nfl)
    df.insert(0, "Season", season)
    df.insert(1, "Week", week)

    df.columns = RAW_COLUMNS
    df[NUMERIC_COLUMNS] = df[NUMERIC_COLUMNS].apply(pd.to_numeric, errors="coerce")

    df["Player"] = clean_player_name(df["Player"])
    df["Player"] = df["Player"].str.replace("Will Fuller V", "Will Fuller", regex=False)

    df = _apply_preseason_placeholder(df, season, week, allow_preseason_placeholder)

    df = df[df["Avg"] != "-"].copy()
    return df[OUTPUT_COLUMNS].reset_index(drop=True)


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
    df = scrape_extra_dash(season, week, allow_preseason_placeholder=args.allow_preseason_placeholder)

    filepath = "data/extraDash_scraperesult.csv"
    df.to_csv(filepath, index=False)
    print(df)
    print(f"\nstored scrape result at {filepath}")
    print(f"execution time: {datetime.datetime.now() - begin_time}")

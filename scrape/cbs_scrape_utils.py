"""Shared cleaning/lookup helpers for the CBS TRUFFLE/KERFUFFLE scrapers.

Consolidates logic that was previously copy-pasted (and buggy in the copy) across
scrape/rosters.py, scrape/kerfuffle/kerfuffle_rosters.py, and the other CBS scrapers.
"""

import datetime
import re

import pandas as pd

# Same period + suffix correction sequence used consistently across the R
# pipeline (global.R) and every Python scraper. The Python side previously had
# a real bug here: `.str.replace(r'.', '', regex=True)` uses a bare `.`, which
# is a regex wildcard matching *any* character under regex=True - it blanked
# the entire Player column instead of removing literal periods. The fix is to
# escape the period (matching R's `str_replace_all(file$Player, "\\.", "")`),
# while keeping the exact same suffix-stripping steps in the same order.
def clean_player_name(names: pd.Series) -> pd.Series:
    names = names.str.replace(r"\.", "", regex=True)
    names = names.str.replace(r" Jr", "", regex=True)
    names = names.str.replace(r" Sr", "", regex=True)
    names = names.str.replace(r" III", "", regex=True)
    names = names.str.replace(r" IV", "", regex=True)
    names = names.str.replace(r" II", "", regex=True)
    # " V" only strips as a trailing generational suffix (end-of-string
    # anchored) - without the $ anchor this would also mangle any last name
    # that happens to start with "V" (Vasquez, Van Noy, etc.)
    names = names.str.replace(r" V$", "", regex=True)
    return names


def get_current_season(as_of: datetime.date = None) -> int:
    """Active roster/contract season for a given date, rolling over each June.

    Rosters and contracts turn over well before games start (draft, free
    agency, cuts all happen in the offseason), so the season label advances in
    June rather than at kickoff in September - e.g. June 2026 through May 2027
    is all "Season 2026".
    """
    as_of = as_of or datetime.date.today()
    return as_of.year if as_of.month >= 6 else as_of.year - 1


def get_current_season_week(as_of: datetime.date = None,
                             sundates_csv: str = "TRUFFLEdashOmni/data/sundates.csv") -> tuple:
    """Active (season, week) for a given date, looked up from the Sunday-kickoff
    reference table - unlike get_current_season() (a simple June-rollover rule
    for roster/contract labeling), this tracks actual NFL game weeks, needed by
    any scraper whose CBS URL is parameterized by week (e.g. playerIDs.py,
    weekly.py, fantasy.py).

    Returns the most recent (Season, Week) whose sunDate is <= as_of - i.e. the
    week whose games have already happened, matching how these scrapers are
    always run mid-week to fetch the just-completed week's data.
    """
    as_of = as_of or datetime.date.today()
    dates = pd.read_csv(sundates_csv)
    dates["sunDate"] = pd.to_datetime(dates["sunDate"], format="%m/%d/%y")
    dates = dates[dates["sunDate"] <= pd.Timestamp(as_of)]
    if dates.empty:
        raise ValueError(f"No sundates.csv row on or before {as_of} - reference table needs extending")
    row = dates.sort_values("sunDate").iloc[-1]
    return int(row["Season"]), int(row["Week"])


_WAIVED_PREFIX = re.compile(r"^W ")


def get_team_abbreviation_lookup(teams_df: pd.DataFrame, league: str, name_col: str = "FullName") -> dict:
    """Team name -> Abbrev lookup for a given league, from data/teams.csv.

    name_col varies by which CBS page is being scraped: the roster page shows
    full team names (FullName), while the stats pages truncate them
    (LogsScrape, e.g. "Arctic..." for "Arctic Fighting Lemurloos").
    """
    league_teams = teams_df[teams_df["League"] == league.upper()]
    return dict(zip(league_teams[name_col], league_teams["Abbrev"]))


def get_team_abbreviation(team_name: str, lookup: dict) -> str:
    """Team name -> abbreviation, passing through waived-player placeholders (e.g. 'W ...') unchanged."""
    if _WAIVED_PREFIX.match(team_name):
        return team_name
    try:
        return lookup[team_name]
    except KeyError:
        print(f"An error occurred while trying to get the team abbreviation for {team_name}")
        return "err"

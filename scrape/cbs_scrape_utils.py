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
    names = names.str.replace(r" II", "", regex=True)
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


_WAIVED_PREFIX = re.compile(r"^W ")


def get_team_abbreviation_lookup(teams_df: pd.DataFrame, league: str) -> dict:
    """Team FullName -> Abbrev lookup for a given league, from data/teams.csv."""
    league_teams = teams_df[teams_df["League"] == league.upper()]
    return dict(zip(league_teams["FullName"], league_teams["Abbrev"]))


def get_team_abbreviation(team_name: str, lookup: dict) -> str:
    """Team name -> abbreviation, passing through waived-player placeholders (e.g. 'W ...') unchanged."""
    if _WAIVED_PREFIX.match(team_name):
        return team_name
    try:
        return lookup[team_name]
    except KeyError:
        print(f"An error occurred while trying to get the team abbreviation for {team_name}")
        return "err"

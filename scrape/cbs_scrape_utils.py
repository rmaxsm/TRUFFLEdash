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
    # Leading/trailing whitespace from a raw scrape would silently defeat
    # every $-anchored suffix below (" V " doesn't match r" V$"), so strip
    # before anything else.
    names = names.str.strip()
    names = names.str.replace(r"\.", "", regex=True)
    # Every generational suffix is end-of-string ($) anchored so it only
    # strips as a trailing suffix - without the anchor these would also
    # mangle any last name that happens to contain the same letters
    # (e.g. "II" is a substring of "III", "V" starts "Vasquez"/"Van Noy").
    # Longest-suffix-first order matters too: "III"/"IV" must be checked
    # before "II" so a real "III"/"IV" name doesn't get half-stripped to
    # "I" or "V" by the shorter pattern matching first.
    names = names.str.replace(r" Jr$", "", regex=True)
    names = names.str.replace(r" Sr$", "", regex=True)
    names = names.str.replace(r" III$", "", regex=True)
    names = names.str.replace(r" IV$", "", regex=True)
    names = names.str.replace(r" II$", "", regex=True)
    names = names.str.replace(r" V$", "", regex=True)
    return names


def get_current_season(as_of: datetime.date = None) -> int:
    """Active roster/contract season for a given date, rolling over each May 1.

    Rosters and contracts turn over well before games start (rookie draft,
    free agency, cuts all happen in the offseason), so the season label
    advances in May rather than at kickoff in September - e.g. May 2026
    through April 2027 is all "Season 2026". Confirmed against CBS's own
    transactions pages while building scrape/transactions.py: a "Round 2,
    (Overall Pick 23)" rookie-draft-pick trade dated 5/12/25 (with no
    explicit draft year in its text, meaning "this season's draft") lives on
    the transactions/.../2025 page - i.e. CBS itself already treats
    early-May dates as the new season, one month earlier than this function
    previously assumed (used to roll over in June).
    """
    as_of = as_of or datetime.date.today()
    return as_of.year if as_of.month >= 5 else as_of.year - 1


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


def get_calendar_week(as_of: datetime.date = None, which: str = "current",
                       calendar_csv: str = "TRUFFLEdashOmni/data/espn_api_nfl_calendar.csv") -> tuple:
    """Active (season, week) for a given date, looked up from ESPN's regular-
    season calendar (Wednesday-to-Wednesday week windows, in
    TRUFFLEdashOmni/data/espn_api_nfl_calendar.csv) rather than sundates.csv's
    Sunday-only reference.

    This is a THIRD season/week concept, distinct from both get_current_season()
    (June-rollover roster labeling) and get_current_season_week() (most recently
    completed Sunday) - don't conflate them. get_current_season_week() can only
    ever tell you about a week whose Sunday has already happened, so it's wrong
    for weekly.py's Thursday/Friday runs (which need the week that's *currently*
    in progress, before its Sunday game). This function resolves that instead,
    using ESPN's own Wed-Wed week boundaries (which already account for the
    occasional Wed/Fri/Sat-shifted week - see espn_api_nfl_calendar.csv's source
    JSON for how those were captured).

    which="current": the week whose [WeekStartUTC, WeekEndUTC] window contains as_of.
    which="previous": one week before that.

    as_of is a bare date (midnight, no time-of-day) compared against the
    calendar's UTC timestamps - which turns out to matter for the Wednesday
    correction run specifically: ESPN's week boundary rolls over a few hours
    *after* midnight (~3am ET / 07-08 UTC), so midnight on the rollover day
    itself still falls inside the OLD week's window. Concretely,
    which="current" on any Wednesday already resolves to the week that just
    finished (verified against the 2026 calendar), which is exactly what the
    correction run wants - no special-casing needed. which="previous" is
    therefore NOT part of the normal daily cadence; it's a manual escape
    hatch for going one week further back than "current" would resolve to.

    Raises if as_of falls outside every window in the file (pre-season,
    post-season, or the file needs extending for a new year) or if
    which="previous" is requested for the file's very first tracked week.
    """
    if which not in ("current", "previous"):
        raise ValueError(f"which must be 'current' or 'previous', got {which!r}")

    as_of = as_of or datetime.date.today()
    as_of_ts = pd.Timestamp(as_of)

    cal = pd.read_csv(calendar_csv)
    cal["WeekStartUTC"] = pd.to_datetime(cal["WeekStartUTC"]).dt.tz_localize(None)
    cal["WeekEndUTC"] = pd.to_datetime(cal["WeekEndUTC"]).dt.tz_localize(None)
    cal = cal.sort_values(["Season", "Week"]).reset_index(drop=True)

    matches = cal[(cal["WeekStartUTC"] <= as_of_ts) & (as_of_ts <= cal["WeekEndUTC"])]
    if matches.empty:
        raise ValueError(
            f"No regular-season calendar week contains {as_of} in {calendar_csv} - either the "
            f"season hasn't started/has already ended, or the file needs extending for a new season"
        )
    idx = matches.index[0]

    if which == "previous":
        if idx == 0:
            raise ValueError(
                f"{as_of} falls in {calendar_csv}'s first tracked week - there is no previous "
                f"week to fall back to"
            )
        idx -= 1

    row = cal.iloc[idx]
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


def get_team_by_teamnum_lookup(teams_df: pd.DataFrame, league: str) -> dict:
    """CBS numeric team id (int, from a /teams/{N} href) -> Abbrev, for a given league.

    Unlike get_team_abbreviation_lookup() (which matches on the team's
    *displayed name text*), this resolves on CBS's permanent per-franchise
    numeric id instead - the only reliable way to identify a team on a page
    that shows historical data, since a franchise's displayed name can
    change over time even though its underlying CBS team id never does. E.g.
    TRUFFLE's team id 12 was displayed as "Windy City Big Apples" in the CBS
    2020 transactions page and is "Madison Muskellunge"/"MAM" today - joining
    on the numeric id resolves straight to the current identity with no
    special-casing, matching how every other td.main.* table already treats
    that franchise's full history as "MAM" throughout.
    """
    league_teams = teams_df[teams_df["League"] == league.upper()]
    lookup = {}
    for team_num, abbrev in zip(league_teams["TeamNum"], league_teams["Abbrev"]):
        try:
            lookup[int(team_num)] = abbrev
        except (ValueError, TypeError):
            # teamscsv has at least one row (KERFUFFLE's NBB, as of 2026-08)
            # with a non-numeric TeamNum placeholder - skip it rather than
            # fail the whole lookup; a team that genuinely needs this
            # resolved will surface as a clear "no TeamNum match" error at
            # the call site instead of a silent wrong mapping.
            print(f"WARNING: {league.upper()} team {abbrev!r} has a non-numeric TeamNum ({team_num!r}) "
                  f"in teamscsv - excluded from the TeamNum lookup")
    return lookup

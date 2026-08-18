"""Scrapes the current week's TRUFFLE + KERFUFFLE starting lineups from CBS
Sports - one row per player (or DST) who was in a fantasy team's active
lineup that week.

This is NOT a stats scrape - see weekly.py for real per-week NFL stats, which
now covers every DST/skill-position player regardless of ownership. This is
purely a lineup/ownership record for td.main.fantasy: which player, on which
fantasy team, in which league, for a given Season+Week. Unlike weekly.py,
rows are never dropped for missing stats (Avg == "-") - an inactive/bye
player who was still started belongs in this table same as anyone else, since
this table never carried stat columns in the first place.

TRUFFLE and KERFUFFLE live on separate CBS domains with separate auth, but
the per-team box-score page structure is otherwise identical - this loops
through every team in both leagues (via Omni's teamscsv TeamNum field)
through the same pipeline rather than maintaining two near-duplicate scripts.
"""

import argparse
import datetime
import re

import pandas as pd
import requests
from bs4 import BeautifulSoup

from cbs_auth import LEAGUE_HOSTS, get_session, verify_session
from cbs_scrape_utils import clean_player_name, get_calendar_week
from omni_client import get_teams_df

ROW_REGEX = re.compile(r"row\d")


def _parse_player_cell(text: str):
    """'Kyle Pitts TE • ATL' -> ('Kyle Pitts', 'TE', 'ATL'); same shape for a
    DST row, e.g. 'Packers DST • GB' -> ('Packers', 'DST', 'GB')."""
    name_pos, _, nfl = text.partition("•")
    tokens = name_pos.strip().split(" ")
    pos = tokens[-1]
    player = " ".join(tokens[:-1])
    return player, pos, nfl.strip()


def scrape_team_lineup(league: str, session: requests.Session, team_num: int,
                        team_abbrev: str, season: int, week: int) -> pd.DataFrame:
    """Scrapes a single team's starting lineup (Offense + Team Defense
    tables) for one week. Both tables share the same per-team page - CBS
    renders no players at all for a team that hasn't set a lineup that week
    (common in the preseason before rosters/DSTs are fully drafted), which
    just yields zero rows here rather than needing special-case handling."""
    host = LEAGUE_HOSTS[league.upper()]
    url = f"https://{host}/stats/stats-main/team:{team_num}/period-{week}:f/TRUFFLEoffense/"
    response = session.get(url, timeout=30)
    soup = BeautifulSoup(response.content, "html.parser")

    records = []
    for tbl in soup.find_all("table", class_="data pinHeader borderTop"):
        for row in tbl.find_all("tr", class_=ROW_REGEX):
            cells = row.find_all(["td", "th"])
            player, pos, nfl = _parse_player_cell(cells[2].getText())
            records.append({
                "Season": season, "Week": week, "League": league.upper(),
                "TrfTm": team_abbrev, "Pos": pos, "Player": player, "NFL": nfl,
            })

    return pd.DataFrame(records, columns=["Season", "Week", "League", "TrfTm", "Pos", "Player", "NFL"])


def scrape_league_fantasy(league: str, teams_df: pd.DataFrame, season: int, week: int) -> pd.DataFrame:
    """Scrapes every team's starting lineup for a single league."""
    session = get_session(league)
    verify_session(session, league)

    league_teams = teams_df[teams_df["League"] == league.upper()]
    frames = [
        scrape_team_lineup(league, session, int(row["TeamNum"]), row["Abbrev"], season, week)
        for _, row in league_teams.iterrows()
    ]
    return pd.concat(frames, ignore_index=True)


def scrape_fantasy(season: int, week: int) -> pd.DataFrame:
    """Scrapes both leagues' starting lineups for a single week and combines
    them into one frame - the real entry point for everything downstream
    (fantasy_to_motherduck.py, the CLI below)."""
    teams_df = get_teams_df()
    leagues = ["TRUFFLE", "KERFUFFLE"]
    df = pd.concat(
        [scrape_league_fantasy(league, teams_df, season, week) for league in leagues],
        ignore_index=True,
    )

    df["Player"] = clean_player_name(df["Player"])
    return df


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--season", type=int, default=None,
                         help="Season to scrape (default: auto-detect from today's date via the ESPN calendar)")
    parser.add_argument("--week", type=int, default=None,
                         help="Week to scrape (default: auto-detect from today's date via the ESPN calendar)")
    parser.add_argument("--which", choices=["current", "previous"], default="current",
                         help="Which calendar week to auto-detect when --season/--week aren't given - see "
                              "get_calendar_week()'s docstring. 'current' is correct for every day of the "
                              "normal cadence; 'previous' is a manual escape hatch only.")
    args = parser.parse_args()

    season, week = args.season, args.week
    if season is None or week is None:
        detected_season, detected_week = get_calendar_week(which=args.which)
        season = season or detected_season
        week = week or detected_week

    begin_time = datetime.datetime.now()
    df = scrape_fantasy(season, week)

    filepath = "data/fantasy_scraperesult.csv"
    df.to_csv(filepath, index=False)
    print(df)
    print(f"\nstored scrape result at {filepath}")
    print(f"execution time: {datetime.datetime.now() - begin_time}")

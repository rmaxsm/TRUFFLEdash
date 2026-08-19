"""Scrapes the current week's TRUFFLE + KERFUFFLE full rosters (bench
players included, not just starters), solves each team's optimal lineup via
linear program, and upserts both td.main.rostered and td.main.optlineups -
one combined script covering the scrape, the LP solve that used to live only
in global.R, and both MotherDuck uploads.

This is NOT the same page as fantasy.py's - that scrapes period-{week}:f
(starting lineup only, for td.main.fantasy's ownership record). This scrapes
period-{week}:p on the same per-team CBS pages, which includes bench players
too - the LP needs the full roster to know what the optimal starting lineup
*would have been*, not just what was actually started.

The LP (solve_optimal_lineup()) ports global.R's `lp("max", obj, constraints,
dir, rhs, all.bin = TRUE)` call: a binary IP that picks exactly 10 players
maximizing total FPts, subject to QB 1-2, RB 2-5, WR 2-5, TE 1-4, DST exactly
1. Verified against 816 historical TRUFFLE+KERFUFFLE team-weeks pulled from
data/optScoring.csv + data/kerfuffle/kerfuffle_optScoring.csv: every one
solves to optimality, and the 672 of those also present in td.main.optlineups
(that table was only ever historically populated through Week 14 each season -
global.R's own backfill loop capped `wks` at 14) match its FPts totals
exactly. A handful of those team-weeks pick a different (but equally-scoring)
player set than the R lpSolve run originally did - the LP has tied-optimal
solutions in those cases, and CBC/PuLP breaks the tie differently than
lpSolve did. That's expected, not a bug: the objective value (total FPts) is
what "optimal" means here, and it always matches.

Unlike weekly.py, this never filters rows by Avg == "-" - a bye/inactive
player who's still on the roster needs to stay in the LP's candidate pool
(with FPts=0, same as the raw scrape gives it) so the solver can correctly
NOT pick them, rather than being excluded outright. This also means there's
no preseason-placeholder concern here the way weekly.py/fantasy.py have one:
every rostered player flows through regardless of season state, just with
FPts=0 for players who haven't played yet.
"""

import argparse
import datetime
import re

import duckdb
import pandas as pd
import pulp
import requests
from bs4 import BeautifulSoup

from cbs_auth import LEAGUE_HOSTS, get_session, verify_session
from cbs_scrape_utils import clean_player_name, get_calendar_week
from omni_client import get_teams_df

ROW_REGEX = re.compile(r"row\d")

# Ports global.R's lp() constraints (~line 320-341): QB gets its own min/max,
# same for RB/WR/TE, DST is fixed at exactly 1. A position not listed here
# (there aren't any today, but global.R hit this with a stray "QB,TE" Pos
# value once) still counts toward TOTAL_STARTERS but has no min/max of its
# own - matching that same fallback behavior without needing to special-case
# it. The reverse case - a listed position with zero matching rows on a
# team's roster (e.g. no DST drafted yet) - must still add the constraint
# (an empty sum forces 0 >= lo, i.e. infeasible), exactly matching R's own
# all-FALSE constraint row math; skipping it here would silently let a team
# through without a real DST.
POSITION_BOUNDS = {
    "QB": (1, 2),
    "RB": (2, 5),
    "WR": (2, 5),
    "TE": (1, 4),
    "DST": (1, 1),
}
TOTAL_STARTERS = 10

ROSTERED_COLUMNS = [
    "SznWkPlPos", "SznPlPos", "PlPos", "SznLgTrf", "LgTrf",
    "Season", "Week", "Player", "Pos", "NFL", "TrfLg", "TrfTm",
]
OPTLINEUPS_COLUMNS = [
    "SznWkPlPos", "SznPlPos", "PlPos", "SznLgTrf", "LgTrf",
    "Season", "Week", "Player", "Pos", "TrfLg", "TrfTm", "FPts",
]


def _parse_player_cell(text: str):
    """'Kyle Pitts TE • ATL' -> ('Kyle Pitts', 'TE', 'ATL'); same shape for a
    DST row, e.g. 'Packers DST • GB' -> ('Packers', 'DST', 'GB')."""
    name_pos, _, nfl = text.partition("•")
    tokens = name_pos.strip().split(" ")
    pos = tokens[-1]
    player = " ".join(tokens[:-1])
    return player, pos, nfl.strip()


def scrape_team_roster(league: str, session: requests.Session, team_num: int,
                        team_abbrev: str, season: int, week: int) -> pd.DataFrame:
    """Scrapes a single team's full roster (Offense + Team Defense tables,
    starters and bench both) for one week. Only Player/Pos/NFL (from the
    player-name cell) and FPts (CBS's "Total" column, always the row's last
    cell on both the offense and defense sub-tables) are kept - neither
    target table needs any of the individual stat columns CBS also shows."""
    host = LEAGUE_HOSTS[league.upper()]
    url = f"https://{host}/stats/stats-main/team:{team_num}/period-{week}:p/TRUFFLEoffense/"
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
                "FPts": pd.to_numeric(cells[-1].getText(), errors="coerce"),
            })

    return pd.DataFrame(
        records, columns=["Season", "Week", "League", "TrfTm", "Pos", "Player", "NFL", "FPts"]
    )


def scrape_league_rosters(league: str, teams_df: pd.DataFrame, season: int, week: int) -> pd.DataFrame:
    """Scrapes every team's full roster for a single league."""
    session = get_session(league)
    verify_session(session, league)

    league_teams = teams_df[teams_df["League"] == league.upper()]
    frames = [
        scrape_team_roster(league, session, int(row["TeamNum"]), row["Abbrev"], season, week)
        for _, row in league_teams.iterrows()
    ]
    return pd.concat(frames, ignore_index=True)


def scrape_rosters(season: int, week: int) -> pd.DataFrame:
    """Scrapes both leagues' full rosters for a single week and combines them
    into one frame - the real entry point for everything downstream
    (build_rostered_table(), build_optlineups_table(), the CLI below)."""
    teams_df = get_teams_df()
    leagues = ["TRUFFLE", "KERFUFFLE"]
    df = pd.concat(
        [scrape_league_rosters(league, teams_df, season, week) for league in leagues],
        ignore_index=True,
    )

    df["Player"] = clean_player_name(df["Player"])
    df["FPts"] = df["FPts"].fillna(0)
    return df


def solve_optimal_lineup(team_week_df: pd.DataFrame) -> pd.DataFrame:
    """Given one team's full roster for one week, returns the subset picked
    by the binary IP that maximizes total FPts subject to POSITION_BOUNDS -
    the Python port of global.R's lp() call. See the module docstring for how
    this was verified against historical data."""
    df = team_week_df.reset_index(drop=True)
    n = len(df)
    prob = pulp.LpProblem("optimal_lineup", pulp.LpMaximize)
    x = [pulp.LpVariable(f"x_{i}", cat="Binary") for i in range(n)]

    prob += pulp.lpSum(df.loc[i, "FPts"] * x[i] for i in range(n))

    for pos, (lo, hi) in POSITION_BOUNDS.items():
        idx = df.index[df["Pos"] == pos].tolist()
        prob += pulp.lpSum(x[i] for i in idx) >= lo
        prob += pulp.lpSum(x[i] for i in idx) <= hi

    prob += pulp.lpSum(x) == TOTAL_STARTERS

    status = prob.solve(pulp.PULP_CBC_CMD(msg=False))
    if pulp.LpStatus[status] != "Optimal":
        identity = df[["Season", "Week", "League", "TrfTm"]].iloc[0].to_dict()
        raise RuntimeError(f"optimal-lineup LP did not solve to optimality for {identity}: "
                            f"{pulp.LpStatus[status]} (roster has {n} players)")

    selected = [i for i in range(n) if x[i].value() > 0.5]
    return df.loc[selected]


def _add_keys(df: pd.DataFrame, season: int, week: int) -> pd.DataFrame:
    """Composite-key columns shared by both target tables' schemas."""
    df = df.copy()
    season_str, week_str = str(season), str(week)
    player_nospace = df["Player"].str.replace(" ", "", regex=False)

    df["SznWkPlPos"] = season_str + "_" + week_str + "_" + player_nospace + "_" + df["Pos"]
    df["SznPlPos"] = season_str + "_" + player_nospace + "_" + df["Pos"]
    df["PlPos"] = player_nospace + "_" + df["Pos"]
    df["SznLgTrf"] = season_str + "_" + df["League"] + "_" + df["TrfTm"]
    df["LgTrf"] = df["League"] + "_" + df["TrfTm"]
    df["TrfLg"] = df["League"]
    df["Season"] = float(season)
    df["Week"] = float(week)
    return df


def build_rostered_table(df: pd.DataFrame, season: int, week: int) -> pd.DataFrame:
    return _add_keys(df, season, week)[ROSTERED_COLUMNS]


def build_optlineups_table(rostered_raw: pd.DataFrame, season: int, week: int) -> pd.DataFrame:
    """Solves every (League, TrfTm) group's optimal lineup and assembles
    td.main.optlineups' shape.

    A team whose roster that week can't satisfy POSITION_BOUNDS (e.g. no
    DST rostered at all, which happens for real during the preseason before
    every team has finished drafting - confirmed live, KERFUFFLE's LC had a
    12-player roster with zero DSTs) is skipped with a warning rather than
    failing the whole week's upload - same "skip rather than crash" precedent
    as fantasy.py's handling of a team with no lineup set yet."""
    solutions = []
    for (league, team), g in rostered_raw.groupby(["League", "TrfTm"]):
        try:
            solutions.append(solve_optimal_lineup(g))
        except RuntimeError as e:
            print(f"WARNING: skipping optlineups for {league} {team} - {e}")
    solved = pd.concat(solutions, ignore_index=True)
    return _add_keys(solved, season, week)[OPTLINEUPS_COLUMNS]


def upsert_rostered(df: pd.DataFrame, database: str, season: int, week: int, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute(
        "SELECT count(*) FROM main.rostered WHERE Season = ? AND Week = ?", [season, week]
    ).fetchone()[0]

    if dry_run:
        print(f"[dry-run] would delete {before} existing Season={season} Week={week} rows from "
              f"{database}.main.rostered and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    con.execute("DELETE FROM main.rostered WHERE Season = ? AND Week = ?", [season, week])
    con.register("new_rostered", df)
    con.execute("INSERT INTO main.rostered SELECT * FROM new_rostered")
    con.execute("COMMIT")

    after = con.execute(
        "SELECT count(*) FROM main.rostered WHERE Season = ? AND Week = ?", [season, week]
    ).fetchone()[0]
    print(f"{database}.main.rostered: replaced {before} Season={season} Week={week} rows with {after} new rows")
    con.close()


def upsert_optlineups(df: pd.DataFrame, database: str, season: int, week: int, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute(
        "SELECT count(*) FROM main.optlineups WHERE Season = ? AND Week = ?", [season, week]
    ).fetchone()[0]

    if dry_run:
        print(f"[dry-run] would delete {before} existing Season={season} Week={week} rows from "
              f"{database}.main.optlineups and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    con.execute("DELETE FROM main.optlineups WHERE Season = ? AND Week = ?", [season, week])
    con.register("new_optlineups", df)
    con.execute("INSERT INTO main.optlineups SELECT * FROM new_optlineups")
    con.execute("COMMIT")

    after = con.execute(
        "SELECT count(*) FROM main.optlineups WHERE Season = ? AND Week = ?", [season, week]
    ).fetchone()[0]
    print(f"{database}.main.optlineups: replaced {before} Season={season} Week={week} rows with {after} new rows")
    con.close()


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
    parser.add_argument("--database", default="td_backup_2025",
                         help="MotherDuck database to write to (use a scratch/backup db for testing - "
                              "production is 'td', never the default)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    args = parser.parse_args()

    season, week = args.season, args.week
    if season is None or week is None:
        detected_season, detected_week = get_calendar_week(which=args.which)
        season = season or detected_season
        week = week or detected_week

    begin_time = datetime.datetime.now()
    rostered_raw = scrape_rosters(season, week)
    rostered_df = build_rostered_table(rostered_raw, season, week)
    optlineups_df = build_optlineups_table(rostered_raw, season, week)

    assert list(rostered_df.columns) == ROSTERED_COLUMNS, "rostered output columns drifted from td.main.rostered's schema"
    assert list(optlineups_df.columns) == OPTLINEUPS_COLUMNS, "optlineups output columns drifted from td.main.optlineups's schema"

    # Debug side effect only, matching fantasy.py/weekly.py's own CSV writes -
    # never read back by anything downstream, just there to eyeball after an
    # automated run.
    rostered_df.to_csv("data/opt_lineups_rostered_scraperesult.csv", index=False)
    optlineups_df.to_csv("data/opt_lineups_optlineups_scraperesult.csv", index=False)

    upsert_rostered(rostered_df, args.database, season, week, dry_run=args.dry_run)
    upsert_optlineups(optlineups_df, args.database, season, week, dry_run=args.dry_run)
    print(f"execution time: {datetime.datetime.now() - begin_time}")

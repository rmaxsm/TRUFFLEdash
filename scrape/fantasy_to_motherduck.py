"""Scrapes a single week's TRUFFLE + KERFUFFLE starting lineups (fantasy.py),
builds td.main.fantasy's composite keys, and upserts into MotherDuck -
touching only that (Season, Week)'s rows, leaving every other week's already-
archived history untouched.

td.main.fantasy is a lineup/ownership record (who started that week, not
their stats - see weekly_to_motherduck.py for real per-week NFL stats), so
unlike weekly there's no preseason-placeholder concern here: fantasy.py never
drops rows for missing stats, since this table never carried stat columns to
begin with.

Usage:
    MOTHERDUCK_TOKEN=... python scrape/fantasy_to_motherduck.py --season 2026 --week 1 [--database td_backup_2025] [--dry-run]
"""

import argparse

import duckdb

from cbs_scrape_utils import get_calendar_week
from fantasy import scrape_fantasy

TARGET_COLUMNS = [
    "SznWkPlPos", "SznPlPos", "PlPos", "SznLgTrf", "LgTrf",
    "Season", "Week", "Player", "Pos", "NFL", "TrfLg", "TrfTm",
]


def build_fantasy_table(df, season: int, week: int):
    df = df.copy()
    season_str = str(season)
    week_str = str(week)
    player_nospace = df["Player"].str.replace(" ", "", regex=False)

    df["SznWkPlPos"] = season_str + "_" + week_str + "_" + player_nospace + "_" + df["Pos"]
    df["SznPlPos"] = season_str + "_" + player_nospace + "_" + df["Pos"]
    df["PlPos"] = player_nospace + "_" + df["Pos"]
    df["SznLgTrf"] = season_str + "_" + df["League"] + "_" + df["TrfTm"]
    df["LgTrf"] = df["League"] + "_" + df["TrfTm"]
    df["TrfLg"] = df["League"]
    df["Season"] = float(season)
    df["Week"] = float(week)

    return df[TARGET_COLUMNS]


def upsert_fantasy(df, database: str, season: int, week: int, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute(
        "SELECT count(*) FROM main.fantasy WHERE Season = ? AND Week = ?", [season, week]
    ).fetchone()[0]

    if dry_run:
        print(f"[dry-run] would delete {before} existing Season={season} Week={week} rows from "
              f"{database}.main.fantasy and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    con.execute("DELETE FROM main.fantasy WHERE Season = ? AND Week = ?", [season, week])
    con.register("new_fantasy", df)
    con.execute("INSERT INTO main.fantasy SELECT * FROM new_fantasy")
    con.execute("COMMIT")

    after = con.execute(
        "SELECT count(*) FROM main.fantasy WHERE Season = ? AND Week = ?", [season, week]
    ).fetchone()[0]
    print(f"{database}.main.fantasy: replaced {before} Season={season} Week={week} rows with {after} new rows")
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

    fantasy_df = scrape_fantasy(season, week)
    keyed = build_fantasy_table(fantasy_df, season, week)

    assert list(keyed.columns) == TARGET_COLUMNS, "output columns drifted from td.main.fantasy's schema"

    # Debug side effect only, matching fantasy.py/weekly.py's own CSV writes -
    # never read back by anything downstream, just there to eyeball after an
    # automated run.
    keyed.to_csv("data/fantasy_scraperesult.csv", index=False)

    upsert_fantasy(keyed, args.database, season, week, dry_run=args.dry_run)

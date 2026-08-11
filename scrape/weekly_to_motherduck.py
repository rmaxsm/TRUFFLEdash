"""Scrapes a single week (weekly.py + extra_dash.py), merges them
(weekly_transform.py), and upserts into MotherDuck's `weekly` table -
touching only that (Season, Week)'s rows, leaving every other week's already-
archived history untouched.

This is the single most important table in the whole pipeline (real per-week
NFL player stats dating back to 2020), so this script is deliberately
conservative:
  - --database defaults to a non-production db - production `td` must be
    passed explicitly.
  - --allow-preseason-placeholder (passed through to weekly.py/extra_dash.py)
    is hard-refused if --database resolves to "td", regardless of anything
    else - synthetic test data must never reach production.

Usage:
    MOTHERDUCK_TOKEN=... python scrape/weekly_to_motherduck.py --season 2026 --week 1 [--database td_backup_2025] [--dry-run]
"""

import argparse

import duckdb

from cbs_scrape_utils import get_calendar_week
from extra_dash import scrape_extra_dash
from weekly import scrape_weekly
from weekly_transform import TARGET_COLUMNS, build_weekly_table


def upsert_weekly(df, database: str, season: int, week: int, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute(
        "SELECT count(*) FROM main.weekly WHERE Season = ? AND Week = ?", [season, week]
    ).fetchone()[0]

    if dry_run:
        print(f"[dry-run] would delete {before} existing Season={season} Week={week} rows from "
              f"{database}.main.weekly and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    con.execute("DELETE FROM main.weekly WHERE Season = ? AND Week = ?", [season, week])
    con.register("new_weekly", df)
    con.execute("INSERT INTO main.weekly SELECT * FROM new_weekly")
    con.execute("COMMIT")

    after = con.execute(
        "SELECT count(*) FROM main.weekly WHERE Season = ? AND Week = ?", [season, week]
    ).fetchone()[0]
    print(f"{database}.main.weekly: replaced {before} Season={season} Week={week} rows with {after} new rows")
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
                              "normal cadence, including the Wednesday correction run; 'previous' is a "
                              "manual escape hatch only.")
    parser.add_argument("--database", default="td_backup_2025",
                         help="MotherDuck database to write to (use a scratch/backup db for testing - "
                              "production is 'td', never the default)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    parser.add_argument("--allow-preseason-placeholder", action="store_true",
                         help="If every scraped row is Avg='-' (no games played yet this season), write "
                              "synthetic placeholder data instead of an empty result. Refused outright if "
                              "--database is 'td'.")
    args = parser.parse_args()

    if args.allow_preseason_placeholder and args.database == "td":
        raise SystemExit(
            "--allow-preseason-placeholder can never be combined with --database td - synthetic "
            "placeholder data must not reach production. Use a scratch/backup database."
        )

    season, week = args.season, args.week
    if season is None or week is None:
        detected_season, detected_week = get_calendar_week(which=args.which)
        season = season or detected_season
        week = week or detected_week

    weekly_df = scrape_weekly(season, week, allow_preseason_placeholder=args.allow_preseason_placeholder)
    extradash_df = scrape_extra_dash(season, week, allow_preseason_placeholder=args.allow_preseason_placeholder)
    merged = build_weekly_table(weekly_df, extradash_df, season, week)

    assert list(merged.columns) == TARGET_COLUMNS, "merged output columns drifted from td.main.weekly's schema"

    # Debug side effect only, matching weekly.py/extra_dash.py's own CSV
    # writes - never read back by anything downstream, just there to eyeball
    # after an automated run.
    merged.to_csv("data/weekly_merged_scraperesult.csv", index=False)

    upsert_weekly(merged, args.database, season, week, dry_run=args.dry_run)

"""Upserts one season's worth of data/snapPer.csv into MotherDuck's `snaps` table.

Unlike backfill_snaps_2020_2025.py (one-time full-table replace), this only
touches the current scraped season's rows - delete + insert by Season, same
pattern as rosters_to_motherduck.py - leaving all other seasons' history
untouched.

Usage:
    MOTHERDUCK_TOKEN=... python scrape/snaps_to_motherduck.py --season 2026 [--database td] [--dry-run]
"""

import argparse

import duckdb
import pandas as pd

from cbs_scrape_utils import get_current_season_week
from snaps_transform import build_snaps_dataframe


def build_season_snaps(season: int, csv_path: str = "data/snapPer.csv") -> pd.DataFrame:
    df = pd.read_csv(csv_path)
    df = df[df["Season"].astype(str) == str(season)].copy()
    df = df.rename(columns={"Team": "NFL", "TTL": "Total"})
    df = df.rename(columns={str(i): f"Wk{i}" for i in range(1, 19)})
    return build_snaps_dataframe(df)


def upsert_snaps(df: pd.DataFrame, season: int, database: str, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute("SELECT count(*) FROM main.snaps WHERE Season = ?", [season]).fetchone()[0]

    if dry_run:
        print(f"[dry-run] would delete {before} existing Season={season} rows from "
              f"{database}.main.snaps and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    con.execute("DELETE FROM main.snaps WHERE Season = ?", [season])
    con.register("new_snaps", df)
    con.execute("INSERT INTO main.snaps SELECT * FROM new_snaps")
    con.execute("COMMIT")

    after = con.execute("SELECT count(*) FROM main.snaps WHERE Season = ?", [season]).fetchone()[0]
    print(f"{database}.main.snaps: replaced {before} Season={season} rows with {after} new rows")
    con.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--season", type=int, default=None,
                         help="Season to upsert (default: auto-detect from today's date via sundates.csv)")
    parser.add_argument("--database", default="td", help="MotherDuck database to write to (use a scratch/backup db for testing)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    args = parser.parse_args()

    season = args.season or get_current_season_week()[0]
    df = build_season_snaps(season)
    upsert_snaps(df, season, args.database, dry_run=args.dry_run)

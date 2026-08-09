"""One-time backfill: replaces MotherDuck's `snaps` table (2020-2025) from a
manually-exported FantasyPros CSV, and adds the new `Total` column (raw season
snap count - not previously tracked in MotherCB, per instruction).

Cross-referenced against td.main.weekly (Season+NFL+Pos matched) before this
was written - see snaps_name_aliases.py for the name-alias findings. Because
this replaces the *entire* table (not just new seasons), it uses
CREATE OR REPLACE TABLE rather than a delete+insert, since the schema itself
is changing (new Total column).

Usage:
    MOTHERDUCK_TOKEN=... python scrape/backfill_snaps_2020_2025.py <csv_path> [--database td] [--dry-run]
"""

import argparse

import duckdb
import pandas as pd

from snaps_transform import build_snaps_dataframe, WK_COLS


def build_snaps_backfill(csv_path: str) -> pd.DataFrame:
    df = pd.read_csv(csv_path)
    df = df.rename(columns={"SEASON": "Season", "PLAYER": "Player", "POS": "Pos", "TEAM": "NFL", "TTL": "Total"})
    df = df.rename(columns={f"{i}.00": f"Wk{i}" for i in range(1, 19)})
    return build_snaps_dataframe(df)


def replace_snaps_table(df: pd.DataFrame, database: str, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute("SELECT count(*) FROM main.snaps").fetchone()[0]

    if dry_run:
        print(f"[dry-run] would replace {before} existing rows in {database}.main.snaps "
              f"with {len(df)} new rows (schema: {list(df.columns)})")
        con.close()
        return

    con.register("new_snaps", df)
    con.execute("CREATE OR REPLACE TABLE main.snaps AS SELECT * FROM new_snaps")

    after = con.execute("SELECT count(*) FROM main.snaps").fetchone()[0]
    print(f"{database}.main.snaps: replaced {before} rows with {after} new rows")
    con.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("csv_path", help="Path to the FantasyPros historical snaps CSV export")
    parser.add_argument("--database", default="td", help="MotherDuck database to write to (use a scratch/backup db for testing)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    args = parser.parse_args()

    df = build_snaps_backfill(args.csv_path)
    replace_snaps_table(df, args.database, dry_run=args.dry_run)

"""Builds the CBS player ID crosswalk and replaces MotherDuck's `ids` table.

Unlike rosters (which has a Season dimension and only touches the current
season's rows), `ids` is a flat, timeless Player->cbsID crosswalk - there's no
history to preserve, so this does a full-table replace each run, matching the
existing R pipeline's overwrite=TRUE behavior.

Usage:
    MOTHERDUCK_TOKEN=... python scrape/ids_to_motherduck.py [--database td] [--dry-run]
"""

import argparse

import duckdb
import pandas as pd

TARGET_COLUMNS = ["PlPos", "Player", "Pos", "NFL", "cbsID"]


def build_ids(playerids_csv: str) -> pd.DataFrame:
    df = pd.read_csv(playerids_csv)
    df = df.rename(columns={"playerID": "cbsID"})
    df["cbsID"] = df["cbsID"].astype(str)

    player_nospace = df["Player"].str.replace(" ", "", regex=False)
    df["PlPos"] = player_nospace + "_" + df["Pos"]

    return df[TARGET_COLUMNS]


def replace_ids(df: pd.DataFrame, database: str, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute("SELECT count(*) FROM main.ids").fetchone()[0]

    if dry_run:
        print(f"[dry-run] would replace {before} existing rows in "
              f"{database}.main.ids with {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    con.execute("DELETE FROM main.ids")
    con.register("new_ids", df)
    con.execute("INSERT INTO main.ids SELECT * FROM new_ids")
    con.execute("COMMIT")

    after = con.execute("SELECT count(*) FROM main.ids").fetchone()[0]
    print(f"{database}.main.ids: replaced {before} rows with {after} new rows")
    con.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--database", default="td", help="MotherDuck database to write to (use a scratch/backup db for testing)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    args = parser.parse_args()

    df = build_ids("data/playerIDs.csv")
    replace_ids(df, args.database, dry_run=args.dry_run)

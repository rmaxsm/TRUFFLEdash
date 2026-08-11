"""Upserts one season's worth of data/espnStats.csv into MotherDuck's `espn` table.

Season-scoped delete + insert, same pattern as rosters_to_motherduck.py /
snaps_to_motherduck.py - leaves all other seasons' history untouched.

Usage:
    MOTHERDUCK_TOKEN=... python scrape/espn_to_motherduck.py [--season 2025] [--database td] [--dry-run]
"""

import argparse

import duckdb
import pandas as pd

from espn_urls import get_season_from_urls, get_urls

TARGET_COLUMNS = [
    "SznPlPos", "PlPos", "Season", "Player", "Pos",
    "xFPts", "FPts", "FPdiff", "xTD", "TD", "TDdiff", "Looks", "RuIn5", "EzTar",
]


def build_season_espn(season: int, csv_path: str = "data/espnStats.csv") -> pd.DataFrame:
    df = pd.read_csv(csv_path)
    df = df[df["Season"].astype(str) == str(season)].copy()

    df["FPdiff"] = df["ActualPts"] - df["xFP"]

    player_nospace = df["Player"].str.replace(" ", "", regex=False)
    df["SznPlPos"] = df["Season"].astype(str) + "_" + player_nospace + "_" + df["Pos"]
    df["PlPos"] = player_nospace + "_" + df["Pos"]

    df = df.rename(columns={
        "xFP": "xFPts", "ActualPts": "FPts", "Diff": "TDdiff",
        "In5": "RuIn5", "EZ": "EzTar",
    })
    df["Season"] = df["Season"].astype(float)

    return df[TARGET_COLUMNS]


def upsert_espn(df: pd.DataFrame, season: int, database: str, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute("SELECT count(*) FROM main.espn WHERE Season = ?", [season]).fetchone()[0]

    if dry_run:
        print(f"[dry-run] would delete {before} existing Season={season} rows from "
              f"{database}.main.espn and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    con.execute("DELETE FROM main.espn WHERE Season = ?", [season])
    con.register("new_espn", df)
    con.execute("INSERT INTO main.espn SELECT * FROM new_espn")
    con.execute("COMMIT")

    after = con.execute("SELECT count(*) FROM main.espn WHERE Season = ?", [season]).fetchone()[0]
    print(f"{database}.main.espn: replaced {before} Season={season} rows with {after} new rows")
    con.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--season", type=int, default=None,
                         help="Season to upsert (default: derived directly from the ESPN URLs themselves)")
    parser.add_argument("--database", default="td", help="MotherDuck database to write to (use a scratch/backup db for testing)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    args = parser.parse_args()

    season = args.season or get_season_from_urls(get_urls())
    df = build_season_espn(season)
    upsert_espn(df, season, args.database, dry_run=args.dry_run)

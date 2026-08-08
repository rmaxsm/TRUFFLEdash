"""Combines the TRUFFLE + KERFUFFLE roster scrape CSVs and upserts the current
season's rows into MotherDuck's `rosters` table.

Ports the logic from global.R's rosters.csv/oldrosters.csv merge (see
truffledash.com/R to Python Scripts/rosters.R) with one key change: rather than
re-reading and re-appending the full oldrosters.csv archive on every run (and
tagging it with a hardcoded season), this only touches the current season's
rows in MotherDuck directly - computed dynamically, not hardcoded - and leaves
every prior season's already-archived rows untouched.

Usage:
    MOTHERDUCK_TOKEN=... python scrape/rosters_to_motherduck.py [--database td] [--dry-run]
"""

import argparse
import os

import duckdb
import pandas as pd

from cbs_scrape_utils import get_current_season

TARGET_COLUMNS = [
    "SznLgTrf", "LgTrf", "TrfLg", "TrfTm", "SznPlPos", "PlPos",
    "Season", "Player", "Pos", "NFL", "Salary", "Contract",
]


def build_current_season_rosters(trf_csv: str, krf_csv: str, season: int) -> pd.DataFrame:
    trf = pd.read_csv(trf_csv)
    trf["League"] = "TRUFFLE"

    krf = pd.read_csv(krf_csv)
    krf["League"] = "KERFUFFLE"
    krf.columns = trf.columns  # colname discrepancy fix, matching global.R

    combined = pd.concat([trf, krf], ignore_index=True)
    combined = combined[["League", "TRUFFLE", "Pos", "Player", "NFL", "Salary", "Contract"]].copy()

    # Build the composite-key strings using the plain int season (so keys read
    # "2026_..." not "2026.0_..."), then cast Season/Contract to float at the
    # end to match the existing MotherDuck column types.
    season_str = str(season)
    player_nospace = combined["Player"].str.replace(" ", "", regex=False)
    combined["SznLgTrf"] = season_str + "_" + combined["League"] + "_" + combined["TRUFFLE"]
    combined["LgTrf"] = combined["League"] + "_" + combined["TRUFFLE"]
    combined["TrfLg"] = combined["League"]
    combined["TrfTm"] = combined["TRUFFLE"]
    combined["SznPlPos"] = season_str + "_" + player_nospace + "_" + combined["Pos"]
    combined["PlPos"] = player_nospace + "_" + combined["Pos"]

    combined["Season"] = float(season)
    combined["Contract"] = combined["Contract"].astype(float)
    combined["Salary"] = combined["Salary"].astype(float)

    return combined[TARGET_COLUMNS]


def upsert_rosters(df: pd.DataFrame, database: str, season: int, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    before = con.execute(
        "SELECT count(*) FROM main.rosters WHERE Season = ?", [season]
    ).fetchone()[0]

    if dry_run:
        print(f"[dry-run] would delete {before} existing Season={season} rows from "
              f"{database}.main.rosters and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    con.execute("DELETE FROM main.rosters WHERE Season = ?", [season])
    con.register("new_rosters", df)
    con.execute("INSERT INTO main.rosters SELECT * FROM new_rosters")
    con.execute("COMMIT")

    after = con.execute(
        "SELECT count(*) FROM main.rosters WHERE Season = ?", [season]
    ).fetchone()[0]
    print(f"{database}.main.rosters: replaced {before} Season={season} rows with {after} new rows")
    con.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--database", default="td", help="MotherDuck database to write to (use a scratch/backup db for testing)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    args = parser.parse_args()

    season = get_current_season()
    df = build_current_season_rosters("data/rosters.csv", "data/kerfuffle/kerfuffle_rosters.csv", season)
    upsert_rosters(df, args.database, season, dry_run=args.dry_run)

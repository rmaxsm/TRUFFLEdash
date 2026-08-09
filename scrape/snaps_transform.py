"""Shared snaps dataframe -> MotherDuck-ready-schema transform.

Used by both backfill_snaps_2020_2025.py (one-time historical load) and
snaps_to_motherduck.py (ongoing scraper's per-season upsert), so both paths
apply identical cleaning/key-building logic.
"""

import numpy as np
import pandas as pd

from cbs_scrape_utils import clean_player_name
from snaps_name_aliases import apply_name_aliases

WK_COLS = [f"Wk{i}" for i in range(1, 19)]
TARGET_COLUMNS = ["SznPlPos", "PlPos", "Season", "Player", "Pos", "NFL"] + WK_COLS + ["Avg", "Total"]


def build_snaps_dataframe(df: pd.DataFrame) -> pd.DataFrame:
    """df must already have columns: Season, Player, Pos, NFL, Wk1..Wk18
    (numeric or "BYE"/blank for missing weeks, on a 0-1 proportion scale),
    Total (raw season snap count). Player names get cleaned/aliased here;
    Avg is always recomputed from the weekly values, not trusted from source.
    """
    df = df.copy()
    df["Player"] = clean_player_name(df["Player"])
    df["Player"] = apply_name_aliases(df["Player"])

    for c in WK_COLS:
        df[c] = pd.to_numeric(df[c].astype(str).str.replace("BYE", "", case=False).replace("", np.nan), errors="coerce")
        df[c] = df[c].replace(0, np.nan)  # matches existing pipeline's 0 -> NA convention

    df["Avg"] = df[WK_COLS].mean(axis=1, skipna=True)

    player_nospace = df["Player"].str.replace(" ", "", regex=False)
    df["SznPlPos"] = df["Season"].astype(str) + "_" + player_nospace + "_" + df["Pos"]
    df["PlPos"] = player_nospace + "_" + df["Pos"]
    df["Season"] = df["Season"].astype(float)

    return df[TARGET_COLUMNS]

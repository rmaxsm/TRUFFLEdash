"""Scrapes ESPN's expected-fantasy-points (xFP) leaderboards for QB/RB/WR/TE
and the expected-touchdowns (xTD) leaderboard, combining them into
data/espnStats.csv.

These are "living" articles ESPN republishes each season under a brand-new,
unpredictable article ID (confirmed no auto-derivable URL pattern across
2024 vs 2025, or even across positions within the same year), but they get
updated continuously through that season, so the 5 URLs only need refreshing
once per year at the start of the season - see espn_urls.py.

No login/cookies needed - verified directly with a plain unauthenticated
request that these articles are fully public (no ESPN+ paywall currently in
effect), unlike the old scripts' now-irrelevant hardcoded 2022 cookies.

Writes data/espnStats.csv, replacing only the scraped season's rows (backing
up the previous version to data/backup/espnStats_backup.csv first).
"""

import argparse

import numpy as np
import pandas as pd
import requests
from bs4 import BeautifulSoup

from cbs_scrape_utils import clean_player_name
from espn_urls import get_season_from_urls, get_urls

REQUEST_HEADERS = {"User-Agent": "Mozilla/5.0"}

# Jeffery Simmons (misspelled on ESPN's own site, not a typo here - a DT who
# gets specialty goal-line package work, which breaks consistency in a
# skill-position-focused leaderboard. Documented historical exclusion.
EXCLUDED_PLAYERS = {"Jeffery Simmons"}


def _fetch_table(url: str):
    response = requests.get(url, headers=REQUEST_HEADERS, timeout=30)
    response.raise_for_status()
    soup = BeautifulSoup(response.content, "html.parser")
    article = soup.find("section", {"id": "article-feed"})
    asides = article.find_all("aside", class_="inline inline-table")
    return asides[1].find("table")


def _parse_header_cells(table) -> list:
    """Returns one name per <th>, including a placeholder for blank spacer
    columns - keeping the count aligned with each row's actual <td> count
    (a blank header still has a corresponding, real, blank data cell)."""
    cols = []
    for i, th in enumerate(table.find_all("th")):
        text = th.get_text().strip()
        if "." in text:
            text = text.split(".", 1)[1].strip()
        if text == "Actual Pts":  # TE page quirk: this header has a space, others don't
            text = "ActualPts"
        cols.append(text or f"_blank{i}")
    return cols


def scrape_position_xfp(url: str, pos: str) -> pd.DataFrame:
    table = _fetch_table(url)
    cols = _parse_header_cells(table)  # ["Player", "xFP", "ActualPts", "FORP", "G", ...position-specific stats]

    rows = []
    for tr in table.find_all("tr", class_="last"):
        cells = tr.find_all("td")
        name_team = cells[0].get_text().split(",")
        team = name_team[-1].strip()
        name = name_team[0].split(" ", 1)[-1]  # drop leading rank prefix, e.g. "1. Trevor Lawrence"
        values = [c.get_text().strip() for c in cells[1:]]
        rows.append([name, team] + values)

    df = pd.DataFrame(rows, columns=["Player", "NFL"] + cols[1:])
    df["Pos"] = pos
    df["xFP"] = pd.to_numeric(df["xFP"], errors="coerce")
    df["ActualPts"] = pd.to_numeric(df["ActualPts"], errors="coerce")
    return df[["Player", "NFL", "Pos", "xFP", "ActualPts"]]


def scrape_xtd(url: str) -> pd.DataFrame:
    table = _fetch_table(url)
    cols = _parse_header_cells(table)  # ["Player", "Looks", "TD", "xTD", "Diff", "In5", "EZ"]

    rows = []
    for tr in table.find_all("tr", class_="last"):
        cells = tr.find_all("td")
        name = cells[0].get_text().split(" ", 1)[-1]  # drop leading rank prefix
        values = [c.get_text().strip() for c in cells[1:]]
        rows.append([name] + values)

    df = pd.DataFrame(rows, columns=["Player"] + cols[1:])
    for c in ["Looks", "TD", "xTD", "Diff", "In5", "EZ"]:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    return df


def scrape_espn_stats(season: int, urls: dict) -> pd.DataFrame:
    position_frames = [
        scrape_position_xfp(urls[pos], pos) for pos in ["QB", "RB", "WR", "TE"]
    ]
    combined = pd.concat(position_frames, ignore_index=True, sort=False)

    xtd = scrape_xtd(urls["XTD"])
    merged = pd.merge(combined, xtd, on="Player", how="outer")

    merged["Player"] = clean_player_name(merged["Player"])
    merged = merged[~merged["Player"].isin(EXCLUDED_PLAYERS)]

    # trade dedup: a player who changed teams mid-season shows up as one row
    # per team-stint in the position-xFP pages (xFP/ActualPts split by team
    # and need summing), but only once in the xTD page (no team column there,
    # so after the outer merge that single xTD/TD/Looks/Diff/In5/EZ value
    # gets duplicated onto every one of that player's rows - max, not sum,
    # avoids double-counting an already-whole-season number).
    agg = merged.assign(Season=season).groupby(["Season", "Pos", "Player"], as_index=False).agg(
        xFP=("xFP", "sum"),
        ActualPts=("ActualPts", "sum"),
        xTD=("xTD", "max"),
        TD=("TD", "max"),
        Looks=("Looks", "max"),
        Diff=("Diff", "max"),
        In5=("In5", "max"),
        EZ=("EZ", "max"),
    )

    agg = agg.replace([np.inf, -np.inf], 0).fillna(0)
    return agg[["Season", "Player", "Pos", "xFP", "ActualPts", "xTD", "TD", "Looks", "Diff", "In5", "EZ"]]


def splice_into_master(df: pd.DataFrame, season: int, filepath: str = "data/espnStats.csv") -> pd.DataFrame:
    """Replaces the scraped season's rows in the master CSV, leaving other seasons untouched."""
    master = pd.read_csv(filepath)

    backup_path = "data/backup/espnStats_backup.csv"
    master.to_csv(backup_path, index=False)
    print(f"backed up existing espnStats.csv to {backup_path}")

    master = master[master["Season"].astype(str) != str(season)]
    updated = pd.concat([master, df], ignore_index=True)
    updated.to_csv(filepath, index=False)
    print(f"espnStats.csv updated: {len(df)} rows for season {season} (file now {len(updated)} rows total)")
    return updated


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--season", type=int, default=None,
                         help="Season to scrape (default: derived directly from the URLs themselves)")
    args = parser.parse_args()

    urls = get_urls()
    season = args.season or get_season_from_urls(urls)
    print(f"scraping ESPN stats for season {season}")

    scraped = scrape_espn_stats(season, urls)
    splice_into_master(scraped, season)

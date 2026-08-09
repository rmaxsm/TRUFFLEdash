"""Scrapes FantasyPros' snap-count-percentage report for a single season.

FantasyPros rebuilt this report as a fully client-rendered page - the table
doesn't exist in the server HTML at all (confirmed: identical result with or
without valid auth cookies via plain `requests`), so unlike every other
scraper in this project, this one drives a real headless browser (Playwright)
to render the page and read the table out of the live DOM.

Requires a logged-in FantasyPros session (a free account only shows ~10 rows;
this project uses a paid account) - see fantasypros_auth.py for how cookies
are supplied. A real requests-based login isn't attempted for the same reason
as CBS: FantasyPros' login page also loads Google reCAPTCHA Enterprise.

Writes data/snapPer.csv, replacing only the scraped season's rows (backing up
the previous version to data/backup/snapPer_backup.csv first) - same
season-scoped splice behavior as the original dre/FantasyPros/snapCountPer.py.
"""

import argparse
import datetime

import pandas as pd
from playwright.sync_api import sync_playwright

from cbs_scrape_utils import clean_player_name, get_current_season_week
from fantasypros_auth import get_cookies_and_headers, to_playwright_cookies
from snaps_name_aliases import apply_name_aliases

WK_COLS = [str(i) for i in range(1, 19)]


def scrape_snap_counts(season: int) -> pd.DataFrame:
    cookies, headers = get_cookies_and_headers()

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        context = browser.new_context(user_agent=headers.get("user-agent"))
        context.add_cookies(to_playwright_cookies(cookies))
        page = context.new_page()

        url = f"https://www.fantasypros.com/nfl/reports/snap-counts/?year={season}&show=perc"
        # not "networkidle" - this page has continuous ad/analytics chatter
        # (Amazon ads, GA, etc.) that never goes quiet; wait for the actual
        # table content instead.
        page.goto(url, wait_until="domcontentloaded")
        page.wait_for_selector("table tbody tr", timeout=30000)

        rows = page.evaluate("""
            () => {
                const table = document.querySelector('table');
                const bodyRows = table.querySelectorAll('tbody tr');
                return Array.from(bodyRows).map(row =>
                    Array.from(row.querySelectorAll('td')).map(c => c.innerText.trim())
                );
            }
        """)
        browser.close()

    if not rows:
        raise RuntimeError(
            f"No rows scraped for season {season} - page may not have rendered, "
            f"or the FantasyPros session may be logged out/expired"
        )

    columns = ["Player", "Pos", "Team"] + WK_COLS + ["TTL", "AVG"]
    df = pd.DataFrame(rows, columns=columns)
    df.insert(0, "Season", season)
    df = df.rename(columns={"Team": "NFL", "TTL": "Total"})

    # clean names before writing the raw CSV, same convention as every other
    # scraper (rosters.py, playerIDs.py) - not deferred to the MotherDuck
    # transform step, so data/snapPer.csv itself always has correct names.
    df["Player"] = clean_player_name(df["Player"])
    df["Player"] = apply_name_aliases(df["Player"])

    # normalize source formatting: strip "%" and convert to 0-1 proportions,
    # strip comma-thousands-separators from Total. "BYE" (and anything else
    # non-numeric) coerces to NaN here via pd.to_numeric - snaps_transform.py
    # then also converts 0 -> NaN downstream, matching the existing pipeline.
    for c in WK_COLS:
        df[c] = pd.to_numeric(df[c].str.rstrip("%"), errors="coerce") / 100
    df["Total"] = pd.to_numeric(df["Total"].str.replace(",", "", regex=False), errors="coerce")
    df["AVG"] = pd.to_numeric(df["AVG"].str.rstrip("%"), errors="coerce") / 100

    df = df.rename(columns={str(i): f"Wk{i}" for i in range(1, 19)})
    return df[["Season", "Player", "Pos", "NFL"] + [f"Wk{i}" for i in range(1, 19)] + ["Total", "AVG"]]


def splice_into_master(df: pd.DataFrame, season: int, filepath: str = "data/snapPer.csv") -> pd.DataFrame:
    """Replaces the scraped season's rows in the master CSV, leaving other seasons untouched."""
    master = pd.read_csv(filepath)

    backup_path = "data/backup/snapPer_backup.csv"
    master.to_csv(backup_path, index=False)
    print(f"backed up existing snapPer.csv to {backup_path}")

    df_for_master = df.rename(columns={f"Wk{i}": str(i) for i in range(1, 19)})
    df_for_master = df_for_master.rename(columns={"NFL": "Team", "Total": "TTL"})
    df_for_master = df_for_master[master.columns]

    master = master[master["Season"].astype(str) != str(season)]
    updated = pd.concat([master, df_for_master], ignore_index=True)
    updated.to_csv(filepath, index=False)
    print(f"snapPer.csv updated: {len(df_for_master)} rows for season {season} (file now {len(updated)} rows total)")
    return updated


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--season", type=int, default=None,
                         help="Season to scrape (default: auto-detect from today's date via sundates.csv)")
    args = parser.parse_args()

    season = args.season or get_current_season_week()[0]
    print(f"scraping snap counts for season {season}")

    scraped = scrape_snap_counts(season)
    splice_into_master(scraped, season)

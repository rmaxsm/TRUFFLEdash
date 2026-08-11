"""ESPN xFP/xTD leaderboard article URLs - refreshed once per year.

These are "living" articles ESPN republishes each season under a brand-new,
unpredictable numeric ID (confirmed: no derivable pattern year-to-year, or
even across positions within the same year - see scrape/espn.py's docstring).
Update DEFAULT_URLS below once a year when ESPN publishes the new season's
leaderboards (find them at espn.com/fantasy/football/story/_/id/.../
...-expected-fantasy-points-xfp-<pos> and .../...-expected-td-opportunity-xtd -
searching "espn fantasy football expected fantasy points <year> <position>"
finds them quickly). No login needed - confirmed these are fully public.

Each can also be overridden per-run via environment variables (how GitHub
Actions workflow_dispatch inputs / repo variables get in) without editing
this file - useful for testing next year's URLs before committing them.
"""

import os
import re

DEFAULT_URLS = {
    "QB": "https://www.espn.com/fantasy/football/story/_/id/46168860/2025-fantasy-football-expected-fantasy-points-xfp-qb",
    "RB": "https://www.espn.com/fantasy/football/story/_/id/46168913/2025-fantasy-football-expected-fantasy-points-xfp-rb",
    "WR": "https://www.espn.com/fantasy/football/story/_/id/46168948/fantasy-football-2025-expected-fantasy-points-xfp-wr",
    "TE": "https://www.espn.com/fantasy/football/story/_/id/46169084/2025-fantasy-football-expected-fantasy-points-xfp-te",
    "XTD": "https://www.espn.com/fantasy/football/story/_/id/46168468/2025-fantasy-football-rankings-nfl-expected-td-opportunity-xtd",
}

ENV_VAR_NAMES = {
    "QB": "ESPN_QB_XFP_URL",
    "RB": "ESPN_RB_XFP_URL",
    "WR": "ESPN_WR_XFP_URL",
    "TE": "ESPN_TE_XFP_URL",
    "XTD": "ESPN_XTD_URL",
}


def get_urls() -> dict:
    return {
        key: os.environ.get(ENV_VAR_NAMES[key]) or DEFAULT_URLS[key]
        for key in DEFAULT_URLS
    }


def get_season_from_urls(urls: dict) -> int:
    """Derives the season directly from the URLs themselves (they always
    embed the season year in the slug, e.g. ".../2025-fantasy-football-...")
    rather than inferring it from today's date - more robust, since it ties
    the season label to whatever's actually being scraped, not a guess."""
    years = set()
    for url in urls.values():
        match = re.search(r"20\d{2}", url)
        if not match:
            raise ValueError(f"Could not find a season year in URL: {url}")
        years.add(int(match.group()))
    if len(years) > 1:
        raise ValueError(f"URLs point to different seasons: {years} - check espn_urls.py / env vars")
    return years.pop()

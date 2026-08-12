"""Thin client for pulling reference data out of Omni's Query API.

Omni's CSV-upload feature creates a brand-new warehouse table (with a fresh
uuid suffix) every time the source file is re-saved, so a script can't hold
onto a stable table name the way it could with a checked-in CSV. Omni's own
topic layer stays stable across re-uploads though - the "teamscsv" topic name
never changes even as the table backing it does - so this queries through
Omni's REST API instead of MotherDuck directly.
"""

import os

import pandas as pd
import requests

OMNI_BASE_URL = "https://truffledash.omniapp.co/api"
MODEL_ID = "64f73f20-4264-4329-bc52-82adccfc7520"  # the "td" model

# teamscsv.view field id -> (label Omni's JSON result keys by, column name the
# existing scrapers expect - matches the old checked-in data/teams.csv schema
# so callers don't need to change).
TEAMS_FIELDS = {
    "teamscsv.trf_lg": ("TRF League", "League"),
    "teamscsv.trf_tm": ("TRF Team", "Abbrev"),
    "teamscsv.lg_trf": ("LgTrf", "LgTrf"),
    "teamscsv.full_team_name": ("Full Team Name", "FullName"),
    "teamscsv.logs": ("Logs", "Logs"),
    "teamscsv.logs_scrape": ("Logs Scrape", "LogsScrape"),
    "teamscsv.mascot": ("Mascot", "Mascot"),
    "teamscsv.location": ("Location", "Location"),
    "teamscsv.owner": ("Owner", "Owner"),
    "teamscsv.yr_founded": ("Year Founded", "YrFounded"),
    "teamscsv.division": ("Division", "Division"),
    "teamscsv.division_abbrev": ("Division Abbreviation", "DivisionAbbrev"),
    "teamscsv.riv_name": ("Rivalry Name", "RivName"),
    "teamscsv.riv_abbrev": ("Rivalry Abbreviation", "RivAbbrev"),
    "teamscsv.team_num": ("TeamNum", "TeamNum"),
}


class OmniQueryError(RuntimeError):
    """Raised when the Omni Query API call fails or returns no usable result."""


def _api_key() -> str:
    key = os.environ.get("OMNI_API_KEY")
    if key:
        return key
    if os.path.exists("credentials/omni_api_key.txt"):
        with open("credentials/omni_api_key.txt") as f:
            return f.read().strip()
    raise OmniQueryError(
        "No Omni API key found - set OMNI_API_KEY or provide credentials/omni_api_key.txt"
    )


def get_teams_df() -> pd.DataFrame:
    """Team/league reference data from the live `teamscsv` topic in Omni.

    Replaces the old checked-in data/teams.csv (retired now that Omni's CSV
    upload feature owns this data) - columns are renamed to match what
    get_team_abbreviation_lookup() and other existing callers already expect.
    """
    response = requests.post(
        f"{OMNI_BASE_URL}/v1/query/run",
        headers={
            "Authorization": f"Bearer {_api_key()}",
            "Content-Type": "application/json",
        },
        json={
            "query": {
                "modelId": MODEL_ID,
                "table": "teamscsv",
                "fields": list(TEAMS_FIELDS),
                "limit": None,
            },
            "resultType": "json",
        },
        timeout=30,
    )
    response.raise_for_status()
    rows = response.json()
    if not isinstance(rows, list):
        raise OmniQueryError(f"Unexpected Omni response shape: {rows}")

    label_to_column = {label: column for label, column in TEAMS_FIELDS.values()}
    return pd.DataFrame(rows).rename(columns=label_to_column)

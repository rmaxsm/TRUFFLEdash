"""Shared CBS Sports session/auth loading for TRUFFLE and KERFUFFLE scrapers.

A real `requests`-based programmatic login was spiked and confirmed infeasible:
CBS's login flow requires clearing a Google reCAPTCHA Enterprise risk check
(POST /api/authex/verify-recaptcha) whose score/token can only be produced by
actually executing Google's JS challenge in a real browser - not something a
plain HTTP client can fake. The existing cookie-based approach has worked
reliably for years, so this module formalizes that approach instead of
replacing it: cookies/headers come from a real logged-in browser session,
refreshed manually on a periodic cadence.

Cookies/headers for each league are loaded from (in priority order):
  1. Environment variables CBS_<LEAGUE>_COOKIES_JSON / CBS_<LEAGUE>_HEADERS_JSON
     (JSON-encoded strings) - this is how GitHub Actions secrets get in.
  2. Local JSON files cookies/<league>_cookies.json / cookies/<league>_headers.json -
     for local development, refreshed by hand from a browser session.
"""

import json
import os

import requests

LEAGUE_HOSTS = {
    "TRUFFLE": "theradicalultimatefflexperience.football.cbssports.com",
    "KERFUFFLE": "kerfuffle.football.cbssports.com",
}


class CbsAuthError(RuntimeError):
    """Raised when no usable CBS session could be constructed or the session looks stale."""


def _load_json_blob(env_var: str, file_path: str) -> dict:
    raw = os.environ.get(env_var)
    if raw:
        return json.loads(raw)
    if os.path.exists(file_path):
        with open(file_path, "r") as f:
            return json.load(f)
    raise CbsAuthError(
        f"No CBS auth data found: set {env_var} or provide {file_path}"
    )


def get_session(league: str) -> requests.Session:
    """Returns a requests.Session pre-loaded with cookies/headers for the given league."""
    league = league.upper()
    if league not in LEAGUE_HOSTS:
        raise CbsAuthError(f"Unknown league {league!r}, expected one of {list(LEAGUE_HOSTS)}")

    league_lower = league.lower()
    cookies = _load_json_blob(
        f"CBS_{league}_COOKIES_JSON", f"cookies/{league_lower}_cookies.json"
    )
    headers = _load_json_blob(
        f"CBS_{league}_HEADERS_JSON", f"cookies/{league_lower}_headers.json"
    )

    session = requests.Session()
    session.cookies.update(cookies)
    session.headers.update(headers)
    return session


def verify_session(session: requests.Session, league: str) -> None:
    """Raises CbsAuthError if the session doesn't look like a logged-in CBS session.

    A stale/expired session typically still returns HTTP 200 but serves a login
    page instead of the requested content, so a status-code check alone is not
    enough - this is exactly the failure mode that silently produced blank/garbage
    scrape output in the past.
    """
    league = league.upper()
    url = f"https://{LEAGUE_HOSTS[league]}/teams/all"
    response = session.get(url, timeout=30)
    if response.status_code != 200:
        raise CbsAuthError(
            f"{league} session check got HTTP {response.status_code} for {url}"
        )
    lowered = response.text.lower()
    if "sign in" in lowered or "log in" in lowered and "teams" not in lowered:
        raise CbsAuthError(
            f"{league} session looks logged out (login page returned) - cookies/headers need refreshing"
        )

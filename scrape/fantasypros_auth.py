"""FantasyPros session loading for Playwright - mirrors cbs_auth.py's pattern.

Same reasoning as CBS: a real requests-based programmatic login is infeasible
(FantasyPros' login page also loads Google reCAPTCHA Enterprise), so this
loads a real logged-in session's cookies instead, refreshed manually on a
periodic cadence via a browser session.

Cookies/headers are loaded from (in priority order):
  1. Environment variables FANTASYPROS_COOKIES_JSON / FANTASYPROS_HEADERS_JSON
     (JSON-encoded strings) - how GitHub Actions secrets get in.
  2. Local JSON files cookies/fantasypros_cookies.json / cookies/fantasypros_headers.json -
     for local development, refreshed by hand from a browser session.
"""

import json
import os


class FantasyProsAuthError(RuntimeError):
    pass


def _load_json_blob(env_var: str, file_path: str) -> dict:
    raw = os.environ.get(env_var)
    if raw:
        return json.loads(raw)
    if os.path.exists(file_path):
        with open(file_path, "r") as f:
            return json.load(f)
    raise FantasyProsAuthError(
        f"No FantasyPros auth data found: set {env_var} or provide {file_path}"
    )


def get_cookies_and_headers() -> tuple:
    cookies = _load_json_blob("FANTASYPROS_COOKIES_JSON", "cookies/fantasypros_cookies.json")
    headers = _load_json_blob("FANTASYPROS_HEADERS_JSON", "cookies/fantasypros_headers.json")
    return cookies, headers


def to_playwright_cookies(cookies: dict, domain: str = ".fantasypros.com") -> list:
    """Converts a flat {name: value} cookie dict into Playwright's expected
    list-of-dicts format for BrowserContext.add_cookies()."""
    return [
        {"name": name, "value": value, "domain": domain, "path": "/"}
        for name, value in cookies.items()
    ]

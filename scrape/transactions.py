"""Scrapes the full transaction history (adds/drops, trades, draft-pick
trades, dead-cap charges) for TRUFFLE + KERFUFFLE from CBS's transactions
log and upserts into td.main.transactions - one row per asset movement (a
player sign, a player drop, one team's side of a player trade, one pick's
movement in a trade, or a dead-cap charge starting/ending), not one row per
CBS table row.

CBS's page renders one <tr> per TEAM's leg of an event, with multiple
<br/>-separated actions inside a single cell when a waiver claim
(sign+drop) happened in one move. This module splits every <tr> back down
to one output row per asset movement, then re-groups those atomic rows into
whole events via TransactionID so a full trade (including any draft picks)
can be reconstructed:
  - a plain sign/drop/waiver-claim row's event is its own CBS row id (every
    <br/>-separated action inside one <tr> belongs to the same waiver claim)
  - a trade leg's (player or pick) event is the *timestamp* prefix shared by
    every leg of that trade - confirmed live: a 2-team player trade's two
    legs share one full row id, but a pick added to the same trade gets its
    own row id while still sharing the same timestamp prefix.
Timestamp-only grouping is not used for non-trade rows because CBS's
overnight waiver-run batch processes many unrelated teams' claims in the
same second - grouping ALL rows by timestamp would wrongly merge those.

Excluded entirely (bare roster-slot/lineup management, no ownership or
salary change at all - confirmed live even though the URL's "all_but_lineup"
filter is supposed to already exclude this category): "Activated",
"Benched", "Moved to {slot}".

Dead-cap charges ("{Player} Dead Cap" - a synthetic salary-cap-charge entry
for a player no longer on the actual roster) get their own TxnType
(DEAD_CAP_ADD/DEAD_CAP_DROP) rather than being folded into SIGN/DROP, since
they don't represent a real roster move. Two CBS renderings exist:
  - with a player link (2022+ observed): Pos/NFL are shown right on the page
    (e.g. "Brian Robinson Dead Cap RB • SF").
  - bare text, no link at all (2020 observed): CBS shows only "{Player} Dead
    Cap Added"/"Dropped" - no Pos/NFL anywhere on the page. Pos/NFL are
    backfilled from td.main.ids (a season-agnostic Player->Pos/NFL/cbsID
    reference table) by the real player's name.
Either way, PlayerID is always left null: even when a link is present, its
target is a synthetic placeholder CBS player page (e.g. .../1000000034),
not the real player's own id - PlPos (built from the REAL player name +
Pos, stripped of the "Dead Cap" suffix) is the reliable join key instead,
matching every other table's convention. The dollar Amount is never shown
on the transactions page itself for a dead-cap row - it's backfilled from
td.main.rosters' Salary column (keyed on the exact "{Player} Dead Cap"
string + Season, matching how rosters.py already stores these rows) when a
match exists; historical seasons that never got a matching rosters row for
that charge (dead-cap tracking predates thorough rosters coverage in some
older seasons) are left null rather than guessed.

Draft-pick identity: a pick is uniquely identified by (year, round,
original owner) - "2026 Round 3" alone is not unique, since every team in
the league has its own 3rd-round pick that year. CBS's pick text has two
mutually exclusive shapes:
  - "{year} Round {n} [(OriginalOwner)] - Draft Pick Traded from {team}" -
    a future/not-yet-run draft class. OriginalOwner is only present once the
    pick has already changed hands before this trade; when absent, the
    immediate sender ({team}) IS the original owner.
  - "Round {n}, (Overall Pick {x}) - Draft Pick Traded from {team}" - this
    season's own rookie draft, already run (no year shown at all - CBS
    means "the current season's draft", i.e. this scrape's own Season).
Verified stable back to TRUFFLE's 2020 season - no row mixes both shapes,
and no row has neither.

CBS numeric team ids (from a row's /teams/{N} href) are used to resolve the
row's OWN team, not the displayed name text - see
get_team_by_teamnum_lookup()'s docstring for why (franchise renames, e.g.
TRUFFLE's team id 12 was "Windy City Big Apples" in 2020, "Madison
Muskellunge"/MAM today). A trade's counterparty/original-owner team is only
ever given as free-text name (never an href), so it's resolved against a
lookup built from every /teams/{N} link *on that same page* first (which
correctly captures a since-renamed team's OLD displayed name, since that's
exactly what the historical page still shows), falling back to the current
Omni teamscsv names for the (normal, common) case where nothing renamed.

Usage:
    MOTHERDUCK_TOKEN=... python scrape/transactions.py [--season 2026] [--league TRUFFLE] \
        [--backfill] [--database td_backup_2025] [--dry-run]
"""

import argparse
import datetime
import re

import duckdb
import pandas as pd
from bs4 import BeautifulSoup

from cbs_auth import LEAGUE_HOSTS, get_session, verify_session
from cbs_scrape_utils import (
    clean_player_name,
    get_current_season,
    get_team_abbreviation_lookup,
    get_team_by_teamnum_lookup,
)
from omni_client import get_teams_df

# Hardcoded per-league backfill start (TRUFFLE's founding predates any
# scraper here; KERFUFFLE's founding year confirmed live - 2020/2022 both
# return CBS's "No Transactions for This Period" empty-state).
LEAGUE_START_SEASON = {"TRUFFLE": 2020, "KERFUFFLE": 2024}

# Enrichment lookups for dead-cap rows always come from production - these
# are stable reference/history tables, read-only, independent of whichever
# --database this run is writing scraped output to (e.g. a td_backup_2025
# test run should still resolve real Pos/Salary from live data).
ENRICHMENT_DATABASE = "td"

NO_TRANSACTIONS_TEXT = "No Transactions for This Period"
TEAM_HREF_RE = re.compile(r"^/teams/(\d+)$")
PLAYERPAGE_HREF_RE = re.compile(r"/playerpage/(\d+)")
SIGNED_RE = re.compile(r"^Signed for \$([\d.]+)$")
TRADED_FROM_RE = re.compile(r"^Traded from (.+)$")
DEAD_CAP_LINK_RE = re.compile(r"^(?P<name>.+?)\s+Dead Cap$")
DEAD_CAP_BARE_RE = re.compile(r"^(?P<name>.+?)\s+Dead Cap\s+(?P<verb>Added|Dropped)$")
EXCLUDED_EXACT = {"Activated", "Benched"}
EXCLUDED_PREFIX_RE = re.compile(r"^Moved to ")
PICK_RE = re.compile(
    r"^(?:(?P<year>\d{4})\s+)?Round\s+(?P<round>\d+)"
    r"(?:,\s*\(Overall Pick\s+(?P<overall>\d+)\))?"
    r"\s*(?:\((?P<orig>[^)]+)\))?"
    r"\s*-\s*Draft Pick Traded from\s+(?P<from>.+)$"
)

TARGET_COLUMNS = [
    "SznPlPos", "PlPos", "SznLgTrf", "LgTrf",
    "TransactionID", "RowID",
    "Season", "TrfLg", "TrfTm", "EffectiveWeek", "TxnDateTime",
    "TxnType", "AssetType",
    "Player", "PlayerID", "Pos", "NFL", "Amount",
    "CounterpartyTeam",
    "DraftPickSeason", "DraftPickRound", "DraftPickOriginalOwner", "DraftPickOverall",
    "AssetList",
]


def _normalize_ws(text: str) -> str:
    return re.sub(r"\s+", " ", text).strip()


POS_NFL_SEP_RE = re.compile(r"[•|]")


def _split_pos_nfl(text: str) -> tuple:
    """'RB • SF' -> ('RB', 'SF'). TRUFFLE's 2020 season used a literal '|'
    instead of the bullet CBS switched to from 2021 on - handle both rather
    than assuming the current separator held historically."""
    parts = POS_NFL_SEP_RE.split(text, maxsplit=1)
    pos = parts[0].strip()
    nfl = parts[1].strip() if len(parts) > 1 else ""
    return pos, nfl


def _split_into_segments(cell) -> list:
    """Splits a <td> whose content may have multiple <br/>-separated actions
    (a waiver claim's sign+drop pair) into one list-of-nodes per action."""
    for icon in cell.find_all("span", class_="playerIconsWrapper"):
        icon.decompose()

    segments, current = [], []
    for node in cell.contents:
        if getattr(node, "name", None) == "br":
            segments.append(current)
            current = []
        else:
            current.append(node)
    segments.append(current)
    return [seg for seg in segments if seg]


def _find_tag(segment, tag_name, class_name):
    return next(
        (n for n in segment if getattr(n, "name", None) == tag_name and class_name in (n.get("class") or [])),
        None,
    )


def _resolve_team(name: str, page_lookup: dict, global_lookup: dict, context: str) -> str:
    if name in page_lookup:
        return page_lookup[name]
    if name in global_lookup:
        return global_lookup[name]
    raise ValueError(f"Could not resolve team name {name!r} to an abbreviation ({context})")


def _build_page_name_lookup(table, teamnum_lookup: dict) -> dict:
    """Team display-name text -> Abbrev, built from this SAME page's own
    /teams/{N} links (not the current Omni teamscsv) - so a since-renamed
    team's old name (still shown verbatim on a historical page) resolves
    correctly via that page's own href, with no hardcoded name needed."""
    lookup = {}
    for a in table.find_all("a", href=TEAM_HREF_RE):
        team_num = int(TEAM_HREF_RE.match(a["href"]).group(1))
        abbrev = teamnum_lookup.get(team_num)
        if abbrev is not None:
            lookup[a.get_text(strip=True)] = abbrev
    return lookup


def _load_ids_pos_lookup() -> dict:
    """Player -> (Pos, NFL), from td.main.ids - a season-agnostic reference
    table, used only to backfill Pos/NFL for the no-player-link dead-cap
    rendering (older seasons) where CBS's transactions page shows no
    position info at all."""
    con = duckdb.connect(f"md:{ENRICHMENT_DATABASE}")
    rows = con.execute("SELECT Player, Pos, NFL FROM main.ids").fetchall()
    con.close()
    return {player: (pos, nfl) for player, pos, nfl in rows}


def _load_dead_cap_salary_lookup() -> dict:
    """(Season, "{Player} Dead Cap") -> Salary, from td.main.rosters - the
    only place CBS's transactions page's dead-cap rows ever get a real
    dollar figure (never shown in the transaction text itself)."""
    con = duckdb.connect(f"md:{ENRICHMENT_DATABASE}")
    rows = con.execute(
        "SELECT Season, Player, Salary FROM main.rosters WHERE Player ILIKE '%Dead Cap'"
    ).fetchall()
    con.close()
    return {(int(season), player): salary for season, player, salary in rows}


def _parse_pick_segment(segment, season: int, page_lookup: dict, global_lookup: dict) -> dict:
    """A draft-pick trade leg has no player link at all - just a bare
    <span class="commish"> with the pick's identity + trade origin."""
    commish = _find_tag(segment, "span", "commish")
    text = _normalize_ws(commish.get_text(" ", strip=True)) if commish else ""
    m = PICK_RE.match(text)
    if not m:
        raise ValueError(f"Unrecognized draft-pick transaction text: {text!r}")

    round_num = int(m.group("round"))
    overall = int(m.group("overall")) if m.group("overall") else None
    # No explicit year means "this season's own (already-run) rookie draft" -
    # see module docstring. A year present is always a future/unresolved pick.
    pick_season = int(m.group("year")) if m.group("year") else season
    from_team = _resolve_team(m.group("from").strip(), page_lookup, global_lookup, context=text)
    # OriginalOwner is the parenthetical team if present, else the pick
    # hasn't changed hands before now, so the immediate sender IS the
    # original owner.
    orig_owner = (
        _resolve_team(m.group("orig").strip(), page_lookup, global_lookup, context=text)
        if m.group("orig") else from_team
    )

    player_display = f"{pick_season} Round {round_num} Pick"
    if overall is not None:
        player_display += f" (#{overall} Ovr)"

    return {
        "TxnType": "TRADE_PICK",
        "AssetType": "PICK",
        "Player": player_display,
        "PlPosName": None,
        "PlayerID": None,
        "Pos": None,
        "NFL": None,
        "Amount": None,
        "CounterpartyTeam": from_team,
        "DraftPickSeason": pick_season,
        "DraftPickRound": round_num,
        "DraftPickOriginalOwner": orig_owner,
        "DraftPickOverall": overall,
    }


def _try_parse_dead_cap_segment(segment, link, season: int, ids_lookup: dict, salary_lookup: dict):
    """Returns a parsed record dict if this segment is a dead-cap charge, or
    None if it's not (so the caller falls through to normal player/pick
    parsing) - see module docstring for the two CBS renderings."""
    if link is not None:
        m = DEAD_CAP_LINK_RE.match(link.get_text(strip=True))
        if not m:
            return None
        real_name = m.group("name")
        commish = _find_tag(segment, "span", "commish")
        if commish is not None:
            verb = _normalize_ws(commish.get_text(" ", strip=True))
        else:
            # Not every Dead Cap Added/Dropped is commissioner-flagged (bare
            # dash-prefixed text, same rendering as a normal non-commish
            # sign/drop) - see _parse_player_segment's identical fallback.
            verb = _normalize_ws("".join(n for n in segment if isinstance(n, str))).lstrip("-").strip()
        if verb not in ("Added", "Dropped"):
            raise ValueError(f"Dead Cap row for {real_name!r} has unexpected action text: {verb!r}")
        pos_team = _find_tag(segment, "span", "playerPositionAndTeam")
        pos, nfl = _split_pos_nfl(pos_team.get_text(strip=True))
    else:
        commish = _find_tag(segment, "span", "commish")
        text = _normalize_ws(commish.get_text(" ", strip=True)) if commish else ""
        m = DEAD_CAP_BARE_RE.match(text)
        if not m:
            return None
        real_name = m.group("name")
        verb = m.group("verb")
        pos, nfl = ids_lookup.get(real_name, (None, None))
        if pos is None:
            print(f"WARNING: no td.main.ids match for Dead Cap player {real_name!r} - "
                  f"Pos/PlPos will be unresolved for this row")

    display_name = f"{real_name} Dead Cap"
    return {
        "TxnType": "DEAD_CAP_ADD" if verb == "Added" else "DEAD_CAP_DROP",
        "AssetType": "PLAYER",
        "Player": display_name,
        "PlPosName": real_name,
        "PlayerID": None,  # always synthetic/unavailable for Dead Cap - see module docstring
        "Pos": pos,
        "NFL": nfl,
        "Amount": salary_lookup.get((season, display_name)),
        "CounterpartyTeam": None,
        "DraftPickSeason": None, "DraftPickRound": None,
        "DraftPickOriginalOwner": None, "DraftPickOverall": None,
    }


def _parse_player_segment(segment, page_lookup: dict, global_lookup: dict):
    """Returns a parsed record dict, or None for an excluded action (a bare
    roster-slot/lineup change with no ownership or salary implication -
    "Activated", "Benched", "Moved to {slot}" - see module docstring)."""
    link = _find_tag(segment, "a", "playerLink")
    player_name = clean_player_name(pd.Series([link.get_text(strip=True)])).iloc[0]
    id_match = PLAYERPAGE_HREF_RE.search(link.get("href", ""))
    player_id = id_match.group(1) if id_match else None

    pos_team = _find_tag(segment, "span", "playerPositionAndTeam")
    pos, nfl = _split_pos_nfl(pos_team.get_text(strip=True))

    commish = _find_tag(segment, "span", "commish")
    if commish is not None:
        action_text = _normalize_ws(commish.get_text(" ", strip=True))
    else:
        action_text = _normalize_ws("".join(n for n in segment if isinstance(n, str))).lstrip("-").strip()

    if action_text in EXCLUDED_EXACT or EXCLUDED_PREFIX_RE.match(action_text):
        return None

    base = {"Player": player_name, "PlPosName": player_name, "PlayerID": player_id, "Pos": pos, "NFL": nfl,
            "AssetType": "PLAYER", "Amount": None, "CounterpartyTeam": None,
            "DraftPickSeason": None, "DraftPickRound": None,
            "DraftPickOriginalOwner": None, "DraftPickOverall": None}

    if action_text in ("Dropped", "Added"):
        # Bare commissioner-driven add/drop (no bid amount shown) - same
        # semantics as a normal Signed/Dropped action otherwise. The
        # commissioner flag itself isn't tracked - not worth a column per
        # explicit decision.
        base["TxnType"] = "DROP" if action_text == "Dropped" else "SIGN"
        return base

    signed_match = SIGNED_RE.match(action_text)
    if signed_match:
        base["TxnType"] = "SIGN"
        base["Amount"] = float(signed_match.group(1))
        return base

    traded_match = TRADED_FROM_RE.match(action_text)
    if traded_match:
        base["TxnType"] = "TRADE_PLAYER"
        base["CounterpartyTeam"] = _resolve_team(
            traded_match.group(1).strip(), page_lookup, global_lookup, context=action_text
        )
        return base

    raise ValueError(f"Unrecognized player transaction text: {action_text!r}")


def _parse_row(row, league: str, season: int, teamnum_lookup: dict, page_lookup: dict, global_lookup: dict,
                ids_lookup: dict, salary_lookup: dict) -> list:
    row_id = row.get("id")
    if not row_id:
        return []

    team_href = row.find("a", href=TEAM_HREF_RE)
    team_num = int(TEAM_HREF_RE.match(team_href["href"]).group(1))
    team_abbrev = teamnum_lookup.get(team_num)
    if team_abbrev is None:
        raise ValueError(f"CBS team id {team_num} ({league} {season}) has no match in teamscsv's TeamNum column")

    ts_prefix = row_id.split("-")[0]
    txn_dt = datetime.datetime.fromtimestamp(int(ts_prefix), tz=datetime.timezone.utc)

    tds = row.find_all("td")
    effective_week = int(tds[3].get_text(strip=True))
    players_cell = tds[2]

    records = []
    for segment in _split_into_segments(players_cell):
        link = _find_tag(segment, "a", "playerLink")

        parsed = _try_parse_dead_cap_segment(segment, link, season, ids_lookup, salary_lookup)
        if parsed is None:
            parsed = (
                _parse_player_segment(segment, page_lookup, global_lookup) if link is not None
                else _parse_pick_segment(segment, season, page_lookup, global_lookup)
            )

        if parsed is None:
            continue
        parsed.update({
            "RowID": row_id,
            "TransactionID": ts_prefix if parsed["TxnType"] in ("TRADE_PLAYER", "TRADE_PICK") else row_id,
            "League": league, "Season": season, "TrfTm": team_abbrev,
            "EffectiveWeek": effective_week, "TxnDateTime": txn_dt,
        })
        records.append(parsed)
    return records


def scrape_league_season_transactions(league: str, season: int, teams_df: pd.DataFrame,
                                       ids_lookup: dict, salary_lookup: dict) -> pd.DataFrame:
    """Scrapes one league's full transaction history for one season."""
    host = LEAGUE_HOSTS[league.upper()]
    session = get_session(league)
    verify_session(session, league)

    url = f"https://{host}/transactions/all/all_but_lineup/{season}?print_rows=9999"
    response = session.get(url, timeout=30)
    soup = BeautifulSoup(response.content, "html.parser")

    table = soup.find("table")
    if table is None or NO_TRANSACTIONS_TEXT in table.get_text():
        return pd.DataFrame(columns=TARGET_COLUMNS)

    teamnum_lookup = get_team_by_teamnum_lookup(teams_df, league)
    page_lookup = _build_page_name_lookup(table, teamnum_lookup)
    global_lookup = get_team_abbreviation_lookup(teams_df, league, name_col="FullName")

    records = []
    for row in table.find_all("tr"):
        records.extend(_parse_row(
            row, league.upper(), season, teamnum_lookup, page_lookup, global_lookup, ids_lookup, salary_lookup
        ))

    if not records:
        return pd.DataFrame(columns=TARGET_COLUMNS)
    return _build_table(pd.DataFrame(records), season)


def _build_table(df: pd.DataFrame, season: int) -> pd.DataFrame:
    df = df.copy()
    season_str = str(season)

    is_pick = df["AssetType"] == "PICK"
    df["PlPos"] = ""
    name_nospace = df.loc[~is_pick, "PlPosName"].str.replace(" ", "", regex=False)
    df.loc[~is_pick, "PlPos"] = name_nospace + "_" + df.loc[~is_pick, "Pos"]
    df.loc[is_pick, "PlPos"] = (
        df.loc[is_pick, "DraftPickSeason"].astype(int).astype(str) + "_"
        + df.loc[is_pick, "DraftPickRound"].astype(int).astype(str) + "_"
        + df.loc[is_pick, "DraftPickOriginalOwner"]
    )

    df["SznPlPos"] = season_str + "_" + df["PlPos"]
    df["LgTrf"] = df["League"] + "_" + df["TrfTm"]
    df["SznLgTrf"] = season_str + "_" + df["LgTrf"]
    df["TrfLg"] = df["League"]

    asset_lists = df.groupby("TransactionID")["PlPos"].apply(lambda s: ",".join(dict.fromkeys(s)))
    df["AssetList"] = df["TransactionID"].map(asset_lists)

    df["Season"] = float(season)
    df["EffectiveWeek"] = df["EffectiveWeek"].astype(float)
    df["DraftPickSeason"] = df["DraftPickSeason"].astype(float)
    df["DraftPickRound"] = df["DraftPickRound"].astype(float)
    df["DraftPickOverall"] = df["DraftPickOverall"].astype(float)
    df["PlayerID"] = df["PlayerID"].astype(float)

    return df[TARGET_COLUMNS]


def scrape_transactions(seasons_by_league: dict) -> pd.DataFrame:
    """seasons_by_league: {"TRUFFLE": [2020, 2021, ...], "KERFUFFLE": [...]}.
    Real entry point for both the backfill and steady-state CLI modes."""
    teams_df = get_teams_df()
    ids_lookup = _load_ids_pos_lookup()
    salary_lookup = _load_dead_cap_salary_lookup()
    frames = [
        scrape_league_season_transactions(league, season, teams_df, ids_lookup, salary_lookup)
        for league, seasons in seasons_by_league.items()
        for season in seasons
    ]
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame(columns=TARGET_COLUMNS)


def upsert_transactions(df: pd.DataFrame, database: str, seasons_by_league: dict, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    # MotherDuck's information_schema spans every attached database in the
    # session, not just this connection's own - table_catalog must be
    # filtered explicitly or a table that only exists in some OTHER
    # database (e.g. td_backup_2025) reads as "exists" here too.
    table_exists = con.execute(
        "SELECT count(*) FROM information_schema.tables "
        "WHERE table_catalog = ? AND table_schema = 'main' AND table_name = 'transactions'",
        [database],
    ).fetchone()[0] > 0

    if not table_exists:
        if dry_run:
            print(f"[dry-run] would CREATE main.transactions in {database} (table does not exist yet) "
                  f"and insert {len(df)} rows")
            con.close()
            return
        con.register("new_transactions", df)
        con.execute("CREATE TABLE main.transactions AS SELECT * FROM new_transactions")
        print(f"{database}.main.transactions: created table, inserted {len(df)} rows")
        con.close()
        return

    pairs = [(league, season) for league, seasons in seasons_by_league.items() for season in seasons]

    def _count(pairs):
        return sum(
            con.execute(
                "SELECT count(*) FROM main.transactions WHERE TrfLg = ? AND Season = ?", [league, float(season)]
            ).fetchone()[0]
            for league, season in pairs
        )

    before = _count(pairs)

    if dry_run:
        print(f"[dry-run] would delete {before} existing rows for {pairs} from "
              f"{database}.main.transactions and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    for league, season in pairs:
        con.execute("DELETE FROM main.transactions WHERE TrfLg = ? AND Season = ?", [league, float(season)])
    con.register("new_transactions", df)
    con.execute("INSERT INTO main.transactions SELECT * FROM new_transactions")
    con.execute("COMMIT")

    after = _count(pairs)
    print(f"{database}.main.transactions: replaced {before} rows for {pairs} with {after} new rows")
    con.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--season", type=int, default=None,
                         help="Single season to scrape (default: current season via get_current_season())")
    parser.add_argument("--league", choices=["TRUFFLE", "KERFUFFLE"], default=None,
                         help="Restrict to one league (default: both)")
    parser.add_argument("--backfill", action="store_true",
                         help="Scrape every season from each league's LEAGUE_START_SEASON through the "
                              "current season, instead of just one season")
    parser.add_argument("--database", default="td_backup_2025",
                         help="MotherDuck database to write to (use a scratch/backup db for testing - "
                              "production is 'td', never the default)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    args = parser.parse_args()

    leagues = [args.league] if args.league else ["TRUFFLE", "KERFUFFLE"]
    current_season = get_current_season()

    if args.backfill:
        seasons_by_league = {
            league: list(range(LEAGUE_START_SEASON[league], current_season + 1)) for league in leagues
        }
    else:
        season = args.season or current_season
        seasons_by_league = {league: [season] for league in leagues}

    begin_time = datetime.datetime.now()
    df = scrape_transactions(seasons_by_league)

    assert list(df.columns) == TARGET_COLUMNS, "output columns drifted from td.main.transactions's schema"

    df.to_csv("data/transactions_scraperesult.csv", index=False)
    print(df)
    print(f"\nstored scrape result at data/transactions_scraperesult.csv")

    upsert_transactions(df, args.database, seasons_by_league, dry_run=args.dry_run)
    print(f"execution time: {datetime.datetime.now() - begin_time}")

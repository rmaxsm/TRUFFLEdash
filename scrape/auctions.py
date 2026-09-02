"""Scrapes one TRUFFLE/KERFUFFLE free-agent auction (CBS's once-a-year,
all-players-available simultaneous-bidding event, distinct from the weekly
waiver claims transactions.py covers) from a CBS draft-results URL, builds
td.main.auctions, and appends the same picks into td.main.transactions as a
new TxnType ("AUCTION") - without ever touching transactions.py itself.

Unlike transactions.py (which auto-detects season/week and runs on a daily
cron), this only ever runs on an explicit, manually-supplied URL - CBS's own
draft-history dropdown turned out too inconsistently labeled across seasons
(TRUFFLE's 2020 auction is named "Reset Auction", not "Free Agency"; a
league's draftId slug isn't stable year to year either) and KERFUFFLE's
2025 auction was actually split across two separate CBS draft events
("Free Agency Auction" then "Auction v2") after being restarted - there's
no reliable way to auto-discover "the auction" for a given league/season,
so every run takes the exact URL, season, and auction date/time as input.

CBS's draft-results page has no /teams/{N} href anywhere (unlike the
transactions page) - team identity is pure display-name text, both in the
"Team" column and in each pick's `data-bids` incremental bid-history
string. To resolve a historical team name safely across a franchise rename
(the same "Windy City Big Apples" -> "Madison Muskellunge" problem
transactions.py solved via its own page's hrefs), this does one extra live
fetch of that SAME league+season's transactions page (which does have
hrefs) purely to build a page-scoped name->abbrev lookup, reusing
transactions.py's own _build_page_name_lookup()/_resolve_team() rather than
re-deriving that logic - falling back to the current Omni teamscsv names
for the normal case where nothing renamed.

Each pick's full incremental bid history is already inline in the page
(no separate AJAX call needed) as `data-bids="Team:-:Amount;-;Team:-:Amount;-;..."`
on the winning-salary link - the last entry always matches the visible
winning salary (spot-checked against 2025 TRUFFLE data). Stored as a JSON
array of {Team, Bid} (Team already resolved to its 3-letter abbrev) in
td.main.auctions' BidHistory column.

Since a nomination has no per-pick timestamp of its own (unlike a real CBS
transaction row, which encodes a unix timestamp in its row id) or a
counterparty, TxnDateTime is one manually-supplied date+time (ET, converted
to UTC) applied uniformly to every pick from that auction, EffectiveWeek is
always 1 (no Week 0/preseason precedent exists anywhere in td.main.weekly),
and each row's TransactionID is deterministic - "AUCTION_{League}_{Season}_
{NominationOrder}" - rather than shared across the whole auction, since
every nomination is its own standalone event (no trade partner, no picks
involved) and AssetList should only ever show that one player's own PlPos.

NominationOrder/BidHistory are auction-specific presentation/sequencing
metadata with no lineage meaning outside this table, so they're NOT carried
into td.main.transactions (which gets every other column, with DraftPick*
left null same as any other plain SIGN-equivalent row).

Upserts are intentionally scoped narrower than transactions.py's own
(League, Season) delete+insert, since a single auctions.py run may cover
only PART of a season's auction (KERFUFFLE's 2025 auction: the first link's
picks are nomination order 1-3, submitted via --nomination-offset 0; the
second link's own native order continues via --nomination-offset 3, i.e.
its nomination 1 becomes stored order 4). td.main.auctions deletes only the
exact NominationOrder values this run is about to reinsert;
td.main.transactions deletes only the exact TransactionID values this run
is about to reinsert - neither ever touches a season's other rows.

Usage:
    MOTHERDUCK_TOKEN=... python scrape/auctions.py \
        --url "https://theradicalultimatefflexperience.football.cbssports.com/draft/results/2025:2:Free%20Agency/nomination" \
        --season 2025 --auction-date 2025-08-24 --auction-time 20:00 \
        [--league TRUFFLE] [--nomination-offset 0] [--database td_backup_2025] [--dry-run]
"""

import argparse
import datetime
import json
import zoneinfo

import duckdb
import pandas as pd
from bs4 import BeautifulSoup

from cbs_auth import LEAGUE_HOSTS, get_session, verify_session
from cbs_scrape_utils import clean_player_name, get_team_abbreviation_lookup, get_team_by_teamnum_lookup
from omni_client import get_teams_df
from transactions import (
    PLAYERPAGE_HREF_RE,
    TARGET_COLUMNS as TRANSACTIONS_COLUMNS,
    _build_page_name_lookup,
    _resolve_team,
    _split_pos_nfl,
)

ET = zoneinfo.ZoneInfo("America/New_York")

AUCTIONS_TARGET_COLUMNS = [
    "SznPlPos", "PlPos", "SznLgTrf", "LgTrf",
    "Season", "TrfLg", "TrfTm",
    "NominationOrder", "Player", "PlayerID", "Pos", "NFL", "Salary",
    "BidHistory",
]


def _infer_league_from_url(url: str) -> str:
    for league, host in LEAGUE_HOSTS.items():
        if host in url:
            return league
    raise ValueError(f"Could not infer league from URL {url!r} - pass --league explicitly")


def _fetch_transactions_page_lookup(league: str, season: int, teamnum_lookup: dict) -> dict:
    """One extra live fetch of the SAME league+season's transactions page,
    purely to resolve historical team display names via that page's own
    /teams/{N} hrefs - see module docstring for why the auction page itself
    can't do this (no hrefs at all on it). Independent of whether
    td.main.transactions has actually been backfilled for this season yet -
    this hits CBS directly, not the database."""
    host = LEAGUE_HOSTS[league]
    session = get_session(league)
    verify_session(session, league)
    url = f"https://{host}/transactions/all/all_but_lineup/{season}?print_rows=9999"
    response = session.get(url, timeout=30)
    soup = BeautifulSoup(response.content, "html.parser")
    table = soup.find("table")
    if table is None:
        return {}
    return _build_page_name_lookup(table, teamnum_lookup)


def scrape_auction(url: str, league: str, season: int, nomination_offset: int, teams_df: pd.DataFrame) -> pd.DataFrame:
    session = get_session(league)
    verify_session(session, league)
    response = session.get(url, timeout=30)
    soup = BeautifulSoup(response.content, "html.parser")

    table = soup.find("table")
    if table is None:
        raise ValueError(f"No results table found at {url!r}")

    teamnum_lookup = get_team_by_teamnum_lookup(teams_df, league)
    page_lookup = _fetch_transactions_page_lookup(league, season, teamnum_lookup)
    global_lookup = get_team_abbreviation_lookup(teams_df, league, name_col="FullName")

    records = []
    for row in table.find_all("tr"):
        tds = row.find_all("td")
        if len(tds) < 4 or tds[2].find("a", class_="playerLink") is None:
            continue

        nomination_order = int(tds[0].get_text(strip=True)) + nomination_offset
        team_abbrev = _resolve_team(
            tds[1].get_text(strip=True), page_lookup, global_lookup, context=f"nomination {nomination_order}"
        )

        player_cell = tds[2]
        for icon in player_cell.find_all("span", class_="playerIconsWrapper"):
            icon.decompose()
        link = player_cell.find("a", class_="playerLink")
        player_name = clean_player_name(pd.Series([link.get_text(strip=True)])).iloc[0]
        id_match = PLAYERPAGE_HREF_RE.search(link.get("href", ""))
        player_id = id_match.group(1) if id_match else None
        pos_team = player_cell.find("span", class_="playerPositionAndTeam")
        pos, nfl = _split_pos_nfl(pos_team.get_text(strip=True))

        # Not every season's page renders a salaryBid link with a data-bids
        # history - TRUFFLE 2023's shows only the final winning salary as
        # plain text, with no per-bid history available at all (likely true
        # for other pre-2023 seasons too). Fall back to the plain salary
        # text and store an empty BidHistory array - the winning bid still
        # lives in Salary/Amount either way.
        bid_link = tds[3].find("a", class_="salaryBid")
        if bid_link is not None:
            salary = float(bid_link.get_text(strip=True))
            bid_history = []
            for entry in bid_link["data-bids"].split(";-;"):
                bid_team_raw, _, bid_amount = entry.rpartition(":-:")
                bid_team_abbrev = _resolve_team(
                    bid_team_raw.strip(), page_lookup, global_lookup,
                    context=f"nomination {nomination_order} bid history",
                )
                bid_history.append({"Team": bid_team_abbrev, "Bid": float(bid_amount)})
        else:
            salary = float(tds[3].get_text(strip=True))
            bid_history = []
        bid_history_json = json.dumps(bid_history)

        records.append({
            "NominationOrder": nomination_order, "TrfTm": team_abbrev, "Player": player_name,
            "PlayerID": player_id, "Pos": pos, "NFL": nfl, "Salary": salary,
            "BidHistory": bid_history_json,
        })

    if not records:
        raise ValueError(f"No auction picks found at {url!r} - check the URL is a draft-results/nomination page")

    return _build_auctions_table(pd.DataFrame(records), season, league)


def _build_auctions_table(df: pd.DataFrame, season: int, league: str) -> pd.DataFrame:
    df = df.copy()
    season_str = str(season)
    player_nospace = df["Player"].str.replace(" ", "", regex=False)

    df["PlPos"] = player_nospace + "_" + df["Pos"]
    df["SznPlPos"] = season_str + "_" + df["PlPos"]
    df["LgTrf"] = league + "_" + df["TrfTm"]
    df["SznLgTrf"] = season_str + "_" + df["LgTrf"]

    df["Season"] = float(season)
    df["TrfLg"] = league
    df["PlayerID"] = df["PlayerID"].astype(float)
    df["NominationOrder"] = df["NominationOrder"].astype(float)
    df["Salary"] = df["Salary"].astype(float)

    return df[AUCTIONS_TARGET_COLUMNS]


def build_auction_transactions(auctions_df: pd.DataFrame, auction_dt: datetime.datetime) -> pd.DataFrame:
    """Same picks, reshaped into td.main.transactions' schema - see module
    docstring for why NominationOrder/BidHistory don't carry over and why
    TransactionID is per-nomination rather than shared across the auction."""
    df = auctions_df.copy()

    df["TransactionID"] = (
        "AUCTION_" + df["TrfLg"] + "_" + df["Season"].astype(int).astype(str) + "_"
        + df["NominationOrder"].astype(int).astype(str)
    )
    df["RowID"] = None
    df["EffectiveWeek"] = 1.0
    df["TxnDateTime"] = auction_dt
    df["TxnType"] = "AUCTION"
    df["AssetType"] = "PLAYER"
    df["Amount"] = df["Salary"]
    df["CounterpartyTeam"] = None
    df["DraftPickSeason"] = None
    df["DraftPickRound"] = None
    df["DraftPickOriginalOwner"] = None
    df["DraftPickOverall"] = None
    df["AssetList"] = df["PlPos"]
    # Every auction pick is its own standalone TransactionID+TrfTm group (no
    # teammates share it), so the team-leg list is trivially just itself.
    df["TeamPlayerList"] = df["Player"]
    df["TeamPlayerListMarkdown"] = "- " + df["Player"]

    return df[TRANSACTIONS_COLUMNS]


def upsert_auctions(df: pd.DataFrame, database: str, dry_run: bool = False) -> None:
    con = duckdb.connect(f"md:{database}")
    table_exists = con.execute(
        "SELECT count(*) FROM information_schema.tables "
        "WHERE table_catalog = ? AND table_schema = 'main' AND table_name = 'auctions'",
        [database],
    ).fetchone()[0] > 0

    if not table_exists:
        if dry_run:
            print(f"[dry-run] would CREATE main.auctions in {database} (table does not exist yet) "
                  f"and insert {len(df)} rows")
            con.close()
            return
        con.register("new_auctions", df)
        con.execute("CREATE TABLE main.auctions AS SELECT * FROM new_auctions")
        print(f"{database}.main.auctions: created table, inserted {len(df)} rows")
        con.close()
        return

    league, season = df["TrfLg"].iloc[0], df["Season"].iloc[0]
    orders = df["NominationOrder"].tolist()

    def _count():
        return sum(
            con.execute(
                "SELECT count(*) FROM main.auctions WHERE TrfLg = ? AND Season = ? AND NominationOrder = ?",
                [league, season, order],
            ).fetchone()[0]
            for order in orders
        )

    before = _count()
    if dry_run:
        print(f"[dry-run] would delete {before} existing rows for {league} {season} nomination orders "
              f"{min(orders):.0f}-{max(orders):.0f} from {database}.main.auctions and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    for order in orders:
        con.execute(
            "DELETE FROM main.auctions WHERE TrfLg = ? AND Season = ? AND NominationOrder = ?",
            [league, season, order],
        )
    con.register("new_auctions", df)
    con.execute("INSERT INTO main.auctions SELECT * FROM new_auctions")
    con.execute("COMMIT")

    after = _count()
    print(f"{database}.main.auctions: replaced {before} rows with {after} new rows "
          f"({league} {season:.0f} nomination orders {min(orders):.0f}-{max(orders):.0f})")
    con.close()


def upsert_auction_transactions(df: pd.DataFrame, database: str, dry_run: bool = False) -> None:
    """Deletes/inserts by exact TransactionID only - never a blanket
    (League, Season) delete like transactions.py's own upsert, since that
    would wipe every non-auction row already backfilled for this season.
    Written entirely here, not in transactions.py, per explicit decision to
    leave that script untouched."""
    con = duckdb.connect(f"md:{database}")
    table_exists = con.execute(
        "SELECT count(*) FROM information_schema.tables "
        "WHERE table_catalog = ? AND table_schema = 'main' AND table_name = 'transactions'",
        [database],
    ).fetchone()[0] > 0
    if not table_exists:
        raise RuntimeError(
            f"{database}.main.transactions does not exist yet - run scrape/transactions.py's backfill "
            f"against this database first"
        )

    transaction_ids = df["TransactionID"].tolist()

    def _count():
        return sum(
            con.execute(
                "SELECT count(*) FROM main.transactions WHERE TransactionID = ?", [tid]
            ).fetchone()[0]
            for tid in transaction_ids
        )

    before = _count()
    if dry_run:
        print(f"[dry-run] would delete {before} existing rows for {len(transaction_ids)} TransactionIDs from "
              f"{database}.main.transactions and insert {len(df)} new rows")
        con.close()
        return

    con.execute("BEGIN TRANSACTION")
    for tid in transaction_ids:
        con.execute("DELETE FROM main.transactions WHERE TransactionID = ?", [tid])
    con.register("new_auction_transactions", df)
    con.execute("INSERT INTO main.transactions SELECT * FROM new_auction_transactions")
    con.execute("COMMIT")

    after = _count()
    print(f"{database}.main.transactions: replaced {before} AUCTION rows with {after} new rows "
          f"({len(transaction_ids)} TransactionIDs)")
    con.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", required=True, help="CBS draft-results URL, e.g. .../draft/results/2025:2:Free Agency/nomination")
    parser.add_argument("--league", choices=["TRUFFLE", "KERFUFFLE"], default=None,
                         help="Override league inferred from --url's host")
    parser.add_argument("--season", type=int, required=True)
    parser.add_argument("--auction-date", required=True, help="YYYY-MM-DD, Eastern Time")
    parser.add_argument("--auction-time", required=True, help="HH:MM (24hr), Eastern Time")
    parser.add_argument("--nomination-offset", type=int, default=0,
                         help="Added to every scraped NominationOrder - use this when one auction was split "
                              "across multiple CBS draft-results links (e.g. KERFUFFLE 2025's restarted "
                              "auction: submit the first link with offset 0, the second with offset 3)")
    parser.add_argument("--database", default="td_backup_2025",
                         help="MotherDuck database to write to (use a scratch/backup db for testing - "
                              "production is 'td', never the default)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing")
    args = parser.parse_args()

    league = args.league or _infer_league_from_url(args.url)
    naive_dt = datetime.datetime.strptime(f"{args.auction_date} {args.auction_time}", "%Y-%m-%d %H:%M")
    auction_dt = naive_dt.replace(tzinfo=ET).astimezone(datetime.timezone.utc)

    begin_time = datetime.datetime.now()
    teams_df = get_teams_df()
    auctions_df = scrape_auction(args.url, league, args.season, args.nomination_offset, teams_df)
    transactions_df = build_auction_transactions(auctions_df, auction_dt)

    assert list(auctions_df.columns) == AUCTIONS_TARGET_COLUMNS, "output columns drifted from td.main.auctions's schema"
    assert list(transactions_df.columns) == TRANSACTIONS_COLUMNS, "output columns drifted from td.main.transactions's schema"

    auctions_df.to_csv("data/auctions_scraperesult.csv", index=False)
    print(auctions_df)
    print(f"\nstored scrape result at data/auctions_scraperesult.csv")

    upsert_auctions(auctions_df, args.database, dry_run=args.dry_run)
    upsert_auction_transactions(transactions_df, args.database, dry_run=args.dry_run)
    print(f"execution time: {datetime.datetime.now() - begin_time}")

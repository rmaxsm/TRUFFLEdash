"""Scrapes the current KERFUFFLE roster snapshot from CBS Sports.

Writes data/kerfuffle/kerfuffle_rosters.csv (backing up the previous version to
data/kerfuffle/backup/kerfuffle_rosters_backup.csv first).
"""

import sys
import os

import pandas as pd
from bs4 import BeautifulSoup

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from cbs_auth import get_session, verify_session
from cbs_scrape_utils import clean_player_name, get_team_abbreviation, get_team_abbreviation_lookup

session = get_session("KERFUFFLE")
verify_session(session, "KERFUFFLE")

# get information from teams document to refer to for shortcuts ext
teamsPd = pd.read_csv("data/teams.csv")
team_lookup = get_team_abbreviation_lookup(teamsPd, "KERFUFFLE")


# takes in a single row of html and returns the stats as list (for players/dst)
def separatePlayers(rows):
  curRow = []
  for i in rows:
    curRow.append(i.getText())
  return curRow

# takes in a single row of html and returns the stats as list deadcap players only
def separatePlayersDeadCap(rows):
  itr = 0
  curRow = []
  for i in rows:
    if itr == 0:
      curRow.append("DC")
    else:
      curRow.append(i.getText())
    itr += 1
  return curRow

# separates the column names
# returns list representing the columns for tables
def separateColumns(row):
  allCols = []
  for i in row:
    allCols.append(i.getText())
  allCols[1] = "Player"
  return allCols


getNFLTeam = lambda x: pd.Series([i.strip() for i in x.split("•")])
getPosition = lambda y: pd.Series([i for i in y.split(" ")][-1])
getPlayer = lambda z: pd.Series(' '.join([i for i in z.split(" ")][:-1]))

# where the connection is made to the kerfuffle cbs website
url = "https://kerfuffle.football.cbssports.com/teams/all"
response = session.get(url)
soup = BeautifulSoup(response.content, 'html.parser')

tbls = soup.find_all("table", {"class": "data data3 pinHeader borderTop"})
dfGlobal = pd.DataFrame()
counter = 0

for tbl in tbls:
  tableRows = tbl.find_all('tr')

  teamName = ""
  cols = []
  players = []

  for row in tableRows:
    if row.has_attr('class') and row['class'][0] == "title":
      nameSplit = row.getText().split(" ")
      nameSplit.pop()
      teamName = ' '.join(nameSplit)
    elif row.has_attr('class') and row['class'][0] == "label" and len(row['class']) == 1:
      cols = separateColumns(row)
    elif row.has_attr('class') and row['class'][0] == "playerRow" and len(row['class']) == 2:
      players.append(separatePlayers(row))
    elif row.has_attr('class') and row['class'][0] == "playerRow" and len(row['class']) == 3:
      playerName = row.find('a').getText()
      if playerName.split(" ")[-1] == "Cap":
        players.append(separatePlayersDeadCap(row))
      else:
        players.append(separatePlayers(row))

  dfTeam = pd.DataFrame(players, columns=cols)
  dfTeam["TRUFFLE"] = get_team_abbreviation(teamName, team_lookup)

  # apply lambda fcts to correct columns
  playerTeam = dfTeam["Player"].apply(getNFLTeam)
  position = playerTeam[0].apply(getPosition)
  player = playerTeam[0].apply(getPlayer)
  nfl = pd.Series(playerTeam[1])

  # add/remove columns for TRUFFLE formatting
  dfTeam["Player"] = player
  dfTeam.insert(1, "PosTRUFFLE", position[0])
  dfTeam.insert(3, "NFL", nfl)

  dfTeam['Player'] = clean_player_name(dfTeam['Player'])
  dfTeam['Player'] = dfTeam['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)

  if counter == 0:
    dfGlobal = dfTeam
  else:
    dfGlobal = pd.concat([dfGlobal, dfTeam], ignore_index=True)
  counter += 1


for index, row in dfGlobal.loc[~(dfGlobal['Pos'] == dfGlobal['PosTRUFFLE'])].iterrows():
  if row['Pos'] == "DC":
    dfGlobal.at[index, 'Pos'] = "DC"
  else:
    dfGlobal.at[index, 'Pos'] = row['PosTRUFFLE']

# shifting/deleting/sorting columns for formatting
truffle = dfGlobal.pop("TRUFFLE")
dfGlobal.insert(2, "TRUFFLE", truffle)
dfGlobal = dfGlobal.drop(["PosTRUFFLE"], axis=1)

# create roster backup
# read the existing csv as a pd df for error checking
masterDf = pd.read_csv("data/kerfuffle/kerfuffle_rosters.csv")
# create backup copy
filepath = "data/kerfuffle/backup/kerfuffle_rosters_backup.csv"
masterDf.to_csv(filepath, index=False)

# save final csv
dfGlobal.to_csv("data/kerfuffle/kerfuffle_rosters.csv", index=False)

print("\n\nkerfuffle roster script complete. saved at data/kerfuffle/kerfuffle_rosters.csv with backup data/kerfuffle/backup/kerfuffle_rosters_backup.csv\n")

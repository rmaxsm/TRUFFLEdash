"""Scrapes the exhaustive CBS player ID crosswalk (playerID, Player, Pos, NFL).

This is league-agnostic (hosted on the TRUFFLE site but covers all NFL
players) and evergreen - CBS's stats page has a stable default view when no
period/week is specified, so this runs the same way every day with no
Season/Week input at all. TRUFFLE team-ownership is deliberately not tracked
here (it's not part of MotherDuck's `ids` table, and nothing else downstream
reads it - global.R's would-be use of it needs a TeamNum column that its own
merge to produce it is already commented out).

Writes data/playerIDs.csv (backing up the previous version to
data/backup/playerIDs_backup.csv first).
"""

import datetime
import re

import pandas as pd
from bs4 import BeautifulSoup

from cbs_auth import get_session, verify_session
from cbs_scrape_utils import clean_player_name

begin_time = datetime.datetime.now()

session = get_session("TRUFFLE")
verify_session(session, "TRUFFLE")


# separates the column names
# returns list representing the columns for tables
def separateColumns(row):
  allCols = []
  for i in row:
    allCols.append(i.getText())
  allCols[1] = "Ownership"
  return allCols

# takes in a single row of html and returns the stats as list (for players/dst)
def separatePlayers(rows):
  itr = 0
  curRow = []
  for i in rows:
    if itr == 0:    #first value always empty string
      #adding player id numbers as temp
      test = str(i.find_all("div")[0])
      actionId = test.split("_")[1]
      curRow.append(actionId.split('"')[0])
    else:       #everything else (including raw ownership text, unused) just gets exact data from source
      curRow.append(i.getText())
    itr += 1
  return curRow

#where the connection is made to the truffle cbs website
url = "https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main/all:FLEX/TRUFFLEoffense/?print_rows=9999"
response = session.get(url)
soup = BeautifulSoup(response.content, 'html.parser')

tbls = soup.find("table", {"class": "data pinHeader"})
combined = tbls.find_all("tr", class_="label")
label = combined[1]

#connect to defense to get all dst stats - uses same logic to clean as anything else - could be not repeated code probably lol
urlDef = "https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main/all:DST/TRUFFLEoffense/?print_rows=99"
responseDef = session.get(urlDef)
soupDef = BeautifulSoup(responseDef.content, 'html.parser')

tblsDef = soupDef.find("table", {"class": "data pinHeader"})
combinedDef = tblsDef.find_all("tr", class_="label")
labelDef = combinedDef[1]

#calls function to clean column headers stored as cols (used in pandas df)
cols = separateColumns(label)

#regex used to find row1/2 (\d means only numbers following exact match of row)
allRows = tbls.find_all("tr", class_=re.compile(r"row\d"))
#html is different for 'owned' teams. Metadata should always load puffins.
allPuffins = tbls.find_all("tr", class_="bgFan")

#calls function to clean column headers stored as cols (used in pandas df)
colsDef = separateColumns(labelDef)

#regex used to find row1/2 (\d means only numbers following exact match of row)
allRowsDef = tblsDef.find_all("tr", class_=re.compile(r"row\d"))
#html is different for 'owned' teams. Metadata should always load puffins.
allPuffinsDef = tblsDef.find_all("tr", class_="bgFan")

#for every player - clean the row and add to list of lists
allPlayers = [separatePlayers(i) for i in allRows]

#add def and puffins players to the master player list
allPlayers.extend([separatePlayers(i) for i in allPuffins])
allPlayers.extend([separatePlayers(i) for i in allRowsDef])
allPlayers.extend([separatePlayers(i) for i in allPuffinsDef])

#lamba functions to split player name team and position
getNFLTeam = lambda x: pd.Series([i.strip() for i in x.split("•")])
getPosition = lambda y: pd.Series([i for i in y.split(" ")][-1])
getPlayer = lambda z: pd.Series(' '.join([i for i in z.split(" ")][:-1]))

#pandas df to represent team
df = pd.DataFrame(allPlayers, columns=cols)

#apply lambda fcts to correct columns
playerTeam = df["Player"].apply(getNFLTeam)
position = playerTeam[0].apply(getPosition)
player = playerTeam[0].apply(getPlayer)
nfl = pd.Series(playerTeam[1])

#add/remove columns for TRUFFLE formatting
# FIND ME - this has been changed because 'Bye' is no longer on the website
df = df.drop(["Rost", "Start"], axis=1)
df["Player"] = player
df["Pos"] = position[0]
df["NFL"] = nfl

df['Player'] = clean_player_name(df['Player'])
df['Player'] = df['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)

playerDb = df[["Action", "Player", "Pos", "NFL"]]
playerDb = playerDb.rename(columns={"Action": "playerID"})
playerDb = playerDb.sort_values(by=['Player', 'Pos'])

#create playerIDs backup
#read the existing csv as a pd df for error checking
masterDf = pd.read_csv("data/playerIDs.csv")
#create backup copy
filepath = "data/backup/playerIDs_backup.csv"
masterDf.to_csv(filepath, index=False)

#save file
playerDb.to_csv("data/playerIDs.csv", index=False)

print(playerDb)
print("\n\nscript complete. execution time:")
print(datetime.datetime.now() - begin_time)

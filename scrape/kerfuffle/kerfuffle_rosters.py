from bs4 import BeautifulSoup
import requests
import pandas as pd
import json

cookies = {}
with open('dre/kerfuffle_cookies.json', 'r') as file:
    cookies = json.load(file)
headers = {}
with open('dre/kerfuffle_headers.json', 'r') as file:
    headers = json.load(file)

#get information from teams document to refer to for shortcuts ext
teamsPd = pd.read_csv("data/teams.csv")
# re-assign the PD to get only KERFUFFLE
teamsPd = teamsPd[teamsPd['League'] == "KERFUFFLE"]

teamsDict = {}
for index, row in teamsPd.iterrows():
  teamsDict[row["FullName"]] = row["Abbrev"]


  
#returns team abbreviation from team name
def getTeamAbbreviation(team):
  try:
    return teamsDict[team]
  except Exception as exp:
    print(teamsDict)
    print("An Error Occuring while trying to get the team abbreviation for " + team)
    return
  
#takes in a single row of html and returns the stats as list (for players/dst)
def separatePlayers(rows):
  curRow = []
  for i in rows:
    curRow.append(i.getText())
  return curRow

#takes in a single row of html and returns the stats as list deadcap players only
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

#separates the column names
#returns list representing the columns for tables 
def separateColumns(row):
  allCols = []
  for i in row:
    allCols.append(i.getText())
  allCols[1] = "Player"
  return allCols


getNFLTeam = lambda x: pd.Series([i.strip() for i in x.split("•")])
getPosition = lambda y: pd.Series([i for i in y.split(" ")][-1])
getPlayer = lambda z: pd.Series(' '.join([i for i in z.split(" ")][:-1]))

#where the connection is made to the truffle cbs website
url = "https://kerfuffle.football.cbssports.com/teams/all"
response = requests.get(url, cookies=cookies, headers=headers)
soup = BeautifulSoup(response.content, 'html.parser')


tbls =  soup.findAll("table", {"class":"data data3 pinHeader borderTop"})
dfGlobal = pd.DataFrame()
counter = 0

for tbl in tbls:
  # print(tbl.prettify())
  tableRows = tbl.findAll('tr')
  
  teamName = ""
  cols = []
  players = []
  
  for row in tableRows:
    if row.has_attr('class') and row['class'][0] == "title":
      # print("THIS IS THE TEAM NAME")
      nameSplit = row.getText().split(" ")
      nameSplit.pop()
      teamName = ' '.join(nameSplit)
    elif row.has_attr('class') and row['class'][0] == "label" and len(row['class']) == 1:
      # print("THIS IS THE COLUMN HEADERS")
      cols = separateColumns(row)
    elif row.has_attr('class') and row['class'][0] == "playerRow" and len(row['class']) == 2:
      # print("THESE ARE ALL THE STARTING PLAYERS")
      players.append(separatePlayers(row))
    elif row.has_attr('class') and row['class'][0] == "playerRow" and len(row['class']) == 3:
      # print("THESE ARE ALL THE BENCHED PLAYERS")
      playerName = row.find('a').getText()
      if (playerName.split(" ")[-1] == "Cap"):
        players.append(separatePlayersDeadCap(row))
      else:
        players.append(separatePlayers(row))

  dfTeam = pd.DataFrame(players, columns = cols)
  dfTeam["TRUFFLE"] = getTeamAbbreviation(teamName)
  
  #apply lambda fcts to correct columns
  playerTeam = dfTeam["Player"].apply(getNFLTeam)
  position = playerTeam[0].apply(getPosition)
  player =  playerTeam[0].apply(getPlayer)
  nfl = pd.Series(playerTeam[1])
  
  
  #add/remove columns for TRUFFLE formatting
  dfTeam["Player"] = player
  dfTeam.insert(1,"PosTRUFFLE", position[0])
  dfTeam.insert(3,"NFL", nfl)
  
  dfTeam['Player'] = dfTeam['Player'].str.replace(r'.', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r' Jr', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r' Sr', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r' III', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r' II', '', regex=True)
  dfTeam['Player'] = dfTeam['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)

  if counter == 0:
    dfGlobal = dfTeam
  else:
    dfGlobal = pd.concat([dfGlobal, dfTeam], ignore_index = True)
  counter += 1
  
  
for index, row in dfGlobal.loc[~(dfGlobal['Pos'] == dfGlobal['PosTRUFFLE'])].iterrows():
  if row['Pos'] == "DC":
    # print("DEAD CAP HARD CODE ASSIGN")
    dfGlobal.at[index, 'Pos'] = "DC"
  else:
    # print("NEED TO ASSIGN THE Pos = PosTRUFFLE")
    dfGlobal.at[index, 'Pos'] = row['PosTRUFFLE']
    
#shifting/deleting/sorting columns for formatting
truffle = dfGlobal.pop("TRUFFLE")
dfGlobal.insert(2,"TRUFFLE", truffle)
dfGlobal = dfGlobal.drop(["PosTRUFFLE"], axis=1)
# dfGlobal = dfGlobal.sort_values(by=['TRUFFLE'])

#create roster backup
#read the existing csv as a pd df for error checking
masterDf = pd.read_csv("data/kerfuffle/kerfuffle_rosters.csv")
#create backup copy
filepath = "data/kerfuffle/backup/kerfuffle_rosters_backup.csv"
masterDf.to_csv(filepath, index=False)

#save final csv
dfGlobal.to_csv("data/kerfuffle/kerfuffle_rosters.csv", index = False)

print(dfGlobal)
print("\n\nkerfuffle roster script complete. saved at data/kerfuffle/kerfuffle_rosters.csv with backup data/kerfuffle/backup/kerfuffle_rosters_backup.csv\n")
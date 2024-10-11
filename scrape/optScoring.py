#this is a python script. It will ask you for year and week then get all the information 
#from cbs for each team for that week and year. Error handling is not great tbh but will do some checking
#just input the right stuff 

import os
import requests
import datetime
import pandas as pd
import numpy as np
import re
import json
from bs4 import BeautifulSoup


#below are 'keys' of some sort from copy and pasting - unclear how they fit in left for reference

#_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session = os.getenv('_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session')
#_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session; = os.getenv('_sn:90$_ss:0$_st:1651846165170$vapi_domain:cbssports.com$dc_visit:34$_se:8$ses_id:1651842081415%3Bexp-session$_pn:7%3Bexp-session$dc_event:12%3Bexp-session$dc_region:us-east-1%3Bexp-session;')

#cookies from website (https://curlconverter.com/#python)
cookies = {}
with open('dre/truffle_cookies.json', 'r') as file:
    cookies = json.load(file)
headers = {}
with open('dre/truffle_headers.json', 'r') as file:
    headers = json.load(file)

#this is the main pandas frame that will be added to throughout
dfGlobal = pd.DataFrame()

#get year and week for url/formatting
season = input("What SEASON is it..? ")
while(len(season) != 4):
  print("get the SEASON right dumbass")
  season = input("What SEASON is it..? ")

week = input("What WEEK is it..? ")
while(len(week) >= 3):
  print("get the week right dumbass")
  week = input("What WEEK is it..? ")

#use current input week year tuple as comparison
currentWeekYear = week + season

begin_time = datetime.datetime.now()


#get information from teams document to refer to for shortcuts ext
teamsPd = pd.read_csv("data/teams.csv")
# re-assign the PD to get only TRUFFLE
teamsPd = teamsPd[teamsPd['League'] == "TRUFFLE"]

teamsDict = {}
for index, row in teamsPd.iterrows():
  teamsDict[row["LogsScrape"]] = row["Abbrev"]  

#returns team abbreviation from team name
def getTeamAbbreviation(team):
  try:
    waived =  re.compile("W ")
    if(waived.match(team)):
      return team
    return teamsDict[team]
  except Exception as exp:
    print("An Error Occuring while trying to get the team abbreviation for " + team)
    return "err"

#separates the column names
#returns list representing the columns for tables 
def separateColumns(row):
  allCols = []
  for i in row:
    allCols.append(i.getText())
  allCols[1] = "TRUFFLE"
  return allCols

#takes in a single row of html and returns the stats as list (for players/dst)
def separatePlayers(rows):
  itr = 0
  curRow = []
  for i in rows:
    if itr == 0:    #first value always empty string
      curRow.append("")
    elif itr == 1:    #second value convert to team abbreviation
      curRow.append(getTeamAbbreviation(i.getText()).strip())   
    else:       #after first two iterations just get exact data from source
      curRow.append(i.getText())
    itr += 1
  return curRow

#lamba functions to split player name team and position
getNFLTeam = lambda x: pd.Series([i.strip() for i in x.split("•")])
getPosition = lambda y: pd.Series([i for i in y.split(" ")][-1])
getPlayer = lambda z: pd.Series(' '.join([i for i in z.split(" ")][:-1]))

for index, row in teamsPd.iterrows():

  teamNum = row["TeamNum"]
  #where the connection is made to the truffle cbs website
  url = "https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main/team:{}/period-{}:p/TRUFFLEoffense/".format(teamNum,week)
  response = requests.get(url, cookies=cookies, headers=headers)
  soup = BeautifulSoup(response.content, 'html.parser')
    
  #finds the outermost 'tables' containing the stats(3 off,kicker,dst in order)
  tbls =  soup.find_all("table", class_="data pinHeader borderTop")
  tblOffense = tbls[0]
  tblDefense = tbls[1]
  
  #gets the column header information as 'label'
  combined = tblOffense.find_all("tr", class_="label")
  label = combined[1]
  
  #calls function to clean column headers stored as cols (used in pandas df)
  cols = separateColumns(label)
  
  #store all players per team as list of lists
  allPlayers = []
  
  #regex used to find row1/2 (\d means only numbers following exact match of row)
  allRowsOffense = tblOffense.find_all("tr", class_=re.compile("row\d"))
  
  #for every player - clean the row and add to list of lists
  for i in allRowsOffense:
    allPlayers.append(separatePlayers(i))
  
  # simlar to offense but with slightly different logic
  defRows = tblDefense.find_all("tr", {"class": re.compile("row\d")})
  for i in defRows:
    curDef = separatePlayers(i)
    #once defense stats are cleaned need to fill the rest of the list as empty
    noneList = ["" for i in range(len(allPlayers[0]) - len(curDef))]
    for j in noneList:
      curDef.append(j)
    allPlayers.append(curDef)
    
  #pandas df to represent team
  df = pd.DataFrame(allPlayers, columns=cols)
  df = df.drop(columns=["Action"])
  df = df.drop(columns=["Bye"])
  
  #apply lambda fcts to correct columns
  playerTeam = df["Player"].apply(getNFLTeam)
  position = playerTeam[0].apply(getPosition)
  player =  playerTeam[0].apply(getPlayer)
  nfl = pd.Series(playerTeam[1])


  #add/remove columns for TRUFFLE formatting
  # df = df.drop(["Bye","Rost", "Start"],axis=1)
  # FIND ME - this has been changed because 'Bye' is no longer on the website
  df = df.drop(["Rost", "Start"],axis=1)
  df["Player"] = player
  df.insert(0,"Season", season)
  df.insert(1,"Week", week)
  df.insert(3,"Pos", position[0])
  df.insert(5,"NFL", nfl)
  

  #moves defensive data - empties out old
  finalVal = len(df.index) - 1
  compToAvg = df.iloc[[finalVal]]["Comp"].values[0]
  df.loc[finalVal,"Avg"] = compToAvg
  df.loc[finalVal, "Comp"] = ''
  total = df.iloc[[finalVal]]["ATT"].values[0]
  df.loc[finalVal,"Total"] = total
  df.loc[finalVal, "ATT"] = ''
  
  #on the first iteration we need to 'create' the main pandas table - it is naive to the size before scraping
  if index == 0:
    dfGlobal = df
  else:
    dfGlobal = pd.concat([dfGlobal,df], ignore_index=True)


#dfGlobal = dfGlobal.replace('', "NA", regex = True)

masterFile = "data/optScoring.csv"

#read the existing csv as a pd df for error checking
masterDf = pd.read_csv(masterFile)
filepath = "data/backup/optScoring_backup.csv"
masterDf.to_csv(filepath, index=False)

#reassign the columns to be equal to that of the existing csv
dfGlobal.columns = masterDf.columns

#player name replacements
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r'.', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r' Jr', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r' Sr', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r' III', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r' II', '', regex=True)
dfGlobal['Player'] = dfGlobal['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)

#create weekyear column to conditionally remove existing data from week being scraped
masterDf["WeekYear"] = masterDf["Week"].astype(str) + masterDf["Season"].astype(str)
#remove
masterDf = masterDf[masterDf["WeekYear"] != currentWeekYear]
#drop the weekyear column post check
masterDf = masterDf.drop(['WeekYear'], axis=1)

#concat scraped df and the masterDf
newmaster = pd.concat([masterDf, dfGlobal], ignore_index=True)

# stores as csv
filepath = "data/backup/optScoring_scraperesult.csv"
newmaster.to_csv(masterFile, index=False)
dfGlobal.to_csv(filepath, index=False)

#ending print outs
# print(dfGlobal)
print("\nstored file in location {}".format(filepath))
print("\nTRUFFLE Optimal Scoring script complete.")

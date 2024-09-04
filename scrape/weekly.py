#this is a python script. It will ask for the week then get all scoring data
#it should append to a current csv file - can stay hardcoded or ask for filepath.

import os
import requests
import datetime
import pandas as pd
import numpy as np
import re
import shutil
from bs4 import BeautifulSoup
import json

cookies = {}
with open('dre/truffle_cookies.json', 'r') as file:
    cookies = json.load(file)
headers = {}
with open('dre/truffle_headers.json', 'r') as file:
    headers = json.load(file)


#get year and week for url/formatting
season = input("What SEASON is it..? ")
while(len(season) != 4):
  print("get the SEASON right dumbass")
  season = input("What SEASON is it..? ")

week = input("What WEEK is it..? ")
while(len(week) >= 3):
  print("get the week right dumbass")
  week = input("What WEEK is it..? ")
# season = 2024
# week = 18

#setting current season
currentWeekYear = week + season

#time stamp
begin_time = datetime.datetime.now()
todays_date = datetime.datetime.today()

#get information from teams document to refer to for shortcuts ext
teamsPd = pd.read_csv("data/teams.csv")

teamsDict = {}
for index, row in teamsPd.iterrows():
  teamsDict[row["LogsScrape"]] = row["Abbrev"]


#returns team abbreviation from team name
def getTeamAbbreviation(team):
  try:
    if(team == "W "):
      ##NEED TO CHECK IF THIS IS WHAT IS CORRECT. WANT SEASON NOT YEAR???
      # return team
      return "FA"
    return teamsDict[team]
  except Exception as exp:
    #print("An Error Occuring while trying to get the team abbreviation for " + team)
    return "W (9/1)"

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
      curRow.append(getTeamAbbreviation(i.getText()))
    else:       #after first two iterations just get exact data from source
      curRow.append(i.getText())
    itr += 1
  return curRow

#where the connection is made to the truffle cbs website
url = "https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main/all:FLEX/period-{}:p/TRUFFLEoffense/?print_rows=9999".format(week)
response = requests.get(url, cookies=cookies, headers=headers)
soup = BeautifulSoup(response.content, 'html.parser')

# complete =  soup.find("div", {"id": "sortableStats"})
tbls =  soup.find("table", {"class":"data pinHeader"})
combined = tbls.find_all("tr", class_="label")
label = combined[1]

#calls function to clean column headers stored as cols (used in pandas df)
cols = separateColumns(label)

# print(cols)

#regex used to find row1/2 (\d means only numbers following exact match of row)
allRows = tbls.find_all("tr", class_=re.compile("row\d"))

#puffins players (/ logged in users players) display under 'bgFan' in html - locate thusly 
puffinsRows = tbls.find_all("tr", class_=re.compile("bgFan"))

allPlayers = []

#for every player - clean the row and add to list of lists
for i in allRows:
  allPlayers.append(separatePlayers(i))
#add puffins players too
for i in puffinsRows:
  #print(separatePlayers(i))
  allPlayers.append(separatePlayers(i))

#lamba functions to split player name team and position
getNFLTeam = lambda x: pd.Series([i.strip() for i in x.split("•")])
getPosition = lambda y: pd.Series([i for i in y.split(" ")][-1])
getPlayer = lambda z: pd.Series(' '.join([i for i in z.split(" ")][:-1]))

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
df.insert(4,"NFL", nfl)

df = df[df['Avg'] != "-"]

#rename columns
df.columns = ["Season", "Week", "TRUFFLE", "Pos", "NFL", "Player", "Opp", "OpRk", "PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt", "RuAtt", "RuYd", "RuTD", "RuFD", "Tar", "Rec", "ReYd", "ReTD", "ReFD", "FL", "Avg", "FPts"]
df['OpRk'] = df['OpRk'].replace('---', '33')

df[df.columns[7:25]] = df[df.columns[7:25]].astype(float)

df['Player'] = df['Player'].str.replace(r'.', '', regex=True)
df['Player'] = df['Player'].str.replace(r' Jr', '', regex=True)
df['Player'] = df['Player'].str.replace(r' Sr', '', regex=True)
df['Player'] = df['Player'].str.replace(r' III', '', regex=True)
df['Player'] = df['Player'].str.replace(r' II', '', regex=True)
df['Player'] = df['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)

df = df.sort_values(by="FPts", ascending = False)

masterFile = "data/weekly.csv"

#read the existing csv as a pd df for error checking
masterDf = pd.read_csv(masterFile)
#create backup copy
filepath = "data/backup/weekly_backup.csv"
masterDf.to_csv(filepath, index=False)

#reassign the columns to be equal to that of the existing csv
df.columns = masterDf.columns

#create weekyear column to conditionally remove existing data from week being scraped
masterDf["WeekYear"] = masterDf["Week"].astype(str) + masterDf["Season"].astype(str)
#remove
masterDf = masterDf[masterDf["WeekYear"] != currentWeekYear]
#drop the weekyear column post check
masterDf = masterDf.drop(['WeekYear'], axis=1)

#concat scraped df and the masterDf
newmaster = pd.concat([masterDf, df], ignore_index=True)

# stores as csv
filepath = "data/backup/weekly_scraperesult.csv"
newmaster.to_csv(masterFile, index=False)
df.to_csv(filepath, index=False)

#ending print outs
print(df)
print("\nstored file in location {}".format(filepath))
print("\n\nscript complete. execution time:")
print(datetime.datetime.now() - begin_time)

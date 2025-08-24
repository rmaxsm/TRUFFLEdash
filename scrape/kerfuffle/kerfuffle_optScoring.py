from bs4 import BeautifulSoup
import requests
import pandas as pd
import re
import argparse
import json

def main(season, week):
  cookies = {}
  with open('dre/kerfuffle_cookies.json', 'r') as file:
      cookies = json.load(file)
  headers = {}
  with open('dre/kerfuffle_headers.json', 'r') as file:
      headers = json.load(file)

  #this is the main pandas frame that will be added to throughout
  dfGlobal = pd.DataFrame()

  #use current input week year tuple as comparison
  currentWeekYear = week + season


  #get information from teams document to refer to for shortcuts ext
  teamsPd = pd.read_csv("data/teams.csv")

  # re-assign the PD to get only KERFUFFLE
  teamsPd = teamsPd[teamsPd['League'] == "KERFUFFLE"]

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
      url = "https://kerfuffle.football.cbssports.com/stats/stats-main/team:{}/period-{}:p/TRUFFLEoffense/".format(teamNum,week)
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

  print(dfGlobal)

  masterFile = "data/kerfuffle/kerfuffle_optScoring.csv"

  #read the existing csv as a pd df for error checking
  masterDf = pd.read_csv(masterFile)
  filepath = "data/kerfuffle/backup/kerfuffle_optScoring_backup.csv"
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
  filepath = "data/kerfuffle/backup/kerfuffle_optScoring_scraperesult.csv"
  newmaster.to_csv(masterFile, index=False)
  dfGlobal.to_csv(filepath, index=False)

  # ending print outs
  # print(dfGlobal)
  print("\nstored file in location {}".format(filepath))
  print("\nkerfuffle fantasy script complete.")

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Generate playerIDs.csv for a given season and week.")
  parser.add_argument("--season", required=True, help="Season year (e.g., 2024)")
  parser.add_argument("--week", required=True, help="Week number (e.g., 1)")
  args = parser.parse_args()
  main(args.season, args.week)
import requests
import pandas as pd
import numpy as np
import re
from bs4 import BeautifulSoup

#separates the column names
#returns list representing the columns for tables 
def separateColumns(row):
  allCols = []
  for i in row:
    allCols.append(i.getText())
  return allCols

#takes in a single row of html and returns the stats as list
def separatePlayers(rows):
  curRow = []
  for i in rows:
    cur = i.getText().replace("%", "")
    if cur != '\xa0':
      curRow.append(cur)
    else:
      curRow.append('-')
  return curRow

def appendToFile(df, season, filepath):
  
  masterFile = filepath

  #read the existing csv as a pd df for error checking
  masterDf = pd.read_csv(masterFile)
  
  #reassign the columns to be equal to that of the existing csv
  df.columns = masterDf.columns

  #reassign the columns to be equal to that of the existing csv
  df.columns = masterDf.columns
  
  # save backup
  backupFile = "data/backup/snapPer_backup.csv"
  masterDf.to_csv(backupFile, index=False)
  print("Snap Per backup saved at {}".format(backupFile))
  
  #create year column to conditionally remove existing data from year being scraped
  masterDf["Year"] = masterDf["Season"].astype(str)
  #remove
  masterDf = masterDf[masterDf["Year"] != season]
  #drop the year column post check
  masterDf = masterDf.drop(['Year'], axis=1)
  
  #concat scraped df and the masterDf
  newmaster = pd.concat([masterDf, df], ignore_index=True)

  # stores updated file as csv over previous 'master' 
  newmaster.to_csv(masterFile, index=False)
  print("Snap Per saved at {}".format(masterFile))

def runSnapPer(season):
    
  response = requests.get('https://www.fantasypros.com/nfl/reports/snap-counts/?year='+season+'&show=perc')
  soup = BeautifulSoup(response.content, 'html.parser')
  
  complete =  soup.find("div", {"id": "main-container"})
  combined = complete.find_all("div", class_="mobile-table")
  tbls = combined[0].find("table")
  headers = tbls.find_all("th")
  #using scraped data create columns/players then combine into pandas dataframe :)

  colHeaders = separateColumns(headers)
  # print(colHeaders)
  
  #iterate over each row with this class name syntax like 'mpb-player-123...'
  allPlayers = []
  for i in soup.select('tr[class*="mpb-player-"]'):
    allPlayers.append(separatePlayers(i))
    
  
  #pandas df to represent team
  df = pd.DataFrame(allPlayers, columns=colHeaders)
  df.insert(0,"Season", season)
  df = df.sort_values(by=['Player'])
  df['Player'] = df['Player'].str.replace(r'.', '', regex=True)
  df['Player'] = df['Player'].str.replace(r' Jr', '', regex=True)
  df['Player'] = df['Player'].str.replace(r' Sr', '', regex=True)
  df['Player'] = df['Player'].str.replace(r' III', '', regex=True)
  df['Player'] = df['Player'].str.replace(r' II', '', regex=True)
  df['Player'] = df['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)
  
  # print(df)
  
  filepath = "data/snapPer.csv"
  
  appendToFile(df, season, filepath)
  
  
def main(season):
  runSnapPer(season)
  print("\nFP Snap Per DONE")

if __name__ == "__main__":
  main()

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

def runSnapPer():
  
  response = requests.get('https://www.fantasypros.com/nfl/reports/snap-counts/?year=2022&show=perc')
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
  df = df.sort_values(by=['Player'])
  df['Player'] = df['Player'].str.replace(r'.', '', regex=True)
  df['Player'] = df['Player'].str.replace(r' Jr', '', regex=True)
  df['Player'] = df['Player'].str.replace(r' Sr', '', regex=True)
  df['Player'] = df['Player'].str.replace(r' III', '', regex=True)
  df['Player'] = df['Player'].str.replace(r' II', '', regex=True)
  df['Player'] = df['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)
  # print(df)
  df.to_csv("dre/FantasyPros/snapPer.csv", index=False)
  print("\nSNAP PER FILE SAVED TO dre/FantasyPros/snapPer.csv")
  print("Fantasy Pros DONE :)\n")
  
def main():
  runSnapPer()
  print("\nFP Snap Per DONE")

if __name__ == "__main__":
  main()


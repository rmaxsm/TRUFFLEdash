import dre.ESPN.espnQB as qb
import dre.ESPN.espnRB as rb
import dre.ESPN.espnWR as wr
import dre.ESPN.espnTE as te
import dre.ESPN.espnXTD as xtd
import pandas as pd


def appendToFile(df, season, filepath):
  
  masterFile = filepath

  #read the existing csv as a pd df for error checking
  masterDf = pd.read_csv(masterFile)
  
  #reassign the columns to be equal to that of the existing csv
  df.columns = masterDf.columns

  #reassign the columns to be equal to that of the existing csv
  df.columns = masterDf.columns
  
  # save backup
  backupFile = "data/backup/espnStats_backup.csv"
  masterDf.to_csv(backupFile, index=False)
  print("EspnAll backup saved at {}".format(backupFile))
  
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
  print("EspnAll saved at {}".format(masterFile))

def main(season):
    
  qb.main(season)
  rb.main(season)
  wr.main(season)
  te.main(season)
  xtd.main(season)

  qbDf = pd.read_pickle("dre/ESPN/qb.pkl")
  rbDf = pd.read_pickle("dre/ESPN/rb.pkl")
  wrDf = pd.read_pickle("dre/ESPN/wr.pkl")
  teDf = pd.read_pickle("dre/ESPN/te.pkl")
  xtdDf = pd.read_pickle("dre/ESPN/xtd.pkl")

  # combinedDf = pd.DataFrame(columns = qbDf.columns)
  combinedDf = pd.concat([qbDf, rbDf, wrDf, teDf], ignore_index=True, sort=False)

  dupe = pd.merge(combinedDf, xtdDf, on='Player', how="outer")

  noDupe = dupe.drop_duplicates(subset=["Player", "NFL", "Pos"])
  noDupe["Season"] = season
  noDupe = noDupe.loc[:, ["Season", "Player","NFL","Pos","xFP","ActualPts","xTD","TD", "Looks", "Diff","In5", "EZ"]]

  noDupe['Player'] = noDupe['Player'].str.replace(r'.', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r' Jr', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r' Sr', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r' III', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r' II', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r'Raleigh Webb X', 'Raleigh Webb', regex=True)

  noDupe = noDupe.fillna('-')

  filepath = "data/espnStats.csv"

  # noDupe.to_csv(filepath, index=False)
  appendToFile(noDupe, season, filepath)
  
  print("ESPN DONE :)\n")
  
if __name__ == "__main__":
  main()

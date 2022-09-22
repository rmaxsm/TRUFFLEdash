import dre.ESPN.espnQB as qb
import dre.ESPN.espnRB as rb
import dre.ESPN.espnWR as wr
import dre.ESPN.espnTE as te
import dre.ESPN.espnXTD as xtd
import pandas as pd


def main():
  qb.main()
  rb.main()
  wr.main()
  te.main()
  xtd.main()
  
  qbDf = pd.read_pickle("dre/ESPN/qb.pkl")
  rbDf = pd.read_pickle("dre/ESPN/rb.pkl")
  wrDf = pd.read_pickle("dre/ESPN/wr.pkl")
  teDf = pd.read_pickle("dre/ESPN/te.pkl")
  xtdDf = pd.read_pickle("dre/ESPN/xtd.pkl")

  # combinedDf = pd.DataFrame(columns = qbDf.columns)
  combinedDf = pd.concat([qbDf, rbDf, wrDf, teDf], ignore_index=True, sort=False)
  
  dupe = pd.merge(combinedDf, xtdDf, on='Player')

  noDupe = dupe.drop_duplicates(subset=["Player", "NFL", "Pos"])
  noDupe = noDupe.loc[:, ["Player","NFL","Pos","xFP","ActualPts","xTD","TD", "Looks", "Diff","In5", "EZ"]]
  
  noDupe['Player'] = noDupe['Player'].str.replace(r'.', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r' Jr', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r' Sr', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r' III', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r' II', '', regex=True)
  noDupe['Player'] = noDupe['Player'].str.replace(r'Will Fuller V', 'Will Fuller', regex=True)
  
  noDupe.to_csv("dre/ESPN/espnStatsFinal.csv", index=False)
  print(noDupe)

  print("\nALL DONE :)")

  
if __name__ == "__main__":
  main()

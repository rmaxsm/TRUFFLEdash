import dre.ESPN.espnQB as qb
import dre.ESPN.espnRB as rb
import dre.ESPN.espnWR as wr
import dre.ESPN.espnTE as te
import dre.ESPN.espnXTD as xtd


def main():
  qb.main()
  rb.main()
  wr.main()
  te.main()
  # xtd.main()
  
  qbDf = pd.read_pickle("dre/ESPN/qb.pkl")
  rbDf = pd.read_pickle("dre/ESPN/rb.pkl")
  wrDf = pd.read_pickle("dre/ESPN/wr.pkl")
  teDf = pd.read_pickle("dre/ESPN/te.pkl")
  xtdDf = pd.read_pickle("dre/ESPN/xtd.pkl")

  combinedDf = pd.DataFrame(columns = qbDf.columns)
  combinedDf = pd.concat([qbDf, rbDf, wrDf, teDf], ignore_index=True, sort=False)
  
  print("\n\nTHIS IS THE COMBINED DATAFRAME WITHOUT XTD\n")
  print(combinedDf)
  #NOW NEED TO ADD THE XTD SOMEHOW. IS PLAYER NAME ENOUGH..?
  print("THIS IS THE XTD TABLE:\n")
  print(xtdDf)
  
if __name__ == "__main__":
  main()

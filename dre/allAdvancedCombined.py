import dre.ESPN.espnAll as espn
import dre.FantasyPros.fpAll as fp
import dre.CBS.cbsAll as cbs
import pandas as pd


def main():
  
  week = input("What week is it? ")
  
 
  espn.main()
  fp.main()
  cbs.main(week)
  
  print("\nALL DONE :)")

  
if __name__ == "__main__":
  main()

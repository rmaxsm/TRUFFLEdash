import dre.ESPN.espnAll as espn
import dre.FantasyPros.snapCountPer as fp
import dre.CBS.cbsAll as cbs
import pandas as pd


def main():
 
  espn.main()
  fp.main()
  cbs.main()
  
  print("\nALL DONE :)")

  
if __name__ == "__main__":
  main()

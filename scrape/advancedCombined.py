import dre.ESPN.espnAll as espn
import dre.FantasyPros.fpAll as fp
import dre.CBS.cbsAll as cbs
import scrape.standingsScript as st
import pandas as pd


def main():
  
  #get year and week for url/formatting
  season = input("What SEASON is it..? ")
  while(len(season) != 4):
    season = input("What SEASON is it..? ")
  
  week = input("What WEEK is it..? ")
  while(len(week) >= 3):
    week = input("What WEEK is it..? ")
  
  espn.main()
  fp.main()
  cbs.main(week,season)
  ##UNCOMMENT BELOW WHEN WE ARE READY
  # st.main(season)
  
  print("\nALL DONE :)")

  
if __name__ == "__main__":
  main()

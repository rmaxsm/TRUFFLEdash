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
  
  try:
    espn.main(season)
  except Exception as e:
    print("There was an error running ESPN Main from ../scrape/advancedCombined.py")
    print("Error is: ", e)
    
  try:
    fp.main(season)
  except Exception as e:
    print("There was an error running FantasyPros Main from ../scrape/advancedCombined.py")
    print("Error is: ", e)
    
  try:  
    cbs.main(week,season)
  except Exception as e:
    print("There was an error running CBS Main from ../scrape/advancedCombined.py")
    print("Error is: ", e)
  
  try:
    st.main(season)
  except Exception as e:
    print("There was an error running Standings Script from ../scrape/advancedCombined.py")
    print("Error is: ", e)
    
  
  print("\nALL DONE :)")

  
if __name__ == "__main__":
  main()

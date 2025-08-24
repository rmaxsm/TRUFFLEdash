import dre.ESPN.espnAll as espn
import dre.FantasyPros.fpAll as fp
import dre.CBS.cbsAll as cbs
import scrape.standingsScript as st
import pandas as pd
import argparse

def main(season, week):
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
        cbs.main(week, season)
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
    parser = argparse.ArgumentParser(description="Run advanced combined script.")
    parser.add_argument("--season", required=True, help="Season year (e.g., 2024)")
    parser.add_argument("--week", required=True, help="Week number (e.g., 1)")
    args = parser.parse_args()
    main(args.season, args.week)
# Python scrape scripts -----
library(reticulate)

#seemingly only needed to use this line once, but leaving it here just in case
#use_python("/usr/local/bin/python3")

#scrape rosters and playerIDs
source_python('scrape/rosters.py')
source_python('scrape/playerIDs.py')

#scrape weekly and fantasy
source_python('scrape/weekly.py')
source_python('scrape/fantasy.py')

#scrape seasons (only need to do this end of year)
#source_python('scrape/seasons.py')
# Python scrape scripts -----
library(reticulate)

#seemingly only needed to use this line once, but leaving it here just in case
#use_python("/usr/local/bin/python3")

#scrape rosters and playerIDs
source_python('scrape/rosters.py')
source_python('scrape/kerfuffle/kerfuffle_rosters.py')

#playerIDs needs an HTML fix
source_python('scrape/playerIDs.py')

#scrape weekly
source_python('scrape/weekly.py')

#scrape fantasy
source_python('scrape/fantasy.py')
source_python('scrape/kerfuffle/kerfuffle_fantasy.py')

#scrape optScoring
source_python('scrape/optScoring.py')
source_python('scrape/kerfuffle/kerfuffle_optScoring.py')

#advancedCombined
source_python('scrape/advancedCombined.py')

#scrape seasons (only need to do this end of year)
source_python('scrape/seasons.py')
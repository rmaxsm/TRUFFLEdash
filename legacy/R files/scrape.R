# Python scrape scripts -----
library(reticulate)
py_install("requests ", pip = TRUE)
py_install("pandas", pip = TRUE)
py_install("os", pip = TRUE)
py_install("datetime", pip = TRUE)
py_install("numpy", pip = TRUE)
py_install("re", pip = TRUE)
py_install("bs4", pip = TRUE)

py_require(c("pands","requests","os","datetime","numpy","re","beautifulsoup4"))


#seemingly only needed to use this line once, but leaving it here just in case
#use_python("~/Library/Python/3.9/bin")

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
#source_python('scrape/seasons.py')
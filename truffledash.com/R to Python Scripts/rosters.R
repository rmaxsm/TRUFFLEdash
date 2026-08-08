# Documentation for Rosters
# current csv name: rosters.csv (+ oldrosters.csv)
# DuckDB table name: rosters
# scraping script source: rosters.csv

# From global.R ----
library(readr)      # for read_csv(), cols() → reading csvs into R
library(data.table) # for as.data.table(), fast joins, ordering, column ops
library(stringr)    # for str_replace_all() → string cleaning/replacement
library(DBI)        # for dbConnect(), dbExecute(), dbWriteTable()
library(duckdb)     # for duckdb() driver and integration with DBI

# initial csv loads
# file of current TRUFFLE & KERFUFFLE rosters and insert league column
trf_rosters <- read_csv("data/rosters.csv", col_types = cols())
trf_rosters$League <- "TRUFFLE"

krf_rosters <- read_csv("data/kerfuffle/kerfuffle_rosters.csv", col_types = cols())
krf_rosters$League <- "KERFUFFLE"

# colnames discrepancy fix (forces identical schema before binding)
colnames(krf_rosters) <- colnames(trf_rosters)

# rbind rosters across leagues and turn into data.table
rosters <- as.data.table(rbind(trf_rosters, krf_rosters)); rm(trf_rosters, krf_rosters)

# enforce consistent column names and order
colnames(rosters) <- c("Pos", "TRUFFLE", "Player", "NFL", "Opp", "GameTime", "Bye", "O/U", "PosRnk", "Ovp", "Rost", "Start", "Salary", "Contract", "Last", "Avg", "Proj", "League")
rosters <- rosters[, c("League", "TRUFFLE", "Player",  "Pos", "NFL", "Opp", "GameTime", "Bye", "O/U", "PosRnk", "Ovp", "Rost", "Start", "Salary", "Contract", "Last", "Avg", "Proj")]

# oldrosters.csv
# get old rosters and merge in current teams to see what TRUFFLE team players were on which year
oldrosters <- as.data.table(read_csv("data/oldrosters.csv", col_types = cols()))

# delete the Ring and BenchCup columns from oldrosters (will be inferred from Awards page in the future)
oldrosters$Ring <- NULL; oldrosters$BenchCup <- NULL

# add current year rosters

# subset of columns to match oldrosters
mergerosters <- rosters[, .(League, TRUFFLE, Pos, Player, NFL, Salary, Contract)]

# create Season column with current year
mergerosters$Season <- 2025

# change column order to match oldrosters
mergerosters <- mergerosters[, .(League, Season, TRUFFLE, Pos, Player, NFL, Salary, Contract)]

# bind together for final oldrosters table (sorted by Player, Season)
oldrosters <- as.data.table(rbind(oldrosters, mergerosters))[order(Player,Season)]

# remove the helper table to keep environment clean
rm(mergerosters)

# From TRUFFLEdashOmni.R ----
# DuckDB + MotherDuck connection setup

# open DuckDB connection via DBI
duckDBcon <- dbConnect(duckdb::duckdb())

# load the MotherDuck extension
dbExecute(duckDBcon, "LOAD 'motherduck'")

# attach the MotherDuck cloud db (prefix md:)
dbExecute(duckDBcon, "ATTACH 'md:'")

# set active schema/database to "td"
dbExecute(duckDBcon, "USE td")
# note: may prompt login popup for MotherDuck auth

# pull in oldrosters as omni_rosters
# add composite keys and standardize player/pos identifiers
omni_rosters <- oldrosters
omni_rosters <- omni_rosters[, .(
  SznLgTrf = paste(Season, League, TRUFFLE, sep = "_"),               # Season + League + Team
  LgTrf    = paste(League, TRUFFLE, sep = "_"),                       # League + Team
  TrfLg    = League,                                                  # League only
  TrfTm    = TRUFFLE,                                                 # Team only
  SznPlPos = paste(Season, str_replace_all(Player, " ", ""), Pos, sep = "_"), # Season + Player(no spaces) + Pos
  PlPos    = paste(str_replace_all(Player, " ", ""), Pos, sep = "_"),        # Player(no spaces) + Pos
  Season,
  Player,
  Pos,
  NFL,
  Salary,
  Contract
)]

# final DuckDB push
# note: currently overwrites the rosters table each run
# TODO: consider appending only current season instead of full overwrite
dbWriteTable(duckDBcon, "rosters", omni_rosters, overwrite = TRUE)
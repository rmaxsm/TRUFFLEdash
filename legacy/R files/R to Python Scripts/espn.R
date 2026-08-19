#Documentation for ESPN Stats from csv read to DuckDB push
#current csv name: espnStats.csv
#DuckDB table name: espn
#scraping script source: advancedCombined.py
# -> Script reads cleaned ESPN fantasy stats from CSV, processes them in R, and prepares to push into DuckDB.


# From global.R ----
# -> Code originally comes from the global.R setup file for your app.

library(data.table)
library(readr)
library(stringr)
library(DBI)
library(duckdb)

#espn data load from CSV, inherits scrape result column names
# -> Reads in espnStats.csv as a data.table with fixed column types to ensure correct numeric/character parsing.
espn <- suppressWarnings(as.data.table(read_csv("data/espnStats.csv", col_types = cols(Season = col_double(),
                                                                                       Player = col_character(),
                                                                                       NFL = col_character(),
                                                                                       Pos = col_character(),
                                                                                       xFP = col_double(),
                                                                                       ActualPts = col_double(),
                                                                                       xTD = col_double(),
                                                                                       TD = col_double(),
                                                                                       Looks = col_double(),
                                                                                       Diff = col_double(),
                                                                                       In5 = col_double(),
                                                                                       EZ = col_double()))))

#insane one off error handling of Jeffrey Simmons getting specialty package goal line work
# -> Removes one player (Jeffery Simmons) whose stats break consistency due to unique goal-line package plays.
espn <- espn[Player != "Jeffery Simmons"]

#this removes NFL team name and deduplicates in season trades
# -> Groups by Season/Position/Player and aggregates stats across rows to collapse duplicates from trades or multiple team entries.
espn <- suppressWarnings(espn[,
                              .(
                                xFP = sum(xFP, na.rm = T),
                                ActualPts = sum(ActualPts, na.rm = T),
                                xTD = max(xTD, na.rm = T),
                                TD = max(TD, na.rm = T),
                                Looks = max(Looks, na.rm = T),
                                Diff = max(Diff, na.rm = T),
                                In5 = max(In5, na.rm = T),
                                EZ = max(EZ, na.rm = T)
                              ),
                              by = .(Season, Pos, Player)])

#removes weird espn null / 0 interpretation
# -> Replaces bad placeholder values (“-Inf”) with 0 to avoid invalid numeric entries.
espn[espn=="-Inf"] <- 0

#calculates difference column
# -> Adds new column FPDiff = Actual fantasy points - Expected fantasy points.
espn$FPDiff <- espn$ActualPts - espn$xFP

#final column renaming and reordering
# -> Renames Diff column to TDDiff and reorders espn table into a clean standardized schema.
colnames(espn)[9] <- "TDDiff"
espn <- espn[, .(Season, Pos, Player, xFP, ActualPts, FPDiff, xTD, TD, TDDiff, Looks, `In5`, EZ)]

# From TRUFFLEdashOmni.R ----
# -> Code originally comes from TRUFFLEdashOmni.R which handles DuckDB integration.

#library loads
# -> Ensures duckdb library is available for database connection.
library(duckdb)

#duckdb connection
# -> Connects to DuckDB, loads the MotherDuck extension, attaches remote storage, and switches to schema "td".
duckDBcon <- dbConnect(duckdb::duckdb())
dbExecute(duckDBcon, "LOAD 'motherduck'")
dbExecute(duckDBcon, "ATTACH 'md:'")
dbExecute(duckDBcon, "USE td")
# might prompt login popup
# -> User may need to authenticate with MotherDuck at this step.

#start with espn table
# -> Copies processed espn data.table into a new object omni_espn for further transformation.
omni_espn <- espn

#add primary and other keys
# -> Creates unique composite keys (season+player+position) for joining and identification.
omni_espn$SznPlPos <- paste(omni_espn$Season, str_replace_all(omni_espn$Player, " ", ""), omni_espn$Pos, sep = "_")
omni_espn$PlPos <- paste(str_replace_all(omni_espn$Player, " ", ""), omni_espn$Pos, sep = "_")

#final columns and order
# -> Renames certain columns, reorders them, and applies final schema for pushing into DuckDB.
omni_espn <- omni_espn[, .(SznPlPos, PlPos,
                           Season,
                           Player, Pos,
                           xFPts = xFP,
                           FPts = ActualPts,
                           FPdiff = FPDiff,
                           xTD,
                           TD,
                           TDdiff = TDDiff,
                           Looks,
                           RuIn5 = In5,
                           EzTar = EZ)]
# -> (currently commented) Writes omni_espn into DuckDB as table "espn", replacing any existing version.
dbWriteTable(duckDBcon, "espn", omni_espn, overwrite = TRUE)
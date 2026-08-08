#Documentation for Fantasy Pros Snap Share Data from csv read to DuckDB push
#current csv name: snapPer.csv
#DuckDB table name: snaps
#scraping script source: advancedCombined.py
# -> Script reads FantasyPros snap percentage data, cleans and formats it, and prepares for DuckDB storage.


# From global.R ----
# -> Code originally sourced from global.R setup.

library(data.table)
library(readr)
library(stringr)

#initial read in of snapPer.csv
# -> Loads snapPer.csv into a data.table, auto-detecting column types.
snaps <- as.data.table(read_csv("data/snapPer.csv", col_types = cols()))

#hard-coded reordering of the columns
# -> Reorders columns into a consistent format (season, pos, player, team, weeks, then summary stats).
snaps <- snaps[, c(1, 3, 2, 4:22, 24, 23)]

#replace bye week string with N/A value
# -> Replaces "bye" entries in weekly snap columns with proper NA values for missing data.
snaps[snaps == "bye"] <- NA

#rename summary columns
# -> Renames the last two summary columns to "Avg" (average snaps) and "Tot" (total snaps).
colnames(snaps)[23:24] <- c("Avg", "Tot")

#dividing snaps by 100 for percentage formatting using a for loop and converting everything effectively to numeric
# -> Loops through weekly and summary columns, converts them to numeric, and scales values from % (e.g. 65) to proportion (0.65). Also replaces zeros with NA.
for (i in 5:24) {
  snaps[[i]] <- as.numeric(snaps[[i]])
  snaps[[i]] <- snaps[[i]]/100
  #}
}
snaps[snaps == 0] <- NA


# From TRUFFLEdashOmni.R ----
# -> Code originally comes from TRUFFLEdashOmni.R which handles DuckDB integration.
#library loads
library(duckdb)

#duckdb connection
duckDBcon <- dbConnect(duckdb::duckdb())
dbExecute(duckDBcon, "LOAD 'motherduck'")
dbExecute(duckDBcon, "ATTACH 'md:'")
dbExecute(duckDBcon, "USE td")
# might prompt login popup

omni_snaps <- snaps

#rename columns so that they're not just number names
# -> Assigns explicit names to all weekly columns (Wk1, Wk2, … Wk18) for clarity and consistency.
colnames(omni_snaps) <- c("Season", "Pos", "Player", "Team", "Wk1", "Wk2", "Wk3", "Wk4", "Wk5", "Wk6", "Wk7", "Wk8", "Wk9", "Wk10", "Wk11", "Wk12", "Wk13", "Wk14", "Wk15", "Wk16", "Wk17", "Wk18", "Avg", "Tot")

#add DuckDB specific ids, SznPlPos most granular level
# -> Creates unique composite IDs: SznPlPos (season+player+position) and PlPos (player+position) for joins and indexing in DuckDB.
omni_snaps$SznPlPos <- paste(omni_snaps$Season, str_replace_all(omni_snaps$Player, " ", ""), omni_snaps$Pos, sep = "_")
omni_snaps$PlPos <- paste(str_replace_all(omni_snaps$Player, " ", ""), omni_snaps$Pos, sep = "_")

#remove the total column and select final columns for DB push
# -> Drops "Tot" column and selects the final schema (IDs, season, player info, weekly proportions, and Avg) for DuckDB storage.
omni_snaps <- omni_snaps[, .(SznPlPos, PlPos,
                             Season, Player, Pos, NFL = Team,
                             Wk1, Wk2, Wk3, Wk4, Wk5, Wk6, Wk7, Wk8, Wk9, Wk10, Wk11, Wk12, Wk13, Wk14, Wk15, Wk16, Wk17, Wk18, Avg
)]

#final DuckDB push
# -> Writes omni_snaps into DuckDB as table "snaps", overwriting any existing version.
dbWriteTable(duckDBcon, "snaps", omni_snaps, overwrite = TRUE)
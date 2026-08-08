#Documentation for CBS Player IDs from csv read to DuckDB push
#current csv name: playerIDs
#DuckDB table name: ids
#scraping script source: playerIDs.py
# -> Script reads CBS player ID mappings, cleans/merges them, and writes into DuckDB.


# From global.R ----
# -> Code originally comes from the global.R setup file.

library(data.table)
library(readr)

#initial read of csv direct from scrape result
# -> Reads playerIDs.csv into a data.table with column types auto-detected.
ids <- as.data.table(read_csv("data/playerIDs.csv", col_types = cols()))

# -> Ensures the playerID column is treated as character instead of numeric (keeps leading zeros, avoids integer issues).
ids$playerID <- as.character(ids$playerID)

#this is honestly legacy and with 2 leagues we should no longer be hardcoding TRUFFLE specific info into this
# -> Assigns "FA" (free agent) to any row whose TRUFFLE league code isn’t in the approved set, then merges in team number mapping from teams table.
ids$TRUFFLE[!(ids$TRUFFLE %in% c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW"))] <- "FA"
ids <- merge(ids, teams[, c("Abbrev", "TeamNum")], by.x = "TRUFFLE", by.y = "Abbrev", all.x = T)


# From TRUFFLEdashOmni.R ----
# -> Code originally comes from TRUFFLEdashOmni.R which handles DuckDB integration.

#library loads
# -> Ensures duckdb package is available for database connection.
library(duckdb)

#duckdb connection
# -> Opens DuckDB connection, loads MotherDuck extension, attaches remote workspace, and switches to schema "td".
duckDBcon <- dbConnect(duckdb::duckdb())
dbExecute(duckDBcon, "LOAD 'motherduck'")
dbExecute(duckDBcon, "ATTACH 'md:'")
dbExecute(duckDBcon, "USE td")
# might prompt login popup
# -> Authentication may be required if not already logged into MotherDuck.

# subset down to actual columns needed including PlPos main ID
# -> Creates final ids table by selecting only required fields and computing PlPos (Player+Pos) as the main identifier.
omni_ids <- ids
omni_ids <- omni_ids[, .(PlPos = paste(str_replace_all(Player, " ", ""), Pos, sep = "_"),
                         Player,
                         Pos,
                         NFL,
                         cbsID = playerID)]

#final table write
# -> Writes omni_ids into DuckDB as table "ids", overwriting any existing version.
dbWriteTable(duckDBcon, "ids", omni_ids, overwrite = TRUE)
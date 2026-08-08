#library loads
library(duckdb)
library(motherduck)
#install.packages("duckdb")
#install.packages("motherduck")

#duckdb connection
duckDBcon <- dbConnect(duckdb::duckdb())
dbExecute(duckDBcon, "INSTALL 'motherduck'")
dbExecute(duckDBcon, "LOAD 'motherduck'")
dbExecute(duckDBcon, "ATTACH 'md:'")
dbExecute(duckDBcon, "USE td")
# might prompt login popup

#sundates
sundates <- as.data.table(readr::read_csv("TRUFFLEdashOmni/data/sundates.csv", col_types = cols()))
# character data types for now
#sundates$Season <- as.character(sundates$Season)
#sundates$Week <- as.character(sundates$Week)
# convert sunDate to date type
#sundates$sunDate <- as.Date(sundates$sunDate, "%m/%d/%Y")

# ---- modify weekly to omni_weekly
# start with weekly
omni_weekly <- weekly[Scoring == "PPFD", -c("Scoring", "Avg")]
#fix Taysom Hill
omni_weekly$Pos[omni_weekly$Pos == "QB,TE"] <- "TE"
omni_weekly <- rbind(omni_weekly, fantasy[Pos == "DST" & Scoring == "PPFD" & League == "TRUFFLE", -c("Scoring", "Avg")], fill = TRUE)
# bring in sundates
omni_weekly <- merge(omni_weekly, sundates, by = c("Season", "Week")); rm(sundates) # remove sundates
# add primary and other keys
omni_weekly$SznWkPlPos <- paste(omni_weekly$Season, omni_weekly$Week, str_replace_all(omni_weekly$Player, " ", ""), omni_weekly$Pos, sep = "_")
omni_weekly$SznPlPos <- paste(omni_weekly$Season, str_replace_all(omni_weekly$Player, " ", ""), omni_weekly$Pos, sep = "_")
omni_weekly$PlPos <- paste(str_replace_all(omni_weekly$Player, " ", ""), omni_weekly$Pos, sep = "_")
# add cols for other scoring formats
omni_weekly$PPFD <- omni_weekly$FPts
omni_weekly$PPR <- omni_weekly$FPts - omni_weekly$RuFD - omni_weekly$ReFD + omni_weekly$Rec
omni_weekly$hPPR <- omni_weekly$FPts - omni_weekly$RuFD - omni_weekly$ReFD + 0.5*omni_weekly$Rec
omni_weekly$STD <- omni_weekly$FPts - omni_weekly$RuFD - omni_weekly$ReFD

# modify and bring in extra dash
omni_ed <- extradash
#fix Taysom Hill
omni_ed$Pos[omni_ed$Pos == "QB,TE"] <- "TE"
# add primary key
omni_ed$SznWkPlPos <- paste(omni_ed$Season, omni_ed$Week, str_replace_all(omni_ed$Player, " ", ""), omni_ed$Pos, sep = "_")
# join extradash to weekly
omni_weekly <- merge(omni_weekly, omni_ed[, .(SznWkPlPos,
                                          Pa20,
                                          Pa40,
                                          Ru20,
                                          Re20,
                                          Re40,
                                          TotYd)], by = "SznWkPlPos", all.x = TRUE); rm(omni_ed)

# final columns and order
omni_weekly <- omni_weekly[, `:=`(TmTar = sum(Tar)), by = .(Season, Week, NFL)][,
                           .(SznWkPlPos, SznPlPos, PlPos,
                             Season, Week, sunDate,
                             Player, Pos, NFL,
                             Opp, OpRk,
                             PaCmp, PaAtt, PaYd, PaTD, PaInt,
                             RuAtt, RuYd, RuTD, RuFD,
                             Tar, Rec, ReYd, ReTD,ReFD,
                             FL,
                             FPts, PosRk, PPFD, PPR, hPPR, STD,
                             Pa20, Pa40, Ru20, Re20, Re40,TmTar, TotYd,
                             ScrimYd = RuYd + ReYd,
                             PaRuYd = PaYd + RuYd
                             )]

# ---- modify fantasy to omni_fantasy
# start with fantasy
omni_fantasy <- fantasy[Scoring == "PPFD", -c("Scoring", "Avg")]
#fix Taysom Hill
omni_fantasy$Pos[omni_fantasy$Pos == "QB,TE"] <- "TE"
# add primary and other keys
omni_fantasy$SznWkPlPos <- paste(omni_fantasy$Season, omni_fantasy$Week, str_replace_all(omni_fantasy$Player, " ", ""), omni_fantasy$Pos, sep = "_")
omni_fantasy$SznPlPos <- paste(omni_fantasy$Season, str_replace_all(omni_fantasy$Player, " ", ""), omni_fantasy$Pos, sep = "_")
omni_fantasy$PlPos <- paste(str_replace_all(omni_fantasy$Player, " ", ""), omni_fantasy$Pos, sep = "_")
# rename TRUFFLE columns
omni_fantasy$TrfLg <- omni_fantasy$League; omni_fantasy$TrfTm <- omni_fantasy$TRUFFLE
# final columns and order
omni_fantasy <- omni_fantasy[, .(SznWkPlPos, SznPlPos, PlPos,
                                 SznLgTrf = paste(Season, TrfLg, TrfTm, sep = "_"),
                                 LgTrf = paste(TrfLg, TrfTm, sep = "_"),
                                 Season, Week, 
                                 Player, Pos, NFL, 
                                 TrfLg, TrfTm)]

# ---- modify files for optimal lineup calculations including in Omni
# optscoring.csv equivalent of all rostered players
omni_rostered <- opt[, -c("Avg")]
#fix Taysom Hill
omni_rostered$Pos[omni_rostered$Pos == "QB,TE"] <- "TE"
# add primary and other keys
omni_rostered$SznWkPlPos <- paste(omni_rostered$Season, omni_rostered$Week, str_replace_all(omni_rostered$Player, " ", ""), omni_rostered$Pos, sep = "_")
omni_rostered$SznPlPos <- paste(omni_rostered$Season, str_replace_all(omni_rostered$Player, " ", ""), omni_rostered$Pos, sep = "_")
omni_rostered$PlPos <- paste(str_replace_all(omni_rostered$Player, " ", ""), omni_rostered$Pos, sep = "_")
# rename TRUFFLE columns
omni_rostered$TrfLg <- omni_rostered$League; omni_rostered$TrfTm <- omni_rostered$TRUFFLE
# final columns and order
omni_rostered <- omni_rostered[, .(SznWkPlPos, SznPlPos, PlPos,
                                   SznLgTrf = paste(Season, TrfLg, TrfTm, sep = "_"),
                                   LgTrf = paste(TrfLg, TrfTm, sep = "_"),
                                   Season, Week, 
                                   Player, Pos, NFL, 
                                   TrfLg, TrfTm)]

# final optimal lineups
omni_optlineups <- optlineups
omni_optlineups$Pos[omni_optlineups$Pos == "QB,TE"] <- "TE"
# add primary and other keys
omni_optlineups$SznWkPlPos <- paste(omni_optlineups$Season, omni_optlineups$Week, str_replace_all(omni_optlineups$Player, " ", ""), omni_optlineups$Pos, sep = "_")
omni_optlineups$SznPlPos <- paste(omni_optlineups$Season, str_replace_all(omni_optlineups$Player, " ", ""), omni_optlineups$Pos, sep = "_")
omni_optlineups$PlPos <- paste(str_replace_all(omni_optlineups$Player, " ", ""), omni_optlineups$Pos, sep = "_")
# rename TRUFFLE columns
omni_optlineups$TrfLg <- omni_optlineups$League; omni_optlineups$TrfTm <- omni_optlineups$TRUFFLE
# final columns and order
omni_optlineups <- omni_optlineups[, .(SznWkPlPos, SznPlPos, PlPos,
                                       SznLgTrf = paste(Season, TrfLg, TrfTm, sep = "_"),
                                       LgTrf = paste(TrfLg, TrfTm, sep = "_"),
                                       Season, Week, 
                                       Player, Pos, 
                                       TrfLg, TrfTm,
                                       FPts)]

# ---- modify seasons to omni_seasons
omni_seasons <- seasons[Scoring == "PPFD", -c("Scoring")]
#fix Taysom Hill
omni_seasons$Pos[omni_seasons$Pos == "QB,TE"] <- "TE"
# add primary and other keys
omni_seasons$SznPlPos <- paste(omni_seasons$Season, str_replace_all(omni_seasons$Player, " ", ""), omni_seasons$Pos, sep = "_")
omni_seasons$PlPos <- paste(str_replace_all(omni_seasons$Player, " ", ""), omni_seasons$Pos, sep = "_")
# add columns for scoring formats
omni_seasons$PPFD <- omni_seasons$FPts
omni_seasons$PPR <- omni_seasons$FPts - omni_seasons$RuFD - omni_seasons$ReFD + omni_seasons$Rec
omni_seasons$hPPR <- omni_seasons$FPts - omni_seasons$RuFD - omni_seasons$ReFD + 0.5*omni_seasons$Rec
omni_seasons$STD <- omni_seasons$FPts - omni_seasons$RuFD - omni_seasons$ReFD
# final columns and order
omni_seasons <- omni_seasons[,
                           .(SznPlPos, PlPos,
                             Season,
                             Player, Pos, NFL, G,
                             PaCmp, PaAtt, PaYd, PaTD, PaInt,
                             RuAtt, RuYd, RuTD, RuFD,
                             Tar, Rec, ReYd, ReTD,ReFD,
                             FL,
                             Avg, FPts, PosRk, PPFD, PPR, hPPR, STD
                             )]

# ---- modify espn stats
#start with espn table
omni_espn <- espn
#add primary and other keys
omni_espn$SznPlPos <- paste(omni_espn$Season, str_replace_all(omni_espn$Player, " ", ""), omni_espn$Pos, sep = "_")
omni_espn$PlPos <- paste(str_replace_all(omni_espn$Player, " ", ""), omni_espn$Pos, sep = "_")
#final columns and order
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


# ---- pull in oldrosters as omni_rosters
omni_rosters <- oldrosters
#fix Taysom Hill
omni_rosters$Pos[omni_rosters$Pos == "QB,TE"] <- "TE"
omni_rosters <- omni_rosters[, .(SznLgTrf = paste(Season, League, TRUFFLE, sep = "_"),
                                 LgTrf = paste(League, TRUFFLE, sep = "_"),
                                 TrfLg = League,
                                 TrfTm = TRUFFLE,
                                 SznPlPos = paste(Season, str_replace_all(Player, " ", ""), Pos, sep = "_"),
                                 PlPos = paste(str_replace_all(Player, " ", ""), Pos, sep = "_"),
                                 Season,
                                 Player,
                                 Pos,
                                 NFL,
                                 Salary,
                                 Contract
                                 )]

# birthdays as well
omni_bdays <- as.data.table(bdays)
omni_bdays$PlPos <- paste(str_replace_all(omni_bdays$Player, " ", ""), omni_bdays$Pos, sep = "_")
omni_bdays <- omni_bdays[, .(PlPos,
                             Player,
                             Pos,
                             NFL,
                             Birthday
                             )]

# ---- Fantasy Pros snap percentages
omni_snaps <- snaps
colnames(omni_snaps) <- c("Season", "Pos", "Player", "Team", "Wk1", "Wk2", "Wk3", "Wk4", "Wk5", "Wk6", "Wk7", "Wk8", "Wk9", "Wk10", "Wk11", "Wk12", "Wk13", "Wk14", "Wk15", "Wk16", "Wk17", "Wk18", "Avg", "Tot")
omni_snaps$SznPlPos <- paste(omni_snaps$Season, str_replace_all(omni_snaps$Player, " ", ""), omni_snaps$Pos, sep = "_")
omni_snaps$PlPos <- paste(str_replace_all(omni_snaps$Player, " ", ""), omni_snaps$Pos, sep = "_")
omni_snaps <- omni_snaps[, .(SznPlPos, PlPos,
                             Season, Player, Pos, NFL = Team,
                             Wk1, Wk2, Wk3, Wk4, Wk5, Wk6, Wk7, Wk8, Wk9, Wk10, Wk11, Wk12, Wk13, Wk14, Wk15, Wk16, Wk17, Wk18, Avg
                             )]

# ---- TRUFFLE awards
omni_awards <- awards
omni_awards <- omni_awards[, .(SznLgTrf = paste(Season, League, TRUFFLE, sep = "_"),
                               LgTrf = paste(League, TRUFFLE, sep = "_"),
                               TrfLg = League,
                               TrfTm = TRUFFLE,
                               SznPlPos = paste(Season, str_replace_all(Winner, " ", ""), Pos, sep = "_"),
                               PlPos = paste(str_replace_all(Winner, " ", ""), Pos, sep = "_"),
                               Season,
                               Winner, Player = Winner,
                               Pos,
                               Award,
                               Logo,
                               Image
)]

# ---- TRUFFLE drafts
omni_drafts <- draft
omni_drafts <- omni_drafts[, .(SznLgTrf = paste(Season, League, TRF, sep = "_"),
                               LgTrf = paste(League, TRF, sep = "_"),
                               TrfLg = League,
                               TrfTm = TRF,
                               SznPlPos = paste(Season, str_replace_all(Player, " ", ""), Pos, sep = "_"),
                               PlPos = paste(str_replace_all(Player, " ", ""), Pos, sep = "_"),
                               Round,
                               Pick = `#`,
                               Season,
                               Player,
                               Pos,
                               Salary,
                               Extension)]

# ---- CBS player IDs
omni_ids <- ids
#fix Taysom Hill
omni_ids$Pos[omni_ids$Pos == "QB,TE"] <- "TE"
omni_ids <- omni_ids[, .(PlPos = paste(str_replace_all(Player, " ", ""), Pos, sep = "_"),
                         Player,
                         Pos,
                         NFL,
                         cbsID = playerID)]

# ---- TRUFFLE teams info
omni_teams <- as.data.table(read_csv("TRUFFLEdashOmni/data/teams_omni.csv", col_types = cols()))
omni_teams <- omni_teams[, .(LgTrf = paste(League, Abbrev, sep = "_"),
                             TrfLg = League,
                             TrfTm = Abbrev,
                             FullName,
                             Owner,
                             Division,
                             DivAbbrev,
                             Location,
                             Mascot,
                             Founded,
                             PrimaryHex = Primary,
                             SecondaryHex = Secondary,
                             TertiaryHex = Tertiary,
                             HighlightHex = Highlight,
                             DashTextHex = DashT,
                             DashBackgroundHex = DashB,
                             TeamNum,
                             RivalryName,
                             Rivalry,
                             CbsURL = TmURL,
                             LogoURL,
                             TdURL,
                             DivLogoURL,
                             RivLogoURL,
                             LgURL
                             )]

# ---- franchise tag records, top 5 paid, and rookierights for extensions
omni_franchisetags <- franchised
omni_franchisetags <- omni_franchisetags[, .(SznPlPos = paste(Season, str_replace_all(Player, " ", ""), Pos, sep = "_"),
                                             PlPos = paste(str_replace_all(Player, " ", ""), Pos, sep = "_"),
                                             League,
                                             Season,
                                             Player,
                                             Pos,
                                             Salary = Value)]

omni_top5paid <- top5paid
omni_top5paid <- omni_top5paid[, .(SznPlPos = paste(Season, str_replace_all(Player, " ", ""), Pos, sep = "_"),
                                   PlPos = paste(str_replace_all(Player, " ", ""), Pos, sep = "_"),
                                   League,
                                   Season,
                                   Player,
                                   Pos,
                                   Salary)]

omni_rookierights <- rookierights
omni_rookierights <- omni_rookierights[, .(PlPos = paste(str_replace_all(Player, " ", ""), Pos, sep = "_"),
                                           League,
                                           Player,
                                           Pos)]



# ---- final motherduck uploads
dbWriteTable(duckDBcon, "weekly", omni_weekly, overwrite = TRUE)
dbWriteTable(duckDBcon, "fantasy", omni_fantasy, overwrite = TRUE)
dbWriteTable(duckDBcon, "rostered", omni_rostered, overwrite = TRUE)
dbWriteTable(duckDBcon, "optlineups", omni_optlineups, overwrite = TRUE)
dbWriteTable(duckDBcon, "seasons", omni_seasons, overwrite = TRUE)
dbWriteTable(duckDBcon, "espn", omni_espn, overwrite = TRUE)
dbWriteTable(duckDBcon, "rosters", omni_rosters, overwrite = TRUE)
dbWriteTable(duckDBcon, "bdays", omni_bdays, overwrite = TRUE)
dbWriteTable(duckDBcon, "snaps", omni_snaps, overwrite = TRUE)
dbWriteTable(duckDBcon, "awards", omni_awards, overwrite = TRUE)
dbWriteTable(duckDBcon, "ids", omni_ids, overwrite = TRUE)
dbWriteTable(duckDBcon, "drafts", omni_drafts, overwrite = TRUE)
dbWriteTable(duckDBcon, "teams", omni_teams, overwrite = TRUE)
dbWriteTable(duckDBcon, "franchisetags", omni_franchisetags, overwrite = TRUE)
dbWriteTable(duckDBcon, "top5paid", omni_top5paid, overwrite = TRUE)
dbWriteTable(duckDBcon, "rookierights", omni_rookierights, overwrite = TRUE)
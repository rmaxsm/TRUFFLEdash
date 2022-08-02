# Library loads & setWD -----

#setWD

library(shiny)
library(tidyverse)
library(shinydashboard)
library(readr)
library(readxl)
library(stringr)
library(data.table)
library(kableExtra)
library(tibble)
library(plotly)
library(reactable)
library(sparkline)
library(htmltools)
library(crosstalk)
library(shinyBS)
library(markdown)

#getting the newly setup ui pages
source("dashboardPage.R")
source("loginPage.R")

#password stuff
num_fails_to_lockout <- 1000
# credentials <- data.frame(user = c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW"),
#                           stringsAsFactors = FALSE)
# saveRDS(credentials, "credentials/credentials.rds")

# setting colors -----
#colors and global options 

minAvg <- 3
currentyr <- 2022

options(reactable.language = reactableLang(
  pagePrevious = "\u276e",
  pageNext = "\u276f"
))

globalcol <- "#84A4D8"
QBcolor <- "#b7e1cd"
RBcolor <- "#f4cccc"
WRcolor <- "#cfe2f3"
TEcolor <- "#fce5cd"
DSTcolor <- "#D9D2E9"
DCcolor <- "#D9D9D9"
IRcolor <- "#ea9999"
minscale <- "#E8F4EE"
maxscale <-"#529667"
greenscale <- "#57E19F"
redscale <- "#F48E90"
fptsbackground <- "#F2F2F2"
fptscolor <- "#1E65D2"
textgreen <- "#00B050"
textred <- "#C00000"

#graphing plotly palet and font
ft <- list(
  family = "Helvetica Neue",
  size = 11)

pal <- c("#b7e1cd", "#f4cccc", "#cfe2f3", "#fce5cd", "#D9D2E9", "#D9D9D9", "#ea9999")
pal <- setNames(pal, c("QB", "RB", "WR", "TE", "DST", "DC", "IR"))

# Reading in and cleaning data from Excels/csvs -----

#file of CBS player IDs
ids <- read_csv("data/playerIDs.csv")
ids$playerID <- as.character(ids$playerID)
ids$TRUFFLE[!(ids$TRUFFLE %in% c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW"))] <- "FA"
ids <- merge(ids, teams[, c("Abbrev", "TeamNum")], by.x = "TRUFFLE", by.y = "Abbrev", all.x = T)

#file of TRUFFLE team info
teams <- read_excel("data/teams.xlsx")

#file of weekly scoring for players started/active in TRUFFLE
fantasy <- read_excel("data/fantasy.xlsx")
cleanFantasy <- function(file) {
  #deselect columns to delete and modify avg
  file <- file[, -c(grep("Del", colnames(file)))]
  file$Avg <- as.numeric(file$Avg)
  
  #modifying player column to parse out positions, teams, and player names
  file <- add_column(file, Pos = NA, .before = "Player")
  file <- add_column(file, NFL = NA, .after = "Player")
  file$NFL <- str_trim(substr(file$Player, as.numeric(gregexpr(pattern = "\\|", file$Player)) + 1, str_length(file$Player)))
  file$Pos <- str_trim(substr(file$Player, as.numeric(gregexpr(pattern = "\\|", file$Player))-4, as.numeric(gregexpr(pattern = "\\|", file$Player))-2))
  file$Player <- substr(file$Player, 1, as.numeric(gregexpr(pattern = "\\|", file$Player))-5)
  file$Player <- str_replace_all(file$Player,"\\.","")
  file$Player <- str_replace_all(file$Player," Jr","")
  file$Player <- str_replace_all(file$Player," Sr","")
  file$Player <- str_replace_all(file$Player," III","")
  file$Player <- str_replace_all(file$Player," II","")
  file$Player <- str_replace_all(file$Player,"Will Fuller V","Will Fuller")
  
  #making small change for defenses
  file$Avg[file$Pos == "DST"] <- file$PaCmp[file$Pos == "DST"]
  file$FPts[file$Pos == "DST"] <- file$PaAtt[file$Pos == "DST"]
  file$PaAtt[file$Pos == "DST"] <- NA
  file$PaCmp[file$Pos == "DST"] <- NA
  
  ints <- colnames(file[is.element(colnames(file), c("TRUFFLE","Pos","Player","NFL","Opp","Avg","FPts"))==F])
  file[,ints] <- lapply(file[,ints], as.integer)
  
  file <- file[, c(24:25, 1, 2, 3, 4, 5:23)]
  
  return(file)
}
fantasy <- as.data.table(cleanFantasy(fantasy))

#file of full season data for players dating back to 2015
seasons <- read_excel("data/seasons.xlsx")
cleanSeasons <- function(file) {
  #deselect columns to delete
  file <- file[, -c(grep("Del", colnames(file)))]
  
  #merge in correct team abbreviations
  file$TRUFFLE <- NA
  file$OpRk <- NULL
  file$Opp <- NULL
  
  #modifying player column to parse out positions, teams, and player names
  file <- add_column(file, G = round(file$FPts/file$Avg), .before = "PaCmp")
  file <- add_column(file, Pos = NA, .before = "Player")
  file <- add_column(file, NFL = NA, .after = "Player")
  file$NFL <- str_trim(substr(file$Player, as.numeric(gregexpr(pattern = "\\|", file$Player)) + 1, str_length(file$Player)))
  file$Pos <- str_trim(substr(file$Player, as.numeric(gregexpr(pattern = "\\|", file$Player))-4, as.numeric(gregexpr(pattern = "\\|", file$Player))-2))
  file$Player <- substr(file$Player, 1, as.numeric(gregexpr(pattern = "\\|", file$Player))-5)
  file$Player <- str_replace_all(file$Player,"\\.","")
  file$Player <- str_replace_all(file$Player," Jr","")
  file$Player <- str_replace_all(file$Player," Sr","")
  file$Player <- str_replace_all(file$Player," III","")
  file$Player <- str_replace_all(file$Player," II","")
  file$Player <- str_replace_all(file$Player,"Will Fuller V","Will Fuller")
  
  ints <- colnames(file[is.element(colnames(file), c("TRUFFLE","Pos","Player","NFL","Opp","Avg","FPts"))==F])
  file[,ints] <- lapply(file[,ints], as.integer)
  
  file <- file[, c(23, 2:22)]
  
  return(file)
}
seasons <- as.data.table(cleanSeasons(seasons))

#import fantasy pros file to use for age
fprosage <- read_excel("data/fprosage.xlsx")
cleanFprosage <- function(file) {
  colnames(file) <- c("DynRk", "Player", "NFL", "DynPosRk", "Bye", "AgePH", "SOS", "EcfADP")
  file$Player <- str_replace_all(file$Player,"\\.","")
  file$Player <- str_replace_all(file$Player," Jr","")
  file$Player <- str_replace_all(file$Player," Sr","")
  file$Player <- str_replace_all(file$Player," III","")
  file$Player <- str_replace_all(file$Player," II","")
  file$Player <- str_replace_all(file$Player,"Will Fuller V","Will Fuller")
  file$Player <- str_replace_all(file$Player,"La'Mical","Lamical")
  
  return(file)
}
fprosage <- cleanFprosage(fprosage)

#file of current TRUFFLE rosters
rosters <- read_excel("data/rosters.xlsx")
cleanRosters <- function(file) {
  #find all the rows with all caps players which designate next team
  dummy <- c(grep("PLAYERS", file$Pos), nrow(file))
  #vector of teams in order
  truffle_ordered <- c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW")
  
  #loop through and input team abbreviations
  for (t in 1:12) {
    file$TRUFFLE[dummy[t]:dummy[t+1]] <- truffle_ordered[t]
  }
  
  injured <- grep("^INJURED$", file$Pos)
  practice <- grep("^PRACTICE$", file$Pos)
  file$IR <- NA
  
  for (t in 1:12) {
    file$IR[injured[t]:practice[t]] <- "IR"
  }
  
  #convert columns to numeric
  file[,which(colnames(file) == "Bye"):which(colnames(file) == "Proj")] <-
    sapply(file[,which(colnames(file) == "Bye"):which(colnames(file) == "Proj")],as.numeric)
  
  #convert to DT
  file <- as.data.table(file)
  
  #takeout unnecessary rows
  file <- file[Proj >= 0]
  
  #modifying player column to parse out positions, teams, and player names
  file <- add_column(file, NFL = NA, .after = "Player")
  file$NFL <- str_trim(substr(file$Player, as.numeric(gregexpr(pattern = "\\|", file$Player)) + 1, str_length(file$Player)))
  file$Pos <- str_trim(substr(file$Player, as.numeric(gregexpr(pattern = "\\|", file$Player))-4, as.numeric(gregexpr(pattern = "\\|", file$Player))-2))
  file$Player <- substr(file$Player, 1, as.numeric(gregexpr(pattern = "\\|", file$Player))-5)
  file$Player <- str_replace_all(file$Player,"\\.","")
  file$Player <- str_replace_all(file$Player," Jr","")
  file$Player <- str_replace_all(file$Player," III","")
  file$Player <- str_replace_all(file$Player," II","")
  file$Player <- str_replace_all(file$Player,"Will Fuller V","Will Fuller")
  file$Pos[grepl("Lynn Bowden", file$Player)] <- "WR"
  file$Player[grepl("Lynn Bowden", file$Player)] <- "Lynn Bowden"
  
  file$Pos[is.na(file$IR) == F] <- paste0(file$IR[is.na(file$IR) == F],"-",file$Pos[is.na(file$IR) == F])
  file$IR <- NULL
  
  #merge in correct team abbreviations
  file <- merge(x = file, y = fprosage[ , c("Player", "AgePH")], by = "Player", all.x=TRUE)
  file <- add_column(file, Age = NA, .after = "Player")
  file$Age <- as.integer(file$AgePH)
  file$AgePH <- NULL
  
  dummy <- grep("Dead Cap", file$Player)
  file$Pos[dummy] <- "DC"
  
  #finalized 
  return(file)
}
rosters <- cleanRosters(rosters)
tmp1 <- rosters
tmp2 <- rosters

#file of weekly scoring across NFL
weekly <- read_excel("data/weekly.xlsx")
cleanWeekly <- function(file) {
  #remove players that didnt play in a week
  file <- filter(file, is.na(Avg) == F)
  
  #deselect columns to delete
  file <- file[, -c(grep("Del", colnames(file)))]
  
  #modifying player column to parse out positions, teams, and player names
  file <- add_column(file, Pos = NA, .before = "Player")
  file <- add_column(file, NFL = NA, .after = "Player")
  file$NFL <- str_trim(substr(file$Player, as.numeric(gregexpr(pattern = "\\|", file$Player)) + 1, str_length(file$Player)))
  file$Pos <- str_trim(substr(file$Player, as.numeric(gregexpr(pattern = "\\|", file$Player))-4, as.numeric(gregexpr(pattern = "\\|", file$Player))-2))
  file$Player <- substr(file$Player, 1, as.numeric(gregexpr(pattern = "\\|", file$Player))-5)
  file$Player <- str_replace_all(file$Player,"\\.","")
  file$Player <- str_replace_all(file$Player," Jr","")
  file$Player <- str_replace_all(file$Player," Sr","")
  file$Player <- str_replace_all(file$Player," III","")
  file$Player <- str_replace_all(file$Player," II","")
  file$Player <- str_replace_all(file$Player,"Will Fuller V","Will Fuller")
  
  #merge in correct team abbreviations
  file$TRUFFLE <- NA
  file <- merge(x = file, y = rosters[ , c("Player", "TRUFFLE")], by = "Player", all.x=TRUE)
  #replace initial TRUFFLE column with Abbrev's and delete extra column
  file$TRUFFLE.x <- file$TRUFFLE.y
  file$TRUFFLE.y <- NULL
  setnames(file, "TRUFFLE.x", "TRUFFLE")
  file$TRUFFLE[is.na(file$TRUFFLE)]  <- "FA"
  
  ints <- colnames(file[is.element(colnames(file), c("TRUFFLE","Pos","Player","NFL","Opp","Avg","FPts"))==F])
  file[,ints] <- lapply(file[,ints], as.integer)
  
  file <- file[, c(24:25, 2, 3, 4, 1, 5:23)]
  
  return(file)
}
weekly <- as.data.table(cleanWeekly(weekly))
weekly <- weekly[order(-Season,Week,-FPts)][, `:=`(PosRk = 1:.N), by = .(Season, Week, Pos)]

#add current season
currentseason <-weekly[, !c("Opp", "OpRk")]
currentseason <- currentseason[Season == max(weekly$Season),
                               .(G = .N,
                                 PaCmp = sum(PaCmp),
                                 PaAtt = sum(PaAtt),
                                 PaYd = sum(PaYd),
                                 PaTD = sum(PaTD),
                                 PaInt = sum(PaInt),
                                 RuAtt = sum(RuAtt),
                                 RuYd = sum(RuYd),
                                 RuTD = sum(RuTD),
                                 RuFD = sum(RuFD),
                                 Tar = sum(Tar),
                                 Rec = sum(Rec),
                                 ReYd = sum(ReYd),
                                 ReTD = sum(ReTD),
                                 ReFD = sum(ReFD),
                                 FL = sum(FL),
                                 Avg = round(mean(FPts),2),
                                 FPts = sum(FPts)
                               ),
                               by = .(Season, Pos, Player, NFL)]

seasons <- rbind(seasons, currentseason)
seasons <- seasons[order(-Season,-FPts)][, `:=`(PosRk = 1:.N), by = .(Season, Pos)]

#file to indicate what players have rookie rights
rookierights <- read_excel("data/rookierights.xlsx")
rookierights <- as.vector(rookierights$Player)

#file of upcoming draft order
draft <- as.data.table(read_excel("data/drafts.xlsx"))

# modifying tables for display -----

# helpful vectors
positionorder <- c("QB","RB","WR","TE", "DST", "IR", "DC")
morethan10 <- currentseason$Player[currentseason$FPts > 10]

#team portal tables
teamportal<- rosters[, .(TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract)]
teamportal <- merge(teamportal, currentseason[, !c( "Season","Pos", "NFL")], by = 'Player', all.x = T)
teamportal$Avg[teamportal$Pos == "DST"] <- rosters$Avg[rosters$Pos == "DST"]
tpoverview <- teamportal[, .(TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract, G, Avg, FPts)][order(match(Pos, positionorder), -Avg)]

ptslogs <- weekly[is.element(Player, rosters$Player),
                  .(ptslog = list(FPts)),
                  by = .(Season, Pos, Player)]

tpoverview <- merge(tpoverview, ptslogs[Season == max(seasons$Season), c("Player","ptslog")], by = 'Player', all.x = T)
tpoverview <- merge(tpoverview, seasons[Season == max(seasons$Season)][, c("Player","PosRk")], by = 'Player', all.x = T)
tpoverview <- tpoverview[, .(TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract, G, PosRk, ptslog, Avg, FPts)][order(match(Pos, positionorder), -Avg)]
#tpoverview <- merge(tpoverview, ids[, c("Player","playerID")], by = 'Player', all.x = T)
#tpoverview <- tpoverview[, .(playerID, TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract, G, PosRk, ptslog, Avg, FPts)][order(match(Pos, positionorder), -Avg)]
#colnames(tpoverview)[1] <- "Action"
#bug fix
tpoverview$Avg[tpoverview$Pos == "DST"] <- NA

#contracts table
contracts <- rosters[, .(TRUFFLE, Pos, Player, Age, NFL, Salary, Contract)]
contracts <- contracts[, `:=`(`'22` = Salary,
                              `'23` = ifelse(Contract > 1, Salary, 
                                             ifelse(Contract == 1 & is.element(Player, rookierights) == F, "FA",
                                                    ifelse(Contract == 1 & is.element(Player, rookierights) == T, "RR", "-"))),
                              `'24` = ifelse(Contract > 2, Salary,
                                             ifelse(Contract == 2 & is.element(Player, rookierights) == F, "FA",
                                                    ifelse(Contract == 2 & is.element(Player, rookierights) == T, "RR", "-"))),
                              `'25` = ifelse(Contract > 3, Salary,
                                             ifelse(Contract == 3 & is.element(Player, rookierights) == F, "FA",
                                                    ifelse(Contract == 3 & is.element(Player, rookierights) == T, "RR", "-")))
)][order(-Salary)]

ppd <- teamportal[, `:=`(`PP$` = round(FPts/Salary,2),
                         `wPP$`= round(Avg/Salary,2))]

#aggregating fantasy data by team
teamsfantasyweekly <- fantasy[,
                              .(FPts = sum(FPts)),
                              by = .(TRUFFLE, Season, Week)]
teamsfantasy <- teamsfantasyweekly[,
                                   .(Weekly = list(FPts),
                                     Low = min(FPts),
                                     High = max(FPts),
                                     StdDev = round(sd(FPts)),
                                     Avg = round(mean(FPts)),
                                     Total = round(sum(FPts))),
                                   by = .(Season, TRUFFLE)][order(-Total)]

pointsleaders <- weekly[,
                        .(ptslogs = list(FPts),
                          Avg = round(mean(FPts),1),
                          Total = round(sum(FPts))),
                        by = .(Season, TRUFFLE, Pos, Player)][order(-Season, match(Pos, positionorder), -Total)][, `:=`(PosRk = 1:.N), by = .(Season, Pos)][, c("Season", "TRUFFLE", "Player", "Pos", "PosRk", "ptslogs", "Avg", "Total")][order(-Total)]

ppbios <- weekly[order(-Season,-Week)]
ppbios <- ppbios[,
                 .(NFL = NFL[1],
                   ptslogs = list(FPts),
                   Avg = round(mean(FPts),1),
                   Total = round(sum(FPts))),
                 by = .(TRUFFLE, Pos, Player)]
ppbios <- merge(ppbios, rosters[, c("Player","Salary", "Contract")], by = 'Player', all.x = T)
ppbios <- merge(ppbios, fprosage[, c("Player", "AgePH", "DynRk", "DynPosRk")])
ppbios <- ppbios[, c("TRUFFLE","Pos","Player", "NFL", "AgePH", "DynRk", "DynPosRk","Salary", "Contract", "ptslogs")]

advanced <- weekly[, .(FPts = sum(FPts),
                       YdPts = round(.04*sum(PaYd) + .1*(sum(RuYd) + sum(ReYd)),1),
                       TDPts = 4*sum(PaTD) + 6*(sum(RuTD) + sum(ReTD)),
                       FDPts = sum(RuFD) + sum(ReFD),
                       RuPts = .1*sum(RuYd) + 6*sum(RuTD) + sum(RuFD),
                       RePts = .1*sum(ReYd) + 6*sum(ReTD) + sum(ReFD),
                       Touch = sum(PaCmp + RuAtt + Rec),
                       Opp = sum(PaAtt + RuAtt + Tar)
),
by = .(Season,TRUFFLE,Pos,Player)][, `:=`(`YdPt%` = YdPts / FPts,
                                          `TDPt%` = TDPts / FPts,
                                          `FDPt%` = FDPts / FPts,
                                          `RuPt%` = RuPts / FPts,
                                          `RePt%` = RePts / FPts,
                                          `FPts/Touch` = round(FPts/Touch, 3),
                                          `FPts/Opp` = round(FPts/Opp, 3)
)][order(-FPts)][, c("Season","TRUFFLE","Pos","Player","FPts","Touch","Opp","FPts/Touch","FPts/Opp","YdPts","TDPts","FDPts","RuPts","RePts","YdPt%","TDPt%","FDPt%","RuPt%","RePt%")]

consistencystart <- as.data.frame(weekly)
consistencystart <- as.data.table(consistencystart)
consistency <- consistencystart[, `:=` (
  top5dum = ifelse(PosRk <= 5, 1, 0),
  top12dum = ifelse(PosRk <= 12, 1, 0),
  top24dum = ifelse(PosRk <= 24, 1, 0),
  top36dum = ifelse(PosRk <= 36, 1, 0),
  nonStartdum = ifelse(PosRk > 36, 1, 0),
  lt10dum = ifelse(FPts < 10, 1, 0),
  gt10dum = ifelse(FPts >= 10, 1, 0),
  gt20dum = ifelse(FPts >= 20, 1, 0),
  gt30dum = ifelse(FPts >= 30, 1, 0)
)][,
   .(Avg = round(mean(FPts),1),
     RelSD = round(sd(FPts)/mean(FPts),2),
     AvgPosRk = round(mean(PosRk),1),
     `Top5 %` = sum(top12dum)/.N,
     `Top12 %` = sum(top12dum)/.N,
     `Top24 %` = sum(top24dum)/.N,
     `Top36 %` = sum(top36dum)/.N,
     `NonStart %` = sum(nonStartdum)/.N,
     `>10 %` = sum(gt10dum)/.N,
     `>20 %` = sum(gt20dum)/.N,
     `>30 %` = sum(gt30dum)/.N
   ),
   by = .(Season ,TRUFFLE, Pos, Player)][order(-Avg)][, c("Season","TRUFFLE","Pos","Player","Avg","RelSD",">10 %",">20 %",">30 %","AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %")]

weeklytop5 <- weekly[, c("Season", "Week", "TRUFFLE", "Pos", "Player", "FPts")][order(Week,-FPts)][, .(TRUFFLE = TRUFFLE[1:5],
                                                                                                       Player = Player[1:5],
                                                                                                       FPts = FPts[1:5]), 
                                                                                                   by = .(Season,Week, Pos)][, .(Season,Week,TRUFFLE,Pos,Player,FPts)]

weeklytop5qb <- weeklytop5[Pos == "QB"]
weeklytop5rb <- weeklytop5[Pos == "RB"]
weeklytop5wr <- weeklytop5[Pos == "WR"]
weeklytop5te <- weeklytop5[Pos == "TE"]

truffleanalysis <- fantasy[,
                           .(FPts = sum(FPts),
                             QBpts = sum(FPts[Pos == "QB"]),
                             RBpts = sum(FPts[Pos == "RB"]),
                             WRpts = sum(FPts[Pos == "WR"]),
                             TEpts = sum(FPts[Pos == "TE"]),
                             DSTpts = sum(FPts[Pos == "DST"]),
                             Ydpts = .04*sum(PaYd,na.rm=T) + .1*sum(RuYd + ReYd,na.rm=T),
                             TDpts = 4*sum(PaTD,na.rm=T) + 6*sum(RuTD + ReTD,na.rm=T),
                             FDpts = sum(RuFD + ReFD,na.rm=T),
                             TOpts = -2*sum(PaInt + FL,na.rm=T)
                           ),
                           by = .(Season, TRUFFLE)][order(-FPts)]

truffleanalysisperc <- fantasy[,
                               .(FPts = sum(FPts),
                                 QBpts = sum(FPts[Pos == "QB"])/sum(FPts),
                                 RBpts = sum(FPts[Pos == "RB"])/sum(FPts),
                                 WRpts = sum(FPts[Pos == "WR"])/sum(FPts),
                                 TEpts = sum(FPts[Pos == "TE"])/sum(FPts),
                                 DSTpts = sum(FPts[Pos == "DST"])/sum(FPts),
                                 Ydpts = (.04*sum(PaYd,na.rm=T) + .1*sum(RuYd + ReYd,na.rm=T))/sum(FPts),
                                 TDpts = (4*sum(PaTD,na.rm=T) + 6*sum(RuTD + ReTD,na.rm=T))/sum(FPts),
                                 FDpts = (sum(RuFD + ReFD,na.rm=T))/sum(FPts),
                                 TOpts = (-2*sum(PaInt + FL,na.rm=T))/sum(FPts)
                               ),
                               by = .(Season, TRUFFLE)][order(-FPts)]

#building the record books -----
recordbookstm <- fantasy[,
                         .(FPts = round(sum(FPts),1),
                           Games = .N,
                           Avg = round(mean(FPts),1),
                           FD = sum(RuFD + ReFD),
                           PaYd = sum(PaYd),
                           PaTD = sum(PaTD),
                           PaInt = sum(PaInt),
                           PaCmp = sum(PaCmp),
                           RuYd = sum(RuYd),
                           RuTD = sum(RuTD),
                           RuFD = sum(RuFD),
                           FL = sum(FL),
                           ReYd = sum(ReYd),
                           ReTD = sum(ReTD),
                           ReFD = sum(ReFD),
                           Rec = sum(Rec)
                         ),
                         by = .(TRUFFLE, Pos, Player)]

recordbookspl <- fantasy[,
                         .(FPts = round(sum(FPts),1),
                           Games = .N,
                           Avg = round(mean(FPts),1),
                           FD = sum(RuFD + ReFD),
                           PaYd = sum(PaYd),
                           PaTD = sum(PaTD),
                           PaInt = sum(PaInt),
                           PaCmp = sum(PaCmp),
                           RuYd = sum(RuYd),
                           RuTD = sum(RuTD),
                           RuFD = sum(RuFD),
                           FL = sum(FL),
                           ReYd = sum(ReYd),
                           ReTD = sum(ReTD),
                           ReFD = sum(ReFD),
                           Rec = sum(Rec)
                         ),
                         by = .(Pos, Player)]

salarybyteam <- rosters[,
                        .(TeamSalary = sum(Salary)),
                        by = TRUFFLE]
rosterbreakdown <- merge(rosters, salarybyteam, by = "TRUFFLE")
rosterbreakdown$SalaryPerc <- rosterbreakdown$Salary / rosterbreakdown$TeamSalary


#fpts
recordplfpts <- recordbookspl[, c("Pos","Player","FPts")][order(-FPts)][1:100, ]
recordtmfpts <- recordbookstm[, c("TRUFFLE","Pos","Player","FPts")][order(-FPts)]
#games
recordplgames <- recordbookspl[, c("Pos","Player","Games")][order(-Games)][1:100, ]
recordtmgames <- recordbookstm[, c("TRUFFLE","Pos","Player","Games")][order(-Games)]
#Avg
recordplavg <- recordbookspl[, c("Pos","Player","Avg")][order(-Avg)][1:100, ]
recordtmavg <- recordbookstm[, c("TRUFFLE","Pos","Player","Avg")][order(-Avg)]
#FD
recordplfd <- recordbookspl[, c("Pos","Player","FD")][order(-FD)][1:100, ]
recordtmfd <- recordbookstm[, c("TRUFFLE","Pos","Player","FD")][order(-FD)]
#PaYd
recordplpayd <- recordbookspl[, c("Pos","Player","PaYd")][order(-PaYd)][1:100, ]
recordtmpayd <- recordbookstm[, c("TRUFFLE","Pos","Player","PaYd")][order(-PaYd)]
#PaTD
recordplpatd <- recordbookspl[, c("Pos","Player","PaTD")][order(-PaTD)][1:100, ]
recordtmpatd <- recordbookstm[, c("TRUFFLE","Pos","Player","PaTD")][order(-PaTD)]
#PaInt
recordplpaint <- recordbookspl[, c("Pos","Player","PaInt")][order(-PaInt)][1:100, ]
recordtmpaint <- recordbookstm[, c("TRUFFLE","Pos","Player","PaInt")][order(-PaInt)]
#PaCmp
recordplpacmp <- recordbookspl[, c("Pos","Player","PaCmp")][order(-PaCmp)][1:100, ]
recordtmpacmp <- recordbookstm[, c("TRUFFLE","Pos","Player","PaCmp")][order(-PaCmp)]
#RuYd
recordplruyd <- recordbookspl[, c("Pos","Player","RuYd")][order(-RuYd)][1:100, ]
recordtmruyd <- recordbookstm[, c("TRUFFLE","Pos","Player","RuYd")][order(-RuYd)]
#TuTD
recordplrutd <- recordbookspl[, c("Pos","Player","RuTD")][order(-RuTD)][1:100, ]
recordtmrutd <- recordbookstm[, c("TRUFFLE","Pos","Player","RuTD")][order(-RuTD)]
#RuFD
recordplrufd <- recordbookspl[, c("Pos","Player","RuFD")][order(-RuFD)][1:100, ]
recordtmrufd <- recordbookstm[, c("TRUFFLE","Pos","Player","RuFD")][order(-RuFD)]
#FL
recordplfl <- recordbookspl[, c("Pos","Player","FL")][order(-FL)][1:100, ]
recordtmfl <- recordbookstm[, c("TRUFFLE","Pos","Player","FL")][order(-FL)]
#ReYd
recordplreyd <- recordbookspl[, c("Pos","Player","ReYd")][order(-ReYd)][1:100, ]
recordtmreyd <- recordbookstm[, c("TRUFFLE","Pos","Player","ReYd")][order(-ReYd)]
#ReTD
recordplretd <- recordbookspl[, c("Pos","Player","ReTD")][order(-ReTD)][1:100, ]
recordtmretd <- recordbookstm[, c("TRUFFLE","Pos","Player","ReTD")][order(-ReTD)]
#ReFD
recordplrefd <- recordbookspl[, c("Pos","Player","ReFD")][order(-ReFD)][1:100, ]
recordtmrefd <- recordbookstm[, c("TRUFFLE","Pos","Player","ReFD")][order(-ReFD)]
#Rec
recordplrec <- recordbookspl[, c("Pos","Player","Rec")][order(-Rec)][1:100, ]
recordtmrec <- recordbookstm[, c("TRUFFLE","Pos","Player","Rec")][order(-Rec)]

awards <- as.data.table(read_excel("data/awards.xlsx"))
allt1 <- awards[Award == "1stTm"][, c("Season", "Pos", "Winner", "TRUFFLE")]
allt2 <- awards[Award == "2ndTm"][, c("Season", "Pos", "Winner", "TRUFFLE")]
award2020 <- awards[Award!="1stTm" & Award!="2ndTm"][Season==2020]
award2021 <- awards[Award!="1stTm" & Award!="2ndTm"][Season==2021]

#reactable column formats ----

smallboxwidth <- 45

#functions
avg_pal <- function(x) { 
  if(is.na(x) == T) {
    rgb(1, 1, 1)
  } else if (x < 0) {
    rbcolor
  } else if (x > 1) {
    maxscale
  } else {
    rgb(colorRamp(c(minscale, maxscale))(ifelse(is.na(x), 0, x)), maxColorValue = 255)
  }
}

#function that preps any guven data frame for the inclusion of action buttons
action_mod <- function(df, team) {
  myteam <- team
  df <- merge(df, ids[, c("Player", "playerID", "TeamNum")], by = 'Player', all.x = T)
  df$Action <- ifelse(df$TRUFFLE == myteam, "www/graphics/actions/drop.png",
                          ifelse(df$TRUFFLE == "FA", "www/graphics/actions/add.png",
                                 "www/graphics/actions/trade.png"))
  df$ActionLink <- ifelse(df$TRUFFLE == myteam, paste0("https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main?selectedplayer=", df$playerID),
                              ifelse(df$TRUFFLE == "FA", paste0("https://theradicalultimatefflexperience.football.cbssports.com/players/playerpage/", df$playerID),
                                     paste0("https://theradicalultimatefflexperience.football.cbssports.com/transactions/trade/", df$playerID, "/", df$TeamNum)))
  return(df)
}

bar_chart <- function(label, width = "100%", height = "16px", fill = QBcolor, background = NULL, prefix = "") {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), paste0(prefix,label), chart)
}

with_tt <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, value)
}

#column definitions / definition functions
actionDef <- colDef(header = with_tt("Act", "Drop players on your team\nTrade for players on other teams\nAdd Free Agents"),
                    align="center", 
                    minWidth = 35, 
                    cell = function(value) {
                      img_src <- knitr::image_uri(value)
                      image <- img(src = img_src, height = "10px", alt = "drop")
                      tagList(
                        div(style = list(display = "inline-block"), image)
                      )
                    })

trfDef <- function(name = "TRF", maxW = 75, filt = TRUE, sort = TRUE) {
  colDef(name = name,
         maxWidth = maxW,
         filterable = filt,
         sortable = sort,
         align = 'center',
         cell = function(value) {
           teamnum <- teams$TeamNum[teams$Abbrev == value]
           team_url <- sprintf("https://theradicalultimatefflexperience.football.cbssports.com/teams/%s", teamnum)
           class <- paste0("trf team-", value)
           tags$a(href = team_url, target = "_blank", class = class, value)
         }
  )
}

posDef <- function(maxW = 48, filt = T, foot = "", sort = T) {
  colDef(maxWidth = maxW,
         filterable = filt,
         footer = foot,
         sortable = sort,
         align = "center",
         style = function(value) {
           if (value == "QB") {
             color <- QBcolor
           } else if (value == "RB") {
             color <- RBcolor
           } else if (value == "WR") {
             color <- WRcolor
           } else if (value == "TE") {
             color <- TEcolor
           } else if (value == "DST") {
             color <- DSTcolor
           }  else if (value == "DC") {
             color <- DCcolor
           }  else if (grepl("IR", value)) {
             color <- IRcolor
           } else {
             color <- "white"
           }
           list(background = color)
         })
}

playerDef <- function(minW = 200, filt = FALSE, sort = T) {
  colDef(minWidth = minW,
         filterable = filt,
         sortable = sort,
         cell = function(value) {
           playerid <- ids$playerID[ids$Player == value]
           player_url <- paste0("https://theradicalultimatefflexperience.football.cbssports.com/players/playerpage/", playerid, "/")
           tags$a(href = player_url, target = "_blank", value)
         })
}

posRkDef <- function(maxW = 62, filt = F) {
  colDef(header = with_tt("PosRk", "Seasonal position rank by total FPts"),
         maxWidth = maxW,
         filterable = filt,
         align = 'right',
         defaultSortOrder = "asc",
         sortNALast = T)
}

ptsLogDef <- function(maxW = 75) {
  colDef(header = with_tt("PtsLog", "Weekly log of FPts"),
         sortable = F,
         align = 'right',
         maxWidth = maxW,
         filterable=F,
         cell = function(values) {
           sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(weekly$FPts))
         })
}

salaryDefBar <- function(minW = 175, foot = F) {
  colDef(minWidth = minW,
         align = 'left',
         format = colFormat(digits=0),
         defaultSortOrder = "desc",
         style = function(value) {
           color <- ifelse(value <= 15, IRcolor, 'black')
           list(color = color)},
         cell = function(value) {
           width <- paste0(value / max(rosters$Salary) * 100, "%")
           bar_chart(ifelse(is.na(value), "", value), width = ifelse(is.na(value), 0, width), prefix = "$")
         },
         footer = function(values) if(foot == T) {paste0("$", sum(values))}
  )
}

salaryDefNobar <- function(minW = 45, foot = F) {
  colDef(minWidth = minW,
         name = "$",
         align = 'right',
         defaultSortOrder = "desc",
         format = colFormat(digits=0, prefix = "$"),
         style = function(value) {
           color <- ifelse(value <= 15, IRcolor, 'black')
           list(color = color)},
         footer = function(values) if(foot == T) {paste0("$", sum(values))}
  )
}

contractDef <- function(minW = 65, filt = T, foot = F, name = "Contract") {
  colDef(minWidth = minW,
         filt = filt,
         name = name,
         style = function(value) {
           background <- ifelse(value == 1, RBcolor,
                                ifelse(value == 2, TEcolor,
                                       ifelse(value == 3, WRcolor, QBcolor)))
           list(background = background)},
         footer = function(values) if(foot == T) {sum(values)}
  )
}

#formatting for future columns (including FA and RR tags)
futurecolDef <- function(maxW = 75, filt = T, foot = F, yr) {
  colDef(header = with_tt(yr, "FA: Free Agent\nRR: Rookie Extension Rights"),
         maxWidth = maxW,
         filterable = filt,
         align = 'right',
         defaultSortOrder = "desc",
         cell = function(value) {
           class <- paste0("tag status-", value)
           htmltools::div(class = class, value)},
         footer = function(values) if(foot == T) {paste0("$", sum(as.numeric(values), na.rm=T))}
  )
}

avgDef <- function(maxW = 65, digs = 1, filt = F, col = T, borderL = F) {
  colDef(header = with_tt("Avg", "Weekly average FPts"),
         maxWidth = maxW,
         format = colFormat(digits = digs),
         filterable = filt,
         defaultSortOrder = "desc",
         class = function(value) if(borderL == T) {"border-left-grey"},
         style = function(value) { 
           normalized <- (value) / (max(seasons$Avg,na.rm=T))
           color <- ifelse(value > 0, avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
           if(col == T) {list(background = color)}
         }
  )
}

fptsWeekDef <- function(maxW = 65, borderL = T, digs = 1, filt = F, col = T) {
  colDef(header = with_tt("FPts", "Fantasy points"),
         maxWidth = 65,
         format = colFormat(digits = digs),
         filterable = filt,
         defaultSortOrder = "desc",
         class = function(value) if(borderL == T) {"border-left-grey"},
         style = function(value) { 
           normalized <- (value) / (max(weekly$FPts[weekly$Season == max(weekly$Season)],na.rm=T))
           color <- ifelse(value > 0, avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
           if(col == T) {list(background = color)}
         }
  )
}

fptsSeasDef <- function(maxW = 65, borderL = F, digs = 1, filt = F, col = T) {
  colDef(header = with_tt("FPts", "Fantasy points"),
         maxWidth = maxW,
         format = colFormat(digits = digs),
         filterable = filt,
         defaultSortOrder = "desc",
         class = function(value) if(borderL == T) {"border-left-grey"},
         style = function(value) { 
           normalized <- (value) / (max(seasons$FPts,na.rm=T))
           color <- ifelse(value > 0, avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
           if(col == T) {list(background = color)}
         }
  )
}

seasonDef <- function(name = "Yr", maxW = 45, filt = F) {
  colDef(name=name,
         maxWidth = maxW,
         defaultSortOrder = "desc",
         align = 'center',
         filterable = filt)
}

nflDef <- colDef(minWidth = 50, align = 'right')

byeDef <- colDef(minWidth = 50, align = 'right')

gDef <- colDef(header = with_tt("G", "Games Played"), minWidth = 40, align = 'right', defaultSortOrder = "desc")

ageDef <- colDef(minWidth = 50, align = 'right', defaultSortOrder = "asc", sortNALast = T)

smallcolDef <- colDef(maxWidth = 100, align = 'left')

weekDef <- colDef("Wk",maxWidth = 44, align = 'center', defaultSortOrder = "desc")

opDef <- colDef(header = with_tt("Op", "Opponent"), maxWidth = 60, align = 'right')
oprkDef <- colDef(header = with_tt("OpRk", "Opponent Rankings vs. Fantasy Posision"), maxWidth = 60, align = 'center', style = function(value) {
  color <- ifelse(value <= 10, 'firebrick',
                  ifelse(value <= 20, 'black', 'green'))
  list(color = color)})

#box score stats reactable formats ----
#passing stats conditional formatting
pacmpDef <- colDef(header = with_tt("Cmp", "Passing Completions"), minWidth = smallboxwidth + 8, align = 'right', class = "border-left-grey", defaultSortOrder = "desc")
paattDef <- colDef(header = with_tt("Att", "Passing Attempts"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")

paydDefWk <- colDef(header = with_tt("Yd", "Passing Yards\nBold if >=300"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 300, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
paydDefSsn <- colDef(header = with_tt("Yd", "Passing Yards\nBold if >=4000"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 4000, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
paydDefNm <- colDef(header = with_tt("Yd", "Passing Yards"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")

patdDefWk <- colDef(header = with_tt("TD", "Passing TDs\nBold if >=3"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
patdDefSsn <- colDef(header = with_tt("TD", "Passing TDs\nBold if >=30"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 30, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
patdDefNm <- colDef(header = with_tt("TD", "Passing TDs"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")

paintDefWk <- colDef(header = with_tt("Int", "Interceptions\nBold if >=3"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'italic', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "asc")
paintDefSsn <- colDef(header = with_tt("Int", "Interceptions\nBold if >=15"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 15, 'italic', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "asc")
paintDefNm <- colDef(header = with_tt("Int", "Interceptions"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "asc")

#Rushing stats
ruattDefWk <- colDef(header = with_tt("Att", "Rushing Attempts\nBold if >=20"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 20, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
ruattDefSsn <- colDef(header = with_tt("Att", "Rushing Attempts\nBold if >=250"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 250, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
ruattDefNm <- colDef(header = with_tt("Att", "Rushing Attempts"), minWidth = smallboxwidth, align = 'right', class = "border-left-grey", defaultSortOrder = "desc")

#Rushing yards
ruydDefWk <- colDef(header = with_tt("Yd", "Rushing Yards\nBold if >=100"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
ruydDefSsn <- colDef(header = with_tt("Yd", "Rushing Yards\nBold if >=1000"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
ruydDefNm <- colDef(header = with_tt("Yd", "Rushing Yards"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")

#Rushing td
rutdDefWk <- colDef(header = with_tt("TD", "Rushing TDs\nBold if >=3"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
rutdDefSsn <- colDef(header = with_tt("TD", "Rushing TDs\nBold if >=10"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
rutdDefNm <- colDef(header = with_tt("TD", "Rushing TDs"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")

#Rushing fd
rufdDefWk <- colDef(header = with_tt("FD", "Rushing First Downs\nBold if >=5"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 5, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
rufdDefSsn <- colDef(header = with_tt("FD", "Rushing First Downs\nBold if >=50"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 50, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
rufdDefNm <- colDef(header = with_tt("FD", "Rushing First Downs"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")

#Targets
tarDefWk <- colDef(header = with_tt("Tar", "Targets\nBold if >=10"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
tarDefSsn <- colDef(header = with_tt("Tar", "Targets\nBold if >=100"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
tarDefNm <- colDef(header = with_tt("Tar", "Targets"), minWidth = smallboxwidth, align = 'right', class = "border-left-grey", defaultSortOrder = "desc")

#Receptions
recDefWk <- colDef(header = with_tt("Rec", "Receptions\nBold if >=10"), minWidth = smallboxwidth + 2, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
recDefSsn <- colDef(header = with_tt("Rec", "Receptions\nBold if >=100"), minWidth = smallboxwidth + 2, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
recDefNm <- colDef(header = with_tt("Rec", "Receptions"), minWidth = smallboxwidth + 2, align = 'right', defaultSortOrder = "desc")

#receiving yards
reydDefWk <- colDef(header = with_tt("Yd", "Receiving Yards\nBold if >=100"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
reydDefSsn <- colDef(header = with_tt("Yd", "Receiving Yards\nBold if >=1000"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
reydDefNm <- colDef(header = with_tt("Yd", "Receiving Yards"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")

#receiving tds
retdDefWk <- colDef(header = with_tt("TD", "Receiving TDs\nBold if >=3"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
retdDefSsn <- colDef(header = with_tt("TD", "Receiving TDs\nBold if >=10"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
retdDefNm <- colDef(header = with_tt("TD", "Receiving TDs"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")

#Receiving fd
refdDefWk <- colDef(header = with_tt("FD", "Receiving First Downs\nBold if >=5"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 5, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
refdDefSsn <- colDef(header = with_tt("FD", "Receiving First Downs\nBold if >=50"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 50, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
refdDefNm <- colDef(header = with_tt("FD", "Receiving First Downs"), minWidth = smallboxwidth, align = 'right')

flDef <- colDef(header = with_tt("Fl", "Fumbles Lost"), minWidth = smallboxwidth - 5, align = 'right', defaultSortOrder = "desc")

#advanced stats reactable formats----
perccolwidth <- 60
othcolwidth <- 45
blankptwidth <- 52
tchDef <- colDef(header = with_tt("Tch", "Touches\n(Completions + Carries + Receptions)"),
                minWidth = othcolwidth,
                align = "right",
                class = "border-left-grey",
                defaultSortOrder = "desc",
                sortNALast = T)
oppDef <- colDef(header = with_tt("Opp", "Opportunities\n(Passing Attempts + Carries + Targets)"),
                minWidth = othcolwidth + 2,
                align = "right",
                defaultSortOrder = "desc",
                sortNALast = T)
fptsPtchDef <- colDef(header = with_tt("FPt/Tch", "FPts per Touch\n(Completions + Carries + Receptions)"),
                     minWidth = 68,
                     align = "right",
                     format = colFormat(digits = 2),
                     defaultSortOrder = "desc",
                     sortNALast = T)
fptsPoppDef <- colDef(header = with_tt("FPt/Opp", "FPts per Opportunity\n(Passing Attempts + Carries + Targets)"),
                     minWidth = 72,
                     align = "right",
                     format = colFormat(digits = 2),
                     defaultSortOrder = "desc",
                     sortNALast = T)
ydptsDef <- colDef(header = with_tt("YdPt", "FPts from Yards\n(Passing + Rushing + Receiving)"),
                  minWidth = blankptwidth,
                  align = "right",
                  class = "border-left-grey",
                  format = colFormat(digits = 1),
                  defaultSortOrder = "desc",
                  sortNALast = T)
tdptsDef <- colDef(header = with_tt("TDPt", "FPts from Touchdowns\n(Passing + Rushing + Receiving)"),
                  minWidth = blankptwidth,
                  align = "right",
                  defaultSortOrder = "desc",
                  sortNALast = T)
fdptsDef <- colDef(header = with_tt("FDPt", "FPts from First Downs\n(Rushing + Receiving)"),
                  minWidth = blankptwidth,
                  align = "right",
                  defaultSortOrder = "desc",
                  sortNALast = T)
ruptsDef <- colDef(header = with_tt("RuPt", "FPts from Rushing\n(Yards + TDs + First Downs)"),
                   minWidth = blankptwidth,
                   align = "right",
                   class = "border-left-grey",
                   format = colFormat(digits = 1),
                   defaultSortOrder = "desc",
                   sortNALast = T)
reptsDef <- colDef(header = with_tt("RePt", "FPts from Receiving\n(Yards + TDs + First Downs)"),
                   minWidth = blankptwidth,
                   align = "right",
                   format = colFormat(digits = 1),
                   defaultSortOrder = "desc",
                   sortNALast = T)
ydptpercDef <- colDef(header = with_tt("YdPt%", "Percentage of Total FPts from Yards\n(Passing + Rushing + Receiving)"),
                      minWidth = perccolwidth,
                      align = "right",
                      format = colFormat(percent = T, digits = 0),
                      class = "border-left-grey",
                      defaultSortOrder = "desc",
                      sortNALast = T)
tdptpercDef <- colDef(header = with_tt("TDPt%", "Percentage of Total FPts from Touchdowns\n(Passing + Rushing + Receiving)"),
                      minWidth = perccolwidth + 2,
                      align = "right",
                      format = colFormat(percent = T, digits = 0),
                      defaultSortOrder = "desc",
                      sortNALast = T)
fdptpercDef = colDef(header = with_tt("FDPt%", "Percentage of Total FPts from First Downs\n(Rushing + Receiving)"),
                     minWidth = perccolwidth + 3,
                     align = "right",
                     format = colFormat(percent = T, digits = 0),
                     defaultSortOrder = "desc",
                     sortNALast = T)
ruptpercDef = colDef(header = with_tt("RuPt%", "Percentage of Total FPts from Rushing\n(Yards + TDs + First Downs)"),
                     minWidth = perccolwidth + 2,
                     align = "right",
                     class = "border-left-grey",
                     format = colFormat(percent = T, digits = 0),
                     defaultSortOrder = "desc",
                     sortNALast = T)
reptpercDef = colDef(header = with_tt("RePt%", "Percentage of Total FPts from Receiving\n(Yards + TDs + First Downs)"),
                     minWidth = perccolwidth + 2,
                     align = "right",
                     format = colFormat(percent = T, digits = 0),
                     defaultSortOrder = "desc",
                     sortNALast = T)

#consistency stats reactable formats----
conscolwidth <- 60
relsdDef <- colDef(header = with_tt("RelSD", "Relative Standard Deviation of Weekly FPts\n(SD / Avg)"),
                   minWidth = conscolwidth,
                   align = "right",
                   format = colFormat(digits = 2),
                   defaultSortOrder = "asc",
                   sortNALast = T)
avgposrkDef <- colDef(header = with_tt("AvgPosRk", "Average Weekly Position Rank by FPts"),
                      minWidth = conscolwidth + 10,
                      align = "right",
                      class = "border-left-grey",
                      defaultSortOrder = "asc",
                      sortNALast = T)
top5pDef <- colDef(header = with_tt("Top5%", "Percentage of Weeks with top 5 Positional Scoring Rank"),
                   minWidth = conscolwidth,
                   align = "right",
                   format = colFormat(percent = T, digits = 0),
                   defaultSortOrder = "desc",
                   sortNALast = T)
top12pDef <- colDef(header = with_tt("Top12%", "Percentage of Weeks with top 12 Positional Scoring Rank"),
                    minWidth = conscolwidth,
                    align = "right",
                    format = colFormat(percent = T, digits = 0),
                    defaultSortOrder = "desc",
                    sortNALast = T)
top24pDef <- colDef(header = with_tt("Top24%", "Percentage of Weeks with top 24 Positional Scoring Rank"),
                    minWidth = conscolwidth,
                    align = "right",
                    format = colFormat(percent = T, digits = 0),
                    defaultSortOrder = "desc",
                    sortNALast = T)
top36pDef <- colDef(header = with_tt("Top36%", "Percentage of Weeks with top 36 Positional Scoring Rank"),
                    minWidth = conscolwidth,
                    align = "right",
                    format = colFormat(percent = T, digits = 0),
                    defaultSortOrder = "desc",
                    sortNALast = T)
nonstartpDef <- colDef(header = with_tt("NonStart%", "Percentage of Weeks outside of top 36 Positional Scoring Rank"),
                       minWidth = 75,
                       align = "right",
                       format = colFormat(percent = T, digits = 0),
                       defaultSortOrder = "desc",
                       sortNALast = T)
g10pDef <- colDef(header = with_tt(">10 %", "Percentage of Weeks scoring >10 FPts"),
                  minWidth = conscolwidth,
                  class = "border-left-grey",
                  align = "right",
                  format = colFormat(percent = T,digits = 0),
                  defaultSortOrder = "desc",
                  sortNALast = T)
g20pDef <- colDef(header = with_tt(">20 %", "Percentage of Weeks scoring >20 FPts"),
                  minWidth = conscolwidth,
                  align = "right",
                  format = colFormat(percent = T, digits = 0),
                  defaultSortOrder = "desc",
                  sortNALast = T)
g30pDef <- colDef(header = with_tt(">30 %", "Percentage of Weeks scoring >30 FPts"),
                  minWidth = conscolwidth,
                  align = "right",
                  format = colFormat(percent = T, digits = 0),
                  defaultSortOrder = "desc",
                  sortNALast = T)
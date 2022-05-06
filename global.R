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

# setting colors -----
#colors and global options 

minAvg <- 3
currentyr <- 2021

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

#graphing plotly palet and font
ft <- list(
  family = "Helvetica Neue",
  size = 11)

pal <- c("#b7e1cd", "#f4cccc", "#cfe2f3", "#fce5cd", "#D9D2E9", "#D9D9D9", "#ea9999")
pal <- setNames(pal, c("QB", "RB", "WR", "TE", "DST", "DC", "IR"))

# Reading in and cleaning data from Excels/csvs -----

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
#bug fix
tpoverview$Avg[tpoverview$Pos == "DST"] <- NA
#tpoverview$G[tpoverview$Pos == "DST"] <- 17
#tpoverview$[tpoverview$Pos == "DST"] <- 17

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
                        by = .(Season, TRUFFLE, Pos, Player)][order(-Total)][Total > 100]

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
)][order(-FPts)][, c("Season","TRUFFLE","Pos","Player","FPts","Touch","Opp","FPts/Touch","FPts/Opp","YdPts","TDPts","FDPts","YdPt%","TDPt%","FDPt%","RuPts",
                     "RePts","RuPt%","RePt%")]

consistencystart <- weekly
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
avg_pal <- function(x) rgb(colorRamp(c("#E1F1E8", maxscale))(x), maxColorValue = 255)
#avg_pal <- function(x) rgb(colorRamp(c(RBcolor, TEcolor, QBcolor))(x), maxColorValue = 255) red to orange to green option

bar_chart <- function(label, width = "100%", height = "16px", fill = QBcolor, background = NULL, prefix = "") {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), paste0(prefix,label), chart)
}

#column definitions
trfDef <- colDef(name = "TRF", align = 'center', maxWidth = 75, 
                 cell = function(value) {
                   class <- paste0("trf team-", value)
                   htmltools::div(class = class, value)}
                 )
trfDeffilt <- colDef(name = "TRF", align = 'center', filterable = T, maxWidth = 75, 
                 cell = function(value) {
                   class <- paste0("trf team-", value)
                   htmltools::div(class = class, value)}
)

posDef <- colDef(align = "center", maxWidth = 47, filterable = T, style = function(value) {
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

posDefnarrow <- colDef(align = "center", maxWidth = 35, filterable = T, style = function(value) {
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

posDefnarrownofilt <- colDef(align = "center", maxWidth = 40, filterable = F, style = function(value) {
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

posDefwidenofilt <- colDef(align = "center", maxWidth = 80, filterable = F, style = function(value) {
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

posDefFooter <- colDef(align = "center", footer = "Total", maxWidth = 47, filterable = T, style = function(value) {
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

playerDef <- colDef(minWidth = 200, filterable = T)

nflDef <- colDef(minWidth = 50, align = 'left')

byeDef <- colDef(minWidth = 60, align = 'left')

gDef <- colDef(minWidth = 50, align = 'right')

ageDef <- colDef(minWidth = 75, align = 'left')

smallcolDef <- colDef(maxWidth = 100, align = 'left')

salaryDef <- colDef(minWidth = 175,align = 'left',
                    format = colFormat(digits=0),
                    style = function(value) {
                      color <- ifelse(value <= 15, IRcolor, 'black')
                      list(color = color)},
                    cell = function(value) {
                      width <- paste0(value / max(rosters$Salary) * 100, "%")
                      bar_chart(ifelse(is.na(value), "", value), width = ifelse(is.na(value), 0, width), prefix = "$")
                    }
)

draftsalaryDefnarrow <- colDef(minWidth = 100,align = 'left',
                    format = colFormat(digits=0),
                    style = function(value) {
                      color <- ifelse(value <= 15, IRcolor, 'black')
                      list(color = color)},
                    cell = function(value) {
                      width <- paste0(value / 50 * 100, "%")
                      bar_chart(ifelse(is.na(value), "", value), width = ifelse(is.na(value), 0, width), prefix = "$")
                    }
)

salaryDefFooter <- colDef(minWidth = 175,align = 'left',
                          format = colFormat(digits=0),
                          style = function(value) {
                            color <- ifelse(value <= 15, IRcolor, 'black')
                            list(color = color)},
                          cell = function(value) {
                            width <- paste0(value / max(rosters$Salary) * 100, "%")
                            bar_chart(ifelse(is.na(value), "", value), width = ifelse(is.na(value), 0, width), prefix = "$")
                          },
                          footer = function(values) paste0("$", sum(values))
)

contractDef <- colDef(minWidth = 75, style = function(value) {
  background <- ifelse(value == 1, RBcolor,
                       ifelse(value == 2, TEcolor,
                              ifelse(value == 3, WRcolor, QBcolor)))
  list(background = background)}
)

contractDefFooter <- colDef(minWidth = 75, style = function(value) {
  background <- ifelse(value == 1, RBcolor,
                       ifelse(value == 2, TEcolor,
                              ifelse(value == 3, WRcolor, QBcolor)))
  list(background = background)},
  footer = function(values) sum(values)
)

futurecolDef <- colDef(align = 'right', maxWidth = 75, cell = function(value) {
  class <- paste0("tag status-", value)
  htmltools::div(class = class, value)}
)

futurecolDefFooter <- colDef(align = 'right', maxWidth = 75, cell = function(value) {
  class <- paste0("tag status-", value)
  htmltools::div(class = class, value)},
  footer = function(values) paste0("$", sum(as.numeric(values), na.rm=T))
)

avgDef <- colDef(maxWidth = 65,
  style = function(value) {
    normalized <- (value) / (max(seasons$Avg,na.rm=T))
    color <- ifelse(value > 0, avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
    list(background = color)
  }
)

fptsDefweekly <- colDef(maxWidth = 65, class = "border-left",
  style = function(value) {
    normalized <- (value) / (max(weekly$FPts[weekly$Season == max(weekly$Season)],na.rm=T))
    color <- ifelse(value > 0, avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
    list(background = color)
  }
)

fptsDefweeklynarrownoline <- colDef(maxWidth = 50,
                              style = function(value) {
                                normalized <- (value) / (max(weekly$FPts[weekly$Season == max(weekly$Season)],na.rm=T))
                                color <- ifelse(value > 0, avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
                                list(background = color)
                              }
)

fptsDefweeklynarrow <- colDef(maxWidth = 50, class = "border-left",
                        style = function(value) {
                          normalized <- (value) / (max(weekly$FPts[weekly$Season == max(weekly$Season)],na.rm=T))
                          color <- ifelse(value > 0, avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
                          list(background = color)
                        }
)

fptsDefseasons <- colDef(maxWidth = 65,
                       style = function(value) {
                         normalized <- (value) / (max(seasons$FPts,na.rm=T))
                         color <- ifelse(value > 0, avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
                         list(background = color)
                       }
)

FPtsstyle <- list(fontWeight = 'bold', background = fptsbackground)

seasonDef <- colDef(name="Yr", maxWidth = smallboxwidth, align = 'center')
weekDef <- colDef("Wk",maxWidth = 50, align = 'center')

oppDef <- colDef(maxWidth = 55, align = 'left')
oprkDef <- colDef(maxWidth = 55, align = 'center', style = function(value) {
  color <- ifelse(value <= 10, 'firebrick',
                  ifelse(value <= 20, 'black', 'green'))
  list(color = color)})

#passing stats conditional formatting
pacmpDef <- colDef(name = "Cmp", minWidth = smallboxwidth, align = 'right', class = "border-left")
paattDef <- colDef(name = "Att", minWidth = smallboxwidth, align = 'right')

paydDefWk <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 300, 'bold', 'plain')
  list(fontWeight = fontWeight)})
paydDefSsn <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 4000, 'bold', 'plain')
  list(fontWeight = fontWeight)})
paydDefNm <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right')

patdDefWk <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)})
patdDefSsn <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 30, 'bold', 'plain')
  list(fontWeight = fontWeight)})
patdDefNm <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right')

paintDefWk <- colDef(name = "Int", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'italic', 'plain')
  list(fontWeight = fontWeight)})
paintDefSsn <- colDef(name = "Int", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 15, 'italic', 'plain')
  list(fontWeight = fontWeight)})
paintDefNm <- colDef(name = "Int", minWidth = smallboxwidth, align = 'right')

#Rushing stats
ruattDefWk <- colDef(name = "Att", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 20, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left")
ruattDefSsn <- colDef(name = "Att", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 250, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left")
ruattDefNm <- colDef(name = "Att", minWidth = smallboxwidth, align = 'right', class = "border-left")

#Rushing yards
ruydDefWk <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)})
ruydDefSsn <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
  list(fontWeight = fontWeight)})
ruydDefNm <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right')

#Rushing td
rutdDefWk <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)})
rutdDefSsn <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)})
rutdDefNm <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right')

#Rushing fd
rufdDefWk <- colDef(name = "FD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 5, 'bold', 'plain')
  list(fontWeight = fontWeight)})
rufdDefSsn <- colDef(name = "FD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 50, 'bold', 'plain')
  list(fontWeight = fontWeight)})
rufdDefNm <- colDef(name = "FD", minWidth = smallboxwidth, align = 'right')

#Targets
tarDefWk <- colDef(minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left")
tarDefSsn <- colDef(minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left")
tarDefNm <- colDef(name = "Tar", minWidth = smallboxwidth, align = 'right', class = "border-left")

#Receptions
recDefWk <- colDef(minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)})
recDefSsn <- colDef(minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)})
recDefNm <- colDef(name = "Rec", minWidth = smallboxwidth, align = 'right')

#receiving yards
reydDefWk <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)})
reydDefSsn <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
  list(fontWeight = fontWeight)})
reydDefNm <- colDef(name = "Yd", minWidth = smallboxwidth, align = 'right')

#receiving tds
retdDefWk <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)})
retdDefSsn <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)})
retdDefNm <- colDef(name = "TD", minWidth = smallboxwidth, align = 'right')

#Receiving fd
refdDefWk <- colDef(name = "FD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 5, 'bold', 'plain')
  list(fontWeight = fontWeight)})
refdDefSsn <- colDef(name = "FD", minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 50, 'bold', 'plain')
  list(fontWeight = fontWeight)})
refdDefNm <- colDef(name = "FD", minWidth = smallboxwidth, align = 'right')

flDef <- colDef(minWidth = smallboxwidth, align = 'right')

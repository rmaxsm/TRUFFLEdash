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
library(fmsb)
library(reticulate)

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
rookieextension <- "#8750AE"
tabletextcol <- "#3A3A3A"
rd1col <- "#CCDAF5"
rd2col <- "#FDF2D0"
rd3col <- "#DCE9D5"

#graphing plotly palet and font
ft <- list(
  family = "Helvetica Neue",
  size = 11)

pal <- c("#b7e1cd", "#f4cccc", "#cfe2f3", "#fce5cd", "#D9D2E9", "#D9D9D9", "#ea9999")
pal <- setNames(pal, c("QB", "RB", "WR", "TE", "DST", "DC", "IR"))

# Reading in and cleaning data from Excels/csvs -----

#file of TRUFFLE team info
#teams <- read_excel("data/oldexcel/teams.xlsx")
teams <- read_csv("data/teams.csv", col_types = cols())

#file of CBS player IDs
ids <- read_csv("data/playerIDs.csv", col_types = cols())
ids$playerID <- as.character(ids$playerID)
ids$TRUFFLE[!(ids$TRUFFLE %in% c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW"))] <- "FA"
ids <- merge(ids, teams[, c("Abbrev", "TeamNum")], by.x = "TRUFFLE", by.y = "Abbrev", all.x = T)

#file of weekly scoring for players started/active in TRUFFLE
fantasy <- as.data.table(read_csv("data/fantasy.csv", col_types = cols()))
#fantasy <- read_excel("data/fantasy2022test.xlsx")
cleanFantasy <- function(file) {
  #deselect columns to delete and modify avg
  file$Avg <- as.numeric(file$Avg)
  
  return(file)
}
fantasy <- as.data.table(cleanFantasy(fantasy))

#file of full season data for players dating back to 2015
seasons <- as.data.table(read_csv("data/seasons.csv", col_types = cols()))


#import fantasy pros file to use for age
#fprosage <- read_excel("data/fprosage.xlsx")
fprosage <- read_csv("data/fprosage.csv", col_types = cols())
cleanFprosage <- function(file) {
  file$TIERS <- NULL
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
rosters <- read_csv("data/rosters.csv", col_types = cols())
cleanRosters <- function(file) {
  
  #convert to DT
  file <- as.data.table(file)
  
  #merge in correct team abbreviations
  file <- merge(x = file, y = fprosage[ , c("Player", "AgePH")], by = "Player", all.x=TRUE)
  file <- add_column(file, Age = NA, .after = "Player")
  file$Age <- as.integer(file$AgePH)
  file$AgePH <- NULL
  
  colnames(file) <- c("Player", "Age", "Pos", "TRUFFLE", "NFL", "Opp", "GameTime", "Bye", "O/U", "PosRnk", "Ovp", "Rost", "Start", "Salary", "Contract", "Last", "Avg", "Proj")
  
  #finalized 
  return(file)
}
rosters <- cleanRosters(rosters)

#get old rosters and merge in current teams to see what TRUFFLE team players were on which year
oldrosters <- read_csv("data/oldrosters.csv", col_types = cols())
mergerosters <- rosters[, c("TRUFFLE", "Pos", "Player", "NFL", "Salary", "Contract")]
mergerosters$Season <- currentyr
mergerosters <- mergerosters[, c("Season", "TRUFFLE", "Pos", "Player", "NFL", "Salary", "Contract")]
oldrosters <- as.data.table(rbind(oldrosters, mergerosters))[order(Player,Season)]
rm(mergerosters)

#file of weekly scoring across NFL
weekly <- as.data.table(read_csv("data/weekly.csv", col_types = cols()))
cleanWeekly <- function(file) {
  #remove players that didnt play in a week
  file <- filter(file, is.na(Avg) == F)
  
  file$TRUFFLE[!(file$TRUFFLE %in% teams$Abbrev)]  <- "FA"

  return(file)
}
weekly <- as.data.table(cleanWeekly(weekly))
weekly <- weekly[order(-Season,-Week,-FPts)][, `:=`(PosRk = 1:.N), by = .(Season, Week, Pos)]

#modified weekly w current truffle for stat center
weeklysc <- weekly
weeklysc$TRUFFLE <- NULL
weeklysc <- merge(x = weeklysc, y = rosters[ , c("Pos", "Player", "TRUFFLE")], by = c("Pos", "Player"), all.x=TRUE)
weeklysc$TRUFFLE[is.na(weeklysc$TRUFFLE)] <- "FA"

#add current season
currentseason <-weekly[, !c("Opp", "OpRk")]
currentseason <- currentseason[Season == max(weekly$Season),
                               .(NFL = NFL[1],
                                 G = .N,
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
                               by = .(Season, Pos, Player)]

if (max(seasons$Season) != max(currentseason$Season)) {
  seasons <- rbind(seasons, currentseason)
}

seasons <- seasons[order(-Season,-FPts, -Avg)][, `:=`(PosRk = 1:.N), by = .(Season, Pos)]

#file to indicate what players have rookie rights
rookierights <- read_csv("data/rookierights.csv", col_types = cols())
rookierights <- as.vector(rookierights$Player)

#file of draft records
draft <- as.data.table(read_csv("data/drafts.csv", col_types = cols()))

#franchise tag files
franchised <- as.data.table(read_csv("data/franchisetag.csv", col_types = cols()))
top5paid <- as.data.table(read_csv("data/top5paid.csv", col_types = cols()))

#rivalry scorers
riv <- as.data.table(read_csv("data/rivalries.csv", col_types = cols()))

rivscores <- as.data.table(read_csv("data/rivalryscores.csv", col_types = cols()))
rivscores$Winner <- ifelse(rivscores$Team1Score > rivscores$Team2Score, rivscores$Team1, rivscores$Team2)

rivfantasy <- merge(fantasy, rivscores)
rivfantasy <- rivfantasy[TRUFFLE == Team1 | TRUFFLE == Team2]
rivscorers <- rivfantasy[,
          .(G = .N,
            FPts = sum(FPts, na.rm = T),
            Avg = round(mean(FPts, na.rm = T),2)),
          by = .(Rivalry, TRUFFLE, Player)]

turkeyscorers <- rivfantasy[Thanksgiving == 1,
                        .(G = .N,
                          FPts = sum(FPts, na.rm = T),
                          Avg = round(mean(FPts, na.rm = T),2)),
                        by = .(Rivalry, TRUFFLE, Player)]


#read in advanced combined files
extradash <- as.data.table(read_csv("data/extraDash.csv", col_types = cols()))
colnames(extradash)[7:20] <- c("Cmp%", "Pa20", "Pa40", "RuYPC", "Ru20", "Tar", "Tar%", "ReYPC", "Re20", "Re40", "ReFD%", "TotYd", "Avg", "FPts")
extradash <- extradash[Avg != "-"]
extradash$TRUFFLE[!(extradash$TRUFFLE %in% teams$Abbrev)] <- "FA"
extradash <- extradash[, c(1:3, 5, 4, 6:20)][order(-Week, -TotYd)]

#merge in other columns for calcs
extradash <- merge(x = extradash, y = weekly[ , c("Season", "Week", "Pos", "Player", "PaCmp", "PaAtt", "RuAtt", "RuYd", "Rec", "ReYd", "ReFD")], by = c("Season", "Week", "Pos", "Player"))

extradashszn <- extradash[,
                          .(TRUFFLE = TRUFFLE[1],
                            G = .N,
                            `Cmp%` = ifelse(sum(PaAtt, na.rm = T) > 0, round(sum(PaCmp, na.rm = T) / sum(PaAtt, na.rm = T), 3), 0),
                            Pa20 = sum(Pa20, na.rm = T),
                            Pa40 = sum(Pa40, na.rm = T),
                            RuYPC = ifelse(sum(RuAtt, na.rm = T) > 0, round(sum(RuYd, na.rm = T) / sum(RuAtt, na.rm = T), 1), 0),
                            Ru20 = sum(Ru20, na.rm = T),
                            Tar = sum(Tar, na.rm = T),
                            `Tar%` = round(mean(`Tar%`, na.rm = T),1),
                            ReYPC = ifelse(sum(Rec, na.rm = T) > 0, round(sum(ReYd, na.rm = T) / sum(Rec, na.rm = T), 1), 0),
                            Re20 = sum(Re20, na.rm = T),
                            Re40 = sum(Re40, na.rm = T),
                            `ReFD%` = ifelse(sum(Rec, na.rm = T) > 0, round(sum(ReFD, na.rm = T) / sum(Rec, na.rm = T),3), 0),
                            TotYd = sum(TotYd, na.rm = T)
                            ),
                          by = .(Season, Pos, Player)
  
]
extradashszn <- extradashszn[, c(1, 4, 2:3, 5:17)][order(-TotYd)]

#espn data
espn <- as.data.table(read_csv("data/espnStats.csv", col_types = cols()))
espn$FPDiff <- espn$ActualPts - espn$xFP
espn <- merge(x = espn, y = rosters[ , c("Pos", "Player", "TRUFFLE")], by = c("Pos", "Player"), all.x=TRUE)
espn$TRUFFLE[is.na(espn$TRUFFLE)] <- "FA"
colnames(espn)[9] <- "TDDiff"
espn <- espn[, c(13, 1:5, 12, 6:7, 9, 8, 10:11)]

#snaps data
snaps <- as.data.table(read_csv("data/snapPer.csv", col_types = cols()))
snaps <- merge(x = snaps, y = rosters[ , c("Pos", "Player", "TRUFFLE")], by = c("Pos", "Player"), all.x=TRUE)
snaps$TRUFFLE[is.na(snaps$TRUFFLE)] <- "FA"
snaps <- snaps[, c(24, 1:21, 23, 22)]
colnames(snaps)[23:24] <- c("Avg", "Total Snaps")
#dividing snaps by 100 for percentage formatting
for (i in 5:22) {
  if(is.numeric(snaps[[i]])) {
    snaps[[i]] <- snaps[[i]]/100
  }
}
snaps$Avg <- snaps$Avg/100

# modifying tables for display -----

# helpful vectors
positionorder <- c("QB","RB","WR","TE", "DST", "IR", "DC")
morethan10 <- currentseason$Player[currentseason$FPts > 10]

#team portal tables
teamportal<- rosters[, .(TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract)]
teamportal <- merge(teamportal, currentseason[, !c( "Season","Pos", "NFL")], by = 'Player', all.x = T)
teamportal$Avg[teamportal$Pos == "DST"] <- rosters$Avg[rosters$Pos == "DST"]
tpoverview <- teamportal[, .(TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract, G, Avg, FPts)][order(match(Pos, positionorder), -Avg)]

ptslogs <- weekly[order(-Season, Week)][is.element(Player, rosters$Player),
                  .(ptslog = rev(list(FPts))),
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
contracts <- merge(x = contracts, y = draft[, .(Pos, Player, Extension)], by = c("Pos", "Player"), all.x = T)[order(Player)]
contracts <- contracts[, `:=`(`'22` = Salary,
                              `'23` = ifelse(Contract > 1, Salary,
                                             ifelse(Contract == 1 & is.element(Player, rookierights) == T, Extension,
                                                    ifelse(Contract == 1 & is.element(Player, rookierights) == F, "FA", "-"))),
                              `'24` = ifelse(Contract > 2, Salary,
                                             ifelse(Contract == 1 & is.element(Player, rookierights) == T, Extension,
                                                    ifelse(Contract == 2 & is.element(Player, rookierights) == T, Extension,
                                                           ifelse(Contract == 2 & is.element(Player, rookierights) == F, "FA", "-")))),
                              `'25` = ifelse(Contract > 3, Salary,
                                             ifelse(Contract == 1 & is.element(Player, rookierights) == T, "FA",
                                             ifelse(Contract == 2 & is.element(Player, rookierights) == T, Extension,
                                                    ifelse(Contract == 3 & is.element(Player, rookierights) == T, Extension,
                                                           ifelse(Contract == 3 & is.element(Player, rookierights) == F, "FA", "-"))))),
                              `'26` = ifelse(Contract > 4, Salary,
                                             ifelse(Contract == 2 & is.element(Player, rookierights) == T, "FA",
                                                    ifelse(Contract == 3 & is.element(Player, rookierights) == T, Extension,
                                                           ifelse(Contract == 4 & is.element(Player, rookierights) == T, Extension,
                                                                  ifelse(Contract == 4 & is.element(Player, rookierights) == F, "FA", "-")))))
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

pointsleaders <- weekly[order(-Season, Week)][,
                        .(ptslogs = list(FPts),
                          Avg = round(mean(FPts),1),
                          Total = round(sum(FPts),1)),
                        by = .(Season, Pos, Player)][order(-Season, match(Pos, positionorder), -Total, -Avg)][, `:=`(PosRk = 1:.N), by = .(Season, Pos)]
pointsleaders <- merge(x = pointsleaders, y = rosters[ , c("Pos", "Player", "TRUFFLE")], by = c("Pos", "Player"), all.x=TRUE)
pointsleaders$TRUFFLE[is.na(pointsleaders$TRUFFLE)] <- "FA"
pointsleaders <- pointsleaders[, c("Season", "TRUFFLE", "Player", "Pos", "PosRk", "ptslogs", "Avg", "Total")][order(-Total, -Avg)]

ppbios <- weekly[Season == max(weekly$Season)][order(-Season,-Week)]
ppbios <- ppbios[,
                 .(TRUFFLE = TRUFFLE[1],
                   NFL = NFL[1],
                   ptslogs = list(FPts),
                   Avg = round(mean(FPts),1),
                   Total = round(sum(FPts))),
                 by = .(Pos, Player)]
ppbios <- merge(ppbios, rosters[, c("Player","Salary", "Contract")], by = 'Player', all.x = T)
ppbios <- merge(ppbios, fprosage[, c("Player", "AgePH", "DynRk", "DynPosRk")])
ppbios <- ppbios[, c("TRUFFLE","Pos","Player", "NFL", "AgePH", "DynRk", "DynPosRk","Salary", "Contract", "ptslogs")]

advanced <- weeklysc[, .(FPts = sum(FPts),
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

consistencystart <- as.data.frame(weeklysc)
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
   by = .(Season, TRUFFLE, Pos, Player)][order(-Avg)][, c("Season","TRUFFLE","Pos","Player","Avg","RelSD",">10 %",">20 %",">30 %","AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %")]

weeklytop5 <- weekly[, c("Season", "Week", "TRUFFLE", "Pos", "Player", "FPts")][order(Week,-FPts)][, .(TRUFFLE = TRUFFLE[1:30],
                                                                                                       Player = Player[1:30],
                                                                                                       FPts = FPts[1:30]), 
                                                                                                   by = .(Season,Week, Pos)][, .(Season,Week,TRUFFLE,Pos,Player,FPts)]

weeklytop5qb <- na.omit(weeklytop5[Pos == "QB"])
weeklytop5rb <- na.omit(weeklytop5[Pos == "RB"])
weeklytop5wr <- na.omit(weeklytop5[Pos == "WR"])
weeklytop5te <- na.omit(weeklytop5[Pos == "TE"])

#rookie extension values
extval <- as.data.table(rbind(c("QB", "1.1-1.6", 70), c("QB", "1.7-1.12", 60), c("QB", "2", 50), c("QB", "3", 30),
                              c("RB", "1.1-1.6", 70), c("RB", "1.7-1.12", 60), c("RB", "2", 50), c("RB", "3", 30),
                              c("WR", "1.1-1.6", 60), c("WR", "1.7-1.12", 50), c("WR", "2", 40), c("WR", "3", 30),
                              c("TE", "1.1-1.6", 30), c("TE", "1.7-1.12", 20), c("TE", "2", 10), c("TE", "3", 5)
))
colnames(extval) <- c("Pos","Pick", "Value")
extval$Value <- as.numeric(extval$Value)

tagvals <- as.data.table(
  rbind(
    c("QB", "First", round(mean(top5paid$Salary[top5paid$Pos == "QB" & top5paid$Season == currentyr]))),
    c("QB", "Second", max(top5paid$Salary[top5paid$Pos == "QB" & top5paid$Season == currentyr]) + 1 ),
    c("RB", "First", round(mean(top5paid$Salary[top5paid$Pos == "RB" & top5paid$Season == currentyr]))),
    c("RB", "Second", max(top5paid$Salary[top5paid$Pos == "RB" & top5paid$Season == currentyr]) + 1 ),
    c("WR", "First", round(mean(top5paid$Salary[top5paid$Pos == "WR" & top5paid$Season == currentyr]))),
    c("WR", "Second", max(top5paid$Salary[top5paid$Pos == "WR" & top5paid$Season == currentyr]) + 1 ),
    c("TE", "First", round(mean(top5paid$Salary[top5paid$Pos == "TE" & top5paid$Season == currentyr]))),
    c("TE", "Second", max(top5paid$Salary[top5paid$Pos == "TE" & top5paid$Season == currentyr]) + 1 )
  )
)
colnames(tagvals) <- c("Pos", "Type", "TagVal")
tagvals$TagVal <- as.numeric(tagvals$TagVal)

#table of players with franchise tag values
ft <- rosters[, c("Pos", "Player", "Salary", "Contract")][order(-Salary)][Pos %in% c("QB", "RB", "WR", "TE")]
ft$TagVal <- NA

for (i in 1:nrow(ft)) {
  pos <- ft$Pos[i]
  pl <- ft$Player[i]
  csal <- ft$Salary[i]
  ccon <- ft$Contract[i]
  
  #set players w >1 yr on contract to ineligible
  if (ccon != 1) {
    ft$TagVal[i] <- "Ineligible (>1 yr)"
    #set players w expiring rookie contracts eligible for extensions to ineligible
  } else if (ccon == 1 & pl %in% rookierights) {
    ft$TagVal[i] <- "Ineligible (Rookie Ext)"
    #set players franchise tagged 2 previous seasons to ineligible
  } else if (pl %in% franchised$Player[franchised$Season == currentyr] & pl %in% franchised$Player[franchised$Season == currentyr - 1]) {
    ft$TagVal[i] <- "Ineligible (tagged 2x)"
    #set players franchise tagged previously to second tag value based on position
  } else if (pl %in% franchised$Player[franchised$Season == currentyr]) {
    ft$TagVal[i] <- tagvals$TagVal[tagvals$Pos == pos & tagvals$Type == "Second"]
    #set players who's current salary is greater than first tag value, to current salary plus 1
  } else if (csal >= tagvals$TagVal[tagvals$Pos == pos & tagvals$Type == "First"]) {
    ft$TagVal[i] <- csal + 1
    #set all other players on 1 year contracts to first tag
  } else {
    ft$TagVal[i] <- tagvals$TagVal[tagvals$Pos == pos & tagvals$Type == "First"]
  }
}

#fantasy portal table
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

pptrufflecareer <- fantasy[Pos != "DST",
                           .(G = .N,
                             PaYd = sum(PaYd, na.rm=T),
                             PaTD = sum(PaTD, na.rm=T),
                             PaInt = sum(PaInt, na.rm=T),
                             RuYd = sum(RuYd, na.rm=T),
                             RuTD = sum(RuTD, na.rm=T),
                             RuFD = sum(RuFD, na.rm=T),
                             ReYd = sum(ReYd, na.rm=T),
                             ReTD = sum(ReTD, na.rm=T),
                             ReFD = sum(ReFD, na.rm=T),
                             Avg = round(sum(FPts, na.rm=T)/.N, 1),
                             FPts = sum(FPts, na.rm=T)),
                           by = .(Pos, Player)]

pptrufflecareerteam <- fantasy[Pos != "DST",
                                    .(Seasons = list(unique(substr(Season, 3, 4))),
                                      G = .N,
                                      PaYd = sum(PaYd, na.rm=T),
                                      PaTD = sum(PaTD, na.rm=T),
                                      PaInt = sum(PaInt, na.rm=T),
                                      RuYd = sum(RuYd, na.rm=T),
                                      RuTD = sum(RuTD, na.rm=T),
                                      RuFD = sum(RuFD, na.rm=T),
                                      ReYd = sum(ReYd, na.rm=T),
                                      ReTD = sum(ReTD, na.rm=T),
                                      ReFD = sum(ReFD, na.rm=T),
                                      Avg = round(sum(FPts, na.rm=T)/.N, 1),
                                      FPts = sum(FPts, na.rm=T)),
                                    by = .(Pos, Player, TRUFFLE)][order(-FPts)]

#radar plot set up ----
radchart_fill <- c(
  rgb(255/255, 38/255, 0, .1),
  rgb(68/255, 114/255, 196/255, .1),
  rgb(0, 176/255, 80/255, .1),
  rgb(255/255, 192/255, 0, .1),
  rgb(112/255, 48/255, 160/255, .1),
  rgb(237/255, 125/255, 49/255, .1),
  rgb(0, 0, 0, .1),
  rgb(255/255, 138/255, 216/255, .1),
  rgb(146/255, 208/255, 80/255, .1),
  rgb(91/255, 155/255, 213/255, .1)
)
radchart_line <- c(
  rgb(255/255, 38/255, 0, .9),
  rgb(68/255, 114/255, 196/255, .9),
  rgb(0, 176/255, 80/255, .9),
  rgb(255/255, 192/255, 0, .9),
  rgb(112/255, 48/255, 160/255, .9),
  rgb(237/255, 125/255, 49/255, .9),
  rgb(0, 0, 0, .9),
  rgb(255/255, 138/255, 216/255, .9),
  rgb(146/255, 208/255, 80/255, .9),
  rgb(91/255, 155/255, 213/255, .9)
)

#aggregate weekly by season, position, player
radarplot <- weekly[,
                    .(FPts = mean(FPts, na.rm = T),
                      Touches = mean(PaCmp, na.rm = T) + mean(RuAtt, na.rm = T) + mean(Rec, na.rm = T),
                      Yd = mean(PaYd, na.rm = T) + mean(RuYd, na.rm = T) + mean(ReYd, na.rm = T),
                      TD = mean(PaTD, na.rm = T) + mean(RuTD, na.rm = T) + mean(ReTD, na.rm = T),
                      FD = mean(RuFD, na.rm = T) + mean(ReFD, na.rm = T)
                    ),
                    by = .(Season, Pos, Player)][order(Player)]
#get the max positional values based on radar plot
radarplotmax <- radarplot[,
                          .(FPts = max(FPts, na.rm = T),
                            Touches = max(Touches, na.rm = T),
                            Yd = max(Yd, na.rm = T),
                            TD = max(TD, na.rm = T),
                            FD = max(FD, na.rm = T)
                          ),
                          by = .(Season, Pos)]
radarplotmax$Player <- "MAX"
radarplotmax <- radarplotmax[, c("Season", "Pos", "Player", "FPts", "Touches", "Yd", "TD", "FD")]
#get min positional values based on radar plot
radarplotmin <- radarplot[,
                          .(FPts = min(FPts, na.rm = T),
                            Touches = min(Touches, na.rm = T),
                            Yd = min(Yd, na.rm = T),
                            TD = min(TD, na.rm = T),
                            FD = min(FD, na.rm = T)
                          ),
                          by = .(Season, Pos)]
radarplotmin$Player <- "MIN"
radarplotmin <- radarplotmin[, c("Season", "Pos", "Player", "FPts", "Touches", "Yd", "TD", "FD")]
#combine all to get correct format
fullradar <- rbind(radarplotmax, radarplotmin, radarplot)
#delete unnecessary tables for efficiency
rm(radarplot, radarplotmax, radarplotmin)

#dummy radarchart when players are incorrectly selected
emptyradar <- as.data.frame(
  rbind(
    c(50, 50, 50, 50, 50),
    c(0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0)
  )
)
colnames(emptyradar) <- c("FPts", "Tch", "Yd", "TD", "FD")
rownames(emptyradar) <- c("MAX", "MIN","Empty")

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

#column formatting ----
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
                              ifelse(df$TRUFFLE == "FA", paste0("https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main?default_add=", df$Pos, ":", df$playerID),
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

trfDef <- function(name = "TRF", maxW = 75, filt = TRUE, sort = TRUE, minW = 75) {
  colDef(name = name,
         aggregate = "unique",
         minWidth = minW,
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
         align = "left",
         
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

tagvalDefBar <- function(minW = 175, foot = F) {
  colDef(header = "Tag Value",
         minWidth = minW,
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

salaryDefNobar <- function(minW = 45, foot = F, title = "$") {
  colDef(minWidth = minW,
         name = title,
         align = 'right',
         defaultSortOrder = "desc",
         format = colFormat(digits=0, prefix = "$"),
         style = function(value) {
           color <- ifelse(value <= 15, IRcolor, 'black')
           list(color = color)},
         footer = function(values) if(foot == T) {paste0("$", sum(values))}
  )
}

tagvalDefNobar <- function(minW = 45, foot = T, title = "$") {
  colDef(minWidth = minW,
         name = title,
         align = 'right',
         defaultSortOrder = "desc",
         format = colFormat(digits=0, prefix = "$"),
         style = function(value) {
           color <- ifelse(value <= 15, IRcolor, 'black')
           list(color = color)},
         footer = function(values) if(foot == T) {paste0("$", round(mean(values)) )}
  )
}

contractDef <- function(minW = 45, filt = T, foot = F, name = "Contract") {
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
  colDef(header = with_tt(yr, "FA: Free Agent\nPurple: Rookie Extension Value"),
         maxWidth = maxW,
         filterable = filt,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value, index) {
           ext <- contracts$Extension[index]
           col <- ifelse(value == "FA", textred, ifelse(value == ext, rookieextension, tabletextcol))
           list(color = col)},
         #cell = function(value) {
           #class <- paste0("tag status-", value)
           #htmltools::div(class = class, value)},
         footer = function(values) if(foot == T) {paste0("$", sum(as.numeric(values), na.rm=T))}
  )
}

avgDef <- function(maxW = 65, digs = 1, filt = F, col = T, borderL = F, foot = F) {
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
         },
         footer = function(values) if(foot == T) {round(mean(as.numeric(values), na.rm=T),2)}
  )
}

fptsDef <- function(maxW = 65, borderL = T, digs = 1, filt = F, foot = F) {
  colDef(header = with_tt("FPts", "Fantasy points"),
         maxWidth = maxW,
         format = colFormat(digits = digs),
         filterable = filt,
         defaultSortOrder = "desc",
         class = function(value) if(borderL == T) {"border-left-grey"},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

fptsWeekDef <- function(maxW = 65, borderL = T, digs = 1, filt = F, col = T) {
  colDef(header = with_tt("FPts", "Fantasy points"),
         maxWidth = maxW,
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

gDef <- function(minW = 40, foot = F) {
  colDef(header = with_tt("G", "Games Played"),
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )}

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
paydDef <- function(minW = 45, foot = F, borderL = F) {
  colDef(header = with_tt("Yd", "Passing Yards"),
         minWidth = minW,
         class = function(value) if(borderL == T) {"border-left-grey"},
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
         )
}

patdDefWk <- colDef(header = with_tt("TD", "Passing TDs\nBold if >=3"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
patdDefSsn <- colDef(header = with_tt("TD", "Passing TDs\nBold if >=30"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 30, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
patdDefNm <- colDef(header = with_tt("TD", "Passing TDs"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
patdDef <- function(minW = 45, foot = F) {
  colDef(header = with_tt("TD", "Passing TDs"),
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

paintDefWk <- colDef(header = with_tt("Int", "Interceptions\nBold if >=3"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'italic', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "asc")
paintDefSsn <- colDef(header = with_tt("Int", "Interceptions\nBold if >=15"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 15, 'italic', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "asc")
paintDefNm <- colDef(header = with_tt("Int", "Interceptions"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "asc")
paintDef <- function(minW = 45, foot = F) {
  colDef(header = with_tt("Int", "Interceptions"),
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Rushing stats
ruattDefWk <- colDef(header = with_tt("Att", "Rushing Attempts\nBold if >=20"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 20, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
ruattDefSsn <- colDef(header = with_tt("Att", "Rushing Attempts\nBold if >=250"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 250, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
ruattDefNm <- colDef(header = with_tt("Att", "Rushing Attempts"), minWidth = smallboxwidth, align = 'right', class = "border-left-grey", defaultSortOrder = "desc")
ruattDef <- function(minW = 45, foot = F, borderL = T) {
  colDef(header = with_tt("Att", "Rushing Attempts"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Rushing yards
ruydDefWk <- colDef(header = with_tt("Yd", "Rushing Yards\nBold if >=100"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
ruydDefSsn <- colDef(header = with_tt("Yd", "Rushing Yards\nBold if >=1000"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
ruydDefNm <- colDef(header = with_tt("Yd", "Rushing Yards"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
ruydDef <- function(minW = 45, foot = F, borderL = F) {
  colDef(header = with_tt("Yd", "Rushing Yards"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Rushing td
rutdDefWk <- colDef(header = with_tt("TD", "Rushing TDs\nBold if >=3"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
rutdDefSsn <- colDef(header = with_tt("TD", "Rushing TDs\nBold if >=10"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
rutdDefNm <- colDef(header = with_tt("TD", "Rushing TDs"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
rutdDef <- function(minW = 45, foot = F, borderL = F) {
  colDef(header = with_tt("TD", "Rushing TDs"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Rushing fd
rufdDefWk <- colDef(header = with_tt("FD", "Rushing First Downs\nBold if >=5"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 5, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
rufdDefSsn <- colDef(header = with_tt("FD", "Rushing First Downs\nBold if >=50"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 50, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
rufdDefNm <- colDef(header = with_tt("FD", "Rushing First Downs"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
rufdDef <- function(minW = 45, foot = F, borderL = F) {
  colDef(header = with_tt("FD", "Rushing TDs"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Targets
tarDefWk <- colDef(header = with_tt("Tar", "Targets\nBold if >=10"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
tarDefSsn <- colDef(header = with_tt("Tar", "Targets\nBold if >=100"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
tarDefNm <- colDef(header = with_tt("Tar", "Targets"), minWidth = smallboxwidth, align = 'right', class = "border-left-grey", defaultSortOrder = "desc")
tarDef <- function(minW = 45, foot = F, borderL = F) {
  colDef(header = with_tt("Tar", "Targets"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Receptions
recDefWk <- colDef(header = with_tt("Rec", "Receptions\nBold if >=10"), minWidth = smallboxwidth + 2, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
recDefSsn <- colDef(header = with_tt("Rec", "Receptions\nBold if >=100"), minWidth = smallboxwidth + 2, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
recDefNm <- colDef(header = with_tt("Rec", "Receptions"), minWidth = smallboxwidth + 2, align = 'right', defaultSortOrder = "desc")
recDef <- function(minW = 47, foot = F, borderL = F) {
  colDef(header = with_tt("Rec", "Receptions"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#receiving yards
reydDefWk <- colDef(header = with_tt("Yd", "Receiving Yards\nBold if >=100"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
reydDefSsn <- colDef(header = with_tt("Yd", "Receiving Yards\nBold if >=1000"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
reydDefNm <- colDef(header = with_tt("Yd", "Receiving Yards"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
reydDef <- function(minW = 47, foot = F, borderL = F) {
  colDef(header = with_tt("Yd", "Receiving Yards"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#receiving tds
retdDefWk <- colDef(header = with_tt("TD", "Receiving TDs\nBold if >=3"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
retdDefSsn <- colDef(header = with_tt("TD", "Receiving TDs\nBold if >=10"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
retdDefNm <- colDef(header = with_tt("TD", "Receiving TDs"), minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
retdDef <- function(minW = 47, foot = F, borderL = F) {
  colDef(header = with_tt("TD", "Receiving TDs"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Receiving fd
refdDefWk <- colDef(header = with_tt("FD", "Receiving First Downs\nBold if >=5"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 5, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
refdDefSsn <- colDef(header = with_tt("FD", "Receiving First Downs\nBold if >=50"), minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 50, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
refdDefNm <- colDef(header = with_tt("FD", "Receiving First Downs"), minWidth = smallboxwidth, align = 'right')
refdDef <- function(minW = 47, foot = F, borderL = F) {
  colDef(header = with_tt("FD", "Receiving First Downs"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

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

# Source UI files ----
source("dashboardPage.R")
source("loginPage.R")

#password stuff
num_fails_to_lockout <- 1000
# credentials <- data.frame(user = c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW"),
#                           stringsAsFactors = FALSE)
# saveRDS(credentials, "credentials/credentials.rds")

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
library(lubridate)

# setting colors -----
#colors and global options 

minAvg <- 3
currentyr <- 2024
isOffseason <- TRUE

options(reactable.language = reactableLang(
  pagePrevious = "\u276e",
  pageNext = "\u276f"
))

tdred <- "#8C2E26"
tdblue <- "#84A4D8"
globalcol <- "#84A4D8"
QBcolor <- "#b7e1cd"
RBcolor <- "#f4cccc"
WRcolor <- "#cfe2f3"
TEcolor <- "#fce5cd"
DSTcolor <- "#D9D2E9"
DCcolor <- "#D9D9D9"
IRcolor <- "#ea9999"
SFcolor <- QBcolor
FLcolor <- DSTcolor
minscale <- "#E8F4EE"
maxscale <-"#529667"
greenscale <- "#57E19F"
redscale <- "#F48E90"
fptsbackground <- "#F2F2F2"
fptscolor <- "#1E65D2"
textgreen <- "#00B050"
textred <- "#C00000"
rookieextension <- "#8750AE"
franchisetag <- "#00B0F0"
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

ggpal <- c(QB = "#8AE2A6", RB = "#FC9592", WR = "#96D3F5", TE = "#FFD087")

# Reading in and cleaning data from Excels/csvs -----

#file of TRUFFLE team info
# teams.csv ----
#teams <- as.data.table(read_csv("data/teams.csv", col_types = cols()))
#demodata
teams <- as.data.table(read_csv("demodata/teams.csv", col_types = cols()))

# ids.csv ----
#file of CBS player IDs
#ids <- as.data.table(read_csv("data/playerIDs.csv", col_types = cols()))
#demodata
ids <- as.data.table(read_csv("demodata/playerIDs.csv", col_types = cols()))

ids$playerID <- as.character(ids$playerID)
ids$TRUFFLE[!(ids$TRUFFLE %in% c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW"))] <- "FA"
ids <- merge(ids, teams[, c("Abbrev", "TeamNum")], by.x = "TRUFFLE", by.y = "Abbrev", all.x = T)

#import fantasy pros file to use for age
#fprosage <- read_excel("data/fprosage.xlsx")
#demodata
fprosage <- read_csv("demodata/fprosage.csv", col_types = cols())
cleanFprosage <- function(file) {
  file$TIERS <- NULL
  file$`ECR VS. ADP` <- NULL
  colnames(file) <- c("DynRk", "Player", "NFL", "DynPosRk", "BestRk", "WorstRk", "AvgDynRk", "SD")
  file$Player <- str_replace_all(file$Player,"\\.","")
  file$Player <- str_replace_all(file$Player," Jr","")
  file$Player <- str_replace_all(file$Player," Sr","")
  file$Player <- str_replace_all(file$Player," III","")
  file$Player <- str_replace_all(file$Player," II","")
  file$Player <- str_replace_all(file$Player,"Will Fuller V","Will Fuller")
  file$Player <- str_replace_all(file$Player,"La'Mical","Lamical")
  
  #extract position
  file$Pos <- substr(file$DynPosRk, 1, 2)
  
  #read in birthdays
  bdays <- read_csv("demodata/birthdays.csv", col_types = cols())
  bdays$player <- str_replace_all(bdays$player,"\\.","")
  bdays$player <- str_replace_all(bdays$player," Jr","")
  bdays$player <- str_replace_all(bdays$player," Sr","")
  bdays$player <- str_replace_all(bdays$player," III","")
  bdays$player <- str_replace_all(bdays$player," II","")
  bdays$player <- str_replace_all(bdays$player,"Will Fuller V","Will Fuller")
  bdays$player <- str_replace_all(bdays$player,"La'Mical","Lamical")
  
  bdays$birthday <- as.Date(bdays$birthday, "%m/%d/%y")
  bdays$Age <- as.integer(floor(difftime(as.Date(Sys.Date(), format='%d/%m/%y'), bdays$birthday, unit="days") / 365.25))
  colnames(bdays) <- c("Player", "Pos", "Team", "Birthday", "Age")
  
  file <- merge(x = file, y = bdays[ , c("Player", "Pos", "Age")], by = c("Player", "Pos"), all.x=TRUE)
  
  return(file)
}
fprosage <- as.data.table(cleanFprosage(fprosage))

# rosters.csv ----
#file of current TRUFFLE rosters
#rosters <- read_csv("data/rosters.csv", col_types = cols())
#demodata
rosters <- read_csv("demodata/rosters.csv", col_types = cols())
cleanRosters <- function(file) {
  
  #convert to DT
  file <- as.data.table(file)
  
  #merge in correct team abbreviations
  file <- merge(x = file, y = fprosage[ , c("Player", "Pos", "Age")], by = c("Player", "Pos"), all.x=TRUE)
  #colnames(file) <- c("Player", "Age",  "Pos", "TRUFFLE", "NFL", "Opp", "GameTime", "Bye", "O/U", "PosRnk", "Ovp", "Rost", "Start", "Salary", "Contract", "Last", "Avg", "Proj")
  #demodata
  colnames(file) <- c("Player", "Pos", "League", "TRUFFLE", "NFL", "Opp", "GameTime", "Bye", "O/U", "PosRnk", "Ovp", "Rost", "Start", "Salary", "Contract", "Last", "Avg", "Proj", "Age")
  file <- file[, c("League", "TRUFFLE", "Player",  "Pos", "NFL", "Age", "Opp", "GameTime", "Bye", "O/U", "PosRnk", "Ovp", "Rost", "Start", "Salary", "Contract", "Last", "Avg", "Proj")]
  
  #finalized 
  return(file)
}
rosters <- cleanRosters(rosters)

# oldrosters.csv ----
#get old rosters and merge in current teams to see what TRUFFLE team players were on which year
#oldrosters <- as.data.table(read_csv("data/oldrosters.csv", col_types = cols()))
#demodata
oldrosters <- as.data.table(read_csv("demodata/oldrosters.csv", col_types = cols()))
#calculate rings and bench cup wins to display in record books
rings <- oldrosters[,
                    .(Rings = sum(Ring),
                      RingYears = lapply(list(paste0(" ",substr(Season[Ring == 1],3,4))), sort, decreasing = F),
                      RingTeams = list(unique(paste0(" ",TRUFFLE[Ring == 1]))),
                      BenchCups = sum(BenchCup),
                      BCYears = lapply(list(paste0(" ",substr(Season[BenchCup == 1],3,4))), sort, decreasing = F),
                      BCTeams = list(unique(paste0(" ",TRUFFLE[BenchCup == 1])))
                    ),
                    by = .(League, Pos, Player)]
rings$RingYears[rings$RingYears == " '"] <- NA; rings$BCYears[rings$BCYears == " '"] <- NA
ringsbyteam <- oldrosters[,
                          .(Rings = sum(Ring),
                            RingYears = lapply(list(paste0(" ",substr(Season[Ring == 1],3,4))), sort, decreasing = F),
                            BenchCups = sum(BenchCup),
                            BCYears = lapply(list(paste0(" ",substr(Season[BenchCup == 1],3,4))), sort, decreasing = F)
                          ),
                          by = .(League, TRUFFLE, Pos, Player)]
ringsbyteam$RingYears[ringsbyteam$RingYears == " "] <- NA; ringsbyteam$BCYears[ringsbyteam$BCYears == " "] <- NA
#delete rings and bench cup columns after use
oldrosters$Ring <- NULL; oldrosters$BenchCup <- NULL

#add current year rosters
mergerosters <- rosters[, .(League, TRUFFLE, Pos, Player, NFL, Salary, Contract)]
mergerosters$Season <- currentyr
mergerosters <- mergerosters[, .(League, Season, TRUFFLE, Pos, Player, NFL, Salary, Contract)]
oldrosters <- as.data.table(rbind(oldrosters, mergerosters))[order(Player,Season)]
rm(mergerosters)

# fantasy.csv ----
#file of weekly scoring for players started/active in TRUFFLE
#fantasy <- as.data.table(read_csv("data/fantasy.csv", col_types = cols()))
#demodata
fantasy <- as.data.table(read_csv("demodata/fantasy.csv", col_types = cols()))
#fantasy <- read_excel("data/fantasy2022test.xlsx")
cleanFantasy <- function(file) {
  #create scoring setting column and initial PPFD file version
  file$Scoring <- "PPFD"
  PPFD <- file
  
  #PPR
  PPR <- file
  PPR$FPts <- PPR$FPts - PPR$RuFD - PPR$ReFD + PPR$Rec
  PPR$Avg <- PPR$FPts
  PPR$Scoring <- "PPR"
  
  #hPPR
  hPPR <- file
  hPPR$FPts <- hPPR$FPts - hPPR$RuFD - hPPR$ReFD + 0.5*hPPR$Rec
  hPPR$Avg <- hPPR$FPts
  hPPR$Scoring <- "hPPR"
  
  #STD
  STD <- file
  STD$FPts <- STD$FPts - STD$RuFD - STD$ReFD
  STD$Avg <- STD$FPts
  STD$Scoring <- "STD"
  
  #combine them and return it
  file <- rbind(PPFD, PPR, hPPR, STD)
  return(file)
}
fantasy <- as.data.table(cleanFantasy(fantasy))

# seasons.csv ----
#file of full season data for players dating back to 2015
#seasons <- as.data.table(read_csv("data/seasons.csv", col_types = cols()))
#demodata
seasons <- as.data.table(read_csv("demodata/seasons.csv", col_types = cols()))
cleanSeasons <- function(file) {
  #create scoring setting column and initial PPFD file version
  file$Scoring <- "PPFD"
  PPFD <- file
  
  #PPR
  PPR <- file
  PPR$FPts <- PPR$FPts - PPR$RuFD - PPR$ReFD + PPR$Rec
  PPR$Avg <- PPR$FPts / PPR$G
  PPR$Scoring <- "PPR"
  
  #hPPR
  hPPR <- file
  hPPR$FPts <- hPPR$FPts - hPPR$RuFD - hPPR$ReFD + 0.5*hPPR$Rec
  hPPR$Avg <- hPPR$FPts / hPPR$G
  hPPR$Scoring <- "hPPR"
  
  #STD
  STD <- file
  STD$FPts <- STD$FPts - STD$RuFD - STD$ReFD
  STD$Avg <- STD$FPts / STD$G
  STD$Scoring <- "STD"
  
  #combine them and return it
  file <- rbind(PPFD, PPR, hPPR, STD)
  return(file)
}
seasons <- as.data.table(cleanSeasons(seasons))

# weekly.csv ----
#file of weekly scoring across NFL
#weekly <- as.data.table(read_csv("data/weekly.csv", col_types = cols()))
#demodata
weekly <- as.data.table(read_csv("demodata/weekly.csv", col_types = cols()))
cleanWeekly <- function(file) {
  #remove players that didnt play in a week
  file <- filter(file, is.na(Avg) == F)
  #create scoring setting column and initial PPFD file version
  file$Scoring <- "PPFD"
  PPFD <- file
  #PPR
  PPR <- file
  PPR$FPts <- PPR$FPts - PPR$RuFD - PPR$ReFD + PPR$Rec
  PPR$Avg <- PPR$FPts
  PPR$Scoring <- "PPR"
  #hPPR
  hPPR <- file
  hPPR$FPts <- hPPR$FPts - hPPR$RuFD - hPPR$ReFD + 0.5*hPPR$Rec
  hPPR$Avg <- hPPR$FPts
  hPPR$Scoring <- "hPPR"
  #STD
  STD <- file
  STD$FPts <- STD$FPts - STD$RuFD - STD$ReFD
  STD$Avg <- STD$FPts
  STD$Scoring <- "STD"
  #combine them and return it
  file <- rbind(PPFD, PPR, hPPR, STD)
  
  file$TRUFFLE[!(file$TRUFFLE %in% teams$Abbrev)] <- "FA"
  
  file <- file[order(Scoring,-Season,-Week,-FPts)][, `:=`(PosRk = 1:.N), by = .(Scoring, Season, Week, Pos)]
  
  return(file)
}
weekly <- as.data.table(cleanWeekly(weekly))

#create weekly with original teams by old
weekly_orig_teams <- weekly
weekly_orig_teams$TRUFFLE <- NULL
weekly_orig_teams <- merge(x = weekly_orig_teams, y = oldrosters[ , .(League, Season, Pos, Player, TRUFFLE)], by = c("League", "Season", "Pos", "Player"), all.x=TRUE)
weekly_orig_teams$TRUFFLE[is.na(weekly_orig_teams$TRUFFLE)] <- "FA"
#weekly just unique scoring records, no League or TRUFFLE info
weekly_no_teams <- weekly[League == "TRUFFLE", -c("League", "TRUFFLE")]


#add current truffle teams
weekly$TRUFFLE <- NULL
weekly <- merge(x = weekly, y = rosters[ , .(League, Pos, Player, TRUFFLE)], by = c("League", "Pos", "Player"), all.x=TRUE)
weekly$TRUFFLE[is.na(weekly$TRUFFLE)] <- "FA"

#add current season
currentseason <- weekly[Season == max(weekly$Season) & League == "TRUFFLE",
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
                        by = .(Scoring, Season, Pos, Player)]

if (max(seasons$Season) != max(currentseason$Season)) {
  seasons <- rbind(seasons, currentseason)
}
rm(currentseason)

seasons <- seasons[order(Scoring, -Season,-FPts, -Avg)][, `:=`(PosRk = 1:.N), by = .(Scoring, Season, Pos)]

#file of projections, mainly to be used during offseason
#demodata
proj <- as.data.table(read_csv("demodata/projections.csv", col_types = cols()))
#until Dre builds cleaning scrape file that accounts for names
proj$Player <- str_replace_all(proj$Player,"\\.","")
proj$Player <- str_replace_all(proj$Player," Jr","")
proj$Player <- str_replace_all(proj$Player," Sr","")
proj$Player <- str_replace_all(proj$Player," III","")
proj$Player <- str_replace_all(proj$Player," II","")
proj$Player <- str_replace_all(proj$Player,"Will Fuller V","Will Fuller")
proj$Player <- str_replace_all(proj$Player,"La'Mical","Lamical")

#add needed columns
proj <- proj[order(-Season,-FPts, -Avg)][, `:=`(PosRk = 1:.N), by = .(Season, Pos)]
proj$ptslogs <- NA
proj$`FPts/Touch` <- round(proj$FPts / (proj$PaCmp + proj$RuAtt + proj$Rec), 3)
proj$`FPts/Opp` <- round(proj$FPts / (proj$PaAtt + proj$RuAtt + proj$Tar), 3)
proj$`YdPt%` <- round((.04*proj$PaYd + .1*(proj$RuYd + proj$ReYd)) / proj$FPts,3)
proj$`TDPt%` <- round((4*proj$PaTD + 6*(proj$RuTD + proj$ReTD)) / proj$FPts,3)
proj$`FDPt%` <- 0


#file of standings
#dedmodata
standings <- as.data.table(read_csv("demodata/standings.csv", col_types = cols()))

#file to indicate what players have rookie rights
#rookierights <- read_csv("data/rookierights.csv", col_types = cols())
#rookierights <- as.vector(rookierights$Player)
#demodata
rookierights <- as.data.table(read_csv("demodata/rookierights.csv", col_types = cols()))

#file of draft records
#draft <- as.data.table(read_csv("data/drafts.csv", col_types = cols()))
#demodata
draft <- as.data.table(read_csv("demodata/drafts.csv", col_types = cols()))

#franchise tag files
#franchised <- as.data.table(read_csv("data/franchisetag.csv", col_types = cols()))
#top5paid <- as.data.table(read_csv("data/top5paid.csv", col_types = cols()))
#demodata
franchised <- as.data.table(read_csv("demodata/franchisetag.csv", col_types = cols()))
top5paid <- as.data.table(read_csv("demodata/top5paid.csv", col_types = cols()))

#rivalry scorers
#riv <- as.data.table(read_csv("data/rivalries.csv", col_types = cols()))
#demodata
riv <- as.data.table(read_csv("demodata/rivalries.csv", col_types = cols()))

#rivalry scores
#rivscores <- as.data.table(read_csv("data/rivalryscores.csv", col_types = cols()))
#demodata
rivscores <- as.data.table(read_csv("demodata/rivalryscores.csv", col_types = cols()))

rivscores$Winner <- ifelse(rivscores$Team1Score > rivscores$Team2Score, rivscores$Team1, rivscores$Team2)
rivscores$Icon <- "www/graphics/rivalrylogos/blank.png"
rivscores$Icon[rivscores$Thanksgiving == 1] <- "www/graphics/rivalrylogos/thanksgiving.png"
rivscores$Icon[(rivscores$Season < 2021 & rivscores$Week >= 14) | (rivscores$Season >= 2021 & rivscores$Week >= 15)] <- "www/graphics/rivalrylogos/PS.png"
rivscores$Icon[rivscores$Championship == 1] <- "www/graphics/rivalrylogos/rfs.png"

#fantasy scores by players in rivalry games
rivfantasy <- fantasy[Scoring == "PPFD" & (paste0(Season, "-", Week, "-", TRUFFLE) %in% paste0(rivscores$Season, "-", rivscores$Week, "-", rivscores$Team1) |
                  paste0(Season, "-", Week, "-", TRUFFLE) %in% paste0(rivscores$Season, "-", rivscores$Week, "-", rivscores$Team2))]
rivfantasy <- merge(rivfantasy, teams[, .(Abbrev, Rivalry)], by.x = "TRUFFLE", by.y = "Abbrev")
rivfantasy$Thanksgiving <- ifelse(paste0(rivfantasy$Season, "-", rivfantasy$Week) %in% paste0(rivscores$Season[rivscores$Thanksgiving == 1], "-", rivscores$Week[rivscores$Thanksgiving == 1]), 1, 0)
rivfantasy <- merge(rivfantasy, rivscores[, .(Season, Week, Rivalry, Icon)], by = c("Season", "Week", "Rivalry"), all.x = TRUE, allow.cartesian=TRUE)
rivfantasy$Yd <- rivfantasy$PaYd + rivfantasy$RuYd + rivfantasy$ReYd
rivfantasy$TD <- rivfantasy$PaTD + rivfantasy$RuTD + rivfantasy$ReTD
rivfantasy$FD <- rivfantasy$RuFD + rivfantasy$ReFD

rivscorers <- rivfantasy[,
                         .(G = .N,
                           FPts = sum(FPts, na.rm = T),
                           Avg = round(mean(FPts, na.rm = T),2)),
                         by = .(Rivalry, TRUFFLE, Pos, Player)]

turkeyscorers <- rivfantasy[Thanksgiving == 1,
                            .(G = .N,
                              FPts = sum(FPts, na.rm = T),
                              Avg = round(mean(FPts, na.rm = T),2)),
                            by = .(Rivalry, TRUFFLE, Pos, Player)]


#read in advanced combined files
#extradash <- as.data.table(read_csv("data/extraDash.csv", col_types = cols()))
#demodata
extradash <- as.data.table(read_csv("demodata/extraDash.csv", col_types = cols()))

colnames(extradash)[6:19] <- c("Cmp%", "Pa20", "Pa40", "RuYPC", "Ru20", "Tar", "Tar%", "ReYPC", "Re20", "Re40", "ReFD%", "TotYd", "Avg", "FPts")

#don't think extradash needs TRUFFLE team info
#extradash$TRUFFLE <- NULL
#extradash <- merge(x = extradash, y = rosters[, .(Pos, Player, TRUFFLE)], by = c("Pos", "Player"), all.x=TRUE)
#extradash$TRUFFLE[!(extradash$TRUFFLE %in% teams$Abbrev)] <- "FA"
extradash <- extradash[Avg != "-"]
extradash <- extradash[, c(3:4, 1:2, 5:19)][order(-Week, -TotYd)]

#merge in other columns for calcs
extradash <- merge(x = extradash, y = weekly_no_teams[Scoring == "PPFD" , .(Season, Week, Pos, Player, PaCmp, PaAtt, RuAtt, RuYd, Rec, ReYd, ReFD)], by = c("Season", "Week", "Pos", "Player"))

extradashszn <- extradash[,
                          .(G = .N,
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
extradashszn <- extradashszn[order(-TotYd)]

#espn data
#demodata
espn <- suppressWarnings(as.data.table(read_csv("demodata/espnStats.csv", col_types = cols(Season = col_double(), Player = col_character(), NFL = col_character(),
                                                                      Pos = col_character(), xFP = col_double(), ActualPts = col_double(), xTD = col_double(),
                                                                      TD = col_double(), Looks = col_double(), Diff = col_double(), In5 = col_double(), EZ = col_double()))))

espn <- espn[Player != "Jeffery Simmons"]
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
espn[espn=="-Inf"] <- 0
#espn$xFP <- as.numeric(espn$xFP); espn$ActualPts <- as.numeric(espn$ActualPts); espn$xTD <- as.numeric(espn$xTD); espn$TD <- as.numeric(espn$TD)
#espn$Looks <- as.numeric(espn$Looks); espn$Diff <- as.numeric(espn$Diff); espn$In5 <- as.numeric(espn$In5); espn$EZ <- as.numeric(espn$EZ)
espn$FPDiff <- espn$ActualPts - espn$xFP

#get rid of TRUFFLE column, do this later
#espn <- merge(x = espn, y = oldrosters[ , .(Season, Pos, Player, TRUFFLE)], by = c("Season", "Pos", "Player"), all.x=TRUE)
#espn$TRUFFLE[is.na(espn$TRUFFLE)] <- "FA"

colnames(espn)[9] <- "TDDiff"
#espn <- espn[, .(Season, TRUFFLE, Pos, Player, xFP, ActualPts, FPDiff, xTD, TD, TDDiff, Looks, `In5`, EZ)]
espn <- espn[, .(Season, Pos, Player, xFP, ActualPts, FPDiff, xTD, TD, TDDiff, Looks, `In5`, EZ)]

#snaps data
#demodata
snaps <- as.data.table(read_csv("demodata/snapPer.csv", col_types = cols()))

#get rid of TRUFFLE column, do this later
#snaps <- merge(x = snaps, y = rosters[ , .(Pos, Player, TRUFFLE)], by = c("Pos", "Player"), all.x=TRUE)
#snaps$TRUFFLE[is.na(snaps$TRUFFLE)] <- "FA"

snaps <- snaps[, c(1, 3, 2, 4:22, 24, 23)]
snaps[snaps == "bye"] <- NA
colnames(snaps)[23:24] <- c("Avg", "Tot")
#dividing snaps by 100 for percentage formatting
for (i in 5:24) {
  #if(is.numeric(snaps[[i]])) {
    snaps[[i]] <- as.numeric(snaps[[i]])
    snaps[[i]] <- snaps[[i]]/100
  #}
}
snaps[snaps == 0] <- NA

# modifying tables for display -----
#all data loads complete

# helpful vectors
positionorder <- c("QB","RB","WR","TE", "DST", "IR", "DC")

#ISSUE
#possibly need to do this in server
#team portal tables
tpoverview <- rosters[, .(League, TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract)]
tpoverview$Scoring <- "PPFD"
tpoverviewPPR <- tpoverview; tpoverviewPPR$Scoring <- "PPR"
tpoverviewhPPR <- tpoverview; tpoverviewhPPR$Scoring <- "hPPR"
tpoverviewSTD <- tpoverview; tpoverviewSTD$Scoring <- "STD"
tpoverview <- rbind(tpoverview, tpoverviewPPR, tpoverviewhPPR, tpoverviewSTD); rm(tpoverviewPPR,tpoverviewhPPR,tpoverviewSTD)

tpoverview <- merge(tpoverview, seasons[Player %in% rosters$Player & Season == currentyr][, !c("Season", "NFL")], by = c('Scoring','Pos','Player'), all.x = T)
tpoverview$Avg[tpoverview$Pos == "DST"] <- NA
tpoverview <- tpoverview[, .(Scoring,League,TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract, G, Avg, FPts)][order(match(Pos, positionorder), -Avg)]

ptslogs <- weekly[order(-Season, Week)][,
                                        .(ptslog = rev(list(FPts))),
                                        by = .(Scoring, Season, Pos, Player)]

tpoverview <- merge(tpoverview, ptslogs[Season == max(seasons$Season), .(Player,Pos,Scoring,ptslog)], by = c('Player','Pos','Scoring'), all.x = T)
tpoverview <- merge(tpoverview, seasons[Season == max(seasons$Season)][, .(Player,Pos,Scoring,PosRk)], by = c('Player','Pos','Scoring'), all.x = T)

if (isOffseason == T) {
  tpoverview$Avg <- NULL
  tpoverview$FPts <- NULL
  tpoverview$ptslog <- 0

  #add in projected average points and total points
  tpoverview <- merge(tpoverview, proj[, .(Player,Pos,Avg,FPts)], by = c('Player','Pos'), all.x = T)

}

tpoverview <- tpoverview[, .(Scoring, League, TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract, G, PosRk, ptslog, Avg, FPts)][order(match(Pos, positionorder), -Avg)]

#create oldrosters views to be used in teamportal for prior years selected
oldrosterstp <- oldrosters[Season >= 2020, .(Season, League, TRUFFLE, Pos, Player, NFL, Salary, Contract)]
oldrosterstp$Scoring <- "PPFD"
oldrosterstpPPR <- oldrosterstp; oldrosterstpPPR$Scoring <- "PPR"
oldrosterstphPPR <- oldrosterstp; oldrosterstphPPR$Scoring <- "hPPR"
oldrosterstpSTD <- oldrosterstp; oldrosterstpSTD$Scoring <- "STD"
oldrosterstp <- rbind(oldrosterstp, oldrosterstpPPR, oldrosterstphPPR, oldrosterstpSTD); rm(oldrosterstpPPR,oldrosterstphPPR,oldrosterstpSTD)

#merge in other columns for oldrosters team portal
oldrosterstp <- merge(oldrosterstp, seasons[Player %in% oldrosterstp$Player, -"NFL"], by = c('Scoring','Season','Pos','Player'), all.x = T)
oldrosterstp$Avg[oldrosterstp$Pos == "DST"] <- NA
oldrosterstp <- oldrosterstp[, .(Scoring, Season, League, TRUFFLE, Pos, Player, NFL, Salary, Contract, G, Avg, FPts)][order(match(Pos, positionorder), -Avg)]
#merge in ptslogs and posrks
oldrosterstp <- merge(oldrosterstp, ptslogs[, .(Scoring,Season,Pos,Player,ptslog)], by = c('Scoring','Season','Pos','Player'), all.x = T)
oldrosterstp <- merge(oldrosterstp, seasons[, .(Scoring,Season,Pos,Player,PosRk)], by = c('Scoring','Season','Pos','Player'), all.x = T)

#create wayoldrostersview for before 2020
wayoldrosterstp <- oldrosters[Season < 2020, .(Season, TRUFFLE, Pos, Player, NFL)]
wayoldrosterstp$Scoring <- "PPFD"
wayoldrosterstpPPR <- wayoldrosterstp; wayoldrosterstpPPR$Scoring <- "PPR"
wayoldrosterstphPPR <- wayoldrosterstp; wayoldrosterstphPPR$Scoring <- "hPPR"
wayoldrosterstpSTD <- wayoldrosterstp; wayoldrosterstpSTD$Scoring <- "STD"
wayoldrosterstp <- rbind(wayoldrosterstp, wayoldrosterstpPPR, wayoldrosterstphPPR, wayoldrosterstpSTD); rm(wayoldrosterstpPPR,wayoldrosterstphPPR,wayoldrosterstpSTD)

#merge in stats for wayoldrosters
wayoldrosterstp <- merge(wayoldrosterstp, seasons[Player %in% wayoldrosterstp$Player, -"NFL"], by = c('Scoring','Season','Pos','Player'), all.x = T)
wayoldrosterstp <- wayoldrosterstp[, .(Scoring, Season, TRUFFLE, Pos, Player, NFL, G, PosRk, Avg, FPts)][order(match(Pos, positionorder), -Avg)]

#franchise tag vals
lgs <- c("TRUFFLE", "KERFUFFLE")
tagvals <- data.table()
for (i in 1:length(lgs)) {
tagvals <- rbind(tagvals, as.data.table(
  rbind(
    c(lgs[i], "QB", "First", round(mean(top5paid$Salary[top5paid$Pos == "QB" & top5paid$Season == currentyr]))),
    c(lgs[i], "QB", "Second", max(top5paid$Salary[top5paid$Pos == "QB" & top5paid$Season == currentyr]) + 1 ),
    c(lgs[i], "RB", "First", round(mean(top5paid$Salary[top5paid$Pos == "RB" & top5paid$Season == currentyr]))),
    c(lgs[i], "RB", "Second", max(top5paid$Salary[top5paid$Pos == "RB" & top5paid$Season == currentyr]) + 1 ),
    c(lgs[i], "WR", "First", round(mean(top5paid$Salary[top5paid$Pos == "WR" & top5paid$Season == currentyr]))),
    c(lgs[i], "WR", "Second", max(top5paid$Salary[top5paid$Pos == "WR" & top5paid$Season == currentyr]) + 1 ),
    c(lgs[i], "TE", "First", round(mean(top5paid$Salary[top5paid$Pos == "TE" & top5paid$Season == currentyr]))),
    c(lgs[i], "TE", "Second", max(top5paid$Salary[top5paid$Pos == "TE" & top5paid$Season == currentyr]) + 1 )
  )
))
}
colnames(tagvals) <- c("League", "Pos", "Type", "TagVal")
tagvals$TagVal <- as.numeric(tagvals$TagVal)

#table of players with franchise tag values
ft <- rosters[, .(League, Pos, Player, Salary, Contract)][order(-Salary)][Pos %in% c("QB", "RB", "WR", "TE")]
ft$TagVal <- NA

for (i in 1:nrow(ft)) {
  lg <- ft$League[i]
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
  } else if (pl %in% franchised$Player[franchised$Season == currentyr & franchised$League == lg] & pl %in% franchised$Player[franchised$Season == currentyr - 1 & franchised$League == lg]) {
    ft$TagVal[i] <- "Ineligible (tagged 2x)"
    #set players franchise tagged previously to second tag value based on position
  } else if (pl %in% franchised$Player[franchised$Season == currentyr & franchised$League == lg]) {
    ft$TagVal[i] <- tagvals$TagVal[tagvals$Pos == pos & tagvals$Type == "Second" & tagvals$League == lg]
    #set players who's current salary is greater than first tag value, to current salary plus 1
    } else if (csal >= tagvals$TagVal[tagvals$Pos == pos & tagvals$Type == "First" & tagvals$League == lg]) {
    ft$TagVal[i] <- csal + 1
    #set all other players on 1 year contracts to first tag
  } else {
    ft$TagVal[i] <- tagvals$TagVal[tagvals$Pos == pos & tagvals$Type == "First" & tagvals$League == lg]
  }
}

#contracts table
contracts <- rosters[, .(League, TRUFFLE, Pos, Player, Age, NFL, Salary, Contract)]
contracts <- merge(x = contracts, y = draft[, .(League, Pos, Player, Extension)], by = c("League", "Pos", "Player"), all.x = T)[order(Player)]
contracts <- merge(x = contracts, y = ft[, .(League, Pos, Player, TagVal)], by = c("League", "Pos", "Player"), all.x = T)[order(Player)]
contracts$Y1 <- NA; contracts$Y2 <- NA; contracts$Y3 <- NA; contracts$Y4 <- NA; contracts$Y5 <- NA

#loop through leagues and contracts to populate future years
suppressWarnings(for (i in 1:nrow(contracts)) {
  lg <- contracts$League[i]
  pl <- contracts$Player[i]
  csal <- contracts$Salary[i]
  ccon <- contracts$Contract[i]
  ext <- contracts$Extension[i]
  tagval <- contracts$TagVal[i]
  
  contracts$Y1[i] <- csal
  contracts$Y2[i] <- ifelse(ccon > 1, csal,
                            ifelse(ccon == 1 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, ext,
                                   ifelse(ccon == 1 & as.numeric(tagval) > 0 & is.na(as.numeric(tagval)) == F, tagval,
                                          ifelse(ccon == 1 & is.element(pl, rookierights$Player[rookierights$League == lg]) == F, "FA", "-"))))
  contracts$Y3[i] <- ifelse(ccon > 2, csal,
                            ifelse(ccon == 1 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, ext,
                                   ifelse(ccon == 2 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, ext,
                                          ifelse(ccon == 1 & is.element(pl, franchised$Player[franchised$Season == currentyr & franchised$League == lg]) == F, "FT",
                                                 ifelse(ccon == 1 & is.element(pl, franchised$Player[franchised$Season == currentyr - 1 & franchised$League == lg]) == F, "FA",
                                                        ifelse(ccon == 2, "FT", "-"))))))
  contracts$Y4[i] <- ifelse(ccon > 3, csal,
                            ifelse(ccon == 1 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, "FT",
                                   ifelse(ccon == 2 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, ext,
                                          ifelse(ccon == 3 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, ext,
                                                 ifelse(ccon == 1 & is.element(pl, franchised$Player[franchised$Season == currentyr & franchised$League == lg]) == F, "FA",
                                                        ifelse(ccon == 2, "FT",
                                                               ifelse(ccon == 3 & is.element(pl, rookierights$Player[rookierights$League == lg]) == F, "FT", "-")))))))
  contracts$Y5[i] <- ifelse(ccon > 4, csal,
                            ifelse(ccon == 1 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, "FT",
                                   ifelse(ccon == 2 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, "FT",
                                          ifelse(ccon == 3 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, ext,
                                                 ifelse(ccon == 4 & is.element(pl, rookierights$Player[rookierights$League == lg]) == T, ext,
                                                        ifelse(ccon == 3 & is.element(pl, rookierights$Player[rookierights$League == lg]) == F, "FT",
                                                               ifelse(ccon == 2 & is.element(pl, rookierights$Player[rookierights$League == lg]) == F, "FA",
                                                                      ifelse(ccon == 4 & is.element(pl, rookierights$Player[rookierights$League == lg]) == F, "FT", "-"))))))))
  
})
contracts <- contracts[order(-Salary)]

#cap corner graph creation
capbyteam <- rosters[,
                        .(TeamSalary = sum(Salary)),
                        by = .(League, TRUFFLE)]
capbyteam <- merge(rosters, capbyteam, by = c("League", "TRUFFLE"))
capbyteam$SalaryPerc <- capbyteam$Salary / capbyteam$TeamSalary

#ppd <- teamportal[, `:=`(`PP$` = round(FPts/Salary,2), `wPP$`= round(Avg/Salary,2))][, c("TRUFFLE", "Pos", "Player", "Avg", "FPts", "PP$", "wPP$")]

#aggregating fantasy data by team
pointsleaders <- weekly[order(-Season, Week)][League == "TRUFFLE",
                                              .(G = .N,
                                                ptslogs = list(FPts),
                                                Avg = round(mean(FPts),1),
                                                Total = round(sum(FPts),1)
                                              ),
                                              by = .(Scoring, Season, Pos, Player)][order(-Season, match(Pos, positionorder), -Total, -Avg)][, `:=`(PosRk = 1:.N), by = .(Scoring, Season, Pos)]
pointsleaders$League <- "TRUFFLE"; pointsleaderscopy <- pointsleaders; pointsleaderscopy$League <- "KERFUFFLE"; pointsleaders <- rbind(pointsleaders, pointsleaderscopy); rm(pointsleaderscopy)

#merge in seasonal ownership info
pointsleaders <- merge(x = pointsleaders, y = oldrosters[ , c("League", "Season", "Pos", "Player", "TRUFFLE")], by = c("League", "Season", "Pos", "Player"), all.x=TRUE)
pointsleaders$TRUFFLE[is.na(pointsleaders$TRUFFLE)] <- "FA"
pointsleaders <- pointsleaders[, .(League, Scoring, Season, TRUFFLE, Player, Pos, PosRk, G, ptslogs, Avg, Total)][order(-Total, -Avg)]

#player portal bios top table
ppbios <- weekly[Season == max(weekly$Season)][order(-Season,-Week)]
ppbios <- ppbios[,
                 .(TRUFFLE = TRUFFLE[1],
                   NFL = NFL[1],
                   ptslogs = list(FPts),
                   Avg = round(mean(FPts),1),
                   Total = round(sum(FPts))),
                 by = .(League, Scoring, Pos, Player)]
ppbios <- merge(ppbios, rosters[, .(League, Player, Pos, Salary, Contract)], by = c('League', 'Player', 'Pos'), all.x = T)
ppbios <- merge(ppbios, fprosage[, .(Player, Age, DynRk, DynPosRk)], by = 'Player')
ppbios <- ppbios[, .(League, Scoring, TRUFFLE,Pos,Player,NFL,Age,DynRk,DynPosRk,Salary,Contract,ptslogs)]

#creating advanced tables across scoring systems ----
advanced <- weekly[League == "TRUFFLE", .(FPts = sum(FPts),
                       YdPts = round(.04*sum(PaYd) + .1*(sum(RuYd) + sum(ReYd)),1),
                       TDPts = 4*sum(PaTD) + 6*(sum(RuTD) + sum(ReTD)),
                       FDPts = ifelse(Scoring == "PPFD", sum(RuFD) + sum(ReFD), 0),
                       RuPts = ifelse(Scoring == "PPFD", .1*sum(RuYd) + 6*sum(RuTD) + sum(RuFD), .1*sum(RuYd) + 6*sum(RuTD)),
                       RePts = ifelse(Scoring == "PPFD", .1*sum(ReYd) + 6*sum(ReTD) + sum(ReFD),
                                      ifelse(Scoring == "PPR", .1*sum(ReYd) + 6*sum(ReTD) + sum(Rec),
                                             ifelse(Scoring == "hPPR", .1*sum(ReYd) + 6*sum(ReTD) + 0.5*sum(Rec),
                                                    .1*sum(ReYd) + 6*sum(ReTD)))),
                       Touch = sum(PaCmp + RuAtt + Rec),
                       Opp = sum(PaAtt + RuAtt + Tar)
),
by = .(Scoring,Season,Pos,Player)][, `:=`(`YdPt%` = YdPts / FPts,
                                                   `TDPt%` = TDPts / FPts,
                                                   `FDPt%` = FDPts / FPts,
                                                   `RuPt%` = RuPts / FPts,
                                                   `RePt%` = RePts / FPts,
                                                   `FPts/Touch` = round(FPts/Touch, 3),
                                                   `FPts/Opp` = round(FPts/Opp, 3)
)][order(-FPts)][, .(Scoring,Season,Pos,Player,FPts,Touch,Opp,`FPts/Touch`,`FPts/Opp`,YdPts,TDPts,FDPts,RuPts,RePts,`YdPt%`,`TDPt%`,`FDPt%`,`RuPt%`,`RePt%`)]
advanced$League <- "TRUFFLE"; advancedcopy <- advanced; advancedcopy$League <- "KERFUFFLE"; advanced <- rbind(advanced, advancedcopy); rm(advancedcopy)
advanced <- merge(x = advanced, y = oldrosters[ , c("League", "Season", "Pos", "Player", "TRUFFLE")], by = c("League", "Season", "Pos", "Player"), all.x=TRUE)


#consistencystats
consistencystart <- weekly_no_teams
#posrank dummies
consistencystart$top5dum <- ifelse(consistencystart$PosRk <= 5, 1, 0)
consistencystart$top12dum <- ifelse(consistencystart$PosRk <= 12, 1, 0)
consistencystart$top24dum <- ifelse(consistencystart$PosRk <= 24, 1, 0)
consistencystart$top36dum <- ifelse(consistencystart$PosRk <= 36, 1, 0)
consistencystart$nonStartdum <- ifelse(consistencystart$PosRk > 36, 1, 0)
#point threshold dummies
consistencystart$lt10dum <- ifelse(consistencystart$FPts < 10, 1, 0)
consistencystart$gt10dum <- ifelse(consistencystart$FPts >= 10, 1, 0)
consistencystart$gt20dum <- ifelse(consistencystart$FPts >= 20, 1, 0)
consistencystart$gt30dum <- ifelse(consistencystart$FPts >= 30, 1, 0)

#creating consistencytable
consistency <- consistencystart[,
   .(G = .N,
     Avg = round(mean(FPts),1),
     RelSD = round(sd(FPts)/mean(FPts),2),
     AvgPosRk = round(mean(PosRk),1),
     `Top5 %` = sum(top5dum)/.N,
     `Top12 %` = sum(top12dum)/.N,
     `Top24 %` = sum(top24dum)/.N,
     `Top36 %` = sum(top36dum)/.N,
     `NonStart %` = sum(nonStartdum)/.N,
     `>10 %` = sum(gt10dum)/.N,
     `>20 %` = sum(gt20dum)/.N,
     `>30 %` = sum(gt30dum)/.N
   ),
   by = .(Scoring, Season, Pos, Player)][order(-Avg)][, .(Scoring, Season,Pos,Player,G,Avg,RelSD,`>10 %`,`>20 %`,`>30 %`,`AvgPosRk`,`Top5 %`,`Top12 %`,`Top24 %`,`Top36 %`, `NonStart %`)]
#rm(consistencystart)
consistency$League <- "TRUFFLE"; consistencycopy <- consistency; consistencycopy$League <- "KERFUFFLE"; consistency <- rbind(consistency, consistencycopy); rm(consistencycopy)
consistency <- merge(x = consistency, y = oldrosters[ , c("League", "Season", "Pos", "Player", "TRUFFLE")], by = c("League", "Season", "Pos", "Player"), all.x=TRUE)

#weeklytop5s
weeklytop5 <- weekly_no_teams[order(Week,-FPts)][, .(#TRUFFLE = NA,
                                                   Player = Player[1:30],
                                                   FPts = FPts[1:30]), 
                                               by = .(Scoring,Season,Week,Pos)][, .(Scoring,Season,Week,Pos,Player,FPts)]

#rookie extension values
extval <- as.data.table(rbind(c("QB", "1.1-1.6", 70), c("QB", "1.7-1.12", 60), c("QB", "2", 50), c("QB", "3", 30),
                              c("RB", "1.1-1.6", 70), c("RB", "1.7-1.12", 60), c("RB", "2", 50), c("RB", "3", 30),
                              c("WR", "1.1-1.6", 60), c("WR", "1.7-1.12", 50), c("WR", "2", 40), c("WR", "3", 30),
                              c("TE", "1.1-1.6", 30), c("TE", "1.7-1.12", 20), c("TE", "2", 10), c("TE", "3", 5)
))
colnames(extval) <- c("Pos","Pick", "Value")
extval$Value <- as.numeric(extval$Value)

#fantasy portal table
truffleanalysis <- fantasy[Scoring == "PPFD",
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
                           by = .(League, Season, TRUFFLE)][order(-FPts)]

truffleanalysisperc <- fantasy[Scoring == "PPFD",
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
                               by = .(League,Season, TRUFFLE)][order(-FPts)]

pptrufflecareer <- fantasy[Pos != "DST" & Scoring == "PPFD",
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
                           by = .(League, Pos, Player)]

pptrufflecareerteam <- fantasy[Pos != "DST" & Scoring == "PPFD",
                               .(Seasons = lapply(list(paste0(" ",unique(substr(Season ,3,4)))), sort, decreasing = F),
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
                               by = .(Pos, Player, League, TRUFFLE)][order(-FPts)]


#byog master table set up -----
byog <- merge(seasons[Season > 2020], advanced[, -c("FPts","TRUFFLE")], by = c('Scoring','Season','Pos','Player'), all.x = T)
byog <- merge(byog, consistency[, -c("Avg","G","TRUFFLE")], by = c('Scoring','Season','Pos','Player'), all.x = T)
byog <- merge(byog, extradashszn[, -c("Tar","G")], by = c('Season','Pos','Player'), all.x = T)
byog <- merge(byog, espn, by = c('Season','Pos','Player'), all.x = T)
byog <- merge(byog, oldrosters[, -c("TRUFFLE","NFL")], by = c('Season','Pos','Player'), all.x = T)
byog$PPFD <- ifelse(byog$Scoring == "PPFD", byog$FPts,
                    ifelse(byog$Scoring == "PPR", byog$FPts - byog$Rec + byog$RuFD + byog$ReFD,
                           ifelse(byog$Scoring == "hPPR", byog$FPts - 0.5*byog$Rec + byog$RuFD + byog$ReFD,
                                  ifelse(byog$Scoring == "STD", byog$FPts + byog$RuFD + byog$ReFD, NA))))
byog$PPR <- ifelse(byog$Scoring == "PPFD", byog$FPts - byog$RuFD - byog$ReFD + byog$Rec,
                    ifelse(byog$Scoring == "PPR", byog$FPts,
                           ifelse(byog$Scoring == "hPPR", byog$FPts + 0.5*byog$Rec,
                                  ifelse(byog$Scoring == "STD", byog$FPts + byog$Rec, NA))))
byog$hPPR <- ifelse(byog$Scoring == "PPFD", byog$FPts - byog$RuFD - byog$ReFD + 0.5*byog$Rec,
                   ifelse(byog$Scoring == "PPR", byog$FPts - 0.5*byog$Rec,
                          ifelse(byog$Scoring == "hPPR", byog$FPts,
                                 ifelse(byog$Scoring == "STD", byog$FPts + 0.5*byog$Rec, NA))))
byog$STD <- ifelse(byog$Scoring == "PPFD", byog$FPts - byog$RuFD - byog$ReFD,
                    ifelse(byog$Scoring == "PPR", byog$FPts - byog$Rec,
                           ifelse(byog$Scoring == "hPPR", byog$FPts - 0.5*byog$Rec,
                                  ifelse(byog$Scoring == "STD", byog$FPts, NA))))

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
radarplot <- weekly[League == "TRUFFLE",
                    .(FPts = mean(FPts, na.rm = T),
                      Touches = mean(PaCmp, na.rm = T) + mean(RuAtt, na.rm = T) + mean(Rec, na.rm = T),
                      Yd = mean(PaYd, na.rm = T) + mean(RuYd, na.rm = T) + mean(ReYd, na.rm = T),
                      TD = mean(PaTD, na.rm = T) + mean(RuTD, na.rm = T) + mean(ReTD, na.rm = T),
                      FD = mean(RuFD, na.rm = T) + mean(ReFD, na.rm = T)
                    ),
                    by = .(Scoring, Season, Pos, Player)][order(Player)]
#get the max positional values based on radar plot
radarplotmax <- radarplot[,
                          .(FPts = max(FPts, na.rm = T),
                            Touches = max(Touches, na.rm = T),
                            Yd = max(Yd, na.rm = T),
                            TD = max(TD, na.rm = T),
                            FD = max(FD, na.rm = T)
                          ),
                          by = .(Scoring, Season, Pos)]
radarplotmax$Player <- "MAX"
radarplotmax <- radarplotmax[, .(Scoring, Season, Pos, Player, FPts, Touches, Yd, TD, FD)]
#get min positional values based on radar plot
radarplotmin <- radarplot[,
                          .(FPts = min(FPts, na.rm = T),
                            Touches = min(Touches, na.rm = T),
                            Yd = min(Yd, na.rm = T),
                            TD = min(TD, na.rm = T),
                            FD = min(FD, na.rm = T)
                          ),
                          by = .(Scoring, Season, Pos)]
radarplotmin$Player <- "MIN"
radarplotmin <- radarplotmin[, .(Scoring, Season, Pos, Player, FPts, Touches, Yd, TD, FD)]
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
recordbookstm <- fantasy[Scoring == "PPFD",
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
                         by = .(League, TRUFFLE, Pos, Player)][Pos != "DST"]

recordbookspl <- fantasy[Scoring == "PPFD",
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
                         by = .(League, Pos, Player)][Pos != "DST"]

#awards <- as.data.table(read_excel("data/awards.xlsx"))
#demodata
awards <- as.data.table(read_csv("demodata/awards.csv", col_types = cols()))

#reactable column formats ----

smallboxwidth <- 45

#functions
z_avg_pal <- function(x) { 
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
z_action_mod <- function(df, team) {
  myteam <- team
  df <- merge(df, ids[, .(Player, playerID, TeamNum)], by = 'Player', all.x = T)
  df$Action <- ifelse(df$TRUFFLE == myteam, "www/graphics/actions/drop.png",
                      ifelse(df$TRUFFLE == "FA", "www/graphics/actions/add.png",
                             "www/graphics/actions/trade.png"))
  df$ActionLink <- ifelse(df$TRUFFLE == myteam, paste0("https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main?selectedplayer=", df$playerID),
                          ifelse(df$TRUFFLE == "FA", paste0("https://theradicalultimatefflexperience.football.cbssports.com/stats/stats-main?default_add=", df$Pos, ":", df$playerID),
                                 paste0("https://theradicalultimatefflexperience.football.cbssports.com/transactions/trade/", df$playerID, "/", df$TeamNum)))
  return(df)
}

z_bar_chart <- function(label, width = "100%", height = "16px", fill = QBcolor, background = NULL, prefix = "") {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), paste0(prefix,label), chart)
}

z_with_tt <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, value)
}

#column definitions / definition functions
z_actionDef <- function() {
  colDef(header = z_with_tt("Act", "Drop players on your team\nTrade for players on other teams\nAdd Free Agents"),
         show = !isguest,
         align="center", 
         minWidth = 35, 
         cell = function(value) {
           img_src <- knitr::image_uri(value)
           image <- img(src = img_src, height = "10px", alt = "drop")
           tagList(
             div(style = list(display = "inline-block"), image)
           )
         })
}

z_trfDef <- function(name = "TRF", maxW = 75, filt = TRUE, sort = TRUE, minW = 75, proj = F) {
  colDef(name = name,
         show = !isguest,
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

z_trfDefRivScores <- function(name = "TRF", maxW = 75, filt = TRUE, sort = TRUE, minW = 75) {
  colDef(name = name,
         show = !isguest,
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
         },
         footer = function(values) {
           tm <- unique(values)[1]
           paste0("W: ", nrow(rivscores[Winner == tm]))
         }
  )
}

z_posDef <- function(maxW = 48, filt = T, foot = NULL, sort = T) {
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
           } else if (value == "DST" | value == "K") {
             color <- DSTcolor
           }  else if (value == "DC") {
             color <- DCcolor
           }  else if (grepl("IR", value)) {
             color <- IRcolor
           } else if (grepl("FL", value)) {
             color <- FLcolor
           } else if (grepl("SF", value)) {
             color <- SFcolor
           } else {
             color <- "white"
           }
           list(background = color)
         })
}

z_playerDef <- function(minW = 200, filt = FALSE, sort = T) {
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

z_posRkDef <- function(maxW = 62, filt = F, proj = F) {
  colDef(header = z_with_tt("PosRk", "Seasonal position rank by total FPts"),
         maxWidth = maxW,
         filterable = filt,
         align = 'right',
         defaultSortOrder = "asc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         sortNALast = T)
}

z_ptsLogDef <- function(maxW = 75) {
  colDef(header = z_with_tt("PtsLog", "Weekly log of FPts"),
         sortable = F,
         align = 'right',
         maxWidth = maxW,
         filterable=F,
         cell = function(values) {
           sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(weekly$FPts))
         })
}

z_salaryDefBar <- function(minW = 175, foot = F) {
  colDef(minWidth = minW,
         align = 'left',
         format = colFormat(digits=0),
         defaultSortOrder = "desc",
         style = function(value) {
           color <- ifelse(value <= 15, IRcolor, 'black')
           list(color = color)},
         cell = function(value) {
           width <- paste0(value / max(rosters$Salary) * 100, "%")
           z_bar_chart(ifelse(is.na(value), "", value), width = ifelse(is.na(value), 0, width), prefix = "$")
         },
         footer = function(values) if(foot == T) {suppressWarnings(paste0("$", sum(values)))}
  )
}

z_tagvalDefBar <- function(minW = 175, foot = F) {
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
           z_bar_chart(ifelse(is.na(value), "", value), width = ifelse(is.na(value), 0, width), prefix = "$")
         },
         footer = function(values) if(foot == T) {suppressWarnings(paste0("$", sum(values)))}
  )
}

z_salaryDefNobar <- function(minW = 45, foot = F, title = "$") {
  colDef(minWidth = minW,
         name = title,
         align = 'right',
         defaultSortOrder = "desc",
         format = colFormat(digits=0, prefix = "$"),
         style = function(value) {
           color <- ifelse(value <= 15, IRcolor, 'black')
           list(color = color)},
         footer = function(values) if(foot == T) {suppressWarnings(paste0("$", sum(values)))}
  )
}

z_tagvalDefNobar <- function(minW = 45, foot = T, title = "$") {
  colDef(minWidth = minW,
         name = title,
         align = 'right',
         defaultSortOrder = "desc",
         format = colFormat(digits=0, prefix = "$"),
         style = function(value) {
           color <- ifelse(value <= 15, IRcolor, 'black')
           list(color = color)},
         footer = function(values) if(foot == T) {suppressWarnings(paste0("$", round(mean(values)) ))}
  )
}

z_contractDef <- function(minW = 45, filt = T, foot = F, title = "Contract") {
  colDef(minWidth = minW,
         filterable = filt,
         name = title,
         style = function(value) {
           background <- ifelse(value == 1, RBcolor,
                                ifelse(value == 2, TEcolor,
                                       ifelse(value == 3, WRcolor, QBcolor)))
           list(background = background)},
         footer = function(values) if(foot == T) {sum(values)}
  )
}

#formatting for future columns (including FA and RR tags)
z_futureColDef <- function(maxW = 75, filt = T, foot = F, yr) {
  colDef(header = z_with_tt(yr, "FA: Free Agent\nPurple: Rookie Extension Value\nBlue: Franchise Tag"),
         maxWidth = maxW,
         filterable = filt,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value, index) {
           tag <- contracts$TagVal[index]
           ext <- contracts$Extension[index]
           col <- ifelse(as.character(value) == "FT", franchisetag,
                         ifelse(as.character(value) == as.character(ext), rookieextension,
                                ifelse(as.character(value) == "FA", textred,
                                       ifelse(as.character(value) == as.character(tag), franchisetag, tabletextcol))))
           list(color = col)},
         footer = function(values) if(foot == T) {suppressWarnings(paste0("$", sum(as.numeric(values), na.rm=T)))}
  )
}

z_avgDef <- function(maxW = 65, digs = 1, filt = F, col = T, borderL = F, foot = F) {
  colDef(header = z_with_tt("Avg", "Weekly average FPts"),
         maxWidth = maxW,
         format = colFormat(digits = digs),
         filterable = filt,
         defaultSortOrder = "desc",
         class = function(value) if(borderL == T) {"border-left-grey"},
         style = function(value) { 
           normalized <- (value) / (max(seasons$Avg,na.rm=T))
           color <- ifelse(value > 0, z_avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
           if(col == T) {list(background = color)}
         },
         footer = function(values) if(foot == T) {round(mean(as.numeric(values), na.rm=T),2)}
  )
}

z_fptsDef <- function(maxW = 65, borderL = T, digs = 1, filt = F, foot = F) {
  colDef(header = z_with_tt("FPts", "Fantasy points"),
         maxWidth = maxW,
         format = colFormat(digits = digs),
         filterable = filt,
         defaultSortOrder = "desc",
         class = function(value) if(borderL == T) {"border-left-grey"},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

z_fptsWeekDef <- function(maxW = 65, borderL = T, digs = 1, filt = F, col = T) {
  colDef(header = z_with_tt("FPts", "Fantasy points"),
         maxWidth = maxW,
         format = colFormat(digits = digs),
         filterable = filt,
         defaultSortOrder = "desc",
         class = function(value) if(borderL == T) {"border-left-grey"},
         style = function(value) { 
           normalized <- (value) / (max(weekly$FPts[weekly$Season == max(weekly$Season)],na.rm=T))
           color <- ifelse(value > 0, z_avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
           if(col == T) {list(background = color)}
         }
  )
}

z_fptsSeasDef <- function(maxW = 65, borderL = F, digs = 1, filt = F, col = T) {
  colDef(header = z_with_tt("FPts", "Fantasy points"),
         maxWidth = maxW,
         format = colFormat(digits = digs),
         filterable = filt,
         defaultSortOrder = "desc",
         class = function(value) if(borderL == T) {"border-left-grey"},
         style = function(value) { 
           normalized <- (value) / (max(seasons$FPts,na.rm=T))
           color <- ifelse(value > 0, z_avg_pal(ifelse(is.na(normalized), 0, normalized)), RBcolor)
           if(col == T) {list(background = color)}
         }
  )
}

z_seasonDef <- function(name = "Yr", maxW = 45, filt = F) {
  colDef(name=name,
         format = colFormat(percent = F),
         maxWidth = maxW,
         defaultSortOrder = "desc",
         align = 'center',
         filterable = filt)
}

z_nflDef <- colDef(minWidth = 50, align = 'right')

z_byeDef <- colDef(minWidth = 50, align = 'right')

z_gDef <- function(minW = 40, foot = F, proj = F) {
  colDef(header = z_with_tt("G", "Games Played"),
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )}

z_ageDef <- colDef(minWidth = 50, align = 'right', defaultSortOrder = "asc", sortNALast = T)

#not used
z_smallColDef <- colDef(maxWidth = 100, align = 'left')

z_weekDef <- colDef("Wk",maxWidth = 44, align = 'center', defaultSortOrder = "desc")

z_opDef <- colDef(header = z_with_tt("Op", "Opponent"), maxWidth = 60, align = 'right')
z_oprkDef <- colDef(header = z_with_tt("OpRk", "Opponent Rankings vs. Fantasy Posision"), maxWidth = 60, align = 'center', style = function(value) {
  color <- ifelse(value <= 10, 'firebrick',
                  ifelse(value <= 20, 'black', 'green'))
  list(color = color)})

#box score stats reactable formats ----
#passing stats conditional formatting
z_pacmpDef <- function(borderL = T, proj = F) {
  colDef(header = z_with_tt("Cmp", "Passing Completions"),
         filterable = F,
         minWidth = smallboxwidth + 8,
         align = 'right', 
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         class = function(value) if(borderL == T) {"border-left-grey"},
         defaultSortOrder = "desc")
}

z_paattDef <- function(proj = F) {
  colDef(header = z_with_tt("Att", "Passing Attempts"),
         filterable = F,
         minWidth = smallboxwidth,
         align = 'right',
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         defaultSortOrder = "desc")
}

z_paydDefWk <- colDef(header = z_with_tt("Yd", "Passing Yards\nBold if >=300"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 300, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_paydDefSsn <- colDef(header = z_with_tt("Yd", "Passing Yards\nBold if >=4000"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 4000, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_paydDefNm <- colDef(header = z_with_tt("Yd", "Passing Yards"), filterable = F, minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
z_paydDef <- function(minW = 45, foot = F, borderL = F, proj = F) {
  colDef(header = z_with_tt("Yd", "Passing Yards"),
         filterable = F,
         minWidth = minW,
         class = function(value) if(borderL == T) {"border-left-grey"},
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

z_patdDefWk <- colDef(header = z_with_tt("TD", "Passing TDs\nBold if >=3"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_patdDefSsn <- colDef(header = z_with_tt("TD", "Passing TDs\nBold if >=30"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 30, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_patdDefNm <- colDef(header = z_with_tt("TD", "Passing TDs"), filterable = F, minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
z_patdDef <- function(minW = 45, foot = F, proj = F) {
  colDef(header = z_with_tt("TD", "Passing TDs"),
         filterable = F,
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

z_paintDefWk <- colDef(header = z_with_tt("Int", "Interceptions\nBold if >=3"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'italic', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_paintDefSsn <- colDef(header = z_with_tt("Int", "Interceptions\nBold if >=15"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 15, 'italic', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_paintDefNm <- colDef(header = z_with_tt("Int", "Interceptions"), filterable = F, minWidth = smallboxwidth, align = 'right', defaultSortOrder = "asc")
z_paintDef <- function(minW = 45, foot = F, proj = F) {
  colDef(header = z_with_tt("Int", "Interceptions"),
         filterable = F,
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Rushing stats
z_ruattDefWk <- colDef(header = z_with_tt("Att", "Rushing Attempts\nBold if >=20"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 20, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
z_ruattDefSsn <- colDef(header = z_with_tt("Att", "Rushing Attempts\nBold if >=250"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 250, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
z_ruattDefNm <- colDef(header = z_with_tt("Att", "Rushing Attempts"), filterable = F, minWidth = smallboxwidth, align = 'right', class = "border-left-grey", defaultSortOrder = "desc")
z_ruattDef <- function(minW = 45, foot = F, borderL = T, proj = F) {
  colDef(header = z_with_tt("Att", "Rushing Attempts"),
         filterable = F,
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Rushing yards
z_ruydDefWk <- colDef(header = z_with_tt("Yd", "Rushing Yards\nBold if >=100"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_ruydDefSsn <- colDef(header = z_with_tt("Yd", "Rushing Yards\nBold if >=1000"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_ruydDefNm <- colDef(header = z_with_tt("Yd", "Rushing Yards"), filterable = F, minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
z_ruydDef <- function(minW = 45, foot = F, borderL = F, proj = F) {
  colDef(header = z_with_tt("Yd", "Rushing Yards"),
         filterable = F,
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Rushing td
z_rutdDefWk <- colDef(header = z_with_tt("TD", "Rushing TDs\nBold if >=3"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_rutdDefSsn <- colDef(header = z_with_tt("TD", "Rushing TDs\nBold if >=10"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_rutdDefNm <- colDef(header = z_with_tt("TD", "Rushing TDs"), filterable = F, minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
z_rutdDef <- function(minW = 45, foot = F, borderL = F, proj = F) {
  colDef(header = z_with_tt("TD", "Rushing TDs"),
         filterable = F,
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Rushing fd
z_rufdDefWk <- colDef(header = z_with_tt("FD", "Rushing First Downs\nBold if >=5"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 5, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_rufdDefSsn <- colDef(header = z_with_tt("FD", "Rushing First Downs\nBold if >=50"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 50, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_rufdDefNm <- colDef(header = z_with_tt("FD", "Rushing First Downs"), filterable = F, minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
z_rufdDef <- function(minW = 45, foot = F, borderL = F, disp = T) {
  colDef(header = z_with_tt("FD", "Rushing TDs"),
         class = function(value) if(borderL == T) {"border-left-grey"},
         show = disp,
         filterable = F,
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Targets
z_tarDefWk <- colDef(header = z_with_tt("Tar", "Targets\nBold if >=10"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
z_tarDefSsn <- colDef(header = z_with_tt("Tar", "Targets\nBold if >=100"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, class = "border-left-grey", defaultSortOrder = "desc")
z_tarDefNm <- colDef(header = z_with_tt("Tar", "Targets"), filterable = F, minWidth = smallboxwidth, align = 'right', class = "border-left-grey", defaultSortOrder = "desc")
z_tarDef <- function(minW = 45, foot = F, borderL = F, proj = F) {
  colDef(header = z_with_tt("Tar", "Targets"),
         filterable = F,
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Receptions
z_recDefWk <- colDef(header = z_with_tt("Rec", "Receptions\nBold if >=10"), filterable = F, minWidth = smallboxwidth + 2, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_recDefSsn <- colDef(header = z_with_tt("Rec", "Receptions\nBold if >=100"), filterable = F, minWidth = smallboxwidth + 2, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_recDefNm <- colDef(header = z_with_tt("Rec", "Receptions"), filterable = F, minWidth = smallboxwidth + 2, align = 'right', defaultSortOrder = "desc")
z_recDef <- function(minW = 47, foot = F, borderL = F, proj = F) {
  colDef(header = z_with_tt("Rec", "Receptions"),
         filterable = F,
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#receiving yards
z_reydDefWk <- colDef(header = z_with_tt("Yd", "Receiving Yards\nBold if >=100"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 100, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_reydDefSsn <- colDef(header = z_with_tt("Yd", "Receiving Yards\nBold if >=1000"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_reydDefNm <- colDef(header = z_with_tt("Yd", "Receiving Yards"), filterable = F, minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
z_reydDef <- function(minW = 47, foot = F, borderL = F, proj = F) {
  colDef(header = z_with_tt("Yd", "Receiving Yards"),
         filterable = F,
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#receiving tds
z_retdDefWk <- colDef(header = z_with_tt("TD", "Receiving TDs\nBold if >=3"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 3, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_retdDefSsn <- colDef(header = z_with_tt("TD", "Receiving TDs\nBold if >=10"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 10, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_retdDefNm <- colDef(header = z_with_tt("TD", "Receiving TDs"), filterable = F, minWidth = smallboxwidth, align = 'right', defaultSortOrder = "desc")
z_retdDef <- function(minW = 47, foot = F, borderL = F, proj = F) {
  colDef(header = z_with_tt("TD", "Receiving TDs"),
         filterable = F,
         class = function(value) if(borderL == T) {"border-left-grey"},
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         style = function(value) if(proj == T) {list(fontStyle = 'italic')},
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

#Receiving fd
z_refdDefWk <- colDef(header = z_with_tt("FD", "Receiving First Downs\nBold if >=5"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 5, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_refdDefSsn <- colDef(header = z_with_tt("FD", "Receiving First Downs\nBold if >=50"), filterable = F, minWidth = smallboxwidth, align = 'right', style = function(value) {
  fontWeight <- ifelse(value >= 50, 'bold', 'plain')
  list(fontWeight = fontWeight)}, defaultSortOrder = "desc")
z_refdDefNm <- colDef(header = z_with_tt("FD", "Receiving First Downs"), filterable = F, minWidth = smallboxwidth, align = 'right')
z_refdDef <- function(minW = 47, foot = F, borderL = F, disp = T) {
  colDef(header = z_with_tt("FD", "Receiving First Downs"),
         filterable = F,
         class = function(value) if(borderL == T) {"border-left-grey"},
         show = disp,
         minWidth = minW,
         align = 'right',
         defaultSortOrder = "desc",
         footer = function(values) if(foot == T) {sum(as.numeric(values), na.rm=T)}
  )
}

z_flDef <- colDef(header = z_with_tt("Fl", "Fumbles Lost"), minWidth = smallboxwidth - 5, align = 'right', defaultSortOrder = "desc")

#advanced stats reactable formats----
z_perccolwidth <- 60
z_othcolwidth <- 45
z_blankptwidth <- 52
z_tchDef <- colDef(header = z_with_tt("Tch", "Touches\n(Completions + Carries + Receptions)"),
                 minWidth = z_othcolwidth,
                 align = "right",
                 class = "border-left-grey",
                 defaultSortOrder = "desc",
                 sortNALast = T)
z_oppDef <- colDef(header = z_with_tt("Opp", "Opportunities\n(Passing Attempts + Carries + Targets)"),
                 minWidth = z_othcolwidth + 2,
                 align = "right",
                 defaultSortOrder = "desc",
                 sortNALast = T)
z_fptsPtchDef <- function(proj = F) {
  colDef(header = z_with_tt("Fp/T", "FPts per Touch\n(Completions + Carries + Receptions)"),
                      minWidth = 64,
                      align = "right",
                      format = colFormat(digits = 2),
                      defaultSortOrder = "desc",
                      style = function(value) if(proj == T) {list(fontStyle = 'italic')},
                      sortNALast = T)
}
z_fptsPoppDef <- function(proj = F) {
  colDef(header = z_with_tt("Fp/O", "FPts per Opportunity\n(Passing Attempts + Carries + Targets)"),
                      minWidth = 64,
                      align = "right",
                      format = colFormat(digits = 2),
                      defaultSortOrder = "desc",
                      style = function(value) if(proj == T) {list(fontStyle = 'italic')},
                      sortNALast = T)
}
z_ydptsDef <- colDef(header = z_with_tt("YdPt", "FPts from Yards\n(Passing + Rushing + Receiving)"),
                   minWidth = z_blankptwidth,
                   align = "right",
                   class = "border-left-grey",
                   format = colFormat(digits = 1),
                   defaultSortOrder = "desc",
                   sortNALast = T)
z_tdptsDef <- colDef(header = z_with_tt("TDPt", "FPts from Touchdowns\n(Passing + Rushing + Receiving)"),
                   minWidth = z_blankptwidth,
                   align = "right",
                   defaultSortOrder = "desc",
                   sortNALast = T)
z_fdptsDef <- colDef(header = z_with_tt("FDPt", "FPts from First Downs\n(Rushing + Receiving)"),
                   minWidth = z_blankptwidth,
                   align = "right",
                   defaultSortOrder = "desc",
                   sortNALast = T)
z_ruptsDef <- colDef(header = z_with_tt("RuPt", "FPts from Rushing\n(Yards + TDs + First Downs)"),
                   minWidth = z_blankptwidth,
                   align = "right",
                   class = "border-left-grey",
                   format = colFormat(digits = 1),
                   defaultSortOrder = "desc",
                   sortNALast = T)
z_reptsDef <- colDef(header = z_with_tt("RePt", "FPts from Receiving\n(Yards + TDs + First Downs)"),
                   minWidth = z_blankptwidth,
                   align = "right",
                   format = colFormat(digits = 1),
                   defaultSortOrder = "desc",
                   sortNALast = T)
z_ydptpercDef <- function(proj = F) {
  colDef(header = z_with_tt("YDp%", "Percentage of Total FPts from Yards\n(Passing + Rushing + Receiving)"),
                      minWidth = z_perccolwidth,
                      align = "right",
                      format = colFormat(percent = T, digits = 0),
                      class = "border-left-grey",
                      defaultSortOrder = "desc",
                      style = function(value) if(proj == T) {list(fontStyle = 'italic')},
                      sortNALast = T)
}
z_tdptpercDef <- function(proj = F) {
  colDef(header = z_with_tt("TDp%", "Percentage of Total FPts from Touchdowns\n(Passing + Rushing + Receiving)"),
                      minWidth = z_perccolwidth + 2,
                      align = "right",
                      format = colFormat(percent = T, digits = 0),
                      defaultSortOrder = "desc",
                      style = function(value) if(proj == T) {list(fontStyle = 'italic')},
                      sortNALast = T)
}
z_fdptpercDef <- function(proj = F) { 
  colDef(header = z_with_tt("FDp%", "Percentage of Total FPts from First Downs\n(Rushing + Receiving)"),
                     minWidth = z_perccolwidth + 3,
                     align = "right",
                     format = colFormat(percent = T, digits = 0),
                     defaultSortOrder = "desc",
                     style = function(value) if(proj == T) {list(fontStyle = 'italic')},
                     sortNALast = T)
}
z_ruptpercDef = colDef(header = z_with_tt("RuPt%", "Percentage of Total FPts from Rushing\n(Yards + TDs + First Downs)"),
                     minWidth = z_perccolwidth + 2,
                     align = "right",
                     class = "border-left-grey",
                     format = colFormat(percent = T, digits = 0),
                     defaultSortOrder = "desc",
                     sortNALast = T)
z_reptpercDef = colDef(header = z_with_tt("RePt%", "Percentage of Total FPts from Receiving\n(Yards + TDs + First Downs)"),
                     minWidth = z_perccolwidth + 2,
                     align = "right",
                     format = colFormat(percent = T, digits = 0),
                     defaultSortOrder = "desc",
                     sortNALast = T)

#consistency stats reactable formats----
z_conscolwidth <- 60
z_relsdDef <- colDef(header = z_with_tt("RelSD", "Relative Standard Deviation of Weekly FPts\n(SD / Avg)"),
                   minWidth = z_conscolwidth,
                   align = "right",
                   format = colFormat(digits = 2),
                   defaultSortOrder = "asc",
                   sortNALast = T)
z_avgposrkDef <- colDef(header = z_with_tt("AvgPosRk", "Average Weekly Position Rank by FPts"),
                      minWidth = z_conscolwidth + 10,
                      align = "right",
                      class = "border-left-grey",
                      defaultSortOrder = "asc",
                      sortNALast = T)
z_top5pDef <- colDef(header = z_with_tt("Top5%", "Percentage of Weeks with top 5 Positional Scoring Rank"),
                   minWidth = z_conscolwidth,
                   align = "right",
                   format = colFormat(percent = T, digits = 0),
                   defaultSortOrder = "desc",
                   sortNALast = T)
z_top12pDef <- colDef(header = z_with_tt("Top12%", "Percentage of Weeks with top 12 Positional Scoring Rank"),
                    minWidth = z_conscolwidth,
                    align = "right",
                    format = colFormat(percent = T, digits = 0),
                    defaultSortOrder = "desc",
                    sortNALast = T)
z_top24pDef <- colDef(header = z_with_tt("Top24%", "Percentage of Weeks with top 24 Positional Scoring Rank"),
                    minWidth = z_conscolwidth,
                    align = "right",
                    format = colFormat(percent = T, digits = 0),
                    defaultSortOrder = "desc",
                    sortNALast = T)
z_top36pDef <- colDef(header = z_with_tt("Top36%", "Percentage of Weeks with top 36 Positional Scoring Rank"),
                    minWidth = z_conscolwidth,
                    align = "right",
                    format = colFormat(percent = T, digits = 0),
                    defaultSortOrder = "desc",
                    sortNALast = T)
z_nonstartpDef <- colDef(header = z_with_tt("NonStart%", "Percentage of Weeks outside of top 36 Positional Scoring Rank"),
                       minWidth = 75,
                       align = "right",
                       format = colFormat(percent = T, digits = 0),
                       defaultSortOrder = "desc",
                       sortNALast = T)
z_g10pDef <- colDef(header = z_with_tt(">10%", "Percentage of Weeks scoring >10 FPts"),
                  minWidth = z_conscolwidth,
                  class = "border-left-grey",
                  align = "right",
                  format = colFormat(percent = T,digits = 0),
                  defaultSortOrder = "desc",
                  sortNALast = T)
z_g20pDef <- colDef(header = z_with_tt(">20%", "Percentage of Weeks scoring >20 FPts"),
                  minWidth = z_conscolwidth,
                  align = "right",
                  format = colFormat(percent = T, digits = 0),
                  defaultSortOrder = "desc",
                  sortNALast = T)
z_g30pDef <- colDef(header = z_with_tt(">30%", "Percentage of Weeks scoring >30 FPts"),
                  minWidth = z_conscolwidth,
                  align = "right",
                  format = colFormat(percent = T, digits = 0),
                  defaultSortOrder = "desc",
                  sortNALast = T)

# Source UI files ----
source("dashboardPage.R")
source("loginPage.R")
source("guestUI.R")

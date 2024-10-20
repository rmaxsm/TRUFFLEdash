# Library loads -----

library(tidyverse)
library(readr)
library(readxl)
library(data.table)
library(corrplot)
library(truncnorm)
library(stringr)
library(lpSolve)

# Reading in data from Excels/csvs -----

#optscoring.csv ----
#file of all fantasy players for optimal scoring calculation
trfopt <- as.data.table(read_csv("data/optScoring.csv", col_types = cols()))
krfopt <- as.data.table(read_csv("data/kerfuffle/kerfuffle_optScoring.csv", col_types = cols()))
trfopt$League <- "TRUFFLE"
krfopt$League <- "KERFUFFLE"
opt <- rbind(trfopt,krfopt); rm(trfopt, krfopt)

#fix
opt$FPts[is.na(opt$FPts)] <- 0

#final table of all optimal lineups by team by week
optlineups <- data <- data.table(
  Season = numeric(),
  Week = numeric(),
  TRUFFLE = character(),
  Pos = character(),
  Player = character(),
  FPts = numeric(),
  League = character()
)

#vector of all truffle teams for loop iteration
TRF <- c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW")
KRF <- c( "ABT","CLC","CPC","DDD","LBC","LC", "MB","NBB","PCP","PP","RR","SBS")
all <- c(TRF,KRF)

#linear program optiimzation loop
for (t in 1:24) {
  for (w in 1:6) {
    #filter optfile down to input data by week & team within loop
    inputdata <- opt[TRUFFLE == all[t] & Week == w]
    
    #linear program definition
    #constraints
    constraints <- rbind(inputdata$Pos == "QB", inputdata$Pos == "QB",
                         inputdata$Pos == "RB", inputdata$Pos == "RB",
                         inputdata$Pos == "WR", inputdata$Pos == "WR",
                         inputdata$Pos == "TE", inputdata$Pos == "TE",
                         inputdata$Pos == "DST",
                         rep(1,nrow(inputdata)))
    #limits
    rhs <- c(1,2,
             2,5,
             2,5,
             1,4,
             1,
             10)
    
    #directions
    dir <- c(">=","<=",
             ">=","<=",
             ">=","<=",
             ">=","<=",
             "=",
             "=")
    
    #define objective function values & run optimization
    obj <- inputdata$FPts
    optimum <- lp("max",obj,constraints,dir,rhs,all.bin = TRUE)
    bestsol <- optimum$solution
    
    #index optimal lineup and order positions
    solution <- inputdata[bestsol > 0, .(Season, Week, TRUFFLE, Pos, Player, FPts, League)]
    positionorder <- c("QB","RB","WR","TE","DST")
    
    optlineups <- rbind(optlineups, solution[order(match(solution$Pos, positionorder)),])
    # Check the value of objective function at optimal point
    print(paste("Total Points: ", optimum$objval, sep=""))
    
  }
}

# aggregate optimal lineups by team by week
optweeks <- optlineups[,
                         .(
                           OptTotal = round(sum(FPts),1)
                         ),
                         by = .(League, Season, Week, TRUFFLE)][order(-Season, Week)]

# aggregate optimal lineups by team for the season
optszn <- optweeks[,
                   .(G = .N,
                     ptslogs = list(OptTotal),
                     Avg = round(mean(OptTotal),1),
                     OptTotal = round(sum(OptTotal),1)
                   ),
                   by = .(League, Season, TRUFFLE)][order(-Season, -OptTotal, -Avg)]

# join in actual scoring data and create final columns
optszn <- merge(x = optszn, y = teamsfantasy[, .(Season, TRUFFLE, Total)], by = c("Season", "TRUFFLE"))
optszn$PtsMissed <- optszn$Total - optszn$OptTotal
optszn$`%Opt` <- round(optszn$Total / optszn$OptTotal, 3) * 100

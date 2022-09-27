#### detail expansion ----

testo <- fantasy[Pos != "DST",
                           .(G = .N,
                             PaYd = sum(PaYd, na.rm=T),
                             PaTD = sum(PaTD, na.rm=T),
                             PaInt = sum(PaInt, na.rm=T),
                             RuYd = sum(RuYd, na.rm=T),
                             RuTD = sum(RuTD, na.rm=T),
                             RuFD = sum(RuFD, na.rm=T),
                             Rec = sum(Rec, na.rm=T),
                             ReYd = sum(ReYd, na.rm=T),
                             ReTD = sum(ReTD, na.rm=T),
                             ReFD = sum(ReFD, na.rm=T),
                             Avg = round(sum(FPts, na.rm=T)/.N, 2),
                             FPts = sum(FPts, na.rm=T)),
                           by = .(Pos, Player)]

trufflecareerstatsbyteam <- fantasy[Pos != "DST",
                 .(Seasons = list(unique(Season)),
                   G = .N,
                   PaYd = sum(PaYd, na.rm=T),
                   PaTD = sum(PaTD, na.rm=T),
                   PaInt = sum(PaInt, na.rm=T),
                   RuYd = sum(RuYd, na.rm=T),
                   RuTD = sum(RuTD, na.rm=T),
                   RuFD = sum(RuFD, na.rm=T),
                   Rec = sum(Rec, na.rm=T),
                   ReYd = sum(ReYd, na.rm=T),
                   ReTD = sum(ReTD, na.rm=T),
                   ReFD = sum(ReFD, na.rm=T),
                   Avg = round(sum(FPts, na.rm=T)/.N, 2),
                   FPts = sum(FPts, na.rm=T)),
                 by = .(Pos, Player, TRUFFLE)][order(-FPts)]


df <- testo[Player %in% c("D'Andre Swift", "Davante Adams")][order(-FPts)]

reactable(df,
          pagination = F,
          height = 'auto',
          filterable = F,
          highlight = T,
          #borderless = T,
          compact = T,
          columns = list(
            #Season = colDef(aggregate = "unique"),
            Pos = posDef(maxW = 75, filt = F),
            Player = playerDef(minW=200),
            G = gDef(),
            PaYd = paydDef(borderL = T),
            PaTD = patdDef(),
            PaInt = paintDef(),
            RuYd = ruydDef(borderL = T),
            RuTD = rutdDef(),
            RuFD = patdDef(),
            Rec = recDef(borderL = T),
            ReYd = reydDef(),
            ReTD = retdDef(),
            ReFD = refdDef(),
            Avg = avgDef(),
            FPts = fptsDef()
          ),
          details = function(index) {
            poi <- df$Player[index]
            reactable(details[Player == poi][, -c("Pos", "Player")],
                      columns = list(
                        TRUFFLE = trfDef(maxW = 95, filt = F),
                        Seasons = colDef(minWidth = 220),
                        G = gDef(),
                        PaYd = paydDef(borderL = T),
                        PaTD = patdDef(),
                        PaInt = paintDef(),
                        RuYd = ruydDef(borderL = T),
                        RuTD = rutdDef(),
                        RuFD = patdDef(),
                        Rec = recDef(borderL = T),
                        ReYd = reydDef(),
                        ReTD = retdDef(),
                        ReFD = refdDef(),
                        Avg = avgDef(),
                        FPts = fptsDef()
                      ))
            
          }
)

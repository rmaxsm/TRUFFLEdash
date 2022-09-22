#### Radarplots ----

library(fmsb)

radarplot <- weekly[,
                     .(FPts = mean(FPts, na.rm = T),
                       Touches = mean(PaCmp, na.rm = T) + mean(RuAtt, na.rm = T) + mean(Rec, na.rm = T),
                       Yd = mean(PaYd, na.rm = T) + mean(RuYd, na.rm = T) + mean(ReYd, na.rm = T),
                       TD = mean(PaTD, na.rm = T) + mean(RuTD, na.rm = T) + mean(ReTD, na.rm = T),
                       FD = mean(RuFD, na.rm = T) + mean(ReFD, na.rm = T)
                     ),
                     by = .(Season, Pos, Player)]

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

fullradar <- rbind(radarplotmax, radarplotmin, radarplot)

radarchart(fullradar[(Season == 2022 & Pos == "WR") & (Player == "MAX" | Player  == "MIN" | Player == "Jaylen Waddle" | Player == "Tyreek Hill")][, 4:8],
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 )



#### TRUFFLE team history ----
radarchart2 <- function(df, axistype=0, seg=4, pty=16, pcol=1:8, plty=1:6, plwd=1,
                        pdensity=NULL, pangle=45, pfcol=NA, cglty=3, cglwd=1,
                        cglcol="navy", axislabcol="blue", title="", maxmin=TRUE,
                        na.itp=TRUE, centerzero=FALSE, vlabels=NULL, vlcex=NULL,
                        caxislabels=NULL, calcex=NULL,
                        paxislabels=NULL, palcex=NULL, ...) {
  if (!is.data.frame(df)) { cat("The data must be given as dataframe.\n"); return() }
  if ((n <- length(df))<3) { cat("The number of variables must be 3 or more.\n"); return() }
  if (maxmin==FALSE) { # when the dataframe does not include max and min as the top 2 rows.
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type="n", frame.plot=FALSE, axes=FALSE, 
       xlab="", ylab="", main=title, asp=1, ...) # define x-y coordinates without any plot
  theta <- seq(90, 450, length=n+1)*pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) { # complementary guide lines, dotted navy line by default
    polygon(xx*(i+CGap)/(seg+CGap), yy*(i+CGap)/(seg+CGap), lty=cglty, lwd=cglwd, border=cglcol)
    if (axistype==1|axistype==3) CAXISLABELS <- paste(i/seg*100,"(%)")
    if (axistype==4|axistype==5) CAXISLABELS <- sprintf("%3.2f",i/seg)
    if (!is.null(caxislabels)&(i<length(caxislabels))) CAXISLABELS <- caxislabels[i+1]
    if (axistype==1|axistype==3|axistype==4|axistype==5) {
      if (is.null(calcex)) text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol) else
        text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol, cex=calcex)
    }
  }
  if (centerzero) {
    arrows(0, 0, xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
  }
  else {
    arrows(xx/(seg+CGap), yy/(seg+CGap), xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
  }
  PAXISLABELS <- df[1,1:n]
  if (!is.null(paxislabels)) PAXISLABELS <- paxislabels
  if (axistype==2|axistype==3|axistype==5) {
    if (is.null(palcex)) text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol) else
      text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol, cex=palcex)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels)) VLABELS <- vlabels
  if (is.null(vlcex)) text(xx*1.2, yy*1.2, VLABELS) else
    text(xx*1.2, yy*1.2, VLABELS, cex=vlcex)
  series <- length(df[[1]])
  SX <- series-2
  if (length(pty) < SX) { ptys <- rep(pty, SX) } else { ptys <- pty }
  if (length(pcol) < SX) { pcols <- rep(pcol, SX) } else { pcols <- pcol }
  if (length(plty) < SX) { pltys <- rep(plty, SX) } else { pltys <- plty }
  if (length(plwd) < SX) { plwds <- rep(plwd, SX) } else { plwds <- plwd }
  if (length(pdensity) < SX) { pdensities <- rep(pdensity, SX) } else { pdensities <- pdensity }
  if (length(pangle) < SX) { pangles <- rep(pangle, SX)} else { pangles <- pangle }
  if (length(pfcol) < SX) { pfcols <- rep(pfcol, SX) } else { pfcols <- pfcol }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg+CGap)+(df[i,]-df[2,])/(df[1,]-df[2,])*seg/(seg+CGap)
    if (sum(!is.na(df[i,]))<3) { cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n",i,df[i,])) # for too many NA's (1.2.2012)
    } else {
      for (j in 1:n) {
        if (is.na(df[i, j])) { # how to treat NA
          if (na.itp) { # treat NA using interpolation
            left <- ifelse(j>1, j-1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left>1, left-1, n)
            }
            right <- ifelse(j<n, j+1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right<n, right+1, 1)
            }
            xxleft <- xx[left]*CGap/(seg+CGap)+xx[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
            yyleft <- yy[left]*CGap/(seg+CGap)+yy[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
            xxright <- xx[right]*CGap/(seg+CGap)+xx[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
            yyright <- yy[right]*CGap/(seg+CGap)+yy[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft; yytmp <- yyleft;
              xxleft <- xxright; yyleft <- yyright;
              xxright <- xxtmp; yyright <- yytmp;
            }
            xxs[j] <- xx[j]*(yyleft*xxright-yyright*xxleft)/(yy[j]*(xxright-xxleft)-xx[j]*(yyright-yyleft))
            yys[j] <- (yy[j]/xx[j])*xxs[j]
          } else { # treat NA as zero (origin)
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j]*CGap/(seg+CGap)+xx[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
          yys[j] <- yy[j]*CGap/(seg+CGap)+yy[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2], col=pfcols[i-2])
      } else {
        polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2], 
                density=pdensities[i-2], angle=pangles[i-2], col=pfcols[i-2])
      }
      points(xx*scale, yy*scale, pch=ptys[i-2], col=pcols[i-2])
      
      ## Line added to add textvalues to points
      text(xx*scale*1.2, yy*scale*1.2, df[3,], cex = .5)
    }
  }
}

radarchart2(fullradar[(Season == 2022 & Pos == "RB") & (Player == "MAX" | Player  == "MIN" | Player == "D'Andre Swift")][, 4:8],
            #vlabels = "labels",
            vlcex = 0.5,
            cglcol = "grey",
            cglty = 1,
            pcol = c(scales::alpha("orange", 1)),
            pfcol = c(scales::alpha("orange", 0.4)),
            plwd = 2
)

#### revamped player portal ----

output$pptesto <- renderReactable({
  testo <- fantasy[Pos != "DST" & Player == "D'Andre Swift",
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
                   by = .(Player, TRUFFLE)]
  
  reactable(testo,
            filter = T,
            columns = list(
              #Season = colDef(aggregate = "unique"),
              TRUFFLE = trfDef(),
              G = colDef(aggregate = "sum"),
              PaYd = colDef(aggregate = "sum"),
              PaTD = colDef(aggregate = "sum"),
              PaInt = colDef(aggregate = "sum"),
              RuYd = colDef(aggregate = "sum"),
              RuTD = colDef(aggregate = "sum"),
              RuFD = colDef(aggregate = "sum"),
              Rec = colDef(aggregate = "sum"),
              ReYd = colDef(aggregate = "sum"),
              ReTD = colDef(aggregate = "sum"),
              ReFD = colDef(aggregate = "sum"),
              Avg = colDef(aggregate = "mean", format = colFormat(digits = 2)),
              FPts = colDef(aggregate = "sum")
            )
  )
  
  
})

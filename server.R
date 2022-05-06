
shinyServer(function(input, output) {
    
    #sponsor logo
    output$sponsor <- renderImage({
        randsponsor <- sample(1:length(list.files("www/graphics/sponsors")), 1)
        list(
            src = sprintf("www/graphics/sponsors/%s", list.files("www/graphics/sponsors")[randsponsor]),
            alt = "softdrink",
            width = "100%")}, deleteFile = FALSE)
    
    #home page ----
    #home page standings
    output$hometeamsfantasy <- renderReactable({
        reactable(teamsfantasy[Season == input$homeseason, c("TRUFFLE", "Weekly", "Low", "High", "Avg", "Total")],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  height = 400,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      TRUFFLE = trfDef,
                      Low = colDef(maxWidth = 60, align = 'left', format = colFormat(digits=2), style = function(value) {
                          fontWeight <- ifelse(value == min(teamsfantasy$Low), 'bold', 'plain')
                          list(fontWeight = fontWeight)
                      }),
                      High = colDef(maxWidth = 60, align = 'left', format = colFormat(digits=2), style = function(value) {
                          fontWeight <- ifelse(value == max(teamsfantasy$High), 'bold', 'plain')
                          list(fontWeight = fontWeight)
                      }),
                      Weekly = colDef(sortable = F, cell = function(values) {
                          sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(teamsfantasyweekly$FPts))
                      }),
                      Avg = colDef(maxWidth = 75, align = 'left'),
                      Total = colDef(minWidth = 150, align = 'left',
                                     format = colFormat(digits=1),
                                     cell = function(value) {
                                         width <- paste0(value / max(teamsfantasy$Total) * 100, "%")
                                         bar_chart(value, width = width)
                                     }
                      )
                  ))
    })
    
    #home page season leaders
    output$homepointsleaders <- renderReactable({
        reactable(pointsleaders[Season == input$homeseason, c("TRUFFLE", "Pos", "Player", "ptslogs", "Avg", "Total")],
                  height = 400,
                  defaultSortOrder = "desc",
                  filterable = T,
                  showPageInfo = FALSE,
                  defaultPageSize = 10,
                  paginationType = 'jump',
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      TRUFFLE = trfDef,
                      Pos = posDef,
                      Player = colDef(minWidth = 150),
                      ptslogs = colDef(name = "PtsLog",sortable = F, maxWidth = 80, filterable=F, cell = function(values) {
                          sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(weekly$FPts))
                      }),
                      Avg = colDef(maxWidth = 50, align = 'left', filterable = F),
                      Total = colDef(filterable = F, align = 'left',
                                     format = colFormat(digits=1),
                                     cell = function(value) {
                                         width <- paste0(value / max(pointsleaders$Total) * 100, "%")
                                         bar_chart(value, width = width)
                                     }
                      )
                  ))
    })
    
    #home weekly top 5 qb
    output$homeweeklytop5qb <- renderReactable({
        reactable(weeklytop5qb[Season == input$homeseason & Week == input$weeklytop5week][, -c("Season","Week","Pos")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      TRUFFLE = trfDef,
                      Player = colDef(minWidth = 125),
                      FPts = fptsDefweeklynarrownoline
                  ),
                  columnGroups = list(colGroup(name = "QB", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
        )
    })
    
    #home weekly top 5 rb
    output$homeweeklytop5rb <- renderReactable({
        reactable(weeklytop5rb[Season == input$homeseason & Week == input$weeklytop5week][, -c("Season","Week","Pos")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      TRUFFLE = trfDef,
                      Player = colDef(minWidth = 125),
                      FPts = fptsDefweeklynarrownoline
                  ),
                  columnGroups = list(colGroup(name = "RB", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
        )
    })
    
    #home weekly top 5 wr
    output$homeweeklytop5wr <- renderReactable({
        reactable(weeklytop5wr[Season == input$homeseason & Week == input$weeklytop5week][, -c("Season","Week","Pos")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      TRUFFLE = trfDef,
                      Player = colDef(minWidth = 125),
                      FPts = fptsDefweeklynarrownoline
                  ),
                  columnGroups = list(colGroup(name = "WR", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
        )
    })
    
    #home weekly top 5 te
    output$homeweeklytop5te <- renderReactable({
        reactable(weeklytop5te[Season == input$homeseason & Week == input$weeklytop5week][, -c("Season","Week","Pos")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      TRUFFLE = trfDef,
                      Player = colDef(minWidth = 125),
                      FPts = fptsDefweeklynarrownoline
                  ),
                  columnGroups = list(colGroup(name = "TE", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
        )
    })
    
    #team portal ----
    #team portal team info
    output$tmportalname <- renderText({paste(input$tmportaltm)})
    output$tmportalabbrev <- renderText({paste(teams$Abbrev[teams$FullName == input$tmportaltm])})
    output$tmportalowner <- renderText({paste(teams$Owner[teams$FullName == input$tmportaltm])})
    output$tmportallocation <- renderText({paste(teams$Location[teams$FullName == input$tmportaltm])})
    output$tmportaldivision <- renderText({paste(teams$Division[teams$FullName == input$tmportaltm])})
    
    #team portal team logo
    output$tmlogo <- renderImage({list(
        src = str_c("www/",teams$Logo[teams$FullName == input$tmportaltm]),
        alt = "TmLogo",
        width = "100%")}, deleteFile = FALSE)
    
    #team portal overview
    output$tpoverview <- renderReactable({
        reactable(tpoverview[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][, !"TRUFFLE"],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  #height = 500,
                  highlight = T,
                  filterable = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Pos = posDef,
                      Player = playerDef,
                      Age = ageDef,
                      Bye = byeDef,
                      NFL = nflDef,
                      Salary = salaryDefFooter,
                      Contract = contractDefFooter,
                      G = gDef,
                      PosRk = colDef(minWidth = 45, align = "right"),
                      ptslog = colDef(name = "PtsLog", sortable = F, cell = function(values) {
                          sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(weekly$FPts))
                      }),
                      Avg = avgDef,
                      FPts = fptsDefseasons
                  ),
                  columnGroups = list(
                      colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                      colGroup(name = "Fantasy", columns = c("G", "PosRk", "ptslog", "Avg", "FPts"), align = 'left')
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
        )
    })
    
    #team portal contracts
    output$tpcontracts <- renderReactable({
        reactable(contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][, !"TRUFFLE"][order(match(Pos, positionorder), -Salary)],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  #height = 500,
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Pos = posDefFooter,
                      Player = playerDef,
                      Age = ageDef,
                      NFL = nflDef,
                      Salary = salaryDefFooter,
                      Contract = contractDefFooter,
                      `'22` = futurecolDefFooter,
                      `'23` = futurecolDefFooter,
                      `'24` = futurecolDefFooter,
                      `'25` = futurecolDefFooter
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
                  columnGroups = list(
                      colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                      colGroup(name = "Future Seasons", columns = c("'22","'23","'24","'25"), align = 'left')
                  )
        )
    })
    
    #team portal boxscore
    output$tpboxscore <- renderReactable({
        reactable(seasons[Player %in% rosters$Player[rosters$TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]] & Season == max(seasons$Season)][order(match(Pos, positionorder), -FPts)][, !c("Season","NFL", "PosRk", "FL")],
                  pagination = F,
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Pos = posDef,
                      Player = colDef(minWidth = 135),
                      G = gDef,
                      PaCmp = colDef(minWidth = 53,name = "Cmp", align = 'right', class = "border-left"),
                      PaAtt = colDef(minWidth = 53,name = "Att", align = 'right'),
                      PaYd = colDef(minWidth = 53,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 4000, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      PaTD = colDef(minWidth = 53,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 30, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      PaInt = colDef(minWidth = 53,name = "Int", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 15, 'italic', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuAtt = colDef(minWidth = 53,name = "Att", align = 'right', class = "border-left", style = function(value) {
                          fontWeight <- ifelse(value >= 250, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuYd = colDef(minWidth = 53,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuTD = colDef(minWidth = 53,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 10, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuFD = colDef(minWidth = 40,name = "FD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 50, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      Tar = colDef(minWidth = 53,align = 'right', class = "border-left", style = function(value) {
                          fontWeight <- ifelse(value >= 100, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      Rec = colDef(minWidth = 53,align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 100, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReYd = colDef(minWidth = 53,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReTD = colDef(minWidth = 53,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 10, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReFD = colDef(minWidth = 40,name = "FD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 50, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      Avg = avgDef,
                      FPts = fptsDefseasons
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    #team portal advanced
    output$tpadvanced <- renderReactable({
        perccolwidth <- 60
        othcolwidth <- 43
        reactable(advanced[Season == max(weekly$Season) & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(match(Pos, positionorder),-FPts)][, -c("TRUFFLE","Season")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  borderless = T,
                  resizable = T,
                  compact = T,
                  columns = list(
                      Pos = posDef,
                      Player = colDef(minWidth = 120, filterable = T),
                      FPts = fptsDefseasons,
                      Touch = colDef(minWidth = othcolwidth, align = "right", class = "border-left"),
                      Opp = colDef(minWidth = othcolwidth, align = "right"),
                      `FPts/Touch` = colDef(minWidth = 75, align = "right"),
                      `FPts/Opp` = colDef(minWidth = 70, align = "right"),
                      YdPts = colDef(minWidth = smallboxwidth, align = "right", class = "border-left"),
                      TDPts = colDef(minWidth = smallboxwidth, align = "right"),
                      FDPts = colDef(minWidth = smallboxwidth, align = "right"),
                      RuPts = colDef(minWidth = smallboxwidth, align = "right"),
                      RePts = colDef(minWidth = smallboxwidth, align = "right"),
                      `YdPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `TDPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `FDPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `RuPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `RePt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1))
                  ),
                  columnGroups = list(
                      colGroup(name = "Volume / Efficiency", columns = c("Touch","Opp","FPts/Touch","FPts/Opp"), align = 'left'),
                      colGroup(name = "Point Source Breakdown", columns = c("YdPts","TDPts","FDPts","YdPt%","TDPt%","FDPt%","RuPts",
                                                                            "RePts","RuPt%","RePt%"), align = 'left')
                  )
        )
    })
    
    #team portal consistency
    output$tpconsistency <- renderReactable({
        perccolwidth <- 60
        reactable(consistency[Season == max(weekly$Season) & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(match(Pos, positionorder),-Avg)][, -c("TRUFFLE","Season")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  borderless = T,
                  resizable = T,
                  compact = T,
                  columns = list(
                      Pos = posDef,
                      Player = colDef(minWidth = 125, filterable = T),
                      Avg = avgDef,
                      RelSD = colDef(minWidth = perccolwidth, align = "right"),
                      AvgPosRk = colDef(name = "Avg",minWidth = perccolwidth, align = "right", class = "border-left"),
                      `Top5 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top12 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top24 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top36 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `NonStart %` = colDef(minWidth = 70, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>10 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>20 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>30 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1))
                      
                  ),
                  columnGroups = list(
                      colGroup(name = "Fantasy Points", columns = c("Avg","RelSD",">10 %",">20 %",">30 %"), align = 'left'),
                      colGroup(name = "Weekly Position Rank", columns = c("AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %"), align = 'left')
                  )
        )
    })
    
    #team portal fantasy logs
    output$tpfantasylogs <- renderReactable({
        reactable(fantasy[Season == max(weekly$Season) & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(Week, -FPts)][, !c("TRUFFLE", "Season", "NFL", "Avg","FL")],
                  paginationType = "jump", defaultPageSize = 10, showPageInfo = FALSE,
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Week = weekDef,
                      Pos = posDef,
                      Player = colDef(minWidth = 150),
                      Opp = oppDef,
                      OpRk = oprkDef,
                      PaCmp = pacmpDef,
                      PaAtt = paattDef,
                      PaYd = paydDefWk,
                      PaTD = patdDefWk,
                      PaInt = paintDefWk,
                      RuAtt = ruattDefWk,
                      RuYd = ruydDefWk,
                      RuTD = rutdDefWk,
                      RuFD = rufdDefWk,
                      Tar = tarDefWk,
                      Rec = recDefWk,
                      ReYd = reydDefWk,
                      ReTD = retdDefWk,
                      ReFD = refdDefWk,
                      FPts = fptsDefweekly
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    #player portal ----
    #player portal bio
    output$ppbios <- renderReactable({
        reactable(ppbios[Player %in% input$player][order(-Salary)],
                  pagination = F,
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      TRUFFLE = trfDef,
                      Pos = posDef,
                      Player = colDef(minWidth = 225),
                      NFL = colDef(align = 'left'),
                      AgePH = colDef(name = "Age"),
                      DynRk = colDef(align = 'left'),
                      DynPosRk = colDef(align = 'left'),
                      Salary = salaryDef,
                      Contract = contractDef,
                      ptslogs = colDef(name = str_c(max(weekly$Season) , " PtsLog"), align = 'right', maxWidth = 100, sortable = F, cell = function(values) {
                          sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(weekly$FPts))
                      })
                  )
        )
    })
    
    #player portal seasons
    output$ppseasons <- renderReactable({
        reactable(seasons[Player %in% input$player][order(-Season, -FPts)][, !c("NFL", "Pos","FL", "PosRk")],
                  pagination = F,
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Season = seasonDef,
                      Player = colDef(minWidth = 135),
                      G = gDef,
                      PaCmp = colDef(minWidth = 53,name = "Cmp", align = 'right', class = "border-left"),
                      PaAtt = colDef(minWidth = 53,name = "Att", align = 'right'),
                      PaYd = colDef(minWidth = 53,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 4000, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      PaTD = colDef(minWidth = 53,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 30, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      PaInt = colDef(minWidth = 53,name = "Int", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 15, 'italic', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuAtt = colDef(minWidth = 53,name = "Att", align = 'right', class = "border-left", style = function(value) {
                          fontWeight <- ifelse(value >= 250, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuYd = colDef(minWidth = 53,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuTD = colDef(minWidth = 53,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 10, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuFD = colDef(minWidth = 40,name = "FD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 50, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      Tar = colDef(minWidth = 53,align = 'right', class = "border-left", style = function(value) {
                          fontWeight <- ifelse(value >= 100, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      Rec = colDef(minWidth = 53,align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 100, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReYd = colDef(minWidth = 53,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 1000, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReTD = colDef(minWidth = 53,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 10, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReFD = colDef(minWidth = 40,name = "FD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 50, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      Avg = avgDef,
                      FPts = fptsDefseasons
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    #player portal advanced
    output$ppadvanced <- renderReactable({
        perccolwidth <- 60
        othcolwidth <- 43
        reactable(advanced[Player %in% input$player][, -c("TRUFFLE","Pos")][order(-FPts)],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  borderless = T,
                  resizable = T,
                  compact = T,
                  columns = list(
                      Player = colDef(minWidth = 120),
                      FPts = fptsDefseasons,
                      Touch = colDef(minWidth = othcolwidth, align = "right", class = "border-left"),
                      Opp = colDef(minWidth = othcolwidth, align = "right"),
                      `FPts/Touch` = colDef(minWidth = 75, align = "right"),
                      `FPts/Opp` = colDef(minWidth = 70, align = "right"),
                      YdPts = colDef(minWidth = smallboxwidth, align = "right", class = "border-left"),
                      TDPts = colDef(minWidth = smallboxwidth, align = "right"),
                      FDPts = colDef(minWidth = smallboxwidth, align = "right"),
                      RuPts = colDef(minWidth = smallboxwidth, align = "right"),
                      RePts = colDef(minWidth = smallboxwidth, align = "right"),
                      `YdPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `TDPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `FDPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `RuPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `RePt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1))
                  ),
                  columnGroups = list(
                      colGroup(name = "Volume / Efficiency", columns = c("Touch","Opp","FPts/Touch","FPts/Opp"), align = 'left'),
                      colGroup(name = "Point Source Breakdown", columns = c("YdPts","TDPts","FDPts","YdPt%","TDPt%","FDPt%","RuPts",
                                                                            "RePts","RuPt%","RePt%"), align = 'left')
                  )
        )
    })
    
    #player portal consistency
    output$ppconsistency <- renderReactable({
        perccolwidth <- 60
        reactable(consistency[Player %in% input$player][order(-Avg)][, -c("TRUFFLE","Pos")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  borderless = T,
                  resizable = T,
                  compact = T,
                  columns = list(
                      Player = colDef(minWidth = 125),
                      Avg = avgDef,
                      RelSD = colDef(minWidth = perccolwidth, align = "right"),
                      AvgPosRk = colDef(name = "Avg",minWidth = perccolwidth, align = "right", class = "border-left"),
                      `Top5 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top12 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top24 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top36 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `NonStart %` = colDef(minWidth = 70, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>10 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>20 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>30 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1))
                      
                  ),
                  columnGroups = list(
                      colGroup(name = "Fantasy Points", columns = c("Avg","RelSD",">10 %",">20 %",">30 %"), align = 'left'),
                      colGroup(name = "Weekly Position Rank", columns = c("AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %"), align = 'left')
                  )
        )
    })
    
    #player portal weekly logs
    output$ppweekly <- renderReactable({
        reactable(weekly[Player %in% input$player][order(-Week, -FPts)][, !c("Pos", "TRUFFLE", "NFL", "Avg", "FL", "PosRk")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 16,
                  pageSizeOptions = c(10, 16, 50, 100),
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  columns = list(
                      Season = seasonDef,
                      Week = weekDef,
                      Player = colDef(minWidth = 135),
                      Opp = oppDef,
                      OpRk = oprkDef,
                      PaCmp = colDef(minWidth = 50,name = "Cmp", align = 'right', class = "border-left"),
                      PaAtt = colDef(minWidth = 50,name = "Att", align = 'right'),
                      PaYd = colDef(minWidth = 50,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 300, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      PaTD = colDef(minWidth = 50,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 3, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      PaInt = colDef(minWidth = 50,name = "Int", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 3, 'italic', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuAtt = colDef(minWidth = 50,name = "Att", align = 'right', class = "border-left", style = function(value) {
                          fontWeight <- ifelse(value >= 20, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuYd = colDef(minWidth = 50,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 100, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuTD = colDef(minWidth = 50,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 3, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      RuFD = colDef(minWidth = 40,name = "FD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 5, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      Tar = colDef(minWidth = 50,align = 'right', class = "border-left", style = function(value) {
                          fontWeight <- ifelse(value >= 10, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      Rec = colDef(minWidth = 50,align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 10, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReYd = colDef(minWidth = 50,name = "Yd", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 100, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReTD = colDef(minWidth = 50,name = "TD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 3, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      ReFD = colDef(minWidth = 40,name = "FD", align = 'right', style = function(value) {
                          fontWeight <- ifelse(value >= 5, 'bold', 'plain')
                          list(fontWeight = fontWeight)}),
                      FPts = fptsDefweekly
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    #fantasy portal ----
    
    output$truffleanalysis <- renderReactable({
        if (input$totalorperc == "Totals") {
            reactable(truffleanalysis[Season == input$fantasyportalseason, !c("Season")],
                      defaultSortOrder = "desc",
                      pagination = FALSE,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          TRUFFLE = trfDef,
                          FPts = colDef(minWidth = 150, align = 'left',
                                        format = colFormat(digits=1),
                                        cell = function(value) {
                                            width <- paste0(value / max(truffleanalysis$FPts) * 100, "%")
                                            bar_chart(round(value,0), width = width)
                                        }),
                          Ydpts = colDef(class = 'border-left')
                      ),
                      columnGroups = list(
                          colGroup(name = "Positional Breakdown", columns = c("QBpts", "RBpts", "WRpts", "TEpts", "DSTpts"), align = 'left'),
                          colGroup(name = "Point Source Breakdown", columns = c("Ydpts", "TDpts", "FDpts", "TOpts"), align = 'left')
                      )
            )
        } else {
            reactable(truffleanalysisperc[Season == input$fantasyportalseason, !c("Season")],
                      defaultSortOrder = "desc",
                      pagination = FALSE,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      defaultColDef = colDef(format = colFormat(percent = T, digits = 1)),
                      columns = list(
                          TRUFFLE = trfDef,
                          FPts = colDef(minWidth = 150, align = 'left',
                                        format = colFormat(digits=1),
                                        cell = function(value) {
                                            width <- paste0(value / max(truffleanalysis$FPts) * 100, "%")
                                            bar_chart(round(value,0), width = width)
                                        }),
                          Ydpts = colDef(class = 'border-left')
                      ),
                      columnGroups = list(
                          colGroup(name = "Positional Breakdown", columns = c("QBpts", "RBpts", "WRpts", "TEpts", "DSTpts"), align = 'left'),
                          colGroup(name = "Point Source Breakdown", columns = c("Ydpts", "TDpts", "FDpts", "TOpts"), align = 'left')
                      )
            )
        }
        
    })
    
    #statcenter ----
    #stat center boxscore
    output$scboxscore <- renderReactable({
        
        if (input$scavailable == "FA") {
            
            reactable(weekly[Season == input$scseason &
                                 Week %in% seq(input$scweekrange[1],input$scweekrange[2]) &
                                 Pos %in% input$scpositions &
                                 TRUFFLE == "FA"
            ][,
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
                Avg = round(mean(FPts),2),
                FPts = sum(FPts)
              ),
              by = .(TRUFFLE, Pos, Player)][order(-FPts)],
            paginationType = "jump",
            showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
            pageSizeOptions = c(10, 20, 50, 100),
            height = 'auto',
            filterable = F,
            highlight = T,
            borderless = T,
            compact = T,
            columns = list(
                TRUFFLE = trfDeffilt,
                Pos = posDef,
                Player = colDef(minWidth = 125, filterable = T),
                G = gDef,
                PaCmp = pacmpDef,
                PaAtt = paattDef,
                PaYd = paydDefNm,
                PaTD = patdDefNm,
                PaInt = paintDefNm,
                RuAtt = ruattDefNm,
                RuYd = ruydDefNm,
                RuTD = rutdDefNm,
                RuFD = rufdDefNm,
                Tar = tarDefNm,
                Rec = recDefNm,
                ReYd = reydDefNm,
                ReTD = retdDefNm,
                ReFD = refdDefNm,
                Avg = colDef(maxWidth = 65, class = "border-left"),
                FPts = colDef(maxWidth = 65)
            ),
            columnGroups = list(
                colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
            )
            ) } else if (input$scavailable == "Owned") {
                
                reactable(weekly[Season == input$scseason &
                                     Week %in% seq(input$scweekrange[1],input$scweekrange[2]) &
                                     Pos %in% input$scpositions &
                                     TRUFFLE %in% c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW")
                ][,
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
                    Avg = round(mean(FPts),2),
                    FPts = sum(FPts)
                  ),
                  by = .(TRUFFLE, Pos, Player)][order(-FPts)],
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                pageSizeOptions = c(10, 20, 50, 100),
                height = 'auto',
                filterable = F,
                highlight = T,
                borderless = T,
                compact = T,
                columns = list(
                    TRUFFLE = trfDeffilt,
                    Pos = posDef,
                    Player = colDef(minWidth = 125, filterable = T),
                    G = gDef,
                    PaCmp = pacmpDef,
                    PaAtt = paattDef,
                    PaYd = paydDefNm,
                    PaTD = patdDefNm,
                    PaInt = paintDefNm,
                    RuAtt = ruattDefNm,
                    RuYd = ruydDefNm,
                    RuTD = rutdDefNm,
                    RuFD = rufdDefNm,
                    Tar = tarDefNm,
                    Rec = recDefNm,
                    ReYd = reydDefNm,
                    ReTD = retdDefNm,
                    ReFD = refdDefNm,
                    Avg = colDef(maxWidth = 65, class = "border-left"),
                    FPts = colDef(maxWidth = 65)
                ),
                columnGroups = list(
                    colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                    colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                    colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                )
                )
            } else {
                
                reactable(weekly[Season == input$scseason &
                                     Week %in% seq(input$scweekrange[1],input$scweekrange[2]) &
                                     Pos %in% input$scpositions
                ][,
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
                    Avg = round(mean(FPts),2),
                    FPts = sum(FPts)
                  ),
                  by = .(TRUFFLE, Pos, Player)][order(-FPts)],
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                pageSizeOptions = c(10, 20, 50, 100),
                height = 'auto',
                filterable = F,
                highlight = T,
                borderless = T,
                compact = T,
                columns = list(
                    TRUFFLE = trfDeffilt,
                    Pos = posDef,
                    Player = colDef(minWidth = 125, filterable = T),
                    G = gDef,
                    PaCmp = pacmpDef,
                    PaAtt = paattDef,
                    PaYd = paydDefNm,
                    PaTD = patdDefNm,
                    PaInt = paintDefNm,
                    RuAtt = ruattDefNm,
                    RuYd = ruydDefNm,
                    RuTD = rutdDefNm,
                    RuFD = rufdDefNm,
                    Tar = tarDefNm,
                    Rec = recDefNm,
                    ReYd = reydDefNm,
                    ReTD = retdDefNm,
                    ReFD = refdDefNm,
                    Avg = colDef(maxWidth = 65, class = "border-left"),
                    FPts = colDef(maxWidth = 65)
                ),
                columnGroups = list(
                    colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                    colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                    colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                )
                )
            }
        
    })
    
    #stat center advanced
    output$scadvanced <- renderReactable({
        perccolwidth <- 60
        othcolwidth <- 43
        reactable(weekly[Player %in% currentseason$Player[currentseason$FPts > input$scpointsmin]][Season == input$scseason &
                                                                                                       Week %in% seq(input$scweekrange[1],input$scweekrange[2]) &
                                                                                                       Pos %in% input$scpositions][,
                                                                                                                                   .(FPts = sum(FPts),
                                                                                                                                     YdPts = round(.04*sum(PaYd) + .1*(sum(RuYd) + sum(ReYd)),1),
                                                                                                                                     TDPts = 4*sum(PaTD) + 6*(sum(RuTD) + sum(ReTD)),
                                                                                                                                     FDPts = sum(RuFD) + sum(ReFD),
                                                                                                                                     RuPts = .1*sum(RuYd) + 6*sum(RuTD) + sum(RuFD),
                                                                                                                                     RePts = .1*sum(ReYd) + 6*sum(ReTD) + sum(ReFD),
                                                                                                                                     Touch = sum(PaCmp + RuAtt + Rec),
                                                                                                                                     Opp = sum(PaAtt + RuAtt + Tar)
                                                                                                                                   ),
                                                                                                                                   by = .(TRUFFLE,Pos,Player)][, `:=`(`YdPt%` = YdPts / FPts,
                                                                                                                                                                      `TDPt%` = TDPts / FPts,
                                                                                                                                                                      `FDPt%` = FDPts / FPts,
                                                                                                                                                                      `RuPt%` = RuPts / FPts,
                                                                                                                                                                      `RePt%` = RePts / FPts,
                                                                                                                                                                      `FPts/Touch` = round(FPts/Touch, 3),
                                                                                                                                                                      `FPts/Opp` = round(FPts/Opp, 3)
                                                                                                                                   )][order(-FPts)][, c("TRUFFLE","Pos","Player","Touch","Opp","FPts/Touch","FPts/Opp","YdPts","TDPts","FDPts","YdPt%","TDPt%","FDPt%","RuPts",
                                                                                                                                                        "RePts","RuPt%","RePt%")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  borderless = T,
                  resizable = T,
                  compact = T,
                  columns = list(
                      TRUFFLE = trfDeffilt,
                      Pos = posDef,
                      Player = colDef(minWidth = 120, filterable = T),
                      Touch = colDef(minWidth = othcolwidth, align = "right", class = "border-left"),
                      Opp = colDef(minWidth = othcolwidth, align = "right"),
                      `FPts/Touch` = colDef(minWidth = 75, align = "right"),
                      `FPts/Opp` = colDef(minWidth = 70, align = "right"),
                      YdPts = colDef(minWidth = smallboxwidth, align = "right", class = "border-left"),
                      TDPts = colDef(minWidth = smallboxwidth, align = "right"),
                      FDPts = colDef(minWidth = smallboxwidth, align = "right"),
                      RuPts = colDef(minWidth = smallboxwidth, align = "right"),
                      RePts = colDef(minWidth = smallboxwidth, align = "right"),
                      `YdPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `TDPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `FDPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `RuPt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `RePt%` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1))
                  ),
                  columnGroups = list(
                      colGroup(name = "Volume / Efficiency", columns = c("Touch","Opp","FPts/Touch","FPts/Opp"), align = 'left'),
                      colGroup(name = "Point Source Breakdown", columns = c("YdPts","TDPts","FDPts","YdPt%","TDPt%","FDPt%","RuPts",
                                                                            "RePts","RuPt%","RePt%"), align = 'left')
                  )
        )
    })
    
    #stat center consistency
    output$scconsistency <- renderReactable({
        perccolwidth <- 60
        reactable(consistencystart[Player %in% currentseason$Player[currentseason$FPts > input$scpointsmin]][Season == input$scseason &
                                                                                                                 Week %in% seq(input$scweekrange[1],input$scweekrange[2]) &
                                                                                                                 Pos %in% input$scpositions][, `:=` (
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
                                                                                                                    by = .(TRUFFLE, Pos, Player)][order(-Avg)][, c("TRUFFLE","Pos","Player","Avg","RelSD",">10 %",">20 %",">30 %","AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  borderless = T,
                  resizable = T,
                  compact = T,
                  columns = list(
                      TRUFFLE = trfDeffilt,
                      Pos = posDef,
                      Player = colDef(minWidth = 125, filterable = T),
                      Avg = colDef(minWidth = perccolwidth, align = "right", class = "border-left"),
                      RelSD = colDef(minWidth = perccolwidth, align = "right"),
                      AvgPosRk = colDef(name = "Avg",minWidth = perccolwidth, align = "right", class = "border-left"),
                      `Top5 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top12 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top24 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `Top36 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `NonStart %` = colDef(minWidth = 70, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>10 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>20 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1)),
                      `>30 %` = colDef(minWidth = perccolwidth, align = "right", format = colFormat(percent = T, digits = 1))
                      
                  ),
                  columnGroups = list(
                      colGroup(name = "Fantasy Points", columns = c("Avg","RelSD",">10 %",">20 %",">30 %"), align = 'left'),
                      colGroup(name = "Weekly Position Rank", columns = c("AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %"), align = 'left')
                  )
        )
    })
    
    #trademachine ----
    output$tmtm1 <- renderReactable({
        reactable(tpoverview[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm1]][, !"TRUFFLE"],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  #height = 500,
                  highlight = T,
                  filterable = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Pos = posDef,
                      Player = playerDef,
                      Age = ageDef,
                      Bye = byeDef,
                      NFL = nflDef,
                      Salary = salaryDefFooter,
                      Contract = contractDefFooter,
                      G = gDef,
                      PosRk = colDef(minWidth = 45, align = "right"),
                      ptslog = colDef(name = "PtsLog", sortable = F, cell = function(values) {
                          sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(weekly$FPts))
                      }),
                      Avg = avgDef,
                      FPts = fptsDefseasons
                  ),
                  columnGroups = list(
                      colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                      colGroup(name = "Fantasy", columns = c("G", "PosRk", "ptslog", "Avg", "FPts"), align = 'left')
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
        )
        
        #tmp1 <- rosters[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm1]]
    })
    
    output$tmtm2 <- renderReactable({
        reactable(tpoverview[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm2]][, !"TRUFFLE"],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  #height = 500,
                  highlight = T,
                  filterable = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Pos = posDef,
                      Player = playerDef,
                      Age = ageDef,
                      Bye = byeDef,
                      NFL = nflDef,
                      Salary = salaryDefFooter,
                      Contract = contractDefFooter,
                      G = gDef,
                      PosRk = colDef(minWidth = 45, align = "right"),
                      ptslog = colDef(name = "PtsLog", sortable = F, cell = function(values) {
                          sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(weekly$FPts))
                      }),
                      Avg = avgDef,
                      FPts = fptsDefseasons
                  ),
                  columnGroups = list(
                      colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                      colGroup(name = "Fantasy", columns = c("G", "PosRk", "ptslog", "Avg", "FPts"), align = 'left')
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
        )
        
        #tmp2 <- rosters[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm2]]
    })
    
    output$tmpls1 <- renderReactable({
        reactable(contracts[Player %in% input$tmtm1players][, !"TRUFFLE"][order(match(Pos, positionorder), -Salary)],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  #height = 500,
                  filterable = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Pos = posDefFooterNofilt,
                      Player = playerDefNofilt,
                      Age = ageDef,
                      NFL = nflDef,
                      Salary = salaryDefFooter,
                      Contract = contractDefFooter,
                      `'22` = futurecolDefFooter,
                      `'23` = futurecolDefFooter,
                      `'24` = futurecolDefFooter,
                      `'25` = futurecolDefFooter
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
                  columnGroups = list(
                      colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                      colGroup(name = "Future Seasons", columns = c("'22","'23","'24","'25"), align = 'left')
                  )
        )
    })
    
    output$tmpls2 <- renderReactable({
        reactable(contracts[Player %in% input$tmtm2players][, !"TRUFFLE"][order(match(Pos, positionorder), -Salary)],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  #height = 500,
                  filterable = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Pos = posDefFooterNofilt,
                      Player = playerDefNofilt,
                      Age = ageDef,
                      NFL = nflDef,
                      Salary = salaryDefFooter,
                      Contract = contractDefFooter,
                      `'22` = futurecolDefFooter,
                      `'23` = futurecolDefFooter,
                      `'24` = futurecolDefFooter,
                      `'25` = futurecolDefFooter
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
                  columnGroups = list(
                      colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                      colGroup(name = "Future Seasons", columns = c("'22","'23","'24","'25"), align = 'left')
                  )
        )
    })
    
    #capcorner ----
    #capcornercontracts
    output$capcornercontracts <- renderReactable({
        reactable(contracts[order(-Salary)],
                  defaultSortOrder = "desc",
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      TRUFFLE = trfDef,
                      Pos = posDef,
                      Player = playerDef,
                      Age = ageDef,
                      NFL = nflDef,
                      Salary = salaryDef,
                      Contract = contractDef,
                      `'22` = futurecolDef,
                      `'23` = futurecolDef,
                      `'24` = futurecolDef,
                      `'25` = futurecolDef
                  ),
                  columnGroups = list(
                      colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                      colGroup(name = "Future Seasons", columns = c("'22","'23","'24","'25"), align = 'left')
                  )
        )
    })
    
    #plot1
    output$plot1 <- renderPlotly({
        plot_ly(
            data = rosterbreakdown,
            y = ~TRUFFLE,
            x = ~Salary,
            #text = ~TeamSalary,
            hovertemplate = ~paste0('<b>',Player, '</b>',
                                    '<br>$', Salary, ', ', Contract, 'yr'),
            marker = list(
                line = list(
                    color = 'white',
                    width = 1
                )),
            color = ~Pos,
            type = "bar",
            colors = pal
            #textposition = 'outside'
        ) %>% layout(
            font = ft,
            title = list(text = "<b>Salary Cap Breakdown by Team,</b> by Player and Position", x=.05),
            xaxis = list(categoryorder = "total ascending", title = "<b>Salary</b>"),
            yaxis = list(categoryorder = "total ascending", title = ""),
            barmode = "stack",
            font = list(color = 'black')
        ) %>% config(displayModeBar = FALSE) %>% 
            add_annotations(data = rosterbreakdown %>% select(TRUFFLE, TeamSalary) %>% unique(),
                            x = 510,
                            y = ~TRUFFLE,
                            text = ~TeamSalary,
                            showarrow = FALSE)
    })
    
    #recordbooks ----
    #record books fantasy points
    output$recordfpts <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplfpts,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          FPts = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Fantasy Points", columns = c("Pos", "Player", "FPts"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmfpts[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          FPts = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Fantasy Points", columns = c("Pos", "Player", "FPts"), align = 'left')
                      ))
        }
    })
    #record books games
    output$recordgames <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplgames,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          Games = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Games Played", columns = c("Pos", "Player", "Games"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmgames[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          Games = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Games Played", columns = c("Pos", "Player", "Games"), align = 'left')
                      ))
        }
    })
    #record books avg
    output$recordavg <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplavg,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          Avg = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Average Fantasy Points", columns = c("Pos", "Player", "Avg"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmavg[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          Avg = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Average Fantasy Points", columns = c("Pos", "Player", "Avg"), align = 'left')
                      ))
        }
    })
    #record books first downs
    output$recordfd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplfd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          FD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "First Downs", columns = c("Pos", "Player", "FD"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmfd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          FD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "First Downs", columns = c("Pos", "Player", "FD"), align = 'left')
                      ))
        }
    })
    #record books passing yards
    output$recordpayd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplpayd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          PaYd = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Passing Yards", columns = c("Pos", "Player", "PaYd"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmpayd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          PaYd = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Passing Yards", columns = c("Pos", "Player", "PaYd"), align = 'left')
                      ))
        }
    })
    #record books passing td
    output$recordpatd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplpatd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          PaTD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Passing Touchdowns", columns = c("Pos", "Player", "PaTD"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmpatd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          PaTD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Passing Touchdowns", columns = c("Pos", "Player", "PaTD"), align = 'left')
                      ))
        }
    })
    #record books interceptions
    output$recordpaint <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplpaint,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          PaInt = colDef(name = "Int", minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Interceptions", columns = c("Pos", "Player", "PaInt"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmpaint[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          PaInt = colDef(name = "Int",minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Interceptions", columns = c("Pos", "Player", "PaInt"), align = 'left')
                      ))
        }
    })
    #record books completions
    output$recordpacmp <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplpacmp,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          PaCmp = colDef(name = "Cmp", minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Completions", columns = c("Pos", "Player", "PaCmp"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmpacmp[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          PaCmp = colDef(name = "Cmp",minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Completions", columns = c("Pos", "Player", "PaCmp"), align = 'left')
                      ))
        }
    })
    #record books rushing yards
    output$recordruyd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplruyd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          RuYd = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Rushing Yards", columns = c("Pos", "Player", "RuYd"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmruyd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          RuYd = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Rushing Yards", columns = c("Pos", "Player", "RuYd"), align = 'left')
                      ))
        }
    })
    #record books rushing touchdowns
    output$recordrutd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplrutd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          RuTD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Rushing Touchdowns", columns = c("Pos", "Player", "RuTD"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmrutd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          RuTD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Rushing Touchdowns", columns = c("Pos", "Player", "RuTD"), align = 'left')
                      ))
        }
    })
    #record books rushing first downs
    output$recordrufd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplrufd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          RuFD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Rushing First Downs", columns = c("Pos", "Player", "RuFD"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmrufd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          RuFD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Rushing First Downs", columns = c("Pos", "Player", "RuFD"), align = 'left')
                      ))
        }
    })
    #record books fumbles
    output$recordfl <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplfl,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          FL = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Fumbles", columns = c("Pos", "Player", "FL"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmfl[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          FL = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Fumbles", columns = c("Pos", "Player", "FL"), align = 'left')
                      ))
        }
    })
    #record books receiving yards
    output$recordreyd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplreyd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          ReYd = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Receiving Yards", columns = c("Pos", "Player", "ReYd"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmreyd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          ReYd = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Receiving Yards", columns = c("Pos", "Player", "ReYd"), align = 'left')
                      ))
        }
    })
    #record books receiving touchdowns
    output$recordretd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplretd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          ReTD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Receiving Touchdowns", columns = c("Pos", "Player", "ReTD"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmretd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          ReTD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Receiving Touchdowns", columns = c("Pos", "Player", "ReTD"), align = 'left')
                      ))
        }
    })
    #record books receiving first downs
    output$recordrefd <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplrefd,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          ReFD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Receiving First Downs", columns = c("Pos", "Player", "ReFD"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmrefd[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          ReFD = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Receiving First Downs", columns = c("Pos", "Player", "ReFD"), align = 'left')
                      ))
        }
    })
    #record books receptions
    output$recordrec <- renderReactable({
        if (input$recordteams == "TRUFFLE") {
            reactable(recordplrec,
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          Rec = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Receptions", columns = c("Pos", "Player", "Rec"), align = 'left')
                      )
            )
        } else {
            reactable(recordtmrec[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                      defaultSortOrder = "desc",
                      filterable = F,
                      sortable = F,
                      showPageInfo = FALSE,
                      paginationType = "simple", defaultPageSize = 5,
                      highlight = T,
                      borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDefnarrownofilt,
                          Player = colDef(minWidth = 140),
                          Rec = colDef(minWidth = 60, align = "left")
                      ),
                      columnGroups = list(colGroup(name = "Receptions", columns = c("Pos", "Player", "Rec"), align = 'left')
                      ))
        }
    })
    
    #awards
    output$historybooksawards <- renderReactable({
        if(input$awardseason == 2020) {
            reactable(
                award2020,
                sortable = FALSE,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 11,
                compact = T,
                columns = list(
                    Season = colDef(show = FALSE),
                    Logo = colDef(name = "", 
                                  align="center", 
                                  minWidth = 30, 
                                  cell = function(value) {
                                      img_src <- knitr::image_uri(value)
                                      image <- img(src = img_src, height = "47px", alt = value)
                                      tagList(
                                          div(style = list(display = "inline-block"), image)
                                      )
                                  }),
                    Award = colDef(
                        # Show species under character names
                        cell = function(value, index) {
                            winner <- award2020$Winner[index]
                            pos <- award2020$Pos[index]
                            trf <- award2020$TRUFFLE[index]
                            div(
                                div(style = list(fontWeight = 600, fontSize=16, color = "#84A4D8"), value),
                                div(style = list(fontSize = 14), paste0(winner, ", ", pos, "  —  ", trf))
                            )
                        }
                    ),
                    Winner = colDef(show = FALSE),
                    Image = colDef(name = "", align="center", minWidth = 50, cell = function(value) {
                        img_src <- knitr::image_uri(value)
                        image <- img(src = img_src, height = "47px", alt = value)
                        tagList(
                            div(style = list(display = "inline-block"), image)
                        )
                    }),
                    Pos = colDef(show = FALSE),
                    TRUFFLE = colDef(show = FALSE)
                )
            ) } else {
                reactable(
                    award2021,
                    sortable = FALSE,
                    showPageInfo = FALSE,
                    paginationType = "simple", defaultPageSize = 11,
                    compact = T,
                    columns = list(
                        Season = colDef(show = FALSE),
                        Logo = colDef(name = "", 
                                      align="center", 
                                      minWidth = 30, 
                                      cell = function(value) {
                                          img_src <- knitr::image_uri(value)
                                          image <- img(src = img_src, height = "47px", alt = value)
                                          tagList(
                                              div(style = list(display = "inline-block"), image)
                                          )
                                      }),
                        Award = colDef(
                            # Show species under character names
                            cell = function(value, index) {
                                winner <- award2021$Winner[index]
                                pos <- award2021$Pos[index]
                                trf <- award2021$TRUFFLE[index]
                                div(
                                    div(style = list(fontWeight = 600, fontSize=16, color = "#84A4D8"), value),
                                    div(style = list(fontSize = 14), paste0(winner, ", ", pos, "  —  ", trf))
                                )
                            }
                        ),
                        Winner = colDef(show = FALSE),
                        Image = colDef(name = "", align="center", minWidth = 50, cell = function(value) {
                            img_src <- knitr::image_uri(value)
                            image <- img(src = img_src, height = "47px", alt = value)
                            tagList(
                                div(style = list(display = "inline-block"), image)
                            )
                        }),
                        Pos = colDef(show = FALSE),
                        TRUFFLE = colDef(show = FALSE)
                    )
                )
            }
    })
    
    output$allt1 <- renderReactable({
        reactable(allt1[Season == input$awardseason][, -"Season"],
                  compact = T,
                  columns = list(
                      Pos = posDefwidenofilt,
                      TRUFFLE = trfDef
                  ),
                  columnGroups = list(colGroup(name = "1st Team", columns = c("Pos", "Winner", "TRUFFLE"), align = 'left')
                  ))
    })
    
    output$allt2 <- renderReactable({
        reactable(allt2[Season == input$awardseason][, -"Season"],
                  compact = T,
                  columns = list(
                      Pos = posDefwidenofilt,
                      TRUFFLE = trfDef
                  ),
                  columnGroups = list(colGroup(name = "2nd Team", columns = c("Pos", "Winner", "TRUFFLE"), align = 'left')
                  ))
    })
    
    #database ----
    #data hub weekly logs
    output$dhweekly <- renderReactable({
        reactable(weekly[order(-FPts)][, !c("TRUFFLE", "NFL", "Avg", "top5dum", "top12dum", "top24dum", "top36dum", "nonStartdum", "PosRk", "lt10dum", "gt10dum", "gt20dum", "gt30dum")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Season = seasonDef,
                      Week = weekDef,
                      Pos = posDef,
                      Player = colDef(minWidth = 150),
                      Opp = oppDef,
                      OpRk = oprkDef,
                      PaCmp = pacmpDef,
                      PaAtt = paattDef,
                      PaYd = paydDefWk,
                      PaTD = patdDefWk,
                      PaInt = paintDefWk,
                      RuAtt = ruattDefWk,
                      RuYd = ruydDefWk,
                      RuTD = rutdDefWk,
                      RuFD = rufdDefWk,
                      Tar = tarDefWk,
                      Rec = recDefWk,
                      ReYd = reydDefWk,
                      ReTD = retdDefWk,
                      ReFD = refdDefWk,
                      FL = flDef,
                      FPts = fptsDefweekly
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    #data hub fantasy logs
    output$dhfantasy <- renderReactable({
        reactable(fantasy[order(-Season, -Week, -FPts)][, !c("Opp", "OpRk", "NFL", "Avg", "PosRk")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Season = seasonDef,
                      Week = weekDef,
                      TRUFFLE = trfDef,
                      Pos = posDef,
                      Player = colDef(minWidth = 150),
                      PaCmp = pacmpDef,
                      PaAtt = paattDef,
                      PaYd = paydDefWk,
                      PaTD = patdDefWk,
                      PaInt = paintDefWk,
                      RuAtt = ruattDefWk,
                      RuYd = ruydDefWk,
                      RuTD = rutdDefWk,
                      RuFD = rufdDefWk,
                      Tar = tarDefWk,
                      Rec = recDefWk,
                      ReYd = reydDefWk,
                      ReTD = retdDefWk,
                      ReFD = refdDefWk,
                      FL = flDef,
                      FPts = fptsDefweekly
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    #datahub seasons
    output$dhseasons <- renderReactable({
        reactable(seasons[order(-FPts)][, !c("TRUFFLE","PosRk")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  resizable = T,
                  columns = list(
                      Season = seasonDef,
                      Pos = posDef,
                      Player = colDef(minWidth = 150),
                      NFL = nflDef,
                      G = gDef,
                      PaCmp = pacmpDef,
                      PaAtt = paattDef,
                      PaYd = paydDefSsn,
                      PaTD = patdDefSsn,
                      PaInt = paintDefSsn,
                      RuAtt = ruattDefSsn,
                      RuYd = ruydDefSsn,
                      RuTD = rutdDefSsn,
                      RuFD = rufdDefSsn,
                      Tar = tarDefSsn,
                      Rec = recDefSsn,
                      ReYd = reydDefSsn,
                      ReTD = retdDefSsn,
                      ReFD = refdDefSsn,
                      FL = flDef,
                      Avg = avgDef,
                      FPts = fptsDefseasons
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    #upcoming draft ----
    output$rd1 <- renderReactable({
        reactable(draft[Season == input$draftseason & Round == 1][, -c("Season", "Round")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  columns = list(
                      `#` = colDef(minWidth = 30, align = "right"),
                      TRF = trfDef,
                      Player = colDef(minWidth = 150, align = "left"),
                      Pos = posDefnarrownofilt,
                      Salary = colDef(minWidth = 60, align = "right")
                  ),
                  columnGroups = list(colGroup(name = "Round 1", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
                  )
        )
    })
    output$rd2 <- renderReactable({
        reactable(draft[Season == input$draftseason & Round == 2][, -c("Season", "Round")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  columns = list(
                      `#` = colDef(minWidth = 30, align = "right"),
                      TRF = trfDef,
                      Player = colDef(minWidth = 150, align = "left"),
                      Pos = posDefnarrownofilt,
                      Salary = colDef(minWidth = 60, align = "right")
                  ),
                  columnGroups = list(colGroup(name = "Round 2", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
                  )
        )
    })
    output$rd3 <- renderReactable({
        reactable(draft[Season == input$draftseason & Round == 3][, -c("Season", "Round")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  borderless = T,
                  compact = T,
                  columns = list(
                      `#` = colDef(minWidth = 30, align = "right"),
                      TRF = trfDef,
                      Player = colDef(minWidth = 150, align = "left"),
                      Pos = posDefnarrownofilt,
                      Salary = colDef(minWidth = 60, align = "right")
                  ),
                  columnGroups = list(colGroup(name = "Round 3", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
                  )
        )
    })
    
})


shinyServer(function(input, output, session) {
  
  #### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      loginPageUI
      
    } else {
      #### Your app's UI code goes here!
      dashboardPageUI
    }
  })
  
  #testing my team function for stuff
  # observe({
  #   updateSelectInput(session, 'tmportaltm', choices = unique(teams$FullName), selected = teams$FullName[teams$Abbrev == globalteam])
  #   })
  
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
                defaultSorted = c("Total"),
                defaultSortOrder = "desc",
                pagination = FALSE,
                height = 420,
                highlight = T,
                #borderless = T,
                compact = T,
                resizable = F,
                columns = list(
                  TRUFFLE = trfDef(filt = FALSE),
                  Weekly = colDef(header = with_tt("Weekly", "Weekly log of team FPts"),
                                  maxWidth = 100,
                                  sortable = F,
                                  cell = function(values) {
                                    sparkline(values,
                                              type = "bar",
                                              chartRangeMin = 0,
                                              chartRangeMax = max(teamsfantasyweekly$FPts))
                                  }),
                  Low = colDef(header = with_tt("Low", "Lowest weekly score"),
                               minWidth = 50,
                               align = 'right',
                               format = colFormat(digits=1),
                               style = function(value) {
                                 fontWeight <- ifelse(value == min(teamsfantasy$Low), 'bold', 'plain')
                                 list(fontWeight = fontWeight)
                               }),
                  High = colDef(header = with_tt("High", "Highest weekly score"),
                                minWidth = 50,
                                align = 'right',
                                format = colFormat(digits=1),
                                style = function(value) {
                                  fontWeight <- ifelse(value == max(teamsfantasy$High), 'bold', 'plain')
                                  list(fontWeight = fontWeight)
                  }),
                  Avg = colDef(header = with_tt("Avg", "Weekly average team FPts"),
                               minWidth = 50,
                               align = 'right'),
                  Total = colDef(header = with_tt("Tot", "Season total team FPts"),
                                 minWidth = 150,
                                 align = 'left',
                                 format = colFormat(digits=0),
                                 cell = function(value) {
                                   width <- paste0(value / max(teamsfantasy$Total) * 100, "%")
                                   bar_chart(value, width = width)
                                 }
                  )
                ))
    })
    
    #home page season leaders
    output$homepointsleaders <- renderReactable({
        reactable(pointsleaders[Season == input$homeseason, c("TRUFFLE", "Pos", "Player", "PosRk", "ptslogs", "Avg", "Total")],
                  height = 420,
                  defaultSorted = c("Total", "Avg"),
                  defaultSortOrder = "desc",
                  filterable = T,
                  showPageInfo = FALSE,
                  defaultPageSize = 10,
                  paginationType = 'simple',
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  resizable = F,
                  columns = list(
                      TRUFFLE = trfDef(),
                      Pos = posDef(),
                      Player = playerDef(minW = 130,
                                         filt = T),
                      PosRk = posRkDef(filt = F),
                      ptslogs = ptsLogDef(),
                      Avg = avgDef(maxW = 48),
                      Total = colDef(header = with_tt("Tot", "Seasonal total FPts"),
                                     maxWidth = 45,
                                     format = colFormat(digits = 0),
                                     filterable = F
                      )
                  )
                  )
    })
    
    #home weekly top 5 qb
    output$homeweeklytop5qb <- renderReactable({
        reactable(weeklytop5qb[Season == input$homeseason & Week == input$weeklytop5week][, -c("Season","Week","Pos")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  defaultPageSize = 10,
                  paginationType = 'simple',
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  resizable = F,
                  columns = list(
                      TRUFFLE = trfDef(filt = FALSE),
                      Player = playerDef(minW = 125, filt = F, sort = F),
                      FPts = fptsWeekDef(maxW = 40, borderL = F)
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
                  defaultPageSize = 10,
                  paginationType = 'simple',
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  resizable = F,
                  columns = list(
                      TRUFFLE = trfDef(filt = FALSE),
                      Player = playerDef(minW = 125, filt = F, sort = F),
                      FPts = fptsWeekDef(maxW = 40, borderL = F)
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
                  defaultPageSize = 10,
                  paginationType = 'simple',
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  resizable = F,
                  columns = list(
                      TRUFFLE = trfDef(filt = FALSE),
                      Player = playerDef(minW = 125, filt = F, sort = F),
                      FPts = fptsWeekDef(maxW = 40, borderL = F)
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
                  defaultPageSize = 10,
                  paginationType = 'simple',
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  resizable = F,
                  columns = list(
                      TRUFFLE = trfDef(filt = FALSE),
                      Player = playerDef(minW = 125, filt = F, sort = F),
                      FPts = fptsWeekDef(maxW = 40, borderL = F)
                  ),
                  columnGroups = list(colGroup(name = "TE", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
        )
    })
    
    #team portal ----
    
    #tpheader
    output$tpheader <- renderReactable({
        reactable(
            teams[teams$FullName == input$tmportaltm, c("FullName", "Abbrev", "Logo", "DivLogo", "RivLogo")],
            sortable = FALSE,
            compact = TRUE,
            columns = list(
                FullName = colDef(name = "Selected Team:",
                                  headerStyle = list(color = "#84A4D8", fontSize = 14, fontWeight = 800),
                                  minWidth = 225,
                                  cell = function(value) {
                                      owner <- teams$Owner[teams$FullName == input$tmportaltm]
                                      col <- teams$Primary[teams$FullName == input$tmportaltm]
                                      div(
                                          div(style = list(fontWeight = 800, fontSize=26, color = col), value),
                                          div(style = list(fontSize = 16), owner)
                                      )
                                  }
                ),
                Abbrev = trfDef(filt = FALSE, name = ""),
                Logo = colDef(name = "",
                              #class = "border-right-grey",
                              align="center", 
                              minWidth = 70, 
                              cell = function(value) {
                                  img_src <- knitr::image_uri(value)
                                  image <- img(src = img_src, height = "70px", alt = value)
                                  tagList(
                                      div(style = list(display = "inline-block"), image)
                                  )
                              }),
                DivLogo = colDef(name = "",
                              class = "border-left-grey",
                              align="center", 
                              minWidth = 70, 
                              cell = function(value) {
                                  img_src <- knitr::image_uri(value)
                                  image <- img(src = img_src, height = "70px", alt = value)
                                  tagList(
                                      div(style = list(display = "inline-block"), image)
                                  )
                              }),
                RivLogo = colDef(name = "",
                                 class = "border-left-grey",
                                 align="center", 
                                 minWidth = 70, 
                                 cell = function(value) {
                                   img_src <- knitr::image_uri(value)
                                   image <- img(src = img_src, height = "60px", alt = value)
                                   tagList(
                                     div(style = list(display = "inline-block"), image)
                                   )
                                 })
            ),
            theme = reactableTheme(
              # Vertically center cells
              cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
            )
        )
    })
    
    #team portal overview
    output$tpoverview <- renderReactable({
      tpoverview <- action_mod(df = tpoverview, team = globalteam)
      selectedteam <- tpoverview[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(match(Pos, positionorder), -Avg)]

        reactable(selectedteam[, .(Action, TRUFFLE, Pos, Player, Age, NFL, Bye, Salary, Contract, G, PosRk, ptslog, Avg, FPts)][, !"TRUFFLE"],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  highlight = T,
                  filterable = T,
                  compact = T,
                  columns = list(
                      Action = colDef(header = with_tt("A", "Action link to add, drop, or trade player"),
                                      sortable = F,
                                      filterable = F,
                                      align="center",
                                      minWidth = 30,
                                      cell = function(value, index) {
                                        action_url <- selectedteam$ActionLink[index]
                                        img_src <- knitr::image_uri(value)
                                        image <- img(src = img_src, height = "15px", alt = value)
                                        tagList(
                                          div(style = list(display = "inline-block"), image)
                                        )
                                        tags$a(href = action_url, target = "_blank", image)
                                      }),
                      Pos = posDef(foot = "Total"),
                      Player = playerDef(filt=T),
                      Age = ageDef,
                      Bye = byeDef,
                      NFL = nflDef,
                      Salary = salaryDefBar(foot = T),
                      Contract = contractDef(foot = T, name = "Yr"),
                      G = gDef(),
                      PosRk = posRkDef(filt = T),
                      ptslog = ptsLogDef(),
                      Avg = avgDef(),
                      FPts = fptsSeasDef()
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
      futurecolDef <- function(maxW = 75, filt = T, foot = F, yr) {
        colDef(header = with_tt(yr, "FA: Free Agent\nPurple: Rookie Extension Value"),
               maxWidth = maxW,
               filterable = filt,
               align = 'right',
               defaultSortOrder = "desc",
               style = function(value, index) {
                 df <- contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][, !c("TRUFFLE")][order(match(Pos, positionorder), -Salary)]
                 ext <- df$Extension[index]
                 col <- ifelse(value == "FA", textred, ifelse(value == ext, rookieextension, tabletextcol))
                 list(color = col)},
               #cell = function(value) {
               #class <- paste0("tag status-", value)
               #htmltools::div(class = class, value)},
               footer = function(values) if(foot == T) {paste0("$", sum(as.numeric(values), na.rm=T))}
        )
      }
      
        reactable(contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][, !c("TRUFFLE")][order(match(Pos, positionorder), -Salary)],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  filterable = T,
                  highlight = T,
                  compact = T,
                  columns = list(
                    Extension = colDef(show = F),
                    Pos = posDef(foot = "Total"),
                    Player = playerDef(filt = T),
                    Age = ageDef,
                    NFL = nflDef,
                    Salary = salaryDefBar(foot = T),
                    Contract = contractDef(name = "Yr", foot = T),
                    `'22` = futurecolDef(yr = "'22", foot = T),
                    `'23` = futurecolDef(yr = "'23", foot = T),
                    `'24` = futurecolDef(yr = "'24", foot = T),
                    `'25` = futurecolDef(yr = "'25", foot = T),
                    `'26` = futurecolDef(yr = "'26", foot = T)
                  ),
                  columnGroups = list(
                    colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                    colGroup(name = "Future Seasons", columns = c("'22","'23","'24","'25","'26"), align = 'left')
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
        )
    })
    
    #team portal boxscore
    output$tpboxscore <- renderReactable({
        reactable(seasons[Player %in% rosters$Player[rosters$TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]] 
                          & Season == max(seasons$Season)][order(match(Pos, positionorder), -FPts)][, !c("Season","NFL", "PosRk", "FL")],
                  pagination = F,
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  compact = T,
                  columns = list(
                      Pos = posDef(),
                      Player = playerDef(minW = 135, filt=T),
                      G = gDef(),
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
                      Avg = avgDef(),
                      FPts = fptsSeasDef()
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
                  pagination = F,
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  compact = T,
                  columns = list(
                      Pos = posDef(),
                      Player = playerDef(minW = 120, filt = T),
                      FPts = fptsSeasDef(),
                      Touch = tchDef,
                      Opp = oppDef,
                      `FPts/Touch` = fptsPtchDef,
                      `FPts/Opp` = fptsPoppDef,
                      YdPts = ydptsDef,
                      TDPts = tdptsDef,
                      FDPts = fdptsDef,
                      RuPts = ruptsDef,
                      RePts = reptsDef,
                      `YdPt%` = ydptpercDef,
                      `TDPt%` = tdptpercDef,
                      `FDPt%` = fdptpercDef,
                      `RuPt%` = ruptpercDef,
                      `RePt%` = reptpercDef
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
                  pagination = F,
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  compact = T,
                  columns = list(
                      Pos = posDef(),
                      Player = playerDef(minW = 125, filt = T),
                      Avg = avgDef(),
                      RelSD = relsdDef,
                      AvgPosRk = avgposrkDef,
                      `Top5 %` = top5pDef,
                      `Top12 %` = top12pDef,
                      `Top24 %` = top24pDef,
                      `Top36 %` = top36pDef,
                      `NonStart %` = nonstartpDef,
                      `>10 %` = g10pDef,
                      `>20 %` = g20pDef,
                      `>30 %` = g30pDef
                  ),
                  columnGroups = list(
                      colGroup(name = "Fantasy Points", columns = c("Avg","RelSD",">10 %",">20 %",">30 %"), align = 'left'),
                      colGroup(name = "Weekly Position Rank", columns = c("AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %"), align = 'left')
                  )
        )
    })
    
    #team portal extradash
    output$tpextradash <- renderReactable({
      reactable(extradashszn[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(match(Pos, positionorder),-TotYd)][, !c("TRUFFLE", "Season")],
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                defaultColDef = colDef(
                  minWidth = 55,
                  align = "right"
                ),
                columns = list(
                  Pos = posDef(),
                  Player = playerDef(filt = T),
                  G = gDef(),
                  `Cmp%` = colDef(header = with_tt("Cmp%", "Completion Percentage"), format = colFormat(percent = T), minWidth = 75),
                  Pa20 = colDef(header = with_tt("20+", "Passing Completions of 20+ Yd"), class = "border-left-grey"),
                  Pa40 = colDef(header = with_tt("40+", "Passing Completions of 40+ Yd")),
                  RuYPC = colDef(header = with_tt("YPC", "Rushing Yards per Carry"), class = "border-left-grey", minWidth = 60),
                  Ru20 = colDef(header = with_tt("20+", "Rushes of 20+ Yd")),
                  Tar = colDef(header = with_tt("Tar", "Targets"), class = "border-left-grey"),
                  `Tar%` = colDef(header = with_tt("Tar%", "Percentage of Team Targets"), minWidth = 70, format = colFormat(suffix = "%")),
                  ReYPC = colDef(header = with_tt("YPC", "Yards per Catch"), minWidth = 60),
                  Re20 = colDef(header = with_tt("20+", "Receptions of 20+ Yd")),
                  Re40 = colDef(header = with_tt("40+", "Receptions of 40+ Yd")),
                  `ReFD%` = colDef(header = with_tt("FD%", "Percentage of Receptions resulting in First Down"), minWidth = 65, format = colFormat(percent = T)),
                  TotYd = colDef(header = with_tt("TotYd", "Total Passing/Rushing/Receiving Yards"), minWidth = 70)
                ),
                columnGroups = list(
                  colGroup(name = "Passing", columns = c("Cmp%","Pa20","Pa40"), align = 'left'),
                  colGroup(name = "Rushing", columns = c("RuYPC", "Ru20"), align = 'left'),
                  colGroup(name = "Receiving", columns = c("Tar", "Tar%", "ReYPC", "Re20", "Re40", "ReFD%"), align = 'left')
                )
      )
    })
    
    #team portal xfpxtd
    output$tpxfpxtd <- renderReactable({
      reactable(espn[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(match(Pos, positionorder),-xFP)][, !c("TRUFFLE", "NFL")],
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                defaultColDef = colDef(
                  minWidth = 50,
                  align = "right"
                ),
                columns = list(
                  Pos = posDef(filt = T),
                  Player = playerDef(minW = 125, filt = T),
                  xFP = colDef(header = with_tt("xFP", "Expected ESPN Fantasy Points")),
                  ActualPts = colDef(header = with_tt("aFP", "Actual ESPN Fantasy Points")),
                  FPDiff = colDef(header = with_tt("Diff", "Difference between the player's total xFP and actual FP")),
                  xTD = colDef(header = with_tt("xTD", "Expected Touchdowns"), class = "border-left-grey"),
                  TD = colDef(header = with_tt("aTD", "Actual Touchdowns")),
                  TDDiff = colDef(header = with_tt("Diff", "Difference between the player's total xTD and actual TD")),
                  Looks = colDef(header = with_tt("Looks", "Carries + Targets")),
                  In5 = colDef(header = with_tt("In5", "Carries inside 5-yard line")),
                  EZ = colDef(header = with_tt("EZ", "End Zone Targets"))
                )
      )
    })
    
    #team portal snap share
    output$tpsnapshare <- renderReactable({
      reactable(snaps[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(match(Pos, positionorder),-`Total Snaps`)][, !c("TRUFFLE", "Team")],
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                defaultColDef = colDef(
                  minWidth = 50,
                  align = "center",
                  format = colFormat(percent = T)
                ),
                columns = list(
                  Pos = posDef(),
                  Player = playerDef(minW = 125, filt = T),
                  `Total Snaps` = colDef(minWidth = 150, format = colFormat(percent = F))
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
                  #borderless = T,
                  compact = T,
                  columns = list(
                      Week = weekDef,
                      Pos = posDef(),
                      Player = playerDef(minW = 150, filt = T),
                      Opp = opDef,
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
                      FPts = fptsWeekDef()
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    #player portal ----
    #player portal bios
    output$ppbios <- renderReactable({
      ppbios <- action_mod(df = ppbios, team = globalteam)
      selectedplayers <- ppbios[Player %in% input$player][order(-Salary)]
      
        reactable(selectedplayers[, .(Action, TRUFFLE, Pos, Player, NFL, AgePH, DynRk, DynPosRk, Salary, Contract, ptslogs)],
                  defaultSorted = c("Salary"),
                  pagination = F,
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  compact = T,
                  columns = list(
                    Action = colDef(header = with_tt("A", "Action link to add, drop, or trade player"),
                                    sortable = F,
                                    filterable = F,
                                    align="center",
                                    minWidth = 30,
                                    cell = function(value, index) {
                                      action_url <- selectedplayers$ActionLink[index]
                                      img_src <- knitr::image_uri(value)
                                      image <- img(src = img_src, height = "15px", alt = value)
                                      tagList(
                                        div(style = list(display = "inline-block"), image)
                                      )
                                      tags$a(href = action_url, target = "_blank", image)
                                    }),
                      TRUFFLE = trfDef(filt = FALSE),
                      Pos = posDef(filt = FALSE),
                      Player = playerDef(minW = 140),
                      NFL = colDef(align = 'left'),
                      AgePH = colDef(name = "Age"),
                      DynRk = colDef(header = with_tt("DynRk", "Fantasy Pros Overall Dynasty Rank"), align = 'left'),
                      DynPosRk = colDef(header = with_tt("DynRk", "Fantasy Pros Positional Dynasty Rank"), align = 'left'),
                      Salary = salaryDefBar(),
                      Contract = contractDef(filt = FALSE, minW = 70),
                      ptslogs = ptsLogDef(maxW = 100)
                  )
        )
    })
    
    #player portal TRUFFLE Career Stats
    output$pptrufflecareerstats <- renderReactable({
      df <- pptrufflecareer[Player %in% input$player][order(-FPts)][, -"Pos"]
      
      reactable(df,
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  #Season = colDef(aggregate = "unique"),
                  Player = playerDef(minW=120),
                  G = gDef(),
                  PaYd = paydDef(borderL = T),
                  PaTD = patdDef(),
                  PaInt = paintDef(),
                  RuYd = ruydDef(borderL = T),
                  RuTD = rutdDef(),
                  RuFD = rufdDef(),
                  ReYd = reydDef(borderL = T),
                  ReTD = retdDef(),
                  ReFD = refdDef(),
                  Avg = avgDef(),
                  FPts = fptsDef()
                ),
                details = function(index) {
                  poi <- df$Player[index]
                  reactable(pptrufflecareerteam[Player == poi][, -c("Pos", "Player")],
                            columns = list(
                              TRUFFLE = trfDef(filt=F),
                              Seasons = colDef(minWidth = 90),
                              G = gDef(),
                              PaYd = paydDef(borderL = T),
                              PaTD = patdDef(),
                              PaInt = paintDef(),
                              RuYd = ruydDef(borderL = T),
                              RuTD = rutdDef(),
                              RuFD = rufdDef(),
                              ReYd = reydDef(borderL = T),
                              ReTD = retdDef(),
                              ReFD = refdDef(),
                              Avg = avgDef(),
                              FPts = fptsDef()
                            ))
                }
      )
      
    })
    
    #player portal Contract History
    output$ppcontracthistory <- renderReactable({
      df <- oldrosters[Player %in% input$player,
                       .(TRUFFLE = TRUFFLE[Season == 2022],
                         Salary = Salary[Season == 2022],
                         Contract = Contract[Season == 2022]
                         ),
                       by = .(Player)][order(-Salary)]

      reactable(df,
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Player = playerDef(minW = 125),
                  TRUFFLE = trfDef(filt = FALSE),
                  Salary = salaryDefNobar(title = "$"),
                  Contract = contractDef(filt = F, name = "Yr")
                ),
                details = function(index) {
                  poi <- df$Player[index]
                  reactable(oldrosters[Player == poi][order(Player, Season)][, c("Season", "TRUFFLE", "Salary", "Contract")],
                            fullWidth = F,
                            columns = list(
                              Season = colDef(name="Season",
                                              maxWidth = 80,
                                              align = 'center',
                                              footer = "Career:"),
                              TRUFFLE = trfDef(filt = F),
                              Salary = salaryDefNobar(foot = T, minW = 80),
                              Contract = colDef(name="Yr",
                                                maxWidth = 45,
                                                align = 'center',
                                                style = function(value) {
                                                  background <- ifelse(value == 1, RBcolor,
                                                                       ifelse(value == 2, TEcolor,
                                                                              ifelse(value == 3, WRcolor, QBcolor)))
                                                  list(background = background)},
                                                footer = function(values) {length(values)})
                            ),
                            defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                            )
                }
      )
    })
    
    #player portal seasons
    output$ppseasons <- renderReactable({
      if(input$ppstatcenterseason == "All") {
        df <- seasons[Season >= 2020]
      } else {
        df <- seasons[Season == as.numeric(input$ppstatcenterseason)]
      }
      
        reactable(df[Player %in% input$player][order(-Season, -FPts)][, !c("NFL", "Pos","FL", "PosRk")],
                  defaultSorted = c("Season", "FPts"),
                  pagination = F,
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  #borderless = T,
                  #compact = T,
                  columns = list(
                      Season = seasonDef(filt = F),
                      Player = playerDef(minW = 135),
                      G = gDef(),
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
                      Avg = avgDef(),
                      FPts = fptsSeasDef()
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
      if(input$ppstatcenterseason == "All") {
        df <- advanced[Season >= 2020]
      } else {
        df <- advanced[Season == as.numeric(input$ppstatcenterseason)]
      }
      
      reactable(df[Player %in% input$player][, -c("TRUFFLE","Pos")][order(-FPts)],
                defaultSorted = c("FPts"),
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                #borderless = T,
                #compact = T,
                columns = list(
                  Season = seasonDef(filt = F),
                  Player = playerDef(minW = 120),
                  FPts = fptsSeasDef(),
                  Touch = tchDef,
                  Opp = oppDef,
                  `FPts/Touch` = fptsPtchDef,
                  `FPts/Opp` = fptsPoppDef,
                  YdPts = ydptsDef,
                  TDPts = tdptsDef,
                  FDPts = fdptsDef,
                  RuPts = ruptsDef,
                  RePts = reptsDef,
                  `YdPt%` = ydptpercDef,
                  `TDPt%` = tdptpercDef,
                  `FDPt%` = fdptpercDef,
                  `RuPt%` = ruptpercDef,
                  `RePt%` = reptpercDef
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
      if(input$ppstatcenterseason == "All") {
        df <- consistency[Season >= 2020]
      } else {
        df <- consistency[Season == as.numeric(input$ppstatcenterseason)]
      }
      reactable(df[Player %in% input$player][order(-Avg)][, -c("TRUFFLE","Pos")],
                defaultSorted = c("Avg"),
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                #borderless = T,
                #compact = T,
                columns = list(
                  Season = seasonDef(filt = F),
                  Player = playerDef(minW = 125),
                  Avg = avgDef(),
                  RelSD = relsdDef,
                  AvgPosRk = avgposrkDef,
                  `Top5 %` = top5pDef,
                  `Top12 %` = top12pDef,
                  `Top24 %` = top24pDef,
                  `Top36 %` = top36pDef,
                  `NonStart %` = nonstartpDef,
                  `>10 %` = g10pDef,
                  `>20 %` = g20pDef,
                  `>30 %` = g30pDef
                ),
                columnGroups = list(
                  colGroup(name = "Fantasy Points", columns = c("Avg","RelSD",">10 %",">20 %",">30 %"), align = 'left'),
                  colGroup(name = "Weekly Position Rank", columns = c("AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %"), align = 'left')
                )
      )
    })
    
    #player portal extra dash
    output$ppextradash <- renderReactable({
      reactable(extradashszn[Player %in% input$player][order(TotYd)][, !c("TRUFFLE", "Pos")],
                pagination = F,
                defaultSorted = c("TotYd"),
                defaultSortOrder = "desc",
                height = 'auto',
                filterable = F,
                highlight = T,
                defaultColDef = colDef(
                  minWidth = 55,
                  align = "right"
                ),
                columns = list(
                  Season = seasonDef(),
                  TRUFFLE = trfDef(),
                  Pos = posDef(),
                  Player = playerDef(filt = F),
                  G = gDef(),
                  `Cmp%` = colDef(header = with_tt("Cmp%", "Completion Percentage"), format = colFormat(percent = T), minWidth = 75),
                  Pa20 = colDef(header = with_tt("20+", "Passing Completions of 20+ Yd"), class = "border-left-grey"),
                  Pa40 = colDef(header = with_tt("40+", "Passing Completions of 40+ Yd")),
                  RuYPC = colDef(header = with_tt("YPC", "Rushing Yards per Carry"), class = "border-left-grey", minWidth = 60),
                  Ru20 = colDef(header = with_tt("20+", "Rushes of 20+ Yd")),
                  Tar = colDef(header = with_tt("Tar", "Targets"), class = "border-left-grey"),
                  `Tar%` = colDef(header = with_tt("Tar%", "Percentage of Team Targets"), minWidth = 70, format = colFormat(suffix = "%")),
                  ReYPC = colDef(header = with_tt("YPC", "Yards per Catch"), minWidth = 60),
                  Re20 = colDef(header = with_tt("20+", "Receptions of 20+ Yd")),
                  Re40 = colDef(header = with_tt("40+", "Receptions of 40+ Yd")),
                  `ReFD%` = colDef(header = with_tt("FD%", "Percentage of Receptions resulting in First Down"), minWidth = 65, format = colFormat(percent = T)),
                  TotYd = colDef(header = with_tt("TotYd", "Total Passing/Rushing/Receiving Yards"), minWidth = 70)
                ),
                columnGroups = list(
                  colGroup(name = "Passing", columns = c("Cmp%","Pa20","Pa40"), align = 'left'),
                  colGroup(name = "Rushing", columns = c("RuYPC", "Ru20"), align = 'left'),
                  colGroup(name = "Receiving", columns = c("Tar", "Tar%", "ReYPC", "Re20", "Re40", "ReFD%"), align = 'left')
                )
      )
    })
    
    #player portal xfpxtd
    output$ppxfpxtd <- renderReactable({
      reactable(espn[Player %in% input$player][order(-xFP)][, !c("TRUFFLE", "Pos", "NFL")],
                pagination = F,
                defaultSorted = c("xFP"),
                defaultSortOrder = "desc",
                height = 'auto',
                filterable = F,
                highlight = T,
                defaultColDef = colDef(
                  minWidth = 50,
                  align = "right"
                ),
                columns = list(
                  Player = playerDef(minW = 125),
                  xFP = colDef(header = with_tt("xFP", "Expected ESPN Fantasy Points")),
                  ActualPts = colDef(header = with_tt("aFP", "Actual ESPN Fantasy Points")),
                  FPDiff = colDef(header = with_tt("Diff", "Difference between the player's total xFP and actual FP")),
                  xTD = colDef(header = with_tt("xTD", "Expected Touchdowns"), class = "border-left-grey"),
                  TD = colDef(header = with_tt("aTD", "Actual Touchdowns")),
                  TDDiff = colDef(header = with_tt("Diff", "Difference between the player's total xTD and actual TD")),
                  Looks = colDef(header = with_tt("Looks", "Carries + Targets")),
                  In5 = colDef(header = with_tt("In5", "Carries inside 5-yard line")),
                  EZ = colDef(header = with_tt("EZ", "End Zone Targets"))
                )
      )
    })
    
    #player portal snap share
    output$ppsnapshare <- renderReactable({
            reactable(snaps[Player %in% input$player][order(-`Total Snaps`)][, !c("TRUFFLE", "Pos", "Team")],
                pagination = F,
                defaultSorted = c("Total Snaps"),
                defaultSortOrder = "desc",
                height = 'auto',
                filterable = F,
                highlight = T,
                defaultColDef = colDef(
                  minWidth = 50,
                  align = "center",
                  format = colFormat(percent = T)
                ),
                columns = list(
                  Player = playerDef(minW = 120),
                  `Total Snaps` = colDef(minWidth = 150, format = colFormat(percent = F))
                )
      )
    })
    
    #player portal radar plot
    output$ppradarplot <- renderPlot({
      margins <- c(0,0,0,0)
      y_lim <- c(-1.1, 1.2)
      x_lim <- c(-1,1)
      
      #check that player has been selected and grab position of selected players(s)
      if(length(input$player >= 1)) {
        selectedposition <- unique(weekly$Pos[weekly$Player %in% input$player])
        
        #only output radar plot if all selected players have same position
        if(length(selectedposition) == 1) {
          y_lim[1] <- -1 - (0.1 * length(input$player))
          #modify data
          df <- as.data.frame(fullradar[(Player %in% input$player | Player == "MAX" | Player == "MIN") & (Season == input$ppradarplotseason & Pos == selectedposition[1])])
          rownames(df) <- df$Player
          df <- df[, c(4:8)]
          colnames(df) <- c("FPts", "Tch", "Yd", "TD", "FD")
          #create the plot
          par(mar= margins )
          radarchart(df,
                     #custom polygon
                     pcol=radchart_line , pfcol= radchart_fill, plwd=3, plty = 1,
                     
                     #custom the grid
                     cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                     
                     #custom labels
                     #custom labels
                     vlcex=1,
                     
                     ylim = y_lim,
                     xlim = x_lim
                     )
          #add legend
          legend(x=-.4, y=-1, legend = rownames(df[-c(1,2),]), bty = "n", pch=20, col=radchart_fill, text.col = "black", cex=1, pt.cex=3)
          
        } else {
          #empty radar chart if multiple positions are selected
          par(mar= margins )
          radarchart(emptyradar,
                     #custom polygon
                     pcol="grey", pfcol= "white", plwd=1, pty = 32,
                     
                     #custom the grid
                     cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                     
                     #custom labels
                     #custom labels
                     vlcex=1,
                     
                     ylim = y_lim,
                     xlim = x_lim)
          
          legend(x=-.4, y=-1, legend = "Multiple positions", bty = "n", text.col = 'black', cex=1, pt.cex=3)
        }
        
      } else {
        #empty radarchart before players are selected
        par(mar= margins )
        radarchart(emptyradar,
                   #custom polygon
                   pcol="grey", pfcol= "white", plwd=1, pty = 32,
                   
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   
                   #custom labels
                   #custom labels
                   vlcex=1,
                   
                   ylim = y_lim,
                   xlim = x_lim)
        
        legend(x=-.4, y=-1, legend = "Select players", bty = "n", text.col = 'black', cex=1, pt.cex=3)
      }
      
    })
    
    #create week by week runchart
    output$ppwbw <- renderPlotly({
      if (length(input$player) == 0) {
        ggplotly(ggplot(weekly,aes(Week,FPts)) + geom_blank() +theme_minimal() + scale_x_continuous(labels=as.character(0:18),breaks=c(0:18)))
      } else {
        df <- weekly[Season == input$ppwbwseason & Player %in% input$player]
        df$Stat <- df[[input$ppwbwstat]]
        ggplotly(ggplot(df, aes(x = Week, y = Stat, color = Player)) +
                   geom_point(size = 4) +
                   geom_line(linetype = "longdash", size = .3) +
                   xlim(0,18) + ylim(0, max(df[[input$ppwbwstat]], na.rm=T)) +
                   ylab(input$ppwbwstat) +
                   theme_minimal() + scale_x_continuous(labels=as.character(0:18),breaks=c(0:18)))
      }
    })
    
    #player portal weekly logs
    output$ppgamelogweekly <- renderReactable({
      if(input$ppgamelogsseason == "All") {
        df <- weekly
      } else {
        df <- weekly[Season == as.numeric(input$ppgamelogsseason)]
      }

        reactable(df[Player %in% input$player][order(Season, Week, -FPts)][, !c("TRUFFLE", "PaCmp", "PaAtt", "NFL", "Avg", "FL", "PosRk")],
                  pagination = F,
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  columns = list(
                      Season = seasonDef(filt = F),
                      Week = weekDef,
                      Pos = posDef(filt = F),
                      Player = playerDef(minW = 135, filt = F),
                      Opp = opDef,
                      OpRk = oprkDef,
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
                      FPts = fptsWeekDef()
                  ),
                  columnGroups = list(
                      colGroup(name = "Passing", columns = c("PaYd", "PaTD", "PaInt"), align = 'left'),
                      colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                      colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                  )
        )
    })
    
    output$ppgamelogfantasy <- renderReactable({
      if(input$ppgamelogsseason == "All") {
        df <- fantasy
      } else {
        df <- fantasy[Season == as.numeric(input$ppgamelogsseason)]
      }
      
      reactable(df[Player %in% input$player][order(Season, Week, -FPts)][, !c("PaAtt", "PaCmp", "Opp", "OpRk", "Tar","NFL", "Avg")],       
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Season = seasonDef(),
                  Week = weekDef,
                  TRUFFLE = trfDef(filt = F),
                  Pos = posDef(filt = F),
                  Player = playerDef(minW = 150, filt = F),
                  PaYd = paydDefWk,
                  PaTD = patdDefWk,
                  PaInt = paintDefWk,
                  RuAtt = ruattDefWk,
                  RuYd = ruydDefWk,
                  RuTD = rutdDefWk,
                  RuFD = rufdDefWk,
                  Rec = recDefWk,
                  ReYd = reydDefWk,
                  ReTD = retdDefWk,
                  ReFD = refdDefWk,
                  FL = flDef,
                  FPts = fptsWeekDef()
                ),
                columnGroups = list(
                  colGroup(name = "Passing", columns = c("PaYd", "PaTD", "PaInt"), align = 'left'),
                  colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                  colGroup(name = "Receiving", columns = c("Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          TRUFFLE = trfDef(filt = FALSE),
                          FPts = colDef(minWidth = 150, align = 'left',
                                        format = colFormat(digits=1),
                                        cell = function(value) {
                                            width <- paste0(value / max(truffleanalysis$FPts) * 100, "%")
                                            bar_chart(round(value,0), width = width)
                                        }),
                          Ydpts = colDef(class = 'border-left-grey')
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
                      #borderless = T,
                      compact = T,
                      defaultColDef = colDef(format = colFormat(percent = T, digits = 1)),
                      columns = list(
                          TRUFFLE = trfDef(filt = FALSE),
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
    #building the tables that take the week sliders into account
    #stat center boxscore
    output$scboxscore <- renderReactable({
      boxscorerange <- weeklysc[Season == input$scseason &
                                         Week %in% seq(input$scweekrange[1],input$scweekrange[2])
      ][,
        .(G = .N,
          PaCmp = sum(PaCmp, na.rm = T),
          PaAtt = sum(PaAtt, na.rm = T),
          PaYd = sum(PaYd, na.rm = T),
          PaTD = sum(PaTD, na.rm = T),
          PaInt = sum(PaInt, na.rm = T),
          RuAtt = sum(RuAtt, na.rm = T),
          RuYd = sum(RuYd, na.rm = T),
          RuTD = sum(RuTD, na.rm = T),
          RuFD = sum(RuFD, na.rm = T),
          Tar = sum(Tar, na.rm = T),
          Rec = sum(Rec, na.rm = T),
          ReYd = sum(ReYd, na.rm = T),
          ReTD = sum(ReTD, na.rm = T),
          ReFD = sum(ReFD, na.rm = T),
          Avg = round(mean(FPts, na.rm = T),2),
          FPts = sum(FPts, na.rm = T),
          TRUFFLEdum = ifelse(TRUFFLE == "FA", "FA", "Owned")
        ),
        by = .(TRUFFLE, Pos, Player)][TRUFFLEdum %in% input$scavailable & Avg >= input$scavgmin & Pos %in% input$scpositions][, !"TRUFFLEdum"]
      
      boxscorerange <- action_mod(df = boxscorerange, team = globalteam)
      
      reactable(boxscorerange[, .(Action,TRUFFLE,Pos,Player,G,PaCmp,PaAtt,PaYd,PaTD,PaInt,RuAtt,RuYd,RuTD,RuFD,Tar,Rec,ReYd,ReTD,ReFD,Avg,FPts)],
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                defaultSorted = c("FPts"),
                pageSizeOptions = c(10, 20, 50, 100),
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                columns = list(
                  Action = colDef(header = with_tt("A", "Action link to add, drop, or trade player"),
                                  sortable = F,
                                  filterable = F,
                                  align="center",
                                  minWidth = 30,
                                  cell = function(value, index) {
                                    action_url <- boxscorerange$ActionLink[index]
                                    img_src <- knitr::image_uri(value)
                                    image <- img(src = img_src, height = "15px", alt = value)
                                    tagList(
                                      div(style = list(display = "inline-block"), image)
                                    )
                                    tags$a(href = action_url, target = "_blank", image)
                                  }),
                  TRUFFLE = trfDef(sort = F, maxW = 60),
                  Pos = posDef(sort = F, maxW = 38),
                  Player = playerDef(minW = 125, filt = T),
                  G = gDef(),
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
                  Avg = avgDef(maxW = 65, borderL = T),
                  FPts = fptsSeasDef(maxW = 65, col = F)
                ),
                columnGroups = list(
                  colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                  colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                  colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                )
      )
    })
    
    #stat center advanced
    #calculating the reactive advanced stats over ranges
    output$scadvanced <- renderReactable({
      advancedrange <- weeklysc[Season == input$scseason & Week %in% seq(input$scweekrange[1],input$scweekrange[2])
      ][,
        .(Avg = round(mean(FPts, na.rm = T),2),
          FPts = sum(FPts),
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
                                           `FPts/Opp` = round(FPts/Opp, 3),
                                           TRUFFLEdum = ifelse(TRUFFLE == "FA", "FA", "Owned")
        )][TRUFFLEdum %in% input$scavailable & Avg >= input$scavgmin & Pos %in% input$scpositions][order(-Avg)][, !c("TRUFFLEdum")]
      
      advancedrange <- action_mod(df = advancedrange, team = globalteam)
      
      reactable(advancedrange[, c("Action","TRUFFLE","Pos","Player","Touch","Opp","FPts/Touch","FPts/Opp","YdPts","TDPts","FDPts","YdPt%","TDPt%","FDPt%","RuPts",
                                  "RePts","RuPt%","RePt%","Avg")],
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                pageSizeOptions = c(10, 20, 50, 100),
                defaultSorted = c("Avg"),
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                columns = list(
                  Action = colDef(header = with_tt("A", "Action link to add, drop, or trade player"),
                                  sortable = F,
                                  filterable = F,
                                  align="center",
                                  minWidth = 30,
                                  cell = function(value, index) {
                                    action_url <- advancedrange$ActionLink[index]
                                    img_src <- knitr::image_uri(value)
                                    image <- img(src = img_src, height = "15px", alt = value)
                                    tagList(
                                      div(style = list(display = "inline-block"), image)
                                    )
                                    tags$a(href = action_url, target = "_blank", image)
                                  }),
                  Avg = colDef(show = F, defaultSortOrder = "desc"),
                  TRUFFLE = trfDef(),
                  Pos = posDef(),
                  Player = playerDef(minW = 120, filt = T),
                  Touch = tchDef,
                  Opp = oppDef,
                  `FPts/Touch` = fptsPtchDef,
                  `FPts/Opp` = fptsPoppDef,
                  YdPts = ydptsDef,
                  TDPts = tdptsDef,
                  FDPts = fdptsDef,
                  RuPts = ruptsDef,
                  RePts = reptsDef,
                  `YdPt%` = ydptpercDef,
                  `TDPt%` = tdptpercDef,
                  `FDPt%` = fdptpercDef,
                  `RuPt%` = ruptpercDef,
                  `RePt%` = reptpercDef
                ),
                columnGroups = list(
                  colGroup(name = "Volume / Efficiency", columns = c("Touch","Opp","FPts/Touch","FPts/Opp"), align = 'left'),
                  colGroup(name = "Point Source Breakdown", columns = c("YdPts","TDPts","FDPts","YdPt%","TDPt%","FDPt%","RuPts",
                                                                        "RePts","RuPt%","RePt%"), align = 'left')
                )
      )
    })
    
    #stat center consistency
    #consistency range filtered table
    output$scconsistency <- renderReactable({
        perccolwidth <- 60
        consistencyrange <- consistencystart[Season == input$scseason &
                                                        Week %in% seq(input$scweekrange[1],input$scweekrange[2])
        ][, `:=` (
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
           .(FPts = sum(FPts, na.rm = T),
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
             `>30 %` = sum(gt30dum)/.N,
             TRUFFLEdum = ifelse(TRUFFLE == "FA", "FA", "Owned")
           ),
           by = .(TRUFFLE, Pos, Player)][TRUFFLEdum %in% input$scavailable & Avg >= input$scavgmin & Pos %in% input$scpositions][, !"TRUFFLEdum"]
        
        consistencyrange <- action_mod(df = consistencyrange, team = globalteam)
        
        reactable(consistencyrange[, .(Action,TRUFFLE,Pos,Player,Avg,RelSD,
                                       `>10 %`,`>20 %`,`>30 %`,AvgPosRk,
                                       `Top5 %`,`Top12 %`,`Top24 %`,`Top36 %`, `NonStart %`)],
                  defaultSorted = "Avg",
                  defaultSortOrder = "desc",
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = F,
                  highlight = T,
                  compact = T,
                  columns = list(
                    Action = colDef(header = with_tt("A", "Action link to add, drop, or trade player"),
                                    sortable = F,
                                    filterable = F,
                                    align="center",
                                    minWidth = 30,
                                    cell = function(value, index) {
                                      action_url <- consistencyrange$ActionLink[index]
                                      img_src <- knitr::image_uri(value)
                                      image <- img(src = img_src, height = "15px", alt = value)
                                      tagList(
                                        div(style = list(display = "inline-block"), image)
                                      )
                                      tags$a(href = action_url, target = "_blank", image)
                                    }),
                      TRUFFLE = trfDef(),
                      Pos = posDef(),
                      Player = playerDef(minW = 125, filt = T),
                      Avg = avgDef(maxW = perccolwidth, borderL = T),
                      RelSD = relsdDef,
                      AvgPosRk = avgposrkDef,
                      `Top5 %` = top5pDef,
                      `Top12 %` = top12pDef,
                      `Top24 %` = top24pDef,
                      `Top36 %` = top36pDef,
                      `NonStart %` = nonstartpDef,
                      `>10 %` = g10pDef,
                      `>20 %` = g20pDef,
                      `>30 %` = g30pDef
                      
                  ),
                  columnGroups = list(
                      colGroup(name = "Fantasy Points", columns = c("Avg","RelSD",">10 %",">20 %",">30 %"), align = 'left'),
                      colGroup(name = "Weekly Position Rank", columns = c("AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %"), align = 'left')
                  )
        )
    })
    
    #stat center extra stats
    output$scextradash <- renderReactable({
      extradashrange <- extradash[Season == input$scseason & Week %in% seq(input$scweekrange[1],input$scweekrange[2])][,
        .(TRUFFLE = TRUFFLE[1],
          G = .N,
          `Cmp%` = ifelse(sum(PaAtt, na.rm = T) > 0, round(sum(PaCmp, na.rm = T) / sum(PaAtt, na.rm = T), 3), 0),
          Pa20 = sum(Pa20, na.rm = T),
          Pa40 = sum(Pa40, na.rm = T),
          RuYPC = ifelse(sum(RuAtt, na.rm = T) > 0, round(sum(RuYd, na.rm = T) / sum(RuAtt, na.rm = T), 1), 0),
          Ru20 = sum(Ru20, na.rm = T),
          Tar = sum(Tar, na.rm = T),
          `Tar%` = round(mean(`Tar%`, na.rm = T), 1),
          ReYPC = ifelse(sum(Rec, na.rm = T) > 0, round(sum(ReYd, na.rm = T) / sum(Rec, na.rm = T), 1), 0),
          Re20 = sum(Re20, na.rm = T),
          Re40 = sum(Re40, na.rm = T),
          `ReFD%` = ifelse(sum(Rec, na.rm = T) > 0, round(sum(ReFD, na.rm = T) / sum(Rec, na.rm = T),3), 0),
          TotYd = sum(TotYd, na.rm = T),
          Avg = round(sum(FPts, na.rm = T) / .N, 1)
        ),
        by = .(Season, Pos, Player)
      ]
      extradashrange$TRUFFLEdum <- ifelse(extradashrange$TRUFFLE == "FA", "FA", "Owned")
      extradashrange <- extradashrange[TRUFFLEdum %in% input$scavailable & Pos %in% input$scpositions & Avg >= input$scavgmin][, !"TRUFFLEdum"]
      
      extradashrange <- action_mod(extradashrange, team = globalteam)
      
      extradashrange$Avg <- NULL
      extradashrange <- extradashrange[, c(20, 2, 4, 3, 1, 5:19, 21)]
      extradashrange <- extradashrange[order(-TotYd)]
      
      reactable(extradashrange[, !c("ActionLink", "playerID", "TeamNum", "Season")],
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                defaultSorted = c("TotYd"),
                defaultSortOrder = "desc",
                pageSizeOptions = c(10, 20, 50, 100),
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                defaultColDef = colDef(
                  minWidth = 55,
                  align = "right"
                ),
                columns = list(
                  Action = colDef(header = with_tt("A", "Action link to add, drop, or trade player"),
                                  sortable = F,
                                  filterable = F,
                                  align="center",
                                  minWidth = 30,
                                  cell = function(value, index) {
                                    action_url <- extradashrange$ActionLink[index]
                                    img_src <- knitr::image_uri(value)
                                    image <- img(src = img_src, height = "15px", alt = value)
                                    tagList(
                                      div(style = list(display = "inline-block"), image)
                                    )
                                    tags$a(href = action_url, target = "_blank", image)
                                  }),
                  TRUFFLE = trfDef(),
                  Pos = posDef(),
                  Player = playerDef(filt = T),
                  G = gDef(),
                  `Cmp%` = colDef(header = with_tt("Cmp%", "Completion Percentage"), format = colFormat(percent = T), minWidth = 75, class = "border-left-grey"),
                  Pa20 = colDef(header = with_tt("20+", "Passing Completions of 20+ Yd")),
                  Pa40 = colDef(header = with_tt("40+", "Passing Completions of 40+ Yd")),
                  RuYPC = colDef(header = with_tt("YPC", "Rushing Yards per Carry"), class = "border-left-grey", minWidth = 60),
                  Ru20 = colDef(header = with_tt("20+", "Rushes of 20+ Yd")),
                  Tar = colDef(header = with_tt("Tar", "Targets"), class = "border-left-grey"),
                  `Tar%` = colDef(header = with_tt("Tar%", "Percentage of Team Targets"), minWidth = 70, format = colFormat(suffix = "%")),
                  ReYPC = colDef(header = with_tt("YPC", "Yards per Catch"), minWidth = 60),
                  Re20 = colDef(header = with_tt("20+", "Receptions of 20+ Yd")),
                  Re40 = colDef(header = with_tt("40+", "Receptions of 40+ Yd")),
                  `ReFD%` = colDef(header = with_tt("FD%", "Percentage of Receptions resulting in First Down"), minWidth = 65, format = colFormat(percent = T)),
                  TotYd = colDef(header = with_tt("TotYd", "Total Passing/Rushing/Receiving Yards"), minWidth = 70)
                ),
                columnGroups = list(
                  colGroup(name = "Passing", columns = c("Cmp%","Pa20","Pa40"), align = 'left'),
                  colGroup(name = "Rushing", columns = c("RuYPC", "Ru20"), align = 'left'),
                  colGroup(name = "Receiving", columns = c("Tar", "Tar%", "ReYPC", "Re20", "Re40", "ReFD%"), align = 'left')
                )
      )
      
      
    })
    
    #stat center xfpxtd
    output$scxfpxtd <- renderReactable({
      espnsc <-espn[order(-xFP)]
      espnsc$TRUFFLEdum <- ifelse(espnsc$TRUFFLE == "FA", "FA", "Owned")
      espnsc <- action_mod(espnsc, team = globalteam)
      espnsc <- espnsc[, c(17, 2, 3, 1, 4:16, 18)]
      
      reactable(espnsc[TRUFFLEdum %in% input$scavailable & Pos %in% input$scpositions][, !c("TRUFFLEdum", "playerID", "TeamNum", "ActionLink")],
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                defaultSorted = c("xFP"),
                defaultSortOrder = "desc",
                pageSizeOptions = c(10, 20, 50, 100),
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                defaultColDef = colDef(
                  minWidth = 50,
                  align = "right",
                  sortNALast = TRUE
                ),
                columns = list(
                  Action = colDef(header = with_tt("A", "Action link to add, drop, or trade player"),
                                  sortable = F,
                                  filterable = F,
                                  align="center",
                                  minWidth = 30,
                                  cell = function(value, index) {
                                    action_url <- espnsc$ActionLink[index]
                                    img_src <- knitr::image_uri(value)
                                    image <- img(src = img_src, height = "15px", alt = value)
                                    tagList(
                                      div(style = list(display = "inline-block"), image)
                                    )
                                    tags$a(href = action_url, target = "_blank", image)
                                  }),
                  TRUFFLE = trfDef(),
                  Pos = posDef(),
                  Player = playerDef(minW = 125, filt = T),
                  NFL = nflDef,
                  xFP = colDef(header = with_tt("xFP", "Expected ESPN Fantasy Points")),
                  ActualPts = colDef(header = with_tt("aFP", "Actual ESPN Fantasy Points")),
                  FPDiff = colDef(header = with_tt("Diff", "Difference between the player's total xFP and actual FP")),
                  xTD = colDef(header = with_tt("xTD", "Expected Touchdowns"), class = "border-left-grey"),
                  TD = colDef(header = with_tt("aTD", "Actual Touchdowns")),
                  TDDiff = colDef(header = with_tt("Diff", "Difference between the player's total xTD and actual TD")),
                  Looks = colDef(header = with_tt("Looks", "Carries + Targets")),
                  In5 = colDef(header = with_tt("In5", "Carries inside 5-yard line")),
                  EZ = colDef(header = with_tt("EZ", "End Zone Targets"))
                )
      )
    })
    
    #stat center snap share
    output$scsnapshare <- renderReactable({
      snapssc <- snaps[order(-`Total Snaps`)]
      snapssc$TRUFFLEdum <- ifelse(snapssc$TRUFFLE == "FA", "FA", "Owned")
      snapssc <- action_mod(snapssc, team = globalteam)
      snapssc <- snapssc[, c(28, 1:27, 29)]
      
      reactable(snapssc[TRUFFLEdum %in% input$scavailable & Pos %in% input$scpositions][, !c("TRUFFLEdum", "playerID", "TeamNum", "ActionLink")],
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                defaultSorted = c("Total Snaps"),
                defaultSortOrder = "desc",
                pageSizeOptions = c(10, 20, 50, 100),
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                defaultColDef = colDef(
                  minWidth = 50,
                  align = "center",
                  format = colFormat(percent = T)
                ),
                columns = list(
                  Action = colDef(header = with_tt("A", "Action link to add, drop, or trade player"),
                                  sortable = F,
                                  filterable = F,
                                  align="center",
                                  minWidth = 30,
                                  cell = function(value, index) {
                                    action_url <- snapssc$ActionLink[index]
                                    img_src <- knitr::image_uri(value)
                                    image <- img(src = img_src, height = "15px", alt = value)
                                    tagList(
                                      div(style = list(display = "inline-block"), image)
                                    )
                                    tags$a(href = action_url, target = "_blank", image)
                                  }),
                  TRUFFLE = trfDef(),
                  Pos = posDef(),
                  Player = playerDef(minW = 125, filt = T),
                  Team = nflDef,
                  `Total Snaps` = colDef(minWidth = 150, format = colFormat(percent = F))
                )
      )
    })
    
    #trademachine ----
    
    tmoverviewtm1 <- reactive(tpoverview[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm1]][order(match(Pos, positionorder), -Salary, Player)])
    tmoverviewtm2 <- reactive(tpoverview[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm2]][order(match(Pos, positionorder), -Salary, Player)])
    contractstm1 <- reactive(contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm1]][order(match(Pos, positionorder), -Salary, Player)][selectedtm1(), ])
    contractstm2 <- reactive(contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm2]][order(match(Pos, positionorder), -Salary, Player)][selectedtm2(), ])
    
    output$tmtm1 <- renderReactable({
      #formatted reactable output
        reactable(tmoverviewtm1()[,!c("TRUFFLE", "G", "Bye", "PosRk")],
                  selection = "multiple", onClick = "select",
                  defaultSortOrder = "desc",
                  sortable = F,
                  pagination = FALSE,
                  highlight = T,
                  filterable = F,
                  #borderless = T,
                  compact = T,
                  columns = list(
                      Pos = posDef(maxW = 38, filt = FALSE),
                      Player = playerDef(minW = 125),
                      Age = colDef(minWidth =  40),
                      NFL = colDef(minWidth =  40),
                      Salary = salaryDefNobar(minW = 45, foot = T),
                      Contract = contractDef(minW = 30, foot = T, name = "Yr", filt = F),
                      ptslog = ptsLogDef(maxW = 70),
                      Avg = avgDef(maxW = 45),
                      FPts = fptsSeasDef(maxW = 50)
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
        )
    })
    
    output$tmtm2 <- renderReactable({
        reactable(tmoverviewtm2()[,!c("TRUFFLE", "G", "Bye", "PosRk")],
                  selection = "multiple", onClick = "select",
                  defaultSortOrder = "desc",
                  sortable = F,
                  pagination = FALSE,
                  highlight = T,
                  filterable = F,
                  #borderless = T,
                  compact = T,
                  resizable = F,
                  columns = list(
                      Pos = posDef(maxW = 38, filt = FALSE),
                      Player = playerDef(minW = 125),
                      Age = colDef(minWidth =  40),
                      NFL = colDef(minWidth =  40),
                      Salary = salaryDefNobar(minW = 45, foot = T),
                      Contract = contractDef(minW = 30, foot = T, name = "Yr", filt = F),
                      ptslog = ptsLogDef(maxW = 70),
                      Avg = avgDef(maxW = 45),
                      FPts = fptsSeasDef(maxW = 50)
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
        )
    })
    
    #storing the selections from trade machine team tables
    selectedtm1 <- reactive(getReactableState("tmtm1", "selected"))
    selectedtm2 <- reactive(getReactableState("tmtm2", "selected"))
    
    #outputs of the players in the trade
    output$tmpls1 <- renderReactable({
        
        if (length(selectedtm1()) >= 1) {
        
        reactable(contractstm1()[, !c("TRUFFLE", "Age")],
                  defaultSortOrder = "desc",
                  pagination = FALSE,
                  sortable = F,
                  filterable = F,
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  resizable = F,
                  columns = list(
                    Extension = colDef(show = F),
                    Pos = posDef(maxW = 38, filt = FALSE),
                    Player = playerDef(minW = 125),
                    NFL = colDef(minWidth =  40),
                    Salary = salaryDefNobar(minW = 45, foot = T),
                    Contract = contractDef(minW = 30, foot = T, name = "Yr"),
                    `'22` = futurecolDef(yr = "'22", maxW = 60, foot = T),
                    `'23` = futurecolDef(yr = "'23", maxW = 60, foot = T),
                    `'24` = futurecolDef(yr = "'24", maxW = 60, foot = T),
                    `'25` = futurecolDef(yr = "'25", maxW = 60, foot = T),
                    `'26` = futurecolDef(yr = "'26", maxW = 60, foot = T)
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
        )
        }
    })
    
    output$tmpls2 <- renderReactable({
        
        
        if (length(selectedtm2()) >= 1) {
            
            reactable(contractstm2()[, !c("TRUFFLE", "Age")],
                      defaultSortOrder = "desc",
                      pagination = FALSE,
                      sortable = F,
                      filterable = F,
                      highlight = T,
                      #borderless = T,
                      compact = T,
                      resizable = F,
                      columns = list(
                        Extension = colDef(show = F),
                        Pos = posDef(maxW = 38, filt = FALSE),
                        Player = playerDef(minW = 125),
                        NFL = colDef(minWidth =  40),
                        Salary = salaryDefNobar(minW = 45, foot = T),
                        Contract = contractDef(minW = 30, foot = T, name = "Yr"),
                        `'22` = futurecolDef(yr = "'22", maxW = 60, foot = T),
                        `'23` = futurecolDef(yr = "'23", maxW = 60, foot = T),
                        `'24` = futurecolDef(yr = "'24", maxW = 60, foot = T),
                        `'25` = futurecolDef(yr = "'25", maxW = 60, foot = T),
                        `'26` = futurecolDef(yr = "'26", maxW = 60, foot = T)
                      ),
                      defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
            )
        }
    })
    
    tm1newcap <- reactive(sum(tmoverviewtm1()$Salary, na.rm = T) + sum(contractstm2()$Salary, na.rm = T) - sum(contractstm1()$Salary, na.rm =T))
    tm2newcap <- reactive(sum(tmoverviewtm2()$Salary, na.rm = T) + sum(contractstm1()$Salary, na.rm = T) - sum(contractstm2()$Salary, na.rm =T))
    

    
    #success field generation
    output$tradesuccess <- renderReactable({
      traderesult <- data.table(
        Outcome = ifelse(tm1newcap() <= 500 & tm2newcap() <= 500,
                         "Success!",
                         "Trade Fails"
        ),
        Reason = ifelse(tm1newcap() <= 500 & tm2newcap() <= 500,
                        "Both teams stay below the $500 salary cap.",
                        "At least 1 of 2 teams exceeds the $500 salary cap."
        ),
        Icon = ifelse(tm1newcap() <= 500 & tm2newcap() <= 500,
                      "www/graphics/tradesuccess.png",
                      "www/graphics/tradefail.png"
        )
      )
      
      if (length(selectedtm1()) >= 1 | length(selectedtm2()) >= 1 ) {
        reactable(traderesult,
                  columns = list(
                    Outcome = colDef(name = "",
                                     #headerStyle = list(color = "#84A4D8", fontSize = 14, fontWeight = 800),
                                     minWidth = 250,
                                     style = function(value) {
                                       backg <- ifelse(value == "Success!", QBcolor, RBcolor)
                                       list(background = backg)
                                     },
                                     cell = function(value, index) {
                                       reason <- traderesult$Reason
                                       col <- ifelse(value == "Success!", textgreen, textred)
                                       backg <- ifelse(value == "Success!", QBcolor, RBcolor)
                                       div(
                                         div(style = list(fontWeight = 800, fontSize=26, color = col), value),
                                         div(style = list(fontSize = 14), reason)
                                       )
                                     }
                    ),
                    Reason = colDef(show = FALSE),
                    Icon = colDef(name = "", align="center", minWidth = 30,
                                  style = function(index) {
                                    backg <- ifelse(traderesult$Outcome == "Success!", QBcolor, RBcolor)
                                    list(background = backg)
                                  },
                                  cell = function(value) {
                                    img_src <- knitr::image_uri(value)
                                    image <- img(src = img_src, height = "50px", alt = value)
                                    tagList(
                                      div(style = list(display = "inline-block"), image)
                                    )
                                  })
                  )
        )
      }
    })
    
    posttrade <- reactive(data.table(
      TRF = c(teams$Abbrev[teams$FullName == input$tmtm1], teams$Abbrev[teams$FullName == input$tmtm2 ]),
      Team = c(input$tmtm1, input$tmtm2),
      PlayersReceived = c(length(selectedtm2()),
                          length(selectedtm1())),
      Net = c(sum(contractstm2()$Salary, na.rm = T) - sum(contractstm1()$Salary, na.rm =T),
              sum(contractstm1()$Salary, na.rm =T) - sum(contractstm2()$Salary, na.rm = T)
      ),
      Salary = c(tm1newcap(),
                 tm2newcap()
      )
    )
    )
    
    #reactable output of post trade salary caps
    output$tradecapresults <- renderReactable({
      #data table of the two teams involved and their adjusted salary caps, after the trade
      reactable(posttrade(),
                compact = T,
                resizable = F,
                columns = list(
                  TRF = trfDef(filt = FALSE),
                  Team = colDef(minWidth = 125),
                  PlayersReceived = colDef(name = "Players Received"),
                  Net = colDef(name = "Net Salary", 
                               minWidth = 50,
                               format = colFormat(digits=0, prefix = "$"),
                               style = function(value) {
                                 color <- ifelse(value < 0, IRcolor, 'black')
                                 list(color = color)}),
                  Salary = colDef(minWidth = 50,
                                  align = 'right',
                                  format = colFormat(digits=0, prefix = "$"),
                                  style = function(value) {
                                    background <- ifelse(value <= 500, QBcolor, RBcolor)
                                    list(background = background)}
                  )
                )
      )
    })
    
    #capcorner ----
    #capcornercontracts
    output$capcornercontracts <- renderReactable({
      reactable(contracts[, !c("Extension")],
                defaultSortOrder = "desc",
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                pageSizeOptions = c(10, 20, 50, 100),
                height = 'auto',
                filterable = T,
                highlight = T,
                #borderless = T,
                compact = T,
                resizable = T,
                columns = list(
                  TRUFFLE = trfDef(),
                  Pos = posDef(),
                  Player = playerDef(filt = T),
                  Age = ageDef,
                  NFL = nflDef,
                  Salary = salaryDefBar(),
                  Contract = contractDef(name = "Yr"),
                  `'22` = futurecolDef(yr = "'22"),
                  `'23` = futurecolDef(yr = "'23"),
                  `'24` = futurecolDef(yr = "'24"),
                  `'25` = futurecolDef(yr = "'25"),
                  `'26` = futurecolDef(yr = "'26")
                ),
                columnGroups = list(
                  colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                  colGroup(name = "Future Seasons", columns = c("'22","'23","'24","'25","'26"), align = 'left')
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
    
    output$extvalqb <- renderReactable({
      
      extvalqb <- extval[Pos == "QB"][, !"Pos"]
      
      extvaldraft <- draft
      extvaldraft$Pick <- paste0(extvaldraft$Round,".",extvaldraft$`#`)
      extvaldraft$ExtensionYr <- extvaldraft$Season + 3
      
      reactable(extvalqb,
                sortable = F,
                columns = list(
                  Pick = colDef(align = "right", style = list(fontWeight = "bold"), minWidth = 50),
                  Value = colDef(
                    minWidth = 50,
                    format = colFormat(prefix = "$"),
                    style = function(value) {
                      if (value == 70 | value == 60) {
                        color <- rd1col
                      } else if (value == 50) {
                        color <- rd2col
                      } else if (value == 30) {
                        color <- rd3col
                      } else {
                        color <- 'white'
                      }
                      list(background = color)
                    })
                ),
                details = function(index) {
                  if (index == 1) {
                    reactable(extvaldraft[Round == 1 & `#` <= 6 & Pos == "QB" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                              )
                  } else if (index == 2) {
                    reactable(extvaldraft[Round == 1 & `#` >= 7 & Pos == "QB" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 3) {
                    reactable(extvaldraft[Round == 2 & Pos == "QB" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 4) {
                    reactable(extvaldraft[Round == 3 & Pos == "QB" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  }
                },
                columnGroups = list(
                  colGroup(name = "QB", columns = c("Pick", "Value"), align = 'left')
                )
      )
    })
    
    output$extvalrb <- renderReactable({
      
      extvalrb <- extval[Pos == "RB"][, !"Pos"]
      
      extvaldraft <- draft
      extvaldraft$Pick <- paste0(extvaldraft$Round,".",extvaldraft$`#`)
      extvaldraft$ExtensionYr <- extvaldraft$Season + 3
      
      reactable(extvalrb,
                sortable = F,
                columns = list(
                  Pick = colDef(align = "right", style = list(fontWeight = "bold"), minWidth = 50),
                  Value = colDef(
                    minWidth = 50,
                    format = colFormat(prefix = "$"),
                    style = function(value) {
                      if (value == 70 | value == 60) {
                        color <- rd1col
                      } else if (value == 50) {
                        color <- rd2col
                      } else if (value == 30) {
                        color <- rd3col
                      } else {
                        color <- 'white'
                      }
                      list(background = color)
                    })
                ),
                details = function(index) {
                  if (index == 1) {
                    reactable(extvaldraft[Round == 1 & `#` <= 6 & Pos == "RB" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 2) {
                    reactable(extvaldraft[Round == 1 & `#` >= 7 & Pos == "RB" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 3) {
                    reactable(extvaldraft[Round == 2 & Pos == "RB" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 4) {
                    reactable(extvaldraft[Round == 3 & Pos == "RB" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  }
                },
                columnGroups = list(
                  colGroup(name = "RB", columns = c("Pick", "Value"), align = 'left')
                )
      )
    })
    
    output$extvalwr <- renderReactable({
      
      extvalwr <- extval[Pos == "WR"][, !"Pos"]
      
      extvaldraft <- draft
      extvaldraft$Pick <- paste0(extvaldraft$Round,".",extvaldraft$`#`)
      extvaldraft$ExtensionYr <- extvaldraft$Season + 3
      
      reactable(extvalwr,
                sortable = F,
                columns = list(
                  Pick = colDef(align = "right", style = list(fontWeight = "bold"), minWidth = 50),
                  Value = colDef(
                    minWidth = 50,
                    format = colFormat(prefix = "$"),
                    style = function(value) {
                      if (value == 60 | value == 50) {
                        color <- rd1col
                      } else if (value == 40) {
                        color <- rd2col
                      } else if (value == 30) {
                        color <- rd3col
                      } else {
                        color <- 'white'
                      }
                      list(background = color)
                    })
                ),
                details = function(index) {
                  if (index == 1) {
                    reactable(extvaldraft[Round == 1 & `#` <= 6 & Pos == "WR" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 2) {
                    reactable(extvaldraft[Round == 1 & `#` >= 7 & Pos == "WR" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 3) {
                    reactable(extvaldraft[Round == 2 & Pos == "WR" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 4) {
                    reactable(extvaldraft[Round == 3 & Pos == "WR" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  }
                },
                columnGroups = list(
                  colGroup(name = "WR", columns = c("Pick", "Value"), align = 'left')
                )
      )
    })
    
    output$extvalte <- renderReactable({
      
      extvalte <- extval[Pos == "TE"][, !"Pos"]
      
      extvaldraft <- draft
      extvaldraft$Pick <- paste0(extvaldraft$Round,".",extvaldraft$`#`)
      extvaldraft$ExtensionYr <- extvaldraft$Season + 3
      
      reactable(extvalte,
                sortable = F,
                columns = list(
                  Pick = colDef(align = "right", style = list(fontWeight = "bold"), minWidth = 50),
                  Value = colDef(
                    minWidth = 50,
                    format = colFormat(prefix = "$"),
                    style = function(value) {
                      if (value == 30 | value == 20) {
                        color <- rd1col
                      } else if (value == 10) {
                        color <- rd2col
                      } else if (value == 5) {
                        color <- rd3col
                      } else {
                        color <- 'white'
                      }
                      list(background = color)
                    })
                ),
                details = function(index) {
                  if (index == 1) {
                    reactable(extvaldraft[Round == 1 & `#` <= 6 & Pos == "TE" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 2) {
                    reactable(extvaldraft[Round == 1 & `#` >= 7 & Pos == "TE" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                              )
                  } else if (index == 3) {
                    reactable(extvaldraft[Round == 2 & Pos == "TE" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  } else if (index == 4) {
                    reactable(extvaldraft[Round == 3 & Pos == "TE" & Player %in% rookierights][, c("Player", "Pick", "ExtensionYr")], 
                              columns = list(
                                Player = playerDef(minW = 150),
                                Pick = colDef(minWidth = 50, align = 'right'),
                                ExtensionYr = colDef(header = with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                              )
                    )
                  }
                },
                columnGroups = list(
                  colGroup(name = "TE", columns = c("Pick", "Value"), align = 'left')
                )
      )
    })
    
    #tagvals output qb
    output$tagvalsqb <- renderReactable({
      tagvalsqb <- tagvals[Pos == "QB"][, !"Pos"]
      
      reactable(tagvalsqb,
                sortable = F,
                columns = list(
                  Type = colDef(header = with_tt("Tag", "First vs. Second time franchising player"),
                                minWidth = 50),
                  TagVal = tagvalDefBar(minW = 100)
                ),
                columnGroups = list(
                  colGroup(name = "QB", columns = c("Type", "TagVal"), align = 'left')
                ),
                details = function(index) {
                  if (index == 1) {
                    reactable(top5paid[Pos == "QB" & Season == currentyr][, c("Player", "Salary")], 
                              columns = list(
                                Player = colDef(minWidth = 150, footer = "Mean"),
                                Salary = tagvalDefNobar()
                              ),
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                    )
                  } else if (index == 2) {
                    reactable(top5paid[Pos == "QB" & Season == currentyr][, c("Player", "Salary")][1], 
                              columns = list(
                                Player = colDef(minWidth = 150),
                                Salary = tagvalDefNobar(foot = F)
                              ),
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                    )
                  }
                }
      )
    })
    
    #tagvals output rb
    output$tagvalsrb <- renderReactable({
      tagvalsrb <- tagvals[Pos == "RB"][, !"Pos"]
      
      reactable(tagvalsrb,
                sortable = F,
                columns = list(
                  Type = colDef(header = with_tt("Tag", "First vs. Second time franchising player"),
                                minWidth = 50),
                  TagVal = tagvalDefBar(minW = 100)
                ),
                columnGroups = list(
                  colGroup(name = "RB", columns = c("Type", "TagVal"), align = 'left')
                ),
                details = function(index) {
                  if (index == 1) {
                    reactable(top5paid[Pos == "RB" & Season == currentyr][, c("Player", "Salary")], 
                              columns = list(
                                Player = colDef(minWidth = 150, footer = "Mean"),
                                Salary = tagvalDefNobar()
                              ),
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                    )
                  } else if (index == 2) {
                    reactable(top5paid[Pos == "RB" & Season == currentyr][, c("Player", "Salary")][1], 
                              columns = list(
                                Player = colDef(minWidth = 150),
                                Salary = tagvalDefNobar(foot = F)
                              ),
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                    )
                  }
                }
      )
    })
    
    #tagvals output wr
    output$tagvalswr <- renderReactable({
      tagvalswr <- tagvals[Pos == "WR"][, !"Pos"]
      
      reactable(tagvalswr,
                sortable = F,
                columns = list(
                  Type = colDef(header = with_tt("Tag", "First vs. Second time franchising player"),
                                minWidth = 50),
                  TagVal = tagvalDefBar(minW = 100)
                ),
                columnGroups = list(
                  colGroup(name = "WR", columns = c("Type", "TagVal"), align = 'left')
                ),
                details = function(index) {
                  if (index == 1) {
                    reactable(top5paid[Pos == "WR" & Season == currentyr][, c("Player", "Salary")], 
                              columns = list(
                                Player = colDef(minWidth = 150, footer = "Mean"),
                                Salary = tagvalDefNobar()
                              ),
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                    )
                  } else if (index == 2) {
                    reactable(top5paid[Pos == "WR" & Season == currentyr][, c("Player", "Salary")][1], 
                              columns = list(
                                Player = colDef(minWidth = 150),
                                Salary = tagvalDefNobar(foot = F)
                              ),
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                    )
                  }
                }
      )
    })
    
    #output tag vals TE
    output$tagvalste <- renderReactable({
      tagvalste <- tagvals[Pos == "TE"][, !"Pos"]
      
      reactable(tagvalste,
                sortable = F,
                columns = list(
                  Type = colDef(header = with_tt("Tag", "First vs. Second time franchising player"),
                                minWidth = 50),
                  TagVal = tagvalDefBar(minW = 100)
                ),
                columnGroups = list(
                  colGroup(name = "TE", columns = c("Type", "TagVal"), align = 'left')
                ),
                details = function(index) {
                  if (index == 1) {
                    reactable(top5paid[Pos == "TE" & Season == currentyr][, c("Player", "Salary")], 
                              columns = list(
                                Player = colDef(minWidth = 150, footer = "Mean"),
                                Salary = salaryDefNobar()
                              ),
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                    )
                  } else if (index == 2) {
                    reactable(top5paid[Pos == "TE" & Season == currentyr][, c("Player", "Salary")][1], 
                              columns = list(
                                Player = colDef(minWidth = 150),
                                Salary = salaryDefNobar(foot = F)
                              ),
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                    )
                  }
                }
      )
    })
    
    #player look up tool for franchise tag
    output$tagvalsplayer <- renderReactable({
      reactable(ft[Player %in% input$tagvalplayer],
                compact = T,
                columns = list(
                  Pos = posDef(filt = F),
                  Player = playerDef(minW = 125),
                  Salary = salaryDefBar(minW = 125),
                  Contract = contractDef(filt = F, name = "Yr"),
                  TagVal = colDef(header = with_tt("Tag Value", "Player Tag Value for Next Year"),
                                  minWidth = 100,
                                  align = "right")
                )
      )
    })
    
    #history books ----
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                      #borderless = T,
                      compact = T,
                      columns = list(
                          Pos = posDef(maxW = 35, filt = FALSE),
                          Player = playerDef(minW = 140),
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
                                div(style = list(fontSize = 14), paste0(winner, ", ", pos, "  |  ", trf))
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
                                    div(style = list(fontSize = 14), paste0(winner, ", ", pos, "  |  ", trf))
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
                      Pos = posDef(maxW = 80, filt = FALSE),
                      TRUFFLE = trfDef(filt = FALSE)
                  ),
                  columnGroups = list(colGroup(name = "1st Team", columns = c("Pos", "Winner", "TRUFFLE"), align = 'left')
                  ))
    })
    
    output$allt2 <- renderReactable({
        reactable(allt2[Season == input$awardseason][, -"Season"],
                  compact = T,
                  columns = list(
                      Pos = posDef(maxW = 80, filt = FALSE),
                      TRUFFLE = trfDef(filt = FALSE)
                  ),
                  columnGroups = list(colGroup(name = "2nd Team", columns = c("Pos", "Winner", "TRUFFLE"), align = 'left')
                  ))
    })
    
    #rivheader
    output$rivheader <- renderReactable({
      reactable(
        riv[riv$RivalryName == input$rivalry, c("Team1Full", "Established", "Logo")],
        sortable = FALSE,
        compact = TRUE,
        columns = list(
          Team1Full = colDef(name = "Teams",
                            headerStyle = list(color = "#84A4D8", fontSize = 14, fontWeight = 800),
                            minWidth = 225,
                            cell = function(value) {
                              tm2 <- riv$Team2Full[riv$RivalryName == input$rivalry]
                              col1 <- teams$Primary[teams$FullName == riv$Team1Full[riv$RivalryName == input$rivalry]]
                              col2 <- teams$Primary[teams$FullName == riv$Team2Full[riv$RivalryName == input$rivalry]]
                              div(
                                div(style = list(fontWeight = 800, fontSize=26, color = col1), value),
                                div(style = list(fontSize = 16), "vs."),
                                div(style = list(fontWeight = 800, fontSize=26, color = col2), tm2),
                              )
                            }
          ),
          Established = colDef(name = "Established",
                               headerStyle = list(color = "#84A4D8", fontSize = 14, fontWeight = 800),
                               align="center"),
          Logo = colDef(name = "Logo",
                        headerStyle = list(color = "#84A4D8", fontSize = 14, fontWeight = 800),
                        align="center", 
                        minWidth = 120, 
                        cell = function(value) {
                             img_src <- knitr::image_uri(value)
                             image <- img(src = img_src, height = "100px", alt = value)
                             tagList(
                               div(style = list(display = "inline-block"), image)
                             )
                           })
        ),
        theme = reactableTheme(
          # Vertically center cells
          cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
        )
      )
    })
    
    #Bench Cup Output
    output$bcgsheet <- renderUI({
      tags$iframe(id = "bcgsheet", 
                  src="https://docs.google.com/spreadsheets/d/e/2PACX-1vSyQkU3S-LR5J0QU2nb8Kh3hQPaax_TsIpVUantq6wFwvaoxcR6K5_moO4xqEqCcGx8vewMBcP3t3xG/pubhtml",
                  height=1020,
                  width='100%',
                  frameborder = 0,
                  marginheight = 0)
    })
    
    #database ----
    #data hub weekly logs
    output$dhweekly <- renderReactable({
        reactable(weekly[order(-FPts)][, !c("TRUFFLE", "NFL", "Avg", "PosRk")],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  compact = T,
                  columns = list(
                      Season = seasonDef(filt = T),
                      Week = weekDef,
                      Pos = posDef(),
                      Player = playerDef(minW = 150, filt = T),
                      Opp = opDef,
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
                      FPts = fptsWeekDef()
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
        reactable(seasons[order(-FPts)][, !"PosRk"],
                  paginationType = "jump",
                  showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                  pageSizeOptions = c(10, 20, 50, 100),
                  height = 'auto',
                  filterable = T,
                  highlight = T,
                  compact = T,
                  columns = list(
                      Season = seasonDef(filt = T),
                      Pos = posDef(),
                      Player = playerDef(minW = 150, filt = T),
                      NFL = nflDef,
                      G = gDef(),
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
                      Avg = avgDef(),
                      FPts = fptsSeasDef()
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
      reactable(fantasy[order(-Season, -Week, -FPts)][, !c("Opp", "OpRk", "NFL", "Avg")],
                paginationType = "jump",
                showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
                pageSizeOptions = c(10, 20, 50, 100),
                height = 'auto',
                filterable = T,
                highlight = T,
                compact = T,
                columns = list(
                  Season = seasonDef(filt = T),
                  Week = weekDef,
                  TRUFFLE = trfDef(),
                  Pos = posDef(),
                  Player = playerDef(minW = 150, filt = T),
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
                  FPts = fptsWeekDef()
                ),
                columnGroups = list(
                  colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                  colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                  colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                )
      )
    })
    
    #draft ----
    output$rd1 <- renderReactable({
        reactable(draft[Season == input$draftseason & Round == 1][, -c("Season", "Round", "Extension")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  columns = list(
                      `#` = colDef(minWidth = 30, align = "right"),
                      TRF = trfDef(filt = FALSE),
                      Player = colDef(minWidth = 150, align = "left"),
                      Pos = posDef(maxW = 40, filt = FALSE),
                      Salary = salaryDefNobar(minW = 60)
                  ),
                  columnGroups = list(colGroup(name = "Round 1", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
                  )
        )
    })
    output$rd2 <- renderReactable({
        reactable(draft[Season == input$draftseason & Round == 2][, -c("Season", "Round", "Extension")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  columns = list(
                      `#` = colDef(minWidth = 30, align = "right"),
                      TRF = trfDef(filt = FALSE),
                      Player = colDef(minWidth = 150, align = "left"),
                      Pos = posDef(maxW = 40, filt = FALSE),
                      Salary = salaryDefNobar(minW = 60)
                  ),
                  columnGroups = list(colGroup(name = "Round 2", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
                  )
        )
    })
    output$rd3 <- renderReactable({
        reactable(draft[Season == input$draftseason & Round == 3][, -c("Season", "Round", "Extension")],
                  defaultSortOrder = "desc",
                  filterable = F,
                  showPageInfo = FALSE,
                  pagination = F,
                  highlight = T,
                  #borderless = T,
                  compact = T,
                  columns = list(
                      `#` = colDef(minWidth = 30, align = "right"),
                      TRF = trfDef(filt = FALSE),
                      Player = colDef(minWidth = 150, align = "left"),
                      Pos = posDef(maxW = 40, filt = FALSE),
                      Salary = salaryDefNobar(minW = 60)
                  ),
                  columnGroups = list(colGroup(name = "Round 3", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
                  )
        )
    })
    
    #### password server code ---------------------------------------------------- 
    # reactive value containing user's authentication status
    user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                                 user_locked_out = FALSE, status = "")
    
    
    
    # authenticate user by:
    #   1. checking whether their team is in the credentials
    observeEvent(input$login_button, {
      credentials <- readRDS("credentials/credentials.rds")
      
      # if user name row and password name row are same, credentials are valid
      #   and retrieve locked out status
      if (toupper(input$user_name) %in% credentials$user) {
        user_input$valid_credentials <- TRUE
      }
      
      # if user is not currently locked out but has now failed login too many times:
      #   1. set current lockout status to TRUE
      #   2. if username is present in credentials DF, set locked out status in 
      #     credentials DF to TRUE and save DF
      
      # if a user has valid credentials and is not locked out, he is authenticated      
      if (user_input$valid_credentials == TRUE) {
        user_input$authenticated <- TRUE
        globalteam <<- toupper(input$user_name)
        updateSelectInput(session, 'tmportaltm', choices = unique(teams$FullName), selected = teams$FullName[teams$Abbrev == globalteam])
        updateSelectInput(session, 'rivalry', choices = unique(teams$RivalryName), selected = teams$RivalryName[teams$Abbrev == globalteam])
        updateSelectInput(session, 'tmtm1', choices = unique(teams$FullName), selected = teams$FullName[teams$Abbrev == globalteam])
      } else {
        user_input$authenticated <- FALSE
      }
      
      # if user is not authenticated, set login status variable for error messages below
      if (user_input$authenticated == FALSE) {
        if (input$user_name == "" || !(input$user_name %in% credentials$user)) {
          user_input$status <- "bad_user"
        } 
      }
    })   
    
    # password entry UI componenets:
    #   username and password text fields, login button
    output$uiLogin <- renderUI({
      wellPanel(
        p("Welcome to"),
        HTML("<span style=color:#84A4D8;font-size:32px>truffle</span><span style =color:#8C2E26;font-weight:bold;font-size:60px;font-family:'Audiowide'>dash</span>  "),
        hr(),
        textInput("user_name", "Enter Your Team:"),
        
        #passwordInput("password", "Password:"),
        
        actionButton("login_button", "Log in")
      )
    })
    
    # red error message if bad credentials
    output$pass <- renderUI({
      if (user_input$status == "bad_user") {
        h5(strong("Team not found!", style = "color:#8C2E26"), align = "center")
      } else {
        ""
      }
    })  
    
})

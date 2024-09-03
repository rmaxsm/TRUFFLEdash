shinyServer(function(input, output, session) {
  
  #### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      loginPageUI
    } else if (isguest == T) {
      #### Your app's UI code goes here!
      guestUI
    } else {
      dashboardPageUI
    }
  })
  
  #### password server code ---------------------------------------------------- 
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "")
  
  
  
  # authenticate user by:
  #   1. checking whether their team is in the credentials
  observeEvent(input$login_button, {
    credentials <- data.frame(league = c(rep("TRUFFLE", 13), rep("KERFUFFLE", 13)),
                              user = c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW","GUEST",
                                       "ABT", "CLC", "CPC", "DDD", "LBC", "LC", "MB","NBB","PCP","PP","RR", "SBS",
                                       "GUEST"),
                              stringsAsFactors = FALSE)
    
    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (toupper(input$user_name) %in% credentials$user & toupper(input$user_league) == credentials$league[credentials$user == toupper(input$user_name)]) {
      user_input$valid_credentials <- TRUE
    }
    
    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in 
    #     credentials DF to TRUE and save DF
    
    # if a user has valid credentials and is not locked out, he is authenticated   
    if (user_input$valid_credentials == TRUE & toupper(input$user_name) == toupper("guest")) {
      isguest <<- TRUE
      user_input$authenticated <- TRUE
    }
    else if (user_input$valid_credentials == TRUE & input$user_name != "guest") {
      isguest <<- FALSE
      user_input$authenticated <- TRUE
      
      #set global variables for league and team
      globalteam <<- toupper(input$user_name)
      globalleague <<- toupper(input$user_league)
      
      #NEED League column, filter down
      teams <<- teams[League == globalleague]
      draft <<- draft[League == globalleague, -"League"]
      awards <<- awards[League == globalleague, -"League"]
      contracts <<- contracts[League == globalleague, -"League"]
      oldrosters <<- oldrosters[League == globalleague, -"League"]
      oldrosterstp <<- oldrosterstp[League == globalleague, -"League"]
      franchised <<- franchised[League == globalleague, -"League"]
      ft <<- ft[League == globalleague, -"League"]
      ppbios <<- ppbios[League == globalleague, -"League"]
      pptrufflecareer <<- pptrufflecareer[League == globalleague, -"League"]
      pptrufflecareerteam <<- pptrufflecareerteam[League == globalleague, -"League"]
      recordbookspl <<- recordbookspl[League == globalleague, -"League"]
      recordbookstm <<- recordbookstm[League == globalleague, -"League"]
      rings <<- rings[League == globalleague, -"League"]
      ringsbyteam <<- ringsbyteam[League == globalleague, -"League"]
      riv <<- riv[League == globalleague, -"League"]
      rivfantasy <<- rivfantasy[League == globalleague, -"League"]
      #rivscorers <<- rivscorers[League == globalleague, -"League"]
      rivscores <<- rivscores[League == globalleague, -"League"]
      rookierights <<- rookierights[League == globalleague, -"League"]
      capbyteam <<- capbyteam[League == globalleague, -"League"]
      rosters <<- rosters[League == globalleague, -"League"]
      tagvals <<- tagvals[League == globalleague, -"League"]
      #teamsfantasyweekly <<- teamsfantasyweekly[League == globalleague, -"League"]
      top5paid <<- top5paid[League == globalleague, -"League"]
      tpoverview <<- tpoverview[League == globalleague, -"League"]
      truffleanalysis <<- truffleanalysis[League == globalleague, -"League"]
      truffleanalysisperc <<- truffleanalysisperc[League == globalleague, -"League"]
      #turkeyscorers <<- turkeyscorers[League == globalleague, -"League"]
      
      #figure out how to merge team info rather than duplicate rows for two leagues
      advanced <<- advanced[League == globalleague, -"League"]; advanced$TRUFFLE[is.na(advanced$TRUFFLE)] <<- "FA"
      consistency <<- consistency[League == globalleague, -"League"]; consistency$TRUFFLE[is.na(consistency$TRUFFLE)] <<- "FA"
      pointsleaders <<- pointsleaders[League == globalleague, -"League"]
      fantasy <<- fantasy[League == globalleague, -"League"]
      weekly <<- weekly[League == globalleague, -"League"]
      weekly_orig_teams <<- weekly_orig_teams[League == globalleague, -"League"]
      #weeklytop5 <<- weeklytop5[League == globalleague, -"League"]
      
      #doing some merging and FA assignment upon logon
      weeklytop5 <<- merge(x = weeklytop5, y = oldrosters[, c("Season", "Pos", "Player", "TRUFFLE")], by = c("Season", "Pos", "Player"), all.x=TRUE); weeklytop5$TRUFFLE[is.na(weeklytop5$TRUFFLE)] <<- "FA"
      espn <<- merge(x = espn, y = oldrosters[, c("Season", "Pos", "Player", "TRUFFLE")], by = c("Season", "Pos", "Player"), all.x=TRUE); espn$TRUFFLE[is.na(espn$TRUFFLE)] <<- "FA"
      snaps <<- merge(x = snaps, y = oldrosters[, c("Season", "Pos", "Player", "TRUFFLE")], by = c("Season", "Pos", "Player"), all.x=TRUE); snaps$TRUFFLE[is.na(snaps$TRUFFLE)] <<- "FA"
      extradash <<- merge(x = extradash, y = oldrosters[, c("Season", "Pos", "Player", "TRUFFLE")], by = c("Season", "Pos", "Player"), all.x=TRUE); extradash$TRUFFLE[is.na(extradash$TRUFFLE)] <<- "FA"
      consistencystart <<- merge(x = consistencystart, y = oldrosters[, c("Season", "Pos", "Player", "TRUFFLE")], by = c("Season", "Pos", "Player"), all.x=TRUE); consistencystart$TRUFFLE[is.na(consistencystart$TRUFFLE)] <<- "FA"
      seasons <<- merge(x = seasons, y = oldrosters[, c("Season", "Pos", "Player", "TRUFFLE")], by = c("Season", "Pos", "Player"), all.x=TRUE); seasons$TRUFFLE[is.na(seasons$TRUFFLE)] <<- "FA"
      proj <<- merge(x = proj, y = oldrosters[, c("Season", "Pos", "Player", "TRUFFLE")], by = c("Season", "Pos", "Player"), all.x=TRUE); proj$TRUFFLE[is.na(proj$TRUFFLE)] <<- "FA"
      
      #updating select inputs across the site at login
      updateSelectInput(session, 'tmportaltm', choices = unique(teams$FullName), selected = teams$FullName[teams$Abbrev == globalteam])
      updateSelectInput(session, 'tmportalyr', choices = if (globalleague == "TRUFFLE") { sort(c(unique(seasons$Season), currentyr), decreasing = T) } else { sort(c(unique(weekly$Season[weekly$League == globalleague]), currentyr)) }, selected = currentyr)
      updateSelectInput(session, 'rivalry', choices = unique(teams$RivalryName), selected = teams$RivalryName[teams$Abbrev == globalteam])
      updateSelectInput(session, 'tmtm1', choices = unique(teams$FullName), selected = teams$FullName[teams$Abbrev == globalteam])
      updateSelectInput(session, 'tmtm2', choices = unique(teams$FullName), selected = teams$FullName[2])
      updateSelectInput(session, 'recordteams', choices = c(globalleague, unique(teams$FullName)), selected = globalleague)
      updateSelectInput(session, 'awardseason', choices = unique(awards$Season), selected = globalleague)
      if (isOffseason) {updateSelectInput(session, 'ppstatcenterseason', choices = c("Proj", as.character(sort(unique(weekly$Season), decreasing = T)), "All"), selected = "Proj")}
      if (isOffseason) {updateSelectInput(session, 'scseason', choices = c("Proj", as.character(sort(unique(weekly$Season), decreasing = T))), selected = "Proj")}
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
      #selectInput("user_league", "Select Your League:", c("TRUFFLE", "KERFUFFLE"), selected = "TRUFFLE"),
      radioButtons("user_league", "Select Your League:", c("TRUFFLE", "KERFUFFLE"), selected = "TRUFFLE", inline = TRUE),
      textInput("user_name", "Enter Your Team:"),
      
      #passwordInput("password", "Password:"),
      
      actionButton("login_button", "Log in")
    )
  })
  
  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "bad_user") {
      h5(strong("Team not found or wrong League selected!", style = "color:#8C2E26"), align = "center")
    } else {
      ""
    }
  })  
  
  #sponsor logo
  output$sponsor <- renderImage({
    randsponsor <- sample(1:length(list.files("www/graphics/sponsors")), 1)
    list(
      src = sprintf("www/graphics/sponsors/%s", list.files("www/graphics/sponsors")[randsponsor]),
      alt = "softdrink",
      width = "100%")}, deleteFile = FALSE)
  
  output$guestintro <- renderImage({
    list(
      src = "www/graphics/guestintro.png",
      alt = "guestinfo",
      width = "100%")}, deleteFile = FALSE)
  
  #home page ----
  #home page standings
  output$hometeamsfantasy <- renderReactable({
    teamsfantasy <- fantasy[,
                            .(FPts = sum(FPts, na.rm = T)),
                            by = .(TRUFFLE, Scoring, Season, Week)][Scoring == input$homescoring,
                                                                    .(Weekly = list(round(FPts,2)),
                                                                      Low = min(FPts, na.rm = T),
                                                                      High = max(FPts, na.rm = T),
                                                                      StdDev = round(sd(FPts, na.rm = T)),
                                                                      Avg = round(mean(FPts, na.rm = T)),
                                                                      Total = round(sum(FPts, na.rm = T))
                                                                    ),
                                                                    by = .(Scoring, Season, TRUFFLE)][order(-Total)]
    
    if (isOffseason == TRUE) {
      append <- proj[TRUFFLE != "FA",
                     .(Scoring = "PPFD",
                       Season = currentyr,
                       Weekly = 0,
                       Low = 0,
                       High = 0,
                       StdDev = 0,
                       Avg = round(sum(FPts, na.rm = T) / 17,1),
                       Total = round(sum(FPts, na.rm = T))
                     ), by = .(TRUFFLE)][order(-Total)]
      teamsfantasy <- rbind(teamsfantasy,append)
    }
    
    reactable(teamsfantasy[Season == input$homeseason, .(TRUFFLE, Weekly, Low, High, Avg, Total)],
              defaultSorted = c("Total"),
              defaultSortOrder = "desc",
              pagination = FALSE,
              height = ifelse(isOffseason, 'auto', 420),
              highlight = T,
              #borderless = T,
              compact = T,
              resizable = F,
              columns = list(
                TRUFFLE = z_trfDef(filt = FALSE),
                Weekly = colDef(header = z_with_tt("Weekly", "Weekly log of team FPts"),
                                maxWidth = 100,
                                sortable = F,
                                cell = function(values) {
                                  sparkline(values,
                                            type = "bar",
                                            chartRangeMin = 0,
                                            chartRangeMax = max(teamsfantasy$High[teamsfantasy$Season == input$homeseason]))
                                }),
                Low = colDef(header = z_with_tt("Low", "Lowest weekly score"),
                             minWidth = 50,
                             align = 'right',
                             format = colFormat(digits=1),
                             style = function(value) {
                               fontWeight <- ifelse(value == min(teamsfantasy$Low[teamsfantasy$Season == input$homeseason] & value > 0, na.rm = T), 'bold', 'plain')
                               list(fontWeight = fontWeight)
                             }),
                High = colDef(header = z_with_tt("High", "Highest weekly score"),
                              minWidth = 50,
                              align = 'right',
                              format = colFormat(digits=1),
                              style = function(value) {
                                fontWeight <- ifelse(value == max(teamsfantasy$High[teamsfantasy$Season == input$homeseason] & value > 0, na.rm = T), 'bold', 'plain')
                                list(fontWeight = fontWeight)
                              }),
                Avg = colDef(header = z_with_tt("Avg", "Weekly average team FPts"),
                             minWidth = 50,
                             align = 'right'),
                Total = colDef(header = z_with_tt("Tot", "Season total team FPts"),
                               minWidth = 150,
                               align = 'left',
                               format = colFormat(digits=0),
                               cell = function(value) {
                                 width <- paste0(value / max(teamsfantasy$Total[teamsfantasy$Season == input$homeseason]) * 100, "%")
                                 z_bar_chart(value, width = width)
                               }
                )
              ),
              if (isOffseason & input$homeseason == currentyr) {
                columnGroups = list(colGroup(name = "CBS Projections shown during offseason, Total = sum of projected FPts across roster",
                                             columns = c("TRUFFLE", "Weekly", "Low", "High", "Avg", "Total"),
                                             align = 'left',
                                             headerStyle = list(fontStyle = 'italic', fontWeight = 'normal')
                )
                )}
    )
  })
  
  #home page season leaders
  output$homepointsleaders <- renderReactable({
    if(isguest == F) {
      
      if (isOffseason == T & input$homeseason == currentyr) {
        
        reactable(proj[Season == input$homeseason, .(TRUFFLE, Pos, Player, PosRk, Avg, FPts)],
                  #height = 440,
                  defaultSorted = c("FPts", "Avg"),
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
                    TRUFFLE = z_trfDef(),
                    Pos = z_posDef(),
                    Player = z_playerDef(minW = 130,
                                         filt = T),
                    PosRk = z_posRkDef(filt = F, proj = T),
                    Avg = z_avgDef(maxW = 55, proj = T),
                    FPts = colDef(header = z_with_tt("Proj", "Seasonal projected FPts"),
                                  maxWidth = 70,
                                  format = colFormat(digits = 0),
                                  filterable = F,
                                  style = list(fontStyle = 'italic'),
                    )
                  ),
                  columnGroups = list(colGroup(name = "CBS Projections shown during offseason, highlighted in italics",
                                               columns = c("TRUFFLE", "Pos", "Player", "PosRk", "Avg", "FPts"),
                                               align = 'left',
                                               headerStyle = list(fontStyle = 'italic', fontWeight = 'normal')
                  )
                  )
                  
        )
        
      } else {
        
        reactable(pointsleaders[Scoring == input$homescoring & Season == input$homeseason, .(TRUFFLE, Pos, Player, PosRk, ptslogs, Avg, Total)],
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
                    TRUFFLE = z_trfDef(),
                    Pos = z_posDef(),
                    Player = z_playerDef(minW = 130,
                                         filt = T),
                    PosRk = z_posRkDef(filt = F),
                    ptslogs = z_ptsLogDef(),
                    Avg = z_avgDef(maxW = 48),
                    Total = colDef(header = z_with_tt("Tot", "Seasonal total FPts"),
                                   maxWidth = 45,
                                   format = colFormat(digits = 0),
                                   filterable = F
                    )
                  )
        )
      } }
    else { #guest else
      reactable(pointsleaders[Scoring == input$homescoring & Season == input$homeseason, .(TRUFFLE, Pos, Player, PosRk, ptslogs, Avg, Total)],
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
                  TRUFFLE = z_trfDef(),
                  Pos = z_posDef(),
                  Player = z_playerDef(minW = 130,
                                       filt = T),
                  PosRk = z_posRkDef(filt = F),
                  ptslogs = z_ptsLogDef(),
                  Avg = z_avgDef(maxW = 48),
                  Total = colDef(header = z_with_tt("Tot", "Seasonal total FPts"),
                                 maxWidth = 45,
                                 format = colFormat(digits = 0),
                                 filterable = F
                  )
                )
      )
      
    }
  })
  
  #home page passing leaders
  output$homepassing <- renderReactable({
    
    if (isOffseason == T & input$homeseason == currentyr) { df <- proj[Season == input$homeseason & PaAtt > 0, .(TRUFFLE, Pos, Player, PaCmp, PaAtt, PaYd, PaTD, PaInt)]  } else {
      df <- seasons[Scoring == input$homescoring & Season == input$homeseason & PaAtt > 0, .(TRUFFLE, Pos, Player, PaCmp, PaAtt, PaYd, PaTD, PaInt)]
    }
    
    reactable(df,
              height = ifelse(isOffseason, 'auto', 420),
              defaultSorted = "PaYd",
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(minW = 130,
                                     filt = T),
                PaCmp = z_pacmpDef(borderL = F, proj = (isOffseason & input$homeseason == currentyr)),
                PaAtt = z_paattDef(proj = (isOffseason & input$homeseason == currentyr)),
                PaYd = z_paydDef(proj = (isOffseason & input$homeseason == currentyr), szn = T),
                PaTD = z_patdDef(proj = (isOffseason & input$homeseason == currentyr), szn = T),
                PaInt = z_paintDef(proj = (isOffseason & input$homeseason == currentyr), szn = T)
              ),
              if (isOffseason & input$homeseason == currentyr) {
                columnGroups = list(colGroup(name = "CBS Projections shown during offseason, highlighted in italics",
                                             columns = c("TRUFFLE", "Pos", "Player", "PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"),
                                             align = 'left',
                                             headerStyle = list(fontStyle = 'italic', fontWeight = 'normal')
                )
                ) }
    )
  })
  
  #home page rushing leaders
  output$homerushing <- renderReactable({
    
    if (isOffseason == T & input$homeseason == currentyr) { df <- proj[Season == input$homeseason & RuAtt > 0, .(TRUFFLE, Pos, Player, RuAtt, RuYd, RuTD, RuFD)]  } else {
      df <- seasons[Scoring == input$homescoring & Season == input$homeseason & RuAtt > 0, .(TRUFFLE, Pos, Player, RuAtt, RuYd, RuTD, RuFD)]
    }
    
    reactable(df,
              height = ifelse(isOffseason, 'auto', 420),
              defaultSorted = "RuYd",
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(minW = 130,
                                     filt = T),
                RuAtt = z_ruattDef(borderL = F, proj = (isOffseason & input$homeseason == currentyr), szn = T),
                RuYd = z_ruydDef(proj = (isOffseason & input$homeseason == currentyr), szn = T),
                RuTD = z_rutdDef(proj = (isOffseason & input$homeseason == currentyr)),
                RuFD = z_rufdDef(disp = !(isOffseason & input$homeseason == currentyr))
              ),
              if (isOffseason & input$homeseason == currentyr) {
                columnGroups = list(colGroup(name = "CBS Projections shown during offseason, highlighted in italics",
                                             columns = c("TRUFFLE", "Pos", "Player", "RuAtt", "RuYd", "RuTD", "RuFD"),
                                             align = 'left',
                                             headerStyle = list(fontStyle = 'italic', fontWeight = 'normal')
                )
                ) }
    )
  })
  
  #home page receiving leaders
  output$homereceiving <- renderReactable({
    
    if (isOffseason == T & input$homeseason == currentyr) { df <- proj[Season == input$homeseason & Rec > 0, .(TRUFFLE, Pos, Player, Tar, Rec, ReYd, ReTD, ReFD)]  } else {
      df <- seasons[Scoring == input$homescoring & Season == input$homeseason & Rec > 0, .(TRUFFLE, Pos, Player, Tar, Rec, ReYd, ReTD, ReFD)]
    }
    
    reactable(df,
              height = ifelse(isOffseason, 'auto', 420),
              defaultSorted = "ReYd",
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(minW = 130,
                                     filt = T),
                Tar = z_tarDef(proj = (isOffseason & input$homeseason == currentyr)),
                Rec = z_recDef(proj = (isOffseason & input$homeseason == currentyr)),
                ReYd = z_reydDef(proj = (isOffseason & input$homeseason == currentyr)),
                ReTD = z_retdDef(proj = (isOffseason & input$homeseason == currentyr)),
                ReFD = z_refdDef(disp = !(isOffseason & input$homeseason == currentyr))
              ),
              if (isOffseason & input$homeseason == currentyr) {
                columnGroups = list(colGroup(name = "CBS Projections shown during offseason, highlighted in italics",
                                             columns = c("TRUFFLE", "Pos", "Player", "Tar", "Rec", "ReYd", "ReTD", "ReFD"),
                                             align = 'left',
                                             headerStyle = list(fontStyle = 'italic', fontWeight = 'normal')
                )
                ) }
    )
  })
  
  #home page advanced leaders
  output$homeadvanced <- renderReactable({
    
    if (isOffseason == T & input$homeseason == currentyr) { df <- proj[Season == input$homeseason & Avg > 5, .(TRUFFLE, Pos, Player, `FPts/Touch`, `FPts/Opp`, `YdPt%`, `TDPt%`, `FDPt%`)]  } else {
      df <- advanced[Scoring == input$homescoring & Season == input$homeseason & Opp / length(unique(weekly$Week[weekly$Season == input$homeseason])) > 5, .(TRUFFLE, Pos, Player, `FPts/Touch`, `FPts/Opp`, `YdPt%`, `TDPt%`, `FDPt%`)]
    }
    
    reactable(df,
              #height = ifelse(isOffseason, 'auto', 420),
              defaultSorted = "FPts/Touch",
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
                TRUFFLE = z_trfDef(maxW = 70),
                Pos = z_posDef(maxW = 44),
                Player = z_playerDef(minW = 120,
                                     filt = T),
                
                `FPts/Touch` = z_fptsPtchDef(proj = (isOffseason & input$homeseason == currentyr)),
                `FPts/Opp` = z_fptsPoppDef(proj = (isOffseason & input$homeseason == currentyr)),
                `YdPt%` = z_ydptpercDef(proj = (isOffseason & input$homeseason == currentyr)),
                `TDPt%` = z_tdptpercDef(proj = (isOffseason & input$homeseason == currentyr)),
                `FDPt%` = z_fdptpercDef(proj = (isOffseason & input$homeseason == currentyr))
              ),
              if (isOffseason & input$homeseason == currentyr) {
                columnGroups = list(colGroup(name = "CBS Projections shown during offseason, highlighted in italics",
                                             columns = c("TRUFFLE", "Pos", "Player", "FPts/Touch", "FPts/Opp", "YdPt%", "TDPt%", "FDPt%"),
                                             align = 'left',
                                             headerStyle = list(fontStyle = 'italic', fontWeight = 'normal')
                )
                ) }
    )
  })
  
  output$homeconsistency <- renderReactable({
    reactable(consistency[Scoring == input$homescoring & Season == input$homeseason & Avg > 5, .(TRUFFLE, Pos, Player, Avg, RelSD, `>10 %`, `>20 %`, `>30 %`)],
              height = ifelse(isOffseason, 'auto', 420),
              defaultSorted = "Avg",
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
                TRUFFLE = z_trfDef(maxW = 70),
                Pos = z_posDef(maxW = 44),
                Player = z_playerDef(minW = 130,
                                     filt = T),
                Avg = z_avgDef(maxW = 55),
                RelSD = z_relsdDef,
                `>10 %` = z_g10pDef,
                `>20 %` = z_g20pDef,
                `>30 %` = z_g30pDef
              ),
              if (isOffseason & input$homeseason == currentyr) {
                columnGroups = list(colGroup(name = "Previous year statistics, projections not available for consistency metrics",
                                             columns = c("TRUFFLE", "Pos", "Player", "Avg", "RelSD", ">10 %", ">20 %", ">30 %"),
                                             align = 'left',
                                             headerStyle = list(fontStyle = 'italic', fontWeight = 'normal')
                )
                ) }
    )
  })
  
  #home weekly top 5 qb
  output$homeweeklytop5qb <- renderReactable({
    if (isguest == F) {
      reactable(na.omit(weeklytop5[Scoring == input$homescoring & Season == input$homeseason & Week == input$weeklytop5week & Pos == "QB"][1:30, .(TRUFFLE, Player, FPts)][order(-FPts)]),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Player = z_playerDef(minW = 125, filt = F, sort = F),
                  FPts = z_fptsWeekDef(maxW = 40, borderL = F)
                ),
                columnGroups = list(colGroup(name = "QB", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
      )
    } else if (isguest == T) {
      reactable(na.omit(weeklytop5[Scoring == input$homescoring & Season == input$homeseason & Week == input$weeklytop5week & Pos == "QB"][1:30, .(TRUFFLE, Pos, Player, FPts)][order(-FPts)]),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Pos = z_posDef(filt = F),
                  Player = z_playerDef(minW = 125, filt = F, sort = F),
                  FPts = z_fptsWeekDef(maxW = 40, borderL = F)
                ),
                columnGroups = list(colGroup(name = "QB", columns = c("TRUFFLE", "Pos", "Player", "FPts"), align = 'left'))
      )
    }
  })
  
  #home weekly top 5 rb
  output$homeweeklytop5rb <- renderReactable({
    if (isguest == F) {
      reactable(na.omit(weeklytop5[Scoring == input$homescoring & Season == input$homeseason & Week == input$weeklytop5week & Pos == "RB"][1:30, .(TRUFFLE, Player, FPts)][order(-FPts)]),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Player = z_playerDef(minW = 125, filt = F, sort = F),
                  FPts = z_fptsWeekDef(maxW = 40, borderL = F)
                ),
                columnGroups = list(colGroup(name = "RB", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
      )
    } else if (isguest == T) {
      reactable(na.omit(weeklytop5[Scoring == input$homescoring & Season == input$homeseason & Week == input$weeklytop5week & Pos == "RB"][1:30, .(TRUFFLE, Pos, Player, FPts)][order(-FPts)]),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Pos = z_posDef(filt = F),
                  Player = z_playerDef(minW = 125, filt = F, sort = F),
                  FPts = z_fptsWeekDef(maxW = 40, borderL = F)
                ),
                columnGroups = list(colGroup(name = "RB", columns = c("TRUFFLE", "Pos", "Player", "FPts"), align = 'left'))
      )
    }
  })
  
  #home weekly top 5 wr
  output$homeweeklytop5wr <- renderReactable({
    if (isguest == F) {
      reactable(na.omit(weeklytop5[Scoring == input$homescoring & Season == input$homeseason & Week == input$weeklytop5week & Pos == "WR"][1:30, .(TRUFFLE, Player, FPts)][order(-FPts)]),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Player = z_playerDef(minW = 125, filt = F, sort = F),
                  FPts = z_fptsWeekDef(maxW = 40, borderL = F)
                ),
                columnGroups = list(colGroup(name = "WR", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
      )
    } else if (isguest == T) {
      reactable(na.omit(weeklytop5[Scoring == input$homescoring & Season == input$homeseason & Week == input$weeklytop5week & Pos == "WR"][1:30, .(TRUFFLE, Pos, Player, FPts)][order(-FPts)]),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Pos = z_posDef(filt = F),
                  Player = z_playerDef(minW = 125, filt = F, sort = F),
                  FPts = z_fptsWeekDef(maxW = 40, borderL = F)
                ),
                columnGroups = list(colGroup(name = "WR", columns = c("TRUFFLE", "Pos", "Player", "FPts"), align = 'left'))
      )
    }
  })
  
  #home weekly top 5 te
  output$homeweeklytop5te <- renderReactable({
    if (isguest == F) {
      reactable(na.omit(weeklytop5[Scoring == input$homescoring & Season == input$homeseason & Week == input$weeklytop5week & Pos == "TE"][1:30, .(TRUFFLE, Player, FPts)][order(-FPts)]),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Player = z_playerDef(minW = 125, filt = F, sort = F),
                  FPts = z_fptsWeekDef(maxW = 40, borderL = F)
                ),
                columnGroups = list(colGroup(name = "TE", columns = c("TRUFFLE", "Player", "FPts"), align = 'left'))
      )
    } else if (isguest == T) {
      reactable(na.omit(weeklytop5[Scoring == input$homescoring & Season == input$homeseason & Week == input$weeklytop5week & Pos == "TE"][1:30, .(TRUFFLE, Pos, Player, FPts)][order(-FPts)]),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Pos = z_posDef(filt = F),
                  Player = z_playerDef(minW = 125, filt = F, sort = F),
                  FPts = z_fptsWeekDef(maxW = 40, borderL = F)
                ),
                columnGroups = list(colGroup(name = "TE", columns = c("TRUFFLE", "Pos", "Player", "FPts"), align = 'left'))
      )
    }
  })
  
  #team portal ----
  
  #tpheader
  output$tpheader <- renderReactable({
    reactable(
      teams[teams$FullName == input$tmportaltm, .(FullName, Abbrev, Logo, DivLogo, RivLogo)],
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
        Abbrev = z_trfDef(filt = FALSE, name = ""),
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
    if(input$tmportalyr == currentyr) {
      tpoverview <- z_action_mod(df = tpoverview[Scoring == input$homescoring], team = globalteam)
      reactable(tpoverview[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm], .(Action, Pos, Player, Age, NFL, Bye, Salary, Contract, G, PosRk, ptslog, Avg, FPts)][order(match(Pos, positionorder), -Avg)],
                defaultSortOrder = "desc",
                pagination = FALSE,
                highlight = T,
                filterable = T,
                compact = T,
                columns = list(
                  Action = colDef(show = !isguest,
                                  header = z_with_tt("A", "Action link to add, drop, or trade player"),
                                  sortable = F,
                                  filterable = F,
                                  align="center",
                                  minWidth = 30,
                                  cell = function(value, index) {
                                    action_url <- tpoverview$ActionLink[index]
                                    img_src <- knitr::image_uri(value)
                                    image <- img(src = img_src, height = "15px", alt = value)
                                    tagList(
                                      div(style = list(display = "inline-block"), image)
                                    )
                                    tags$a(href = action_url, target = "_blank", image)
                                  }),
                  Pos = z_posDef(foot = "Total"),
                  Player = z_playerDef(filt=T),
                  Age = z_ageDef,
                  Bye = z_byeDef,
                  NFL = z_nflDef,
                  Salary = z_salaryDefBar(foot = T),
                  Contract = z_contractDef(foot = T, title ="Yr"),
                  G = z_gDef(),
                  PosRk = z_posRkDef(filt = T),
                  ptslog = z_ptsLogDef(),
                  Avg = z_avgDef(proj = isOffseason),
                  FPts = z_fptsSeasDef(proj = isOffseason)
                ),
                columnGroups = list(
                  if (isOffseason == T & input$tmportalyr == currentyr) {colGroup(name = "CBS Projections for Avg & FPts shown during offseason",
                                                  columns = c("Action", "Pos", "Player", "Age", "NFL", "Bye"),
                                                  align = 'left',
                                                  headerStyle = list(fontStyle = 'italic', fontWeight = 'normal'))},
                  colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                  colGroup(name = "Fantasy", columns = c("G", "PosRk", "ptslog", "Avg", "FPts"), align = 'left')
                ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      )
    }
    else if (input$tmportalyr < 2020) {
      reactable(wayoldrosterstp[Scoring == input$homescoring & Season == input$tmportalyr & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm], .(Pos, Player, NFL, G, PosRk, Avg, FPts)][order(match(Pos, positionorder), -Avg)],
                defaultSortOrder = "desc",
                pagination = FALSE,
                highlight = T,
                filterable = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(),
                  Player = z_playerDef(filt=T),
                  NFL = z_nflDef,
                  G = z_gDef(),
                  PosRk = z_posRkDef(filt = T),
                  Avg = z_avgDef(),
                  FPts = z_fptsSeasDef()
                ),
                columnGroups = list(
                  colGroup(name = "Fantasy", columns = c("G", "PosRk", "Avg", "FPts"), align = 'left')
                ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      )
    } else {
      reactable(oldrosterstp[Scoring == input$homescoring & Season == input$tmportalyr & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm], .(Pos, Player, NFL, Salary, Contract, G, PosRk, ptslog, Avg, FPts)][order(match(Pos, positionorder), -Avg)],
                defaultSortOrder = "desc",
                pagination = FALSE,
                highlight = T,
                filterable = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(foot = "Total"),
                  Player = z_playerDef(filt=T),
                  NFL = z_nflDef,
                  Salary = z_salaryDefBar(foot = T),
                  Contract = z_contractDef(foot = T, title ="Yr"),
                  G = z_gDef(),
                  PosRk = z_posRkDef(filt = T),
                  ptslog = z_ptsLogDef(),
                  Avg = z_avgDef(),
                  FPts = z_fptsSeasDef()
                ),
                columnGroups = list(
                  colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                  colGroup(name = "Fantasy", columns = c("G", "PosRk", "ptslog", "Avg", "FPts"), align = 'left')
                ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      )
    }
  })
  
  contractsreact <- reactive(contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][, !c("TRUFFLE", "TagVal")][order(match(Pos, positionorder), -Salary, Player)])
  
  selectedguys <- reactive(getReactableState("tpcontracts", "selected"))
  
  #team portal contracts
  output$tpcontracts <- renderReactable({
    if (input$tmportalyr == currentyr) {
      z_futureColDef <- function(maxW = 75, filt = T, foot = F, yr) {
        colDef(header = z_with_tt(yr, "FA: Free Agent\nPurple: Rookie Extension Value\nBlue: Franchise Tag"),
               maxWidth = maxW,
               filterable = filt,
               align = 'right',
               defaultSortOrder = "desc",
               style = function(value, index) {
                 df <- contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][, !c("TRUFFLE")][order(match(Pos, positionorder), -Salary, Player)]
                 pl <- as.numeric()
                 tag <- df$TagVal[index]
                 ext <- df$Extension[index]
                 col <- ifelse(as.character(value) == as.character("FT"), franchisetag,
                               ifelse(as.character(value) == as.character(ext), rookieextension,
                                      ifelse(as.character(value) == as.character("FA"), textred,
                                             ifelse(as.character(value) == as.character(tag), franchisetag, tabletextcol))))
                 list(color = col)},
               #cell = function(value) {
               #class <- paste0("tag status-", value)
               #htmltools::div(class = class, value)},
               footer = function(values) if(foot == T) {paste0("$", sum(as.numeric(values), na.rm=T))}
        )
      }
      
      reactable(contractsreact(),
                selection = "multiple", onClick = "select",
                defaultSortOrder = "desc",
                pagination = FALSE,
                filterable = T,
                highlight = T,
                compact = T,
                columns = list(
                  Extension = colDef(show = F),
                  Pos = z_posDef(foot = "Total"),
                  Player = z_playerDef(filt = T),
                  Age = z_ageDef,
                  NFL = z_nflDef,
                  Salary = z_salaryDefBar(foot = T),
                  Contract = z_contractDef(title ="Yr", foot = T),
                  Y1 = z_futureColDef(yr = "'24", foot = T),
                  Y2 = z_futureColDef(yr = "'25", foot = T),
                  Y3 = z_futureColDef(yr = "'26", foot = T),
                  Y4 = z_futureColDef(yr = "'27", foot = T),
                  Y5 = z_futureColDef(yr = "'28", foot = T)
                ),
                columnGroups = list(
                  colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                  colGroup(name = "Future Seasons", columns = c("Y1","Y2","Y3","Y4","Y5"), align = 'left')
                ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      )
    }
    else if (input$tmportalyr < 2020) {
      emptydf <- as.data.table("Sorry, no data available. Salaries & contracts only introduced in 2020.")
      colnames(emptydf) <- c("Error")
      reactable(emptydf)
    } else {
      reactable(oldrosterstp[Scoring == input$homescoring & Season == input$tmportalyr & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm], .(Pos, Player, NFL, Salary, Contract)][order(match(Pos, positionorder), -Salary)],
                defaultSortOrder = "desc",
                pagination = FALSE,
                highlight = T,
                filterable = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(foot = "Total"),
                  Player = z_playerDef(filt=T),
                  NFL = z_nflDef,
                  Salary = z_salaryDefBar(foot = T),
                  Contract = z_contractDef(foot = T, title ="Yr")
                ),
                columnGroups = list(
                  colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left')
                ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      )
    }
  })
  
  #team portal boxscore
  #change to include not exclude columns
  output$tpboxscore <- renderReactable({
    
    if (isOffseason == T & input$tmportalyr == currentyr) { df <- proj[Player %in% oldrosters$Player[oldrosters$TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm] & oldrosters$Season == input$tmportalyr] 
                                                                       & Season == input$tmportalyr][order(match(Pos, positionorder), -FPts)][, .(Pos, Player, G, PaCmp, PaAtt, PaYd, PaTD, PaInt, RuAtt, RuYd, RuTD, RuFD, Tar, Rec, ReYd, ReTD, ReFD, Avg, FPts)]  } else {
      df <- seasons[Scoring == input$homescoring & Player %in% oldrosters$Player[oldrosters$TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm] & oldrosters$Season == input$tmportalyr] 
              & Season == input$tmportalyr][order(match(Pos, positionorder), -FPts)][, .(Pos, Player, G, PaCmp, PaAtt, PaYd, PaTD, PaInt, RuAtt, RuYd, RuTD, RuFD, Tar, Rec, ReYd, ReTD, ReFD, Avg, FPts)]
    }
    
    reactable(df,
              pagination = F,
              height = 'auto',
              filterable = T,
              highlight = T,
              compact = T,
              columns = list(
                Pos = z_posDef(),
                Player = z_playerDef(minW = 135, filt=T),
                G = z_gDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                PaCmp = z_pacmpDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                PaAtt = z_paattDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                PaYd = z_paydDef(proj = (isOffseason & input$tmportalyr == currentyr), szn = T),
                PaTD = z_patdDef(proj = (isOffseason & input$tmportalyr == currentyr), szn = T),
                PaInt = z_paintDef(proj = (isOffseason & input$tmportalyr == currentyr), szn = T),
                RuAtt = z_ruattDef(borderL = T, proj = (isOffseason & input$tmportalyr == currentyr), szn = T),
                RuYd = z_ruydDef(proj = (isOffseason & input$tmportalyr == currentyr), szn = T),
                RuTD = z_rutdDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                RuFD = z_rufdDef(disp = (isOffseason & input$tmportalyr == currentyr)),
                Tar = z_tarDef(borderL = T, proj = (isOffseason & input$tmportalyr == currentyr)),
                Rec = z_recDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                ReYd = z_reydDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                ReTD = z_retdDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                ReFD = z_refdDef(disp = (isOffseason & input$tmportalyr == currentyr)),
                Avg = z_avgDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                FPts = z_fptsSeasDef(proj = (isOffseason & input$tmportalyr == currentyr))
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
    
    if (isOffseason == T & input$tmportalyr == currentyr) { df <- proj[Player %in% oldrosters$Player[oldrosters$TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm] & oldrosters$Season == input$tmportalyr] 
                                                                       & Season == input$tmportalyr][order(match(Pos, positionorder), -FPts)][, .(Pos, Player, FPts, Touch, Opp, `FPts/Touch`, `FPts/Opp`,YdPts,TDPts,FDPts,RuPts,RePts,`YdPt%`,`TDPt%`,`FDPt%`,`RuPt%`,`RePt%`)]  } else {
    df <- advanced[Scoring == input$homescoring & Season == input$tmportalyr &
                     Player %in% oldrosters$Player[oldrosters$TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm] &
                                                     oldrosters$Season == input$tmportalyr]][order(match(Pos, positionorder),-FPts)][, 
                                                                                                                                     .(Pos, Player, FPts, Touch, Opp, `FPts/Touch`, `FPts/Opp`,YdPts,TDPts,FDPts,RuPts,RePts,`YdPt%`,`TDPt%`,`FDPt%`,`RuPt%`,`RePt%`)]
    }
    
    if (input$tmportalyr >= 2020) {
      z_perccolwidth <- 60
      z_othcolwidth <- 43
      reactable(df,
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(),
                  Player = z_playerDef(minW = 120, filt = T),
                  FPts = z_fptsSeasDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  Touch = z_tchDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  Opp = z_oppDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  `FPts/Touch` = z_fptsPtchDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  `FPts/Opp` = z_fptsPoppDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  YdPts = z_ydptsDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  TDPts = z_tdptsDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  FDPts = z_fdptsDef(proj = (isOffseason & input$tmportalyr == currentyr), disp = !(isOffseason & input$tmportalyr == currentyr)),
                  RuPts = z_ruptsDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  RePts = z_reptsDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  `YdPt%` = z_ydptpercDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  `TDPt%` = z_tdptpercDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  `FDPt%` = z_fdptpercDef(proj = (isOffseason & input$tmportalyr == currentyr), disp = !(isOffseason & input$tmportalyr == currentyr)),
                  `RuPt%` = z_ruptpercDef(proj = (isOffseason & input$tmportalyr == currentyr)),
                  `RePt%` = z_reptpercDef(proj = (isOffseason & input$tmportalyr == currentyr))
                ),
                columnGroups = list(
                  colGroup(name = "Volume / Efficiency", columns = c("Touch","Opp","FPts/Touch","FPts/Opp"), align = 'left'),
                  colGroup(name = "Point Source Breakdown", columns = c("YdPts","TDPts","FDPts","YdPt%","TDPt%","FDPt%","RuPts",
                                                                        "RePts","RuPt%","RePt%"), align = 'left')
                )
      )
    }
    else {
      emptydf <- as.data.table("Sorry, no data available. Advanced statistics only introduced in 2020.")
      colnames(emptydf) <- c("Error")
      reactable(emptydf)
    }
  })
  
  #team portal consistency
  output$tpconsistency <- renderReactable({
    if (input$tmportalyr >= 2020) {
      z_perccolwidth <- 60
      reactable(consistency[Scoring == input$homescoring & Season == input$tmportalyr & Player %in% oldrosters$Player[oldrosters$TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm] & oldrosters$Season == input$tmportalyr]][order(match(Pos, positionorder),-Avg)][, -c("Scoring","TRUFFLE","Season")],
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                columns = list(
                  G = z_gDef(),
                  Pos = z_posDef(),
                  Player = z_playerDef(minW = 125, filt = T),
                  Avg = z_avgDef(),
                  RelSD = z_relsdDef,
                  AvgPosRk = z_avgposrkDef,
                  `Top5 %` = z_top5pDef,
                  `Top12 %` = z_top12pDef,
                  `Top24 %` = z_top24pDef,
                  `Top36 %` = z_top36pDef,
                  `NonStart %` = z_nonstartpDef,
                  `>10 %` = z_g10pDef,
                  `>20 %` = z_g20pDef,
                  `>30 %` = z_g30pDef
                ),
                columnGroups = list(
                  colGroup(name = "Fantasy Points", columns = c("Avg","RelSD",">10 %",">20 %",">30 %"), align = 'left'),
                  colGroup(name = "Weekly Position Rank", columns = c("AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %"), align = 'left')
                )
      )
    } else {
      emptydf <- as.data.table("Sorry, no data available. Consistency statistics only introduced in 2020.")
      colnames(emptydf) <- c("Error")
      reactable(emptydf)
    }
  })
  
  #team portal extradash
  output$tpextradash <- renderReactable({
    if (input$tmportalyr >= 2022) {
      reactable(extradashszn[Season == input$tmportalyr & Player %in% oldrosters$Player[oldrosters$TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm] & oldrosters$Season == input$tmportalyr]][order(match(Pos, positionorder),-TotYd)][, !c("TRUFFLE", "Season")],
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
                  Pos = z_posDef(),
                  Player = z_playerDef(filt = T),
                  G = z_gDef(),
                  `Cmp%` = colDef(header = z_with_tt("Cmp%", "Completion Percentage"), format = colFormat(percent = T), minWidth = 75),
                  Pa20 = colDef(header = z_with_tt("20+", "Passing Completions of 20+ Yd"), class = "border-left-grey"),
                  Pa40 = colDef(header = z_with_tt("40+", "Passing Completions of 40+ Yd")),
                  RuYPC = colDef(header = z_with_tt("YPC", "Rushing Yards per Carry"), class = "border-left-grey", minWidth = 60),
                  Ru20 = colDef(header = z_with_tt("20+", "Rushes of 20+ Yd")),
                  Tar = colDef(header = z_with_tt("Tar", "Targets"), class = "border-left-grey"),
                  `Tar%` = colDef(header = z_with_tt("Tar%", "Percentage of Team Targets"), minWidth = 70, format = colFormat(suffix = "%")),
                  ReYPC = colDef(header = z_with_tt("YPC", "Yards per Catch"), minWidth = 60),
                  Re20 = colDef(header = z_with_tt("20+", "Receptions of 20+ Yd")),
                  Re40 = colDef(header = z_with_tt("40+", "Receptions of 40+ Yd")),
                  `ReFD%` = colDef(header = z_with_tt("FD%", "Percentage of Receptions resulting in First Down"), minWidth = 65, format = colFormat(percent = T)),
                  TotYd = colDef(header = z_with_tt("TotYd", "Total Passing/Rushing/Receiving Yards"), minWidth = 70)
                ),
                columnGroups = list(
                  colGroup(name = "Passing", columns = c("Cmp%","Pa20","Pa40"), align = 'left'),
                  colGroup(name = "Rushing", columns = c("RuYPC", "Ru20"), align = 'left'),
                  colGroup(name = "Receiving", columns = c("Tar", "Tar%", "ReYPC", "Re20", "Re40", "ReFD%"), align = 'left')
                )
      )
    } else {
      emptydf <- as.data.table("Sorry, no data available. Extradash statistics only introduced in 2022.")
      colnames(emptydf) <- c("Error")
      reactable(emptydf)
    }
  })
  
  #team portal xfpxtd
  output$tpxfpxtd <- renderReactable({
    if (input$tmportalyr >= 2022) {
      reactable(espn[Season == input$tmportalyr & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(match(Pos, positionorder),-xFP)][, !c("TRUFFLE", "Season")],
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
                  Pos = z_posDef(filt = T),
                  Player = z_playerDef(minW = 125, filt = T),
                  xFP = colDef(header = z_with_tt("xFP", "Expected ESPN Fantasy Points")),
                  ActualPts = colDef(header = z_with_tt("aFP", "Actual ESPN Fantasy Points")),
                  FPDiff = colDef(header = z_with_tt("Diff", "Difference between the player's total xFP and actual FP")),
                  xTD = colDef(header = z_with_tt("xTD", "Expected Touchdowns"), class = "border-left-grey"),
                  TD = colDef(header = z_with_tt("aTD", "Actual Touchdowns")),
                  TDDiff = colDef(header = z_with_tt("Diff", "Difference between the player's total xTD and actual TD")),
                  Looks = colDef(header = z_with_tt("Looks", "Carries + Targets")),
                  In5 = colDef(header = z_with_tt("In5", "Carries inside 5-yard line")),
                  EZ = colDef(header = z_with_tt("EZ", "End Zone Targets"))
                )
      )
    } else {
      emptydf <- as.data.table("Sorry, no data available. ESPN expected statistics only introduced in 2022.")
      colnames(emptydf) <- c("Error")
      reactable(emptydf)
    }
    
  })
  
  #team portal snap share
  output$tpsnapshare <- renderReactable({
    if (input$tmportalyr >= 2022) {
      reactable(snaps[Season == input$tmportalyr & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(match(Pos, positionorder),-Tot)][, !c("TRUFFLE", "Team", "Season")],
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                defaultColDef = colDef(
                  minWidth = 48,
                  align = "center",
                  format = colFormat(percent = T)
                ),
                columns = list(
                  Pos = z_posDef(),
                  Player = z_playerDef(minW = 125, filt = T),
                  `18` = colDef(class = "border-right-grey"),
                  Tot = colDef(minWidth = 50, format = colFormat(percent = F))
                )
      )
    } else {
      emptydf <- as.data.table("Sorry, no data available. Snap share statistics only introduced in 2022.")
      colnames(emptydf) <- c("Error")
      reactable(emptydf)
    }
  })
  
  #team portal fantasy logs
  output$tpfantasylogs <- renderReactable({
    if (input$tmportalyr >= 2020) {
      reactable(fantasy[Scoring == input$homescoring & Season == input$tmportalyr & TRUFFLE == teams$Abbrev[teams$FullName == input$tmportaltm]][order(Week, -FPts)][, !c("Scoring","TRUFFLE", "Season", "NFL", "Avg","FL")],
                paginationType = "jump", defaultPageSize = 10, showPageInfo = FALSE,
                height = 'auto',
                filterable = T,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Week = z_weekDef,
                  Pos = z_posDef(),
                  Player = z_playerDef(minW = 150, filt = T),
                  Opp = z_opDef,
                  OpRk = z_oprkDef,
                  PaCmp = z_pacmpDef(),
                  PaAtt = z_paattDef(),
                  PaYd = z_paydDef(wk = T),
                  PaTD = z_patdDef(wk = T),
                  PaInt = z_paintDef(wk = T),
                  RuAtt = z_ruattDef(wk = T),
                  RuYd = z_ruydDef(wk = T),
                  RuTD = z_rutdDefWk,
                  RuFD = z_rufdDefWk,
                  Tar = z_tarDefWk,
                  Rec = z_recDefWk,
                  ReYd = z_reydDefWk,
                  ReTD = z_retdDefWk,
                  ReFD = z_refdDefWk,
                  FPts = z_fptsWeekDef()
                ),
                columnGroups = list(
                  colGroup(name = "Passing", columns = c("PaCmp", "PaAtt", "PaYd", "PaTD", "PaInt"), align = 'left'),
                  colGroup(name = "Rushing", columns = c("RuAtt", "RuYd", "RuTD", "RuFD"), align = 'left'),
                  colGroup(name = "Receiving", columns = c("Tar", "Rec", "ReYd", "ReTD", "ReFD"), align = 'left')
                )
      )
    } else {
      emptydf <- as.data.table("Sorry, no data available. Weekly fantasy logs only introduced in 2020.")
      colnames(emptydf) <- c("Error")
      reactable(emptydf)
    }
  })
  
  #player portal ----
  #player portal bios
  output$ppbios <- renderReactable({
    if (isguest == F) {
      
      ppbios <- z_action_mod(df = ppbios, team = globalteam)
      selectedplayers <- ppbios[Scoring == input$homescoring & Player %in% input$player][order(-Salary)]
      
      reactable(selectedplayers[, .(Action, TRUFFLE, Pos, Player, NFL, Age, DynRk, DynPosRk, Salary, Contract, ptslogs)],
                defaultSorted = c("Salary"),
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                columns = list(
                  Action = colDef(show = !isguest,
                                  header = z_with_tt("A", "Action link to add, drop, or trade player"),
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  Pos = z_posDef(filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  NFL = colDef(align = 'left'),
                  Age = colDef(name = "Age"),
                  DynRk = colDef(header = z_with_tt("DynRk", "Fantasy Pros Overall Dynasty Rank"), align = 'left'),
                  DynPosRk = colDef(header = z_with_tt("DynRk", "Fantasy Pros Positional Dynasty Rank"), align = 'left'),
                  Salary = z_salaryDefBar(),
                  Contract = z_contractDef(filt = FALSE, minW = 70),
                  ptslogs = z_ptsLogDef(maxW = 100)
                )
      )
    } else { 
      selectedplayers <- ppbios[Scoring == input$homescoring & Player %in% input$player][order(Player)]
      
      reactable(selectedplayers[, .(Pos, Player, NFL, Age, DynRk, DynPosRk, ptslogs)],
                pagination = F,
                height = 'auto',
                filterable = F,
                highlight = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  NFL = colDef(align = 'left'),
                  Age = colDef(name = "Age"),
                  DynRk = colDef(header = z_with_tt("DynRk", "Fantasy Pros Overall Dynasty Rank"), align = 'left'),
                  DynPosRk = colDef(header = z_with_tt("DynRk", "Fantasy Pros Positional Dynasty Rank"), align = 'left'),
                  ptslogs = z_ptsLogDef(maxW = 100)
                )
      )
      
    }
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
                Player = z_playerDef(minW=120),
                G = z_gDef(),
                PaYd = z_paydDef(borderL = T),
                PaTD = z_patdDef(),
                PaInt = z_paintDef(),
                RuYd = z_ruydDef(borderL = T),
                RuTD = z_rutdDef(),
                RuFD = z_rufdDef(),
                ReYd = z_reydDef(borderL = T),
                ReTD = z_retdDef(),
                ReFD = z_refdDef(),
                Avg = z_avgDef(),
                FPts = z_fptsDef()
              ),
              details = function(index) {
                poi <- df$Player[index]
                reactable(pptrufflecareerteam[Player == poi][, -c("Pos", "Player")],
                          columns = list(
                            TRUFFLE = z_trfDef(filt=F),
                            Seasons = colDef(minWidth = 90),
                            G = z_gDef(),
                            PaYd = z_paydDef(borderL = T),
                            PaTD = z_patdDef(),
                            PaInt = z_paintDef(),
                            RuYd = z_ruydDef(borderL = T),
                            RuTD = z_rutdDef(),
                            RuFD = z_rufdDef(),
                            ReYd = z_reydDef(borderL = T),
                            ReTD = z_retdDef(),
                            ReFD = z_refdDef(),
                            Avg = z_avgDef(),
                            FPts = z_fptsDef()
                          ))
              }
    )
    
  })
  
  #player portal Contract History
  output$ppcontracthistory <- renderReactable({
    df <- oldrosters[Player %in% input$player,
                     .(TRUFFLE = TRUFFLE[Season == currentyr],
                       Salary = Salary[Season == currentyr],
                       Contract = Contract[Season == currentyr]
                     ),
                     by = .(Player)][order(-Salary)]
    
    df <- rbind(df, ids[Player %in% input$player,
                        .(TRUFFLE = "FA",
                          Salary = 0,
                          Contract = 0),
                        by = .(Player)])
    
    reactable(df,
              pagination = F,
              height = 'auto',
              filterable = F,
              highlight = T,
              #borderless = T,
              compact = T,
              columns = list(
                Player = z_playerDef(minW = 125),
                TRUFFLE = z_trfDef(filt = FALSE),
                Salary = z_salaryDefNobar(title = "$"),
                Contract = z_contractDef(filt = F, title ="Yr")
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
                            TRUFFLE = z_trfDef(filt = F),
                            Salary = z_salaryDefNobar(foot = T, minW = 80),
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
      df <- seasons[Season >= 2020 & Scoring == input$homescoring & Player %in% input$player][order(-Season, -FPts)][, .(Season,Player,G,PaCmp,PaAtt,PaYd,PaTD,PaInt,RuAtt,RuYd,RuTD,RuFD,Tar,Rec,ReYd,ReTD,ReFD,Avg,FPts)]
    } else if (input$ppstatcenterseason == "Proj") 
      df <- proj[Player %in% input$player][order(-Season, -FPts)][, .(Season,Player,G,PaCmp,PaAtt,PaYd,PaTD,PaInt,RuAtt,RuYd,RuTD,RuFD,Tar,Rec,ReYd,ReTD,ReFD,Avg,FPts)]
    else {
      df <- seasons[Season == as.numeric(input$ppstatcenterseason) & Scoring == input$homescoring & Player %in% input$player][order(-Season, -FPts)][, .(Season,Player,G,PaCmp,PaAtt,PaYd,PaTD,PaInt,RuAtt,RuYd,RuTD,RuFD,Tar,Rec,ReYd,ReTD,ReFD,Avg,FPts)]
    }
    
    reactable(df,
              defaultSorted = c("Season", "FPts"),
              pagination = F,
              height = 'auto',
              filterable = F,
              highlight = T,
              #borderless = T,
              #compact = T,
              columns = list(
                Season = z_seasonDef(filt = F),
                Player = z_playerDef(minW = 135),
                G = z_gDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                PaCmp = z_pacmpDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                PaAtt = z_paattDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                PaYd = z_paydDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"), szn = T),
                PaTD = z_patdDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"), szn = T),
                PaInt = z_paintDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"), szn = T),
                RuAtt = z_ruattDef(borderL = T, proj = (isOffseason & input$ppstatcenterseason == "Proj"), szn = T),
                RuYd = z_ruydDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"), szn = T),
                RuTD = z_rutdDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                RuFD = z_rufdDef(disp = !(isOffseason & input$ppstatcenterseason == "Proj")),
                Tar = z_tarDef(borderL = T, proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                Rec = z_recDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                ReYd = z_reydDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                ReTD = z_retdDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                ReFD = z_refdDef(disp = !(isOffseason & input$ppstatcenterseason == "Proj")),
                Avg = z_avgDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                FPts = z_fptsSeasDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"))
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
      df <- advanced[Season >= 2020 & Scoring == input$homescoring & Player %in% input$player][, .(Season,Player,FPts,Touch,Opp,`FPts/Touch`,`FPts/Opp`,YdPts,TDPts,FDPts,RuPts,RePts,`YdPt%`,`TDPt%`,`FDPt%`,`RuPt%`,`RePt%`)][order(-FPts)]
    } else if (input$ppstatcenterseason == "Proj") {
      df <- proj[Player %in% input$player][, .(Season,Player,FPts,Touch,Opp,`FPts/Touch`,`FPts/Opp`,YdPts,TDPts,FDPts,RuPts,RePts,`YdPt%`,`TDPt%`,`FDPt%`,`RuPt%`,`RePt%`)][order(-FPts)]
    } else {
      df <- advanced[Season == as.numeric(input$ppstatcenterseason) & Scoring == input$homescoring & Player %in% input$player][, .(Season,Player,FPts,Touch,Opp,`FPts/Touch`,`FPts/Opp`,YdPts,TDPts,FDPts,RuPts,RePts,`YdPt%`,`TDPt%`,`FDPt%`,`RuPt%`,`RePt%`)][order(-FPts)]
    }
    
    reactable(df,
              defaultSorted = c("FPts"),
              pagination = F,
              height = 'auto',
              filterable = F,
              highlight = T,
              columns = list(
                Season = z_seasonDef(filt = F),
                Player = z_playerDef(minW = 120),
                FPts = z_fptsSeasDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                Touch = z_tchDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                Opp = z_oppDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `FPts/Touch` = z_fptsPtchDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `FPts/Opp` = z_fptsPoppDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                YdPts = z_ydptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                TDPts = z_tdptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                FDPts = z_fdptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"), disp = !(isOffseason & input$ppstatcenterseason == "Proj")),
                RuPts = z_ruptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                RePts = z_reptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `YdPt%` = z_ydptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `TDPt%` = z_tdptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `FDPt%` = z_fdptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"), disp = !(isOffseason & input$ppstatcenterseason == "Proj")),
                `RuPt%` = z_ruptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `RePt%` = z_reptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"))
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
    reactable(df[Scoring == input$homescoring & Player %in% input$player][order(-Avg)][, -c("Scoring","TRUFFLE","Pos")],
              defaultSorted = c("Avg"),
              pagination = F,
              height = 'auto',
              filterable = F,
              highlight = T,
              #borderless = T,
              #compact = T,
              columns = list(
                Season = z_seasonDef(filt = F),
                Player = z_playerDef(minW = 125),
                G = z_gDef(),
                Avg = z_avgDef(),
                RelSD = z_relsdDef,
                AvgPosRk = z_avgposrkDef,
                `Top5 %` = z_top5pDef,
                `Top12 %` = z_top12pDef,
                `Top24 %` = z_top24pDef,
                `Top36 %` = z_top36pDef,
                `NonStart %` = z_nonstartpDef,
                `>10 %` = z_g10pDef,
                `>20 %` = z_g20pDef,
                `>30 %` = z_g30pDef
              ),
              columnGroups = list(
                colGroup(name = "Fantasy Points", columns = c("Avg","RelSD",">10 %",">20 %",">30 %"), align = 'left'),
                colGroup(name = "Weekly Position Rank", columns = c("AvgPosRk","Top5 %","Top12 %","Top24 %","Top36 %", "NonStart %"), align = 'left')
              )
    )
  })
  
  #player portal extra dash
  output$ppextradash <- renderReactable({
    reactable(extradashszn[Player %in% input$player][order(TotYd) & Season == as.numeric(input$ppstatcenterseason)][, !c("TRUFFLE", "Pos")],
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
                Season = z_seasonDef(),
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(filt = F),
                G = z_gDef(),
                `Cmp%` = colDef(header = z_with_tt("Cmp%", "Completion Percentage"), format = colFormat(percent = T), minWidth = 75),
                Pa20 = colDef(header = z_with_tt("20+", "Passing Completions of 20+ Yd"), class = "border-left-grey"),
                Pa40 = colDef(header = z_with_tt("40+", "Passing Completions of 40+ Yd")),
                RuYPC = colDef(header = z_with_tt("YPC", "Rushing Yards per Carry"), class = "border-left-grey", minWidth = 60),
                Ru20 = colDef(header = z_with_tt("20+", "Rushes of 20+ Yd")),
                Tar = colDef(header = z_with_tt("Tar", "Targets"), class = "border-left-grey"),
                `Tar%` = colDef(header = z_with_tt("Tar%", "Percentage of Team Targets"), minWidth = 70, format = colFormat(suffix = "%")),
                ReYPC = colDef(header = z_with_tt("YPC", "Yards per Catch"), minWidth = 60),
                Re20 = colDef(header = z_with_tt("20+", "Receptions of 20+ Yd")),
                Re40 = colDef(header = z_with_tt("40+", "Receptions of 40+ Yd")),
                `ReFD%` = colDef(header = z_with_tt("FD%", "Percentage of Receptions resulting in First Down"), minWidth = 65, format = colFormat(percent = T)),
                TotYd = colDef(header = z_with_tt("TotYd", "Total Passing/Rushing/Receiving Yards"), minWidth = 70)
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
    if(input$ppstatcenterseason == "All") {
      df <- espn
    } else {
      df <- espn[Season == as.numeric(input$ppstatcenterseason)]
    }
    
    reactable(df[Player %in% input$player][order(-xFP)][, !c("TRUFFLE", "Pos")],
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
                Season = z_seasonDef(),
                Player = z_playerDef(minW = 125),
                xFP = colDef(header = z_with_tt("xFP", "Expected ESPN Fantasy Points")),
                ActualPts = colDef(header = z_with_tt("aFP", "Actual ESPN Fantasy Points")),
                FPDiff = colDef(header = z_with_tt("Diff", "Difference between the player's total xFP and actual FP")),
                xTD = colDef(header = z_with_tt("xTD", "Expected Touchdowns"), class = "border-left-grey"),
                TD = colDef(header = z_with_tt("aTD", "Actual Touchdowns")),
                TDDiff = colDef(header = z_with_tt("Diff", "Difference between the player's total xTD and actual TD")),
                Looks = colDef(header = z_with_tt("Looks", "Carries + Targets")),
                In5 = colDef(header = z_with_tt("In5", "Carries inside 5-yard line")),
                EZ = colDef(header = z_with_tt("EZ", "End Zone Targets"))
              )
    )
  })
  
  #player portal snap share
  output$ppsnapshare <- renderReactable({
    
    if(input$ppstatcenterseason == "All") {
      df <- snaps
    } else {
      df <- snaps[Season == as.numeric(input$ppstatcenterseason)]
    }
    
    reactable(df[Player %in% input$player][order(-Tot)][, !c("TRUFFLE", "Pos", "Team")],
              pagination = F,
              defaultSorted = c("Tot"),
              defaultSortOrder = "desc",
              height = 'auto',
              filterable = F,
              highlight = T,
              defaultColDef = colDef(
                minWidth = 48,
                align = "center",
                format = colFormat(percent = T)
              ),
              columns = list(
                Season = z_seasonDef(),
                Player = z_playerDef(minW = 120),
                `18` = colDef(class = "border-right-grey"),
                Tot = colDef(minWidth = 50, format = colFormat(percent = F))
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
        df <- as.data.frame(fullradar[Scoring == input$homescoring & (Player %in% input$player | Player == "MAX" | Player == "MIN") & (Season == input$ppradarplotseason & Pos == selectedposition[1]), ][, !"Scoring"])
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
      ggplotly(ggplot(weekly[Scoring == input$homescoring],aes(Week,FPts)) + geom_blank() +theme_minimal() + scale_x_continuous(labels=as.character(0:18),breaks=c(0:18)))
    } else {
      df <- weekly[Scoring == input$homescoring & Season == input$ppwbwseason & Player %in% input$player]
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
    if(input$ppstatcenterseason == "All") {
      df <- weekly_no_teams
    } else {
      df <- weekly_no_teams[Season == as.numeric(input$ppgamelogsseason)]
    }
    
    reactable(df[Scoring == input$homescoring & Player %in% input$player][order(Season, Week, -FPts)][, !c("Scoring", "PaCmp", "PaAtt", "NFL", "Avg", "FL", "PosRk")],
              pagination = F,
              height = 'auto',
              filterable = F,
              highlight = T,
              #borderless = T,
              compact = T,
              columns = list(
                Season = z_seasonDef(filt = F),
                Week = z_weekDef,
                Pos = z_posDef(filt = F),
                Player = z_playerDef(minW = 135, filt = F),
                Opp = z_opDef,
                OpRk = z_oprkDef,
                PaYd = z_paydDef(wk = T),
                PaTD = z_patdDef(wk = T),
                PaInt = z_paintDef(wk = T),
                RuAtt = z_ruattDef(wk = T),
                RuYd = z_ruydDef(wk = T),
                RuTD = z_rutdDefWk,
                RuFD = z_rufdDefWk,
                Tar = z_tarDefWk,
                Rec = z_recDefWk,
                ReYd = z_reydDefWk,
                ReTD = z_retdDefWk,
                ReFD = z_refdDefWk,
                FPts = z_fptsWeekDef()
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
    
    reactable(df[Scoring == input$homescoring & Player %in% input$player][order(Season, Week, -FPts)][, !c("Scoring","PaAtt", "PaCmp", "Opp", "OpRk", "Tar","NFL", "Avg")],       
              pagination = F,
              height = 'auto',
              filterable = F,
              highlight = T,
              #borderless = T,
              compact = T,
              columns = list(
                Season = z_seasonDef(),
                Week = z_weekDef,
                TRUFFLE = z_trfDef(filt = F),
                Pos = z_posDef(filt = F),
                Player = z_playerDef(minW = 150, filt = F),
                PaYd = z_paydDef(wk = T),
                PaTD = z_patdDef(wk = T),
                PaInt = z_paintDef(wk = T),
                RuAtt = z_ruattDef(wk = T),
                RuYd = z_ruydDef(wk = T),
                RuTD = z_rutdDefWk,
                RuFD = z_rufdDefWk,
                Rec = z_recDefWk,
                ReYd = z_reydDefWk,
                ReTD = z_retdDefWk,
                ReFD = z_refdDefWk,
                FL = z_flDef,
                FPts = z_fptsWeekDef()
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  FPts = colDef(minWidth = 150, align = 'left',
                                format = colFormat(digits=1),
                                cell = function(value) {
                                  width <- paste0(value / max(truffleanalysis$FPts) * 100, "%")
                                  z_bar_chart(round(value,0), width = width)
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
                  TRUFFLE = z_trfDef(filt = FALSE),
                  FPts = colDef(minWidth = 150, align = 'left',
                                format = colFormat(digits=1),
                                cell = function(value) {
                                  width <- paste0(value / max(truffleanalysis$FPts) * 100, "%")
                                  z_bar_chart(round(value,0), width = width)
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
    
    if (isOffseason & input$scseason == "Proj") {
      boxscorerange <- proj
      boxscorerange$TRUFFLEdum <- ifelse(boxscorerange$TRUFFLE == "FA", "FA", "Owned")
    } else {
    
    boxscorerange <- weekly[Scoring == input$homescoring & Season == input$scseason &
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
    
    }
    
    boxscorerange <- z_action_mod(df = boxscorerange, team = globalteam)
    
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
                Action = colDef(show = !isguest,
                                header = z_with_tt("A", "Action link to add, drop, or trade player"),
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
                TRUFFLE = z_trfDef(sort = F, maxW = 60),
                Pos = z_posDef(sort = F, maxW = 38),
                Player = z_playerDef(minW = 125, filt = T),
                G = z_gDef(proj = (isOffseason & input$scseason == "Proj")),
                PaCmp = z_pacmpDef(proj = (isOffseason & input$scseason == "Proj")),
                PaAtt = z_paattDef(proj = (isOffseason & input$scseason == "Proj")),
                PaYd = z_paydDef(proj = (isOffseason & input$scseason == "Proj"), szn = T),
                PaTD = z_patdDef(proj = (isOffseason & input$scseason == "Proj"), szn = T),
                PaInt = z_paintDef(proj = (isOffseason & input$scseason == "Proj"), szn = T),
                RuAtt = z_ruattDef(borderL = T, proj = (isOffseason & input$scseason == "Proj"), szn = T),
                RuYd = z_ruydDef(proj = (isOffseason & input$scseason == "Proj"), szn = T),
                RuTD = z_rutdDef(proj = (isOffseason & input$scseason == "Proj")),
                RuFD = z_rufdDef(disp = !(isOffseason & input$scseason == "Proj")),
                Tar = z_tarDef(borderL = T, proj = (isOffseason & input$scseason == "Proj")),
                Rec = z_recDef(proj = (isOffseason & input$scseason == "Proj")),
                ReYd = z_reydDef(proj = (isOffseason & input$scseason == "Proj")),
                ReTD = z_retdDef(proj = (isOffseason & input$scseason == "Proj")),
                ReFD = z_refdDef(disp = !(isOffseason & input$scseason == "Proj")),
                Avg = z_avgDef(proj = (isOffseason & input$scseason == "Proj"), borderL = T, maxW = 65),
                FPts = z_fptsSeasDef(proj = (isOffseason & input$scseason == "Proj"), col = F, maxW = 65)
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
    
    if (isOffseason & input$scseason == "Proj") {
      advancedrange <- proj
      advancedrange$TRUFFLEdum <- ifelse(advancedrange$TRUFFLE == "FA", "FA", "Owned")
    } else {
    
    advancedrange <- weekly[Season == input$scseason & Week %in% seq(input$scweekrange[1],input$scweekrange[2])
    ][,
      .(Avg = round(mean(FPts, na.rm = T),2),
        FPts = sum(FPts),
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
      by = .(Scoring,TRUFFLE,Pos,Player)][, `:=`(`YdPt%` = YdPts / FPts,
                                                 `TDPt%` = TDPts / FPts,
                                                 `FDPt%` = FDPts / FPts,
                                                 `RuPt%` = RuPts / FPts,
                                                 `RePt%` = RePts / FPts,
                                                 `FPts/Touch` = round(FPts/Touch, 3),
                                                 `FPts/Opp` = round(FPts/Opp, 3),
                                                 TRUFFLEdum = ifelse(TRUFFLE == "FA", "FA", "Owned")
      )][Scoring == input$homescoring & TRUFFLEdum %in% input$scavailable & Avg >= input$scavgmin & Pos %in% input$scpositions][order(-Avg)][, !c("Scoring","TRUFFLEdum")]
    
    }
    
    advancedrange <- z_action_mod(df = advancedrange, team = globalteam)
    
    reactable(advancedrange[, .(Action,TRUFFLE,Pos,Player,Touch,Opp,`FPts/Touch`,`FPts/Opp`,YdPts,TDPts,FDPts,`YdPt%`,`TDPt%`,`FDPt%`,RuPts,
                                RePts,`RuPt%`,`RePt%`,Avg)],
              paginationType = "jump",
              showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
              pageSizeOptions = c(10, 20, 50, 100),
              defaultSorted = c("Avg"),
              height = 'auto',
              filterable = F,
              highlight = T,
              compact = T,
              columns = list(
                Action = colDef(show = !isguest,
                                header = z_with_tt("A", "Action link to add, drop, or trade player"),
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(minW = 120, filt = T),
                Touch = z_tchDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                Opp = z_oppDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `FPts/Touch` = z_fptsPtchDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `FPts/Opp` = z_fptsPoppDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                YdPts = z_ydptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                TDPts = z_tdptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                FDPts = z_fdptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"), disp = !(isOffseason & input$ppstatcenterseason == "Proj")),
                RuPts = z_ruptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                RePts = z_reptsDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `YdPt%` = z_ydptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `TDPt%` = z_tdptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `FDPt%` = z_fdptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"), disp = !(isOffseason & input$ppstatcenterseason == "Proj")),
                `RuPt%` = z_ruptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj")),
                `RePt%` = z_reptpercDef(proj = (isOffseason & input$ppstatcenterseason == "Proj"))
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
    z_perccolwidth <- 60
    consistencyrange <- consistencystart[Scoring == input$homescoring & Season == input$scseason &
                                           Week %in% seq(input$scweekrange[1],input$scweekrange[2])
    ][,
      .(G = .N,
        FPts = sum(FPts, na.rm = T),
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
    
    consistencyrange <- z_action_mod(df = consistencyrange, team = globalteam)
    
    reactable(consistencyrange[, .(Action,TRUFFLE,Pos,Player,G,Avg,RelSD,
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
                Action = colDef(show = !isguest,
                                header = z_with_tt("A", "Action link to add, drop, or trade player"),
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                G = z_gDef(),
                Player = z_playerDef(minW = 125, filt = T),
                Avg = z_avgDef(maxW = z_perccolwidth, borderL = T),
                RelSD = z_relsdDef,
                AvgPosRk = z_avgposrkDef,
                `Top5 %` = z_top5pDef,
                `Top12 %` = z_top12pDef,
                `Top24 %` = z_top24pDef,
                `Top36 %` = z_top36pDef,
                `NonStart %` = z_nonstartpDef,
                `>10 %` = z_g10pDef,
                `>20 %` = z_g20pDef,
                `>30 %` = z_g30pDef
                
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
    
    extradashrange <- z_action_mod(extradashrange, team = globalteam)
    
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
                Action = colDef(show = !isguest,
                                header = z_with_tt("A", "Action link to add, drop, or trade player"),
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(filt = T),
                G = z_gDef(),
                `Cmp%` = colDef(header = z_with_tt("Cmp%", "Completion Percentage"), format = colFormat(percent = T), minWidth = 75, class = "border-left-grey"),
                Pa20 = colDef(header = z_with_tt("20+", "Passing Completions of 20+ Yd")),
                Pa40 = colDef(header = z_with_tt("40+", "Passing Completions of 40+ Yd")),
                RuYPC = colDef(header = z_with_tt("YPC", "Rushing Yards per Carry"), class = "border-left-grey", minWidth = 60),
                Ru20 = colDef(header = z_with_tt("20+", "Rushes of 20+ Yd")),
                Tar = colDef(header = z_with_tt("Tar", "Targets"), class = "border-left-grey"),
                `Tar%` = colDef(header = z_with_tt("Tar%", "Percentage of Team Targets"), minWidth = 70, format = colFormat(suffix = "%")),
                ReYPC = colDef(header = z_with_tt("YPC", "Yards per Catch"), minWidth = 60),
                Re20 = colDef(header = z_with_tt("20+", "Receptions of 20+ Yd")),
                Re40 = colDef(header = z_with_tt("40+", "Receptions of 40+ Yd")),
                `ReFD%` = colDef(header = z_with_tt("FD%", "Percentage of Receptions resulting in First Down"), minWidth = 65, format = colFormat(percent = T)),
                TotYd = colDef(header = z_with_tt("TotYd", "Total Passing/Rushing/Receiving Yards"), minWidth = 70)
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
    espnsc <-espn[Season == input$scseason][order(-xFP)][, !c("Season")]
    espnsc$TRUFFLEdum <- ifelse(espnsc$TRUFFLE == "FA", "FA", "Owned")
    espnsc <- z_action_mod(espnsc, team = globalteam)
    espnsc <- espnsc[, .(Action,TRUFFLE,Pos,Player,xFP,ActualPts,FPDiff,xTD,TD,TDDiff,Looks,In5,EZ,TRUFFLEdum,playerID,TeamNum,ActionLink)]
    
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
                Action = colDef(show = !isguest,
                                header = z_with_tt("A", "Action link to add, drop, or trade player"),
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(minW = 125, filt = T),
                NFL = z_nflDef,
                xFP = colDef(header = z_with_tt("xFP", "Expected ESPN Fantasy Points")),
                ActualPts = colDef(header = z_with_tt("aFP", "Actual ESPN Fantasy Points")),
                FPDiff = colDef(header = z_with_tt("Diff", "Difference between the player's total xFP and actual FP")),
                xTD = colDef(header = z_with_tt("xTD", "Expected Touchdowns"), class = "border-left-grey"),
                TD = colDef(header = z_with_tt("aTD", "Actual Touchdowns")),
                TDDiff = colDef(header = z_with_tt("Diff", "Difference between the player's total xTD and actual TD")),
                Looks = colDef(header = z_with_tt("Looks", "Carries + Targets")),
                In5 = colDef(header = z_with_tt("In5", "Carries inside 5-yard line")),
                EZ = colDef(header = z_with_tt("EZ", "End Zone Targets"))
              )
    )
  })
  
  #stat center snap share
  output$scsnapshare <- renderReactable({
    snapssc <- snaps[Season == input$scseason][order(-Tot)][, !c("Season")]
    snapssc$TRUFFLEdum <- ifelse(snapssc$TRUFFLE == "FA", "FA", "Owned")
    snapssc <- z_action_mod(snapssc, team = globalteam)
    snapssc <- snapssc[, c(28, 24, 2, 3, 1, 4:23, 25:27, 29)]
    
    reactable(snapssc[TRUFFLEdum %in% input$scavailable & Pos %in% input$scpositions][, !c("TRUFFLEdum", "playerID", "TeamNum", "ActionLink", "Team")],
              paginationType = "jump",
              showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
              defaultSorted = c("Tot"),
              defaultSortOrder = "desc",
              pageSizeOptions = c(10, 20, 50, 100),
              height = 'auto',
              filterable = F,
              highlight = T,
              compact = T,
              defaultColDef = colDef(
                minWidth = 48,
                align = "center",
                format = colFormat(percent = T)
              ),
              columns = list(
                Action = colDef(show = !isguest,
                                header = z_with_tt("A", "Action link to add, drop, or trade player"),
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(minW = 125, filt = T),
                Team = z_nflDef,
                `18` = colDef(class = "border-right-grey"),
                Tot = colDef(minWidth = 50, format = colFormat(percent = F))
              )
    )
  })
  
  #byog ----
  #create week by week runchart
  output$byog <- renderPlotly({
    if(input$byogseason == "All") {
      df <- byog
    } else {
      df <- byog[Season == as.numeric(input$byogseason)]
    }
    
    df <- df[Scoring == input$homescoring & Pos %in% input$byogpositions & G > input$byoggamesmin & Avg > input$byogavgmin]
    df$X <- df[[input$byogX]]
    df$Y <- df[[input$byogY]]
    ggplotly(ggplot(df, aes(x = X, y = Y, group = Player,  color = Pos,
                            text = paste0("</br><i>Season:</i> <b>", Season,
                                          "</b></br><i>Player:</i> <b>", Player,
                                          "</b></br><i>", input$byogX, "</i>: <b>", X,
                                          "</b></br><i>", input$byogY, "</i>: <b>", Y, "</b>")
    )) +
      geom_point(size = 2) +
      xlim(min(df[[input$byogX]], na.rm=T), max(df[[input$byogX]], na.rm=T)) + ylim(min(df[[input$byogY]], na.rm=T), max(df[[input$byogY]], na.rm=T)) +
      ylab(input$byogY) + xlab(input$byogX) +
      theme_minimal() + scale_color_manual(values=ggpal), tooltip = "text"
    )
  })
  
  # df <- byog[Scoring == "PPFD" & Season == 2022 & Pos %in% c("QB", "RB", "WR", "TE") & G > 1 & Avg > 5]
  # df$X <- df[["Salary"]]
  # df$Y <- df[["FPts"]]
  # df <- df[, .(Pos, Player, X, Y)]
  # test1 <- "Salary"
  # test2 <- "FPts"
  # 
  # ggpal <- c(QB = "#b7e1cd", RB = "#f4cccc", WR = "#9633FF", "#E2FF33")
  # ggplotly(ggplot(df, aes(x = X, y = Y, group = Player,
  #                         text = paste0("</br><i>Season:</i> <b>", Season,
  #                                      "</b></br><i>Player:</i> <b>", Player,
  #                                      "</b></br><i>", test1, "</i>: <b>", X,
  #                                      "</b></br><i>", test2, "</i>: <b>", Y, "</b>")
  #                         )) +
  #            geom_point(aes(col = Pos), size = 2) +
  #            xlim(min(df[["Salary"]], na.rm=T), max(df[["Salary"]], na.rm=T)) + ylim(min(df[["FPts"]], na.rm=T), max(df[["FPts"]], na.rm=T)) +
  #            ylab("FPts") + xlab("Salary") +
  #            theme_minimal() + scale_color_manual(values=ggpal), tooltip = "text"
  # )
  
  #trademachine ----
  
  tmoverviewtm1 <- reactive(tpoverview[Scoring == input$homescoring & TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm1]][order(match(Pos, positionorder), -Salary, Player)])
  tmoverviewtm2 <- reactive(tpoverview[Scoring == input$homescoring & TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm2]][order(match(Pos, positionorder), -Salary, Player)])
  contractstm1 <- reactive(contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm1]][order(match(Pos, positionorder), -Salary, Player)][selectedtm1(), ])
  contractstm2 <- reactive(contracts[TRUFFLE == teams$Abbrev[teams$FullName == input$tmtm2]][order(match(Pos, positionorder), -Salary, Player)][selectedtm2(), ])
  
  output$tmtm1 <- renderReactable({
    #formatted reactable output
    reactable(tmoverviewtm1()[,!c("Scoring","TRUFFLE", "G", "Bye", "PosRk")],
              selection = "multiple", onClick = "select",
              defaultSortOrder = "desc",
              sortable = F,
              pagination = FALSE,
              highlight = T,
              filterable = F,
              #borderless = T,
              compact = T,
              columns = list(
                Pos = z_posDef(maxW = 38, filt = FALSE),
                Player = z_playerDef(minW = 125),
                Age = colDef(minWidth =  40),
                NFL = colDef(minWidth =  40),
                Salary = z_salaryDefNobar(minW = 45, foot = T),
                Contract = z_contractDef(minW = 30, foot = T, title ="Yr", filt = F),
                ptslog = z_ptsLogDef(maxW = 70),
                Avg = z_avgDef(maxW = 45),
                FPts = z_fptsSeasDef(maxW = 50)
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    )
  })
  
  output$tmtm2 <- renderReactable({
    reactable(tmoverviewtm2()[,!c("Scoring","TRUFFLE", "G", "Bye", "PosRk")],
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
                Pos = z_posDef(maxW = 38, filt = FALSE),
                Player = z_playerDef(minW = 125),
                Age = colDef(minWidth =  40),
                NFL = colDef(minWidth =  40),
                Salary = z_salaryDefNobar(minW = 45, foot = T),
                Contract = z_contractDef(minW = 30, foot = T, title ="Yr", filt = F),
                ptslog = z_ptsLogDef(maxW = 70),
                Avg = z_avgDef(maxW = 45),
                FPts = z_fptsSeasDef(maxW = 50)
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
      
      reactable(contractstm1()[, !c("TRUFFLE", "Age", "TagVal")],
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
                  Pos = z_posDef(maxW = 38, filt = FALSE),
                  Player = z_playerDef(minW = 125),
                  NFL = colDef(minWidth =  40),
                  Salary = z_salaryDefNobar(minW = 45, foot = T),
                  Contract = z_contractDef(minW = 30, foot = T, title ="Yr", filt = F),
                  `'24` = z_futureColDef(yr = "'24", maxW = 60, foot = T, filt = F),
                  `'25` = z_futureColDef(yr = "'25", maxW = 60, foot = T, filt = F),
                  `'26` = z_futureColDef(yr = "'26", maxW = 60, foot = T, filt = F),
                  `'27` = z_futureColDef(yr = "'27", maxW = 60, foot = T, filt = F),
                  `'28` = z_futureColDef(yr = "'28", maxW = 60, foot = T, filt = F)
                ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      )
    }
  })
  
  output$tmpls2 <- renderReactable({
    
    
    if (length(selectedtm2()) >= 1) {
      
      reactable(contractstm2()[, !c("TRUFFLE", "Age", "TagVal")],
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
                  Pos = z_posDef(maxW = 38, filt = FALSE),
                  Player = z_playerDef(minW = 125),
                  NFL = colDef(minWidth =  40),
                  Salary = z_salaryDefNobar(minW = 45, foot = T),
                  Contract = z_contractDef(minW = 30, foot = T, title ="Yr", filt = F),
                  Y1 = z_futureColDef(yr = "'24", maxW = 60, foot = T, filt = F),
                  Y2 = z_futureColDef(yr = "'25", maxW = 60, foot = T, filt = F),
                  Y3 = z_futureColDef(yr = "'26", maxW = 60, foot = T, filt = F),
                  Y4 = z_futureColDef(yr = "'27", maxW = 60, foot = T, filt = F),
                  Y5 = z_futureColDef(yr = "'28", maxW = 60, foot = T, filt = F)
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
                TRF = z_trfDef(filt = FALSE),
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
    reactable(contracts[, !c("Extension","TagVal")],
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
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(filt = T),
                Age = z_ageDef,
                NFL = z_nflDef,
                Salary = z_salaryDefBar(),
                Contract = z_contractDef(title ="Yr"),
                Y1 = z_futureColDef(yr = "'24"),
                Y2 = z_futureColDef(yr = "'25"),
                Y3 = z_futureColDef(yr = "'26"),
                Y4 = z_futureColDef(yr = "'27"),
                Y5 = z_futureColDef(yr = "'28")
              ),
              columnGroups = list(
                colGroup(name = "Financials", columns = c("Salary", "Contract"), align = 'left'),
                colGroup(name = "Future Seasons", columns = c("Y1","Y2","Y3","Y4","Y5"), align = 'left')
              )
    )
  })
  
  #plot1
  output$plot1 <- renderPlotly({
    plot_ly(
      data = capbyteam,
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
      add_annotations(data = capbyteam %>% select(TRUFFLE, TeamSalary) %>% unique(),
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
                  reactable(extvaldraft[Round == 1 & `#` <= 6 & Pos == "QB" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 2) {
                  reactable(extvaldraft[Round == 1 & `#` >= 7 & Pos == "QB" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 3) {
                  reactable(extvaldraft[Round == 2 & Pos == "QB" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 4) {
                  reactable(extvaldraft[Round == 3 & Pos == "QB" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
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
                  reactable(extvaldraft[Round == 1 & `#` <= 6 & Pos == "RB" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 2) {
                  reactable(extvaldraft[Round == 1 & `#` >= 7 & Pos == "RB" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 3) {
                  reactable(extvaldraft[Round == 2 & Pos == "RB" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 4) {
                  reactable(extvaldraft[Round == 3 & Pos == "RB" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
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
                  reactable(extvaldraft[Round == 1 & `#` <= 6 & Pos == "WR" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 2) {
                  reactable(extvaldraft[Round == 1 & `#` >= 7 & Pos == "WR" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 3) {
                  reactable(extvaldraft[Round == 2 & Pos == "WR" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 4) {
                  reactable(extvaldraft[Round == 3 & Pos == "WR" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
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
                  reactable(extvaldraft[Round == 1 & `#` <= 6 & Pos == "TE" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 2) {
                  reactable(extvaldraft[Round == 1 & `#` >= 7 & Pos == "TE" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 3) {
                  reactable(extvaldraft[Round == 2 & Pos == "TE" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
                            )
                  )
                } else if (index == 4) {
                  reactable(extvaldraft[Round == 3 & Pos == "TE" & Player %in% rookierights$Player][, c("Player", "Pick", "ExtensionYr")], 
                            columns = list(
                              Player = z_playerDef(minW = 150),
                              Pick = colDef(minWidth = 50, align = 'right'),
                              ExtensionYr = colDef(header = z_with_tt("Ext Yr", "Year Player is Rookie Extension Eligible"), minWidth = 50)
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
                Type = colDef(header = z_with_tt("Tag", "First vs. Second time franchising player"),
                              minWidth = 50),
                TagVal = z_tagvalDefBar(minW = 100)
              ),
              columnGroups = list(
                colGroup(name = "QB", columns = c("Type", "TagVal"), align = 'left')
              ),
              details = function(index) {
                if (index == 1) {
                  reactable(top5paid[Pos == "QB" & Season == currentyr + 1][, c("Player", "Salary")], 
                            columns = list(
                              Player = colDef(minWidth = 150, footer = "Mean"),
                              Salary = z_tagvalDefNobar()
                            ),
                            defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                  )
                } else if (index == 2) {
                  reactable(top5paid[Pos == "QB" & Season == currentyr + 1][, c("Player", "Salary")][1], 
                            columns = list(
                              Player = colDef(minWidth = 150),
                              Salary = z_tagvalDefNobar(foot = F)
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
                Type = colDef(header = z_with_tt("Tag", "First vs. Second time franchising player"),
                              minWidth = 50),
                TagVal = z_tagvalDefBar(minW = 100)
              ),
              columnGroups = list(
                colGroup(name = "RB", columns = c("Type", "TagVal"), align = 'left')
              ),
              details = function(index) {
                if (index == 1) {
                  reactable(top5paid[Pos == "RB" & Season == currentyr + 1][, c("Player", "Salary")], 
                            columns = list(
                              Player = colDef(minWidth = 150, footer = "Mean"),
                              Salary = z_tagvalDefNobar()
                            ),
                            defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                  )
                } else if (index == 2) {
                  reactable(top5paid[Pos == "RB" & Season == currentyr + 1][, c("Player", "Salary")][1], 
                            columns = list(
                              Player = colDef(minWidth = 150),
                              Salary = z_tagvalDefNobar(foot = F)
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
                Type = colDef(header = z_with_tt("Tag", "First vs. Second time franchising player"),
                              minWidth = 50),
                TagVal = z_tagvalDefBar(minW = 100)
              ),
              columnGroups = list(
                colGroup(name = "WR", columns = c("Type", "TagVal"), align = 'left')
              ),
              details = function(index) {
                if (index == 1) {
                  reactable(top5paid[Pos == "WR" & Season == currentyr + 1][, c("Player", "Salary")], 
                            columns = list(
                              Player = colDef(minWidth = 150, footer = "Mean"),
                              Salary = z_tagvalDefNobar()
                            ),
                            defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                  )
                } else if (index == 2) {
                  reactable(top5paid[Pos == "WR" & Season == currentyr + 1][, c("Player", "Salary")][1], 
                            columns = list(
                              Player = colDef(minWidth = 150),
                              Salary = z_tagvalDefNobar(foot = F)
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
                Type = colDef(header = z_with_tt("Tag", "First vs. Second time franchising player"),
                              minWidth = 50),
                TagVal = z_tagvalDefBar(minW = 100)
              ),
              columnGroups = list(
                colGroup(name = "TE", columns = c("Type", "TagVal"), align = 'left')
              ),
              details = function(index) {
                if (index == 1) {
                  reactable(top5paid[Pos == "TE" & Season == currentyr + 1][, c("Player", "Salary")], 
                            columns = list(
                              Player = colDef(minWidth = 150, footer = "Mean"),
                              Salary = z_salaryDefNobar()
                            ),
                            defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                  )
                } else if (index == 2) {
                  reactable(top5paid[Pos == "TE" & Season == currentyr + 1][, c("Player", "Salary")][1], 
                            columns = list(
                              Player = colDef(minWidth = 150),
                              Salary = z_salaryDefNobar(foot = F)
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
                Pos = z_posDef(filt = F),
                Player = z_playerDef(minW = 125),
                Salary = z_salaryDefBar(minW = 125),
                Contract = z_contractDef(filt = F, title ="Yr"),
                TagVal = colDef(header = z_with_tt("Tag Value", "Player Tag Value for Next Year"),
                                minWidth = 100,
                                align = "right")
              )
    )
  })
  
  #history books ----
  #record books rings
  output$recordrings <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(rings[Rings > 0, !c("BenchCups","BCYears","BCTeams")][order(-Rings)],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 40, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  Rings = colDef(minWidth = 60, align = "left"),
                  RingYears = colDef(name = "Years"),
                  RingTeams = colDef(name = "Teams")
                ),
                columnGroups = list(colGroup(name = "TRUFFLE Championships", columns = c("Pos", "Player", "Rings","RingYears","RingTeams"), align = 'left')
                )
      )
    } else {
      reactable(ringsbyteam[Rings > 0 & TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams], !c("TRUFFLE","BenchCups","BCYears","BCTeams")][order(-Rings)],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 40, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  Rings = colDef(minWidth = 60, align = "left"),
                  RingYears = colDef(name = "Years")
                ),
                columnGroups = list(colGroup(name = "TRUFFLE Championships", columns = c("Pos", "Player", "Rings","RingYears"), align = 'left')
                ))
    }
  })
  #record books rings
  output$recordbenchcups <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(rings[BenchCups > 0, !c("Rings","RingYears","RingTeams")][order(-BenchCups)],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 40, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  BenchCups = colDef(name = "Cups", minWidth = 60, align = "left"),
                  BCYears = colDef(name = "Years"),
                  BCTeams = colDef(name = "Teams")
                ),
                columnGroups = list(colGroup(name = "TRUFFLE Bench Cups", columns = c("Pos", "Player", "BenchCups","BCYears","BCTeams"), align = 'left')
                )
      )
    } else {
      reactable(ringsbyteam[TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] & BenchCups > 0, !c("TRUFFLE","Rings","RingYears","RingTeams")][order(-BenchCups)],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 40, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  BenchCups = colDef(name = "Cups", minWidth = 60, align = "left"),
                  BCYears = colDef(name = "Years")
                ),
                columnGroups = list(colGroup(name = "TRUFFLE Bench Cups", columns = c("Pos", "Player", "BenchCups", "BCYears"), align = 'left')
                ))
    }
  })
  #record books fantasy points
  output$recordfpts <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,FPts)][order(-FPts)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  FPts = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Fantasy Points", columns = c("Pos", "Player", "FPts"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,FPts)][order(-FPts)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  FPts = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Fantasy Points", columns = c("Pos", "Player", "FPts"), align = 'left')
                ))
    }
  })
  #record books games
  output$recordgames <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,Games)][order(-Games)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  Games = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Games Played", columns = c("Pos", "Player", "Games"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,Games)][order(-Games)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  Games = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Games Played", columns = c("Pos", "Player", "Games"), align = 'left')
                ))
    }
  })
  #record books avg
  output$recordavg <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,Games,Avg)][Games >= if (globalleague == "TRUFFLE") {10} else {1}][, .(Pos, Player, Avg)][order(-Avg)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  Avg = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Average Fantasy Points", columns = c("Pos", "Player", "Avg"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,Games,Avg)][Games > 10][, .(TRUFFLE, Pos, Player, Avg)][order(-Avg)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  Avg = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Average Fantasy Points", columns = c("Pos", "Player", "Avg"), align = 'left')
                ))
    }
  })
  #record books first downs
  output$recordfd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,FD)][order(-FD)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  FD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "First Downs", columns = c("Pos", "Player", "FD"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,FD)][order(-FD)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  FD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "First Downs", columns = c("Pos", "Player", "FD"), align = 'left')
                ))
    }
  })
  #record books passing yards
  output$recordpayd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,PaYd)][order(-PaYd)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  PaYd = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Passing Yards", columns = c("Pos", "Player", "PaYd"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,PaYd)][order(-PaYd)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  PaYd = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Passing Yards", columns = c("Pos", "Player", "PaYd"), align = 'left')
                ))
    }
  })
  #record books passing td
  output$recordpatd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,PaTD)][order(-PaTD)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  PaTD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Passing Touchdowns", columns = c("Pos", "Player", "PaTD"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,PaTD)][order(-PaTD)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  PaTD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Passing Touchdowns", columns = c("Pos", "Player", "PaTD"), align = 'left')
                ))
    }
  })
  #record books interceptions
  output$recordpaint <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,PaInt)][order(-PaInt)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  PaInt = colDef(name = "Int", minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Interceptions", columns = c("Pos", "Player", "PaInt"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,PaInt)][order(-PaInt)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  PaInt = colDef(name = "Int",minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Interceptions", columns = c("Pos", "Player", "PaInt"), align = 'left')
                ))
    }
  })
  #record books completions
  output$recordpacmp <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,PaCmp)][order(-PaCmp)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  PaCmp = colDef(name = "Cmp", minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Completions", columns = c("Pos", "Player", "PaCmp"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,PaCmp)][order(-PaCmp)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  PaCmp = colDef(name = "Cmp",minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Completions", columns = c("Pos", "Player", "PaCmp"), align = 'left')
                ))
    }
  })
  #record books rushing yards
  output$recordruyd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,RuYd)][order(-RuYd)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  RuYd = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Rushing Yards", columns = c("Pos", "Player", "RuYd"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,RuYd)][order(-RuYd)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  RuYd = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Rushing Yards", columns = c("Pos", "Player", "RuYd"), align = 'left')
                ))
    }
  })
  #record books rushing touchdowns
  output$recordrutd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,RuTD)][order(-RuTD)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  RuTD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Rushing Touchdowns", columns = c("Pos", "Player", "RuTD"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,RuTD)][order(-RuTD)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  RuTD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Rushing Touchdowns", columns = c("Pos", "Player", "RuTD"), align = 'left')
                ))
    }
  })
  #record books rushing first downs
  output$recordrufd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,RuFD)][order(-RuFD)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  RuFD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Rushing First Downs", columns = c("Pos", "Player", "RuFD"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,RuFD)][order(-RuFD)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  RuFD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Rushing First Downs", columns = c("Pos", "Player", "RuFD"), align = 'left')
                ))
    }
  })
  #record books fumbles
  output$recordfl <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,FL)][order(-FL)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  FL = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Fumbles", columns = c("Pos", "Player", "FL"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,FL)][order(-FL)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  FL = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Fumbles", columns = c("Pos", "Player", "FL"), align = 'left')
                ))
    }
  })
  #record books receiving yards
  output$recordreyd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,ReYd)][order(-ReYd)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  ReYd = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Receiving Yards", columns = c("Pos", "Player", "ReYd"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,ReYd)][order(-ReYd)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  ReYd = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Receiving Yards", columns = c("Pos", "Player", "ReYd"), align = 'left')
                ))
    }
  })
  #record books receiving touchdowns
  output$recordretd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,ReTD)][order(-ReTD)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  ReTD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Receiving Touchdowns", columns = c("Pos", "Player", "ReTD"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,ReTD)][order(-ReTD)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  ReTD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Receiving Touchdowns", columns = c("Pos", "Player", "ReTD"), align = 'left')
                ))
    }
  })
  #record books receiving first downs
  output$recordrefd <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,ReFD)][order(-ReFD)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  ReFD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Receiving First Downs", columns = c("Pos", "Player", "ReFD"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,ReFD)][order(-ReFD)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  ReFD = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Receiving First Downs", columns = c("Pos", "Player", "ReFD"), align = 'left')
                ))
    }
  })
  #record books receptions
  output$recordrec <- renderReactable({
    if (input$recordteams == globalleague) {
      reactable(recordbookspl[, .(Pos,Player,Rec)][order(-Rec)][1:100, ],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  Rec = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Receptions", columns = c("Pos", "Player", "Rec"), align = 'left')
                )
      )
    } else {
      reactable(recordbookstm[, .(TRUFFLE,Pos,Player,Rec)][order(-Rec)][TRUFFLE == teams$Abbrev[teams$FullName == input$recordteams] ][, -"TRUFFLE"],
                defaultSortOrder = "desc",
                filterable = F,
                sortable = F,
                showPageInfo = FALSE,
                paginationType = "simple", defaultPageSize = 5,
                highlight = T,
                #borderless = T,
                compact = T,
                columns = list(
                  Pos = z_posDef(maxW = 35, filt = FALSE),
                  Player = z_playerDef(minW = 140),
                  Rec = colDef(minWidth = 60, align = "left")
                ),
                columnGroups = list(colGroup(name = "Receptions", columns = c("Pos", "Player", "Rec"), align = 'left')
                ))
    }
  })
  
  #awards
  output$historybooksawards <- renderReactable({
    selectedawards <- awards[Award!="1stTm" & Award!="2ndTm"][Season == input$awardseason]
    
    reactable(
      selectedawards,
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
            winner <- selectedawards$Winner[index]
            pos <- selectedawards$Pos[index]
            trf <- selectedawards$TRUFFLE[index]
            if(winner == "-") {
              div(
                div(style = list(fontWeight = 600, fontSize=16, color = "#84A4D8"), value),
                div(style = list(fontSize = 14, color = "#BFBFBF"), paste0("N/A"))
              )
            } else {
              div(
                div(style = list(fontWeight = 600, fontSize=16, color = "#84A4D8"), value),
                div(style = list(fontSize = 14), paste0(winner, ", ", pos, "  |  ", trf))
              )
            }
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
  })
  
  output$allt1 <- renderReactable({
    reactable(awards[Award == "1stTm"][, .(Season, Pos, Winner, TRUFFLE)][Season == input$awardseason][, -"Season"],
              compact = T,
              columns = list(
                Pos = z_posDef(maxW = 80, filt = FALSE),
                TRUFFLE = z_trfDef(filt = FALSE)
              ),
              columnGroups = list(colGroup(name = "1st Team", columns = c("Pos", "Winner", "TRUFFLE"), align = 'left')
              ))
  })
  
  output$allt2 <- renderReactable({
    reactable(awards[Award == "2ndTm"][, .(Season, Pos, Winner, TRUFFLE)][Season == input$awardseason][, -"Season"],
              compact = T,
              columns = list(
                Pos = z_posDef(maxW = 80, filt = FALSE),
                TRUFFLE = z_trfDef(filt = FALSE)
              ),
              columnGroups = list(colGroup(name = "2nd Team", columns = c("Pos", "Winner", "TRUFFLE"), align = 'left')
              ))
  })
  
  #homepage version
  output$homeawards <- renderReactable({
    homeawards <- awards[Award!="1stTm" & Award!="2ndTm"][Season == input$homeseason]
    
    reactable(
      homeawards,
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
            winner <- homeawards$Winner[index]
            pos <- homeawards$Pos[index]
            trf <- homeawards$TRUFFLE[index]
            if(winner == "-") {
              div(
                div(style = list(fontWeight = 600, fontSize=16, color = "#84A4D8"), value),
                div(style = list(fontSize = 14, color = "#BFBFBF"), paste0("N/A"))
              )
            } else {
              div(
                div(style = list(fontWeight = 600, fontSize=16, color = "#84A4D8"), value),
                div(style = list(fontSize = 14), paste0(winner, ", ", pos, "  |  ", trf))
              )
            }
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
  })
  
  output$homeallt1 <- renderReactable({
    reactable(allt1[Season == input$homeseason][, -"Season"],
              compact = T,
              columns = list(
                Pos = z_posDef(maxW = 80, filt = FALSE),
                TRUFFLE = z_trfDef(filt = FALSE)
              ),
              columnGroups = list(colGroup(name = "1st Team", columns = c("Pos", "Winner", "TRUFFLE"), align = 'left')
              ))
  })
  
  output$homeallt2 <- renderReactable({
    reactable(allt2[Season == input$homeseason][, -"Season"],
              compact = T,
              columns = list(
                Pos = z_posDef(maxW = 80, filt = FALSE),
                TRUFFLE = z_trfDef(filt = FALSE)
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
  
  output$rivscores <- renderReactable({
    df <- rivscores[Rivalry == riv$Rivalry[riv$RivalryName == input$rivalry], .(Season, Week, Icon, Team1, Team1Score, Team2Score, Team2)][order(-Season, - Week)]
    
    reactable(df,
              compact = T,
              pagination = F,
              defaultSortOrder = "desc",
              highlight = T,
              columns = list(
                Season = colDef(name="Yr",
                                maxWidth = 50,
                                defaultSortOrder = "desc",
                                align = 'center',
                                footer = "Total:"),
                Week = colDef("Wk", maxWidth = 50, align = 'center', defaultSortOrder = "desc"),
                Icon = colDef(name = "", 
                              align="center", 
                              minWidth = 40,
                              class = "border-right-grey",
                              cell = function(value) {
                                img_src <- knitr::image_uri(value)
                                image <- img(src = img_src, height = "20px", alt = value)
                                tagList(
                                  div(style = list(display = "inline-block"), image)
                                )
                              }),
                Team1 = z_trfDefRivScores(filt = FALSE, maxW = 100),
                Team1Score = colDef(name = "Score",
                                    #minWidth = 100,
                                    format = colFormat(digits = 2),
                                    align = 'center',
                                    footer = function(values) {sum(as.numeric(values), na.rm=T)},
                                    style = function(value, index) {
                                      df <- rivscores[Rivalry == riv$Rivalry[riv$RivalryName == input$rivalry], .(Season, Week, Team1, Team1Score, Team2Score, Team2)][order(-Season, - Week)]
                                      oppscore <- df$Team2Score[index]
                                      col <- ifelse(value > oppscore, QBcolor, NA)
                                      list(background = col)}
                ),
                Team2Score = colDef(name = "Score",
                                    #minWidth = 100,
                                    format = colFormat(digits = 2),
                                    align = 'center',
                                    class = "border-left-grey",
                                    footer = function(values) {sum(as.numeric(values), na.rm=T)},
                                    style = function(value, index) {
                                      df <- rivscores[Rivalry == riv$Rivalry[riv$RivalryName == input$rivalry], .(Season, Week, Team1, Team1Score, Team2Score, Team2)][order(-Season, - Week)]
                                      oppscore <- df$Team1Score[index]
                                      col <- ifelse(value > oppscore, QBcolor, NA)
                                      list(background = col)}
                ),
                Team2 = z_trfDefRivScores(filt = FALSE, maxW = 100)
              ),
              details = function(index) {
                season <- df$Season[index]
                week <- df$Week[index]
                teams <- c(df$Team1[index], df$Team2[index])
                reactable(rivfantasy[Season == season & Week == week & TRUFFLE %in% teams][, .(TRUFFLE, Pos, Player, FPts)][order(TRUFFLE, match(Pos, positionorder))],
                          compact = T,
                          defaultSortOrder = "desc",
                          sortable = F,
                          pagination = F,
                          highlight = T,
                          columns = list(
                            TRUFFLE = z_trfDef(filt=F),
                            Pos = z_posDef(filt = FALSE, foot = " "),
                            Player = z_playerDef(minW = 160),
                            FPts = z_fptsWeekDef()
                          ))
              },
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
    )
  })
  
  output$rivleaders <- renderReactable({
    df <- rivscorers[Rivalry == riv$Rivalry[riv$RivalryName == input$rivalry], .(TRUFFLE, Pos, Player, G, Avg, FPts)][order(-FPts)]
    
    reactable(df,
              compact = T,
              showPageInfo = FALSE,
              defaultSortOrder = "desc",
              defaultPageSize = 10,
              paginationType = 'simple',
              highlight = T,
              columns = list(
                TRUFFLE = z_trfDef(filt = FALSE),
                Pos = z_posDef(filt = FALSE),
                Player = z_playerDef(minW = 160),
                G = z_gDef(),
                FPts = z_fptsDef(),
                Avg = z_avgDef()
              ),
              details = function(index) {
                team <- df$TRUFFLE[index]
                pos <- df$Pos[index]
                player <- df$Player[index]
                
                reactable(rivfantasy[Player == player & Pos == pos & TRUFFLE == team][, .(Season, Week, Icon, Yd, TD, FD, FPts)][order(-Season, -Week)],
                          compact = T,
                          defaultSortOrder = "desc",
                          sortable = F,
                          pagination = F,
                          highlight = T,
                          columns = list(
                            Season = colDef(name="Yr",
                                            maxWidth = 70,
                                            defaultSortOrder = "desc",
                                            align = 'center',
                                            footer = " "),
                            Week = colDef("Wk", maxWidth = 70, align = 'center', defaultSortOrder = "desc"),
                            Icon = colDef(name = "", 
                                          align="center", 
                                          minWidth = 40,
                                          class = "border-right-grey",
                                          cell = function(value) {
                                            img_src <- knitr::image_uri(value)
                                            image <- img(src = img_src, height = "20px", alt = value)
                                            tagList(
                                              div(style = list(display = "inline-block"), image)
                                            )
                                          }),
                            Yd = colDef(header = z_with_tt("Yd", "Tot Yards = Pa + Ru + Re"), maxWidth = 80),
                            TD = colDef(header = z_with_tt("TD", "Tot TD = Pa + Ru + Re"), maxWidth = 80),
                            FD = colDef(header = z_with_tt("FD", "Tot FD = Ru + Re"), maxWidth = 80),
                            FPts = z_fptsWeekDef()
                          ))
              }
    )
  })
  
  #Bench Cup Output
  output$bcgsheet <- renderUI({
    tags$iframe(id = "bcgsheet", 
                src=if (globalleague == "TRUFFLE") { "https://docs.google.com/spreadsheets/d/e/2PACX-1vTjTy8adhC-I9-Pemw_oez5B5lO6BqogZ66H8sA10gW7kSoFg91pDudNP-Il7H5vzJr2WCyZT1RTp7G/pubhtml" } else {
                  "https://docs.google.com/spreadsheets/d/e/2PACX-1vR6KKRn3h51W8V9-t0kXcSjpblZVCw5B8O593W5pOtvp4HbWVnYE8-60ZcPeVjscQKaV0qeyZ3eDAVt/pubhtml"
                },
                height=1020,
                width='100%',
                frameborder = 0,
                marginheight = 0)
  })
  
  #database ----
  #data hub weekly logs
  output$dhweekly <- renderReactable({
    reactable(weekly[Scoring == input$homescoring][order(-FPts)][, !c("Scoring","TRUFFLE", "NFL", "Avg", "PosRk")],
              paginationType = "jump",
              showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
              pageSizeOptions = c(10, 20, 50, 100),
              height = 'auto',
              filterable = T,
              highlight = T,
              compact = T,
              columns = list(
                Season = z_seasonDef(filt = T),
                Week = z_weekDef,
                Pos = z_posDef(),
                Player = z_playerDef(minW = 150, filt = T),
                Opp = z_opDef,
                OpRk = z_oprkDef,
                PaCmp = z_pacmpDef(),
                PaAtt = z_paattDef(),
                PaYd = z_paydDef(wk = T),
                PaTD = z_patdDef(wk = T),
                PaInt = z_paintDef(wk = T),
                RuAtt = z_ruattDef(wk = T),
                RuYd = z_ruydDef(wk = T),
                RuTD = z_rutdDefWk,
                RuFD = z_rufdDefWk,
                Tar = z_tarDefWk,
                Rec = z_recDefWk,
                ReYd = z_reydDefWk,
                ReTD = z_retdDefWk,
                ReFD = z_refdDefWk,
                FL = z_flDef,
                FPts = z_fptsWeekDef()
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
    reactable(seasons[Scoring == input$homescoring][order(-FPts)][, !c("TRUFFLE","PosRk","Scoring")],
              paginationType = "jump",
              showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
              pageSizeOptions = c(10, 20, 50, 100),
              height = 'auto',
              filterable = T,
              highlight = T,
              compact = T,
              columns = list(
                Season = z_seasonDef(filt = T),
                Pos = z_posDef(),
                Player = z_playerDef(minW = 150, filt = T),
                NFL = z_nflDef,
                G = z_gDef(),
                PaCmp = z_pacmpDef(),
                PaAtt = z_paattDef(),
                PaYd = z_paydDef(szn = T),
                PaTD = z_patdDef(szn = T),
                PaInt = z_paintDef(szn = T),
                RuAtt = z_ruattDef(szn = T),
                RuYd = z_ruydDef(szn = T),
                RuTD = z_rutdDefSsn,
                RuFD = z_rufdDefSsn,
                Tar = z_tarDefSsn,
                Rec = z_recDefSsn,
                ReYd = z_reydDefSsn,
                ReTD = z_retdDefSsn,
                ReFD = z_refdDefSsn,
                FL = z_flDef,
                Avg = z_avgDef(),
                FPts = z_fptsSeasDef()
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
    reactable(fantasy[Scoring == input$homescoring][order(-Season, -Week, -FPts)][, !c("Scoring","Opp", "OpRk", "NFL", "Avg")],
              paginationType = "jump",
              showPageInfo = FALSE, showPageSizeOptions = TRUE, defaultPageSize = 20,
              pageSizeOptions = c(10, 20, 50, 100),
              height = 'auto',
              filterable = T,
              highlight = T,
              compact = T,
              columns = list(
                Season = z_seasonDef(filt = T),
                Week = z_weekDef,
                TRUFFLE = z_trfDef(),
                Pos = z_posDef(),
                Player = z_playerDef(minW = 150, filt = T),
                PaCmp = z_pacmpDef(),
                PaAtt = z_paattDef(),
                PaYd = z_paydDef(wk = T),
                PaTD = z_patdDef(wk = T),
                PaInt = z_paintDef(wk = T),
                RuAtt = z_ruattDef(wk = T),
                RuYd = z_ruydDef(wk = T),
                RuTD = z_rutdDefWk,
                RuFD = z_rufdDefWk,
                Tar = z_tarDefWk,
                Rec = z_recDefWk,
                ReYd = z_reydDefWk,
                ReTD = z_retdDefWk,
                ReFD = z_refdDefWk,
                FL = z_flDef,
                FPts = z_fptsWeekDef()
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
                TRF = z_trfDef(filt = FALSE),
                Player = colDef(minWidth = 150, align = "left"),
                Pos = z_posDef(maxW = 40, filt = FALSE),
                Salary = z_salaryDefNobar(minW = 60)
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
                TRF = z_trfDef(filt = FALSE),
                Player = colDef(minWidth = 150, align = "left"),
                Pos = z_posDef(maxW = 40, filt = FALSE),
                Salary = z_salaryDefNobar(minW = 60)
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
                TRF = z_trfDef(filt = FALSE),
                Player = colDef(minWidth = 150, align = "left"),
                Pos = z_posDef(maxW = 40, filt = FALSE),
                Salary = z_salaryDefNobar(minW = 60)
              ),
              columnGroups = list(colGroup(name = "Round 3", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
              )
    )
  })
  
  #draft ----
  output$homerd1 <- renderReactable({
    reactable(draft[Season == input$homedraftseason & Round == 1][, -c("Season", "Round", "Extension")],
              defaultSortOrder = "desc",
              filterable = F,
              showPageInfo = FALSE,
              pagination = F,
              highlight = T,
              #borderless = T,
              compact = T,
              columns = list(
                `#` = colDef(minWidth = 30, align = "right"),
                TRF = z_trfDef(filt = FALSE),
                Player = colDef(minWidth = 150, align = "left"),
                Pos = z_posDef(maxW = 40, filt = FALSE),
                Salary = z_salaryDefNobar(minW = 60)
              ),
              columnGroups = list(colGroup(name = "Round 1", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
              )
    )
  })
  output$homerd2 <- renderReactable({
    reactable(draft[Season == input$homedraftseason & Round == 2][, -c("Season", "Round", "Extension")],
              defaultSortOrder = "desc",
              filterable = F,
              showPageInfo = FALSE,
              pagination = F,
              highlight = T,
              #borderless = T,
              compact = T,
              columns = list(
                `#` = colDef(minWidth = 30, align = "right"),
                TRF = z_trfDef(filt = FALSE),
                Player = colDef(minWidth = 150, align = "left"),
                Pos = z_posDef(maxW = 40, filt = FALSE),
                Salary = z_salaryDefNobar(minW = 60)
              ),
              columnGroups = list(colGroup(name = "Round 2", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
              )
    )
  })
  output$homerd3 <- renderReactable({
    reactable(draft[Season == input$homedraftseason & Round == 3][, -c("Season", "Round", "Extension")],
              defaultSortOrder = "desc",
              filterable = F,
              showPageInfo = FALSE,
              pagination = F,
              highlight = T,
              #borderless = T,
              compact = T,
              columns = list(
                `#` = colDef(minWidth = 30, align = "right"),
                TRF = z_trfDef(filt = FALSE),
                Player = colDef(minWidth = 150, align = "left"),
                Pos = z_posDef(maxW = 40, filt = FALSE),
                Salary = z_salaryDefNobar(minW = 60)
              ),
              columnGroups = list(colGroup(name = "Round 3", columns = c("#","TRF","Player","Pos","Salary"), align = 'left')
              )
    )
  })
  
  #Stop the app timing out
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
})

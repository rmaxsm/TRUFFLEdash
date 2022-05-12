
tagList(
  tags$head(includeCSS("www/styles.css")),
  
  # Define UI for application that draws a histogram
  dashboardPage(title="TRUFFLEdash", skin = "black",
                # Header & Menu Items -----
                #customizing the header
                dashboardHeader(title = HTML(" <span style=color:#84A4D8;font-size:24px>truffle</span><span style =color:#8C2E26;font-weight:bold;font-size:40px;font-family:'Audiowide'>dash</span>  ")),
                
                #laying out the sidebar menu
                dashboardSidebar(sidebarMenu(
                  br(),
                  menuItem("Home", tabName = "home", icon = icon("football-ball")),
                  menuItem("Team Portal", tabName = "teamportal", icon = icon("users")),
                  menuItem("Player Portal", tabName = "playerportal", icon = icon("user")),
                  menuItem("Stat Center", tabName = "statcenter", icon = icon("chart-bar")),
                  menuItem("Trade Machine", tabName = "trademachine" , icon = icon("gears")),
                  menuItem("Fantasy Portal", tabName = "fantasyportal", icon = icon("facebook-f")),
                  menuItem("Cap Corner", tabName = "capcorner", icon = icon("dollar-sign")),
                  menuItem("History Books", tabName = "historybooks", icon = icon("book")),
                  menuItem("Database", tabName = "database", icon = icon("database")),
                  menuItem("Rookie Draft", tabName = "draft", icon = icon("business-time"))
                  #menuItem("BYOG", tabName = "byog", icon = icon("chart-line"))
                )
                ),
                # Dashboard Body -----
                dashboardBody(
                  
                  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
                  # Tab Items -----
                  #tabitems align with sidebar options
                  tabItems(
                    # Home Page -----
                    tabItem(tabName = "home",
                            wellPanel(class = "well",
                                      fluidRow(
                                        column(width=4, style = 'padding-top:10px;',
                                               HTML(" Welcome to <span style=color:#84A4D8;font-size:14px>truffle</span><span style =color:#8C2E26;font-weight:bold;font-size:30px;font-family:'Audiowide'>dash</span>")
                                        ),
                                        column(width = 4,
                                               selectInput("homeseason", HTML("<span style=color:#84A4D8;font-size:14px>Season</span>"), unique(weekly$Season), selected = max(weekly$Season) )
                                        )
                                        ,
                                        column(width=4, align = "right",
                                               imageOutput('sponsor',width = '170px',height='70px'))
                                      )
                            ),
                            
                            fluidRow(
                              column(width = 6,
                                     wellPanel(class = "well",
                                               h2("Standings"),
                                               reactableOutput('hometeamsfantasy', width = "100%")
                                     )
                              ),
                              column(width = 6,
                                     wellPanel(class = "well",
                                               h2("Season Leaders"),
                                               reactableOutput('homepointsleaders', height = "100%"),
                                     ))
                              
                            ),
                            wellPanel(class = "well",
                                      fluidRow(
                                        column(width = 10,
                                               h2("Weekly Leaders")
                                        ),
                                        column(width = 2,
                                               selectInput("weeklytop5week", "Week", unique(weekly$Week), selected = max(weekly$Week[weekly$Season == max(weekly$Season)]) )
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 3,
                                               reactableOutput('homeweeklytop5qb')
                                        ),
                                        column(width = 3,
                                               reactableOutput('homeweeklytop5rb')
                                        ),
                                        column(width = 3,
                                               reactableOutput('homeweeklytop5wr')
                                        ),
                                        column(width = 3,
                                               reactableOutput('homeweeklytop5te')
                                        )
                                      )
                            )
                    ),
                    # Team Portal -----
                    tabItem(tabName = "teamportal",
                            wellPanel(
                              
                              sidebarLayout(
                                sidebarPanel(
                                  fluidRow(selectInput("tmportaltm",HTML("<span style=color:#84A4D8;font-size:14px>Select Team:</span>"),unique(teams$FullName)), style ="padding-top:10px;padding-left:20px;padding-right:20px;z-index:10000;")
                                ),
                                mainPanel(br(),
                                  reactableOutput('tpheader'))
                                
                              ), style = "background-color:#FFFFFF;padding-top:20px"), #end sidebarLayout
                            
                            navbarPage(title = strong(textOutput('tmportalabbrev'), class = "wayback"),
                                       
                                       tabPanel("Overview",
                                                wellPanel(class = "well",
                                                          reactableOutput('tpoverview')
                                                )
                                       ),
                                       
                                       tabPanel("Contracts",
                                                wellPanel(class = "well",
                                                          reactableOutput('tpcontracts')
                                                )
                                       ),
                                       
                                       tabPanel("Box Score",
                                                wellPanel(class = "well",
                                                          reactableOutput('tpboxscore')
                                                )
                                       ),
                                       
                                       tabPanel("Advanced",
                                                wellPanel(class = "well",
                                                          reactableOutput('tpadvanced')
                                                )
                                       ),
                                       
                                       tabPanel("Consistency",
                                                wellPanel(class = "well",
                                                          reactableOutput('tpconsistency')
                                                )
                                       ),
                                       
                                       tabPanel("Fantasy Logs",
                                                wellPanel(class = "well",
                                                          reactableOutput('tpfantasylogs')
                                                )
                                       )
                                       
                            )
                            
                    ), #end teamportal
                    # Player Portal -----
                    tabItem(tabName = "playerportal",
                            wellPanel(style = "background-color:#FFFFFF, padding-top:0px",
                                      selectizeInput('player',h2("Select Player:"),choices = sort(unique(weekly$Player)), selected = NULL, multiple = T)
                            ),
                            wellPanel(class = "well",
                                      h2("Player Info"),
                                      reactableOutput('ppbios')
                            ),
                            wellPanel(class = "well",
                                      h2("Season Log"),
                                      reactableOutput('ppseasons')
                            ),
                            wellPanel(class = "well",
                                      h2("Advanced Stats"),
                                      reactableOutput('ppadvanced')
                            ),
                            wellPanel(class = "well",
                                      h2("Consistency Stats"),
                                      reactableOutput('ppconsistency')
                            ),
                            wellPanel(class = "well",
                                      h2(str_c(max(weekly$Season) , " Game Log")),
                                      reactableOutput('ppweekly')
                            )),
                    
                    tabItem(tabName = "fantasyportal",
                            wellPanel(class = "well",
                                      fluidRow(
                                        column(width = 8,
                                               h2("Fantasy Portal")
                                        ),
                                        column(width = 2,
                                               selectInput("fantasyportalseason", "Season", unique(weekly$Season), selected = max(weekly$Season) )
                                        ),
                                        column(width = 2,
                                               selectInput("totalorperc", "View", c("Totals","Percentages"), selected = "Totals" )
                                        )
                                      ),
                                      reactableOutput('truffleanalysis')
                                      
                            )
                            
                    ),
                    # Stat Center -----
                    tabItem(tabName = "statcenter",
                            wellPanel(
                              fluidRow(
                                column(width = 12,
                                       h2("Stat Center"))
                              ),
                              fluidRow(
                                column(width = 2,
                                       selectInput("scseason", "Season", unique(weekly$Season), selected = max(weekly$Season) )
                                ),
                                column(width = 3,
                                       sliderInput("scweekrange", "Weeks",
                                                   min = 1, max = max(weekly$Week[weekly$Season == max(weekly$Season)]),
                                                   value = c(1,max(weekly$Week[weekly$Season == max(weekly$Season)])),
                                                   step = 1, round = T)
                                ),
                                column(width = 3,
                                       checkboxGroupInput("scpositions", "Positions", choices = c("QB","RB","WR","TE"), selected = c("QB","RB","WR","TE"),
                                                          inline = T)
                                ),
                                column(width = 2,
                                       checkboxGroupInput("scavailable", "Availability", choiceNames = c("Owned","FA"), choiceValues = list( c("AFL","CC","CRB","ELP","FRR","GF","MAM","MCM","MWM","NN","VD","WLW"), "FA"), selected = c("Owned", "FA"),
                                                          inline = T)
                                ),
                                column(width = 2,
                                       sliderInput("scpointsmin", "Minimum Point Threshold",
                                                   min = min(currentseason$FPts), max = max(currentseason$FPts),
                                                   value = 50,
                                                   step = 1, round = T)
                                )
                                
                              )
                            ),
                            navbarPage(title = HTML("<span style=color:#84A4D8>StatCenter</span>"),
                                       tabPanel("Box Score",
                                                wellPanel(class = "well",
                                                          reactableOutput('scboxscore')
                                                )
                                       ),
                                       tabPanel("Advanced",
                                                wellPanel(class = "well",
                                                          reactableOutput('scadvanced')
                                                )
                                       ),
                                       tabPanel("Consistency",
                                                wellPanel(class = "well",
                                                          reactableOutput('scconsistency')
                                                )
                                       )
                                       
                            )
                            
                    ),
                    
                    # Trade Machine -----
                    tabItem(tabName = "trademachine",
                            wellPanel(class = "well",
                                      fluidRow(column(width = 9,
                                                      h2("Trade Machine"),
                                                      p("An interactive, and helpful tool, designed to be a peace offering from the Commissioner for making the rules so complicated."),),
                                               column(width = 3#,
                                                      #selectInput("tmview", "Team View", c("Overview", "Contracts", "Box Score", "Advanced", "Consistency"))
                                               )
                                               )

                                        ),

                            fluidRow(
                              column(width = 6,
                                     wellPanel(class = "well",
                                               selectInput("tmtm1", HTML("<span style=color:#84A4D8;font-size:14px>Select Team 1:</span>"), unique(teams$FullName), unique(teams$FullName)[1]),
                                               reactableOutput('tmtm1')
                                               )
                                     ),
                              column(width = 6,
                                     wellPanel(class = "well",
                                               selectInput("tmtm2",HTML("<span style=color:#84A4D8;font-size:14px>Select Team 2:</span>"), unique(teams$FullName), unique(teams$FullName)[2]),
                                               reactableOutput('tmtm2')
                                               )
                                     )
                              ),

                            fluidRow(
                              column(width = 6,
                                     wellPanel(class = "well",
                                               p(HTML("<span style=color:#84A4D8;font-size:14px><b>Players in Trade Team 1:</b></span>")),
                                               reactableOutput('tmpls1')
                                     )
                              ),
                              column(width = 6,
                                     wellPanel(class = "well",
                                               p(HTML("<span style=color:#84A4D8;font-size:14px><b>Players in Trade Team 2:</b></span>")),
                                               reactableOutput('tmpls2')
                                     )
                              )
                            ),

                            fluidRow(
                              column(width = 6,
                                     wellPanel(class = "well",
                                               p(HTML("<span style=color:#84A4D8;font-size:14px><b>Trade Outcome:</b></span>")),
                                               reactableOutput('tradesuccess')
                                     )
                              ),
                              column(width = 6,
                                     wellPanel(class = "well",
                                               p(HTML("<span style=color:#84A4D8;font-size:14px><b>Trade-Adjusted Salary Caps:</b></span>")),
                                               reactableOutput('tradecapresults')
                                     )
                              )
                            )
                            
                    ), #end trademachine tab item
                    
                    # Cap Corner -----
                    tabItem(tabName = "capcorner",
                            
                            navbarPage(title = HTML("<span style=color:#84A4D8>Cap Corner</span>"),
                                       
                                       tabPanel("Contracts",
                                                class = "well",
                                                reactableOutput('capcornercontracts')
                                       ),
                                       
                                       tabPanel("Cap Breakdown",
                                                class = "well",
                                                #p("Salary Cap Breakdown by Team"),
                                                plotlyOutput('plot1', height = 555)
                                                
                                       )
                                       
                            ) #end navbarpage
                            
                    ), #end capcorner tab item
                    
                    # History Books -----
                    tabItem(tabName = "historybooks",
                            
                            navbarPage(title = HTML("<span style=color:#84A4D8>History Books</span>"),
                                       
                                       tabPanel("Records",
                                                wellPanel(class = "well",
                                                          fluidRow(
                                                            column(width = 9,
                                                                   h2("TRUFFLE League and Franchise Records"),
                                                                   p("*Statistics only based on statistics produced in active TRUFFLE lineups beginning with 2020 season.")
                                                            ),
                                                            column(width = 3,
                                                                   br(),
                                                                   selectInput("recordteams", "TRUFFLE / Team", c("TRUFFLE", unique(teams$FullName)), selected = "TRUFFLE" )
                                                            )
                                                          )),
                                                wellPanel(class = "well",
                                                          h2("Overall"),
                                                          fluidRow(
                                                            column(width = 3,
                                                                   reactableOutput('recordfpts')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordgames')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordavg')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordfd')
                                                            )
                                                          ),
                                                          h2("Passing"),
                                                          fluidRow(
                                                            column(width = 3,
                                                                   reactableOutput('recordpayd')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordpatd')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordpaint')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordpacmp')
                                                            )
                                                          ),
                                                          h2("Rushing"),
                                                          fluidRow(
                                                            column(width = 3,
                                                                   reactableOutput('recordruyd')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordrutd')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordrufd')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordfl')
                                                            )
                                                          ),
                                                          h2("Receiving"),
                                                          fluidRow(
                                                            column(width = 3,
                                                                   reactableOutput('recordreyd')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordretd')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordrefd')
                                                            ),
                                                            column(width = 3,
                                                                   reactableOutput('recordrec')
                                                            )
                                                          )
                                                )
                                                
                                       ),
                                       
                                       tabPanel("Awards",
                                                
                                                wellPanel(class = "well",
                                                          fluidRow(
                                                            column(width = 9,
                                                                   h2("Annual End Of Season TRUFFLE Awards"),
                                                                   p("Awards and All-TRUFFLE Teams annually voted for by TRUFFLE owners.")
                                                            ),
                                                            column(width = 3,
                                                                   selectInput("awardseason", "Season", unique(awards$Season), selected = max(awards$Season))
                                                            )
                                                          )
                                                ),
                                                wellPanel(class = "well",
                                                          fluidRow(
                                                            column(width=6,
                                                                   h2("Award Winners"),
                                                                   reactableOutput('historybooksawards')
                                                            ),
                                                            column(width=6,
                                                                   h2("All-TRUFFLE"),
                                                                   reactableOutput('allt1'),
                                                                   reactableOutput('allt2')
                                                            )
                                                          )
                                                )
                                       ) #end tabpanel
                                       
                            ) #end navbarpage
                            
                    ), #end record books tab item
                    
                    # Database -----
                    tabItem(tabName = "database",
                            
                            navbarPage(title = HTML("<span style=color:#84A4D8>DataHub</span>"),
                                       
                                       tabPanel("Weekly",
                                                reactableOutput('dhweekly')
                                       ),
                                       
                                       tabPanel("Seasons",
                                                reactableOutput('dhseasons')
                                       ),
                                       
                                       tabPanel("Fantasy",
                                                reactableOutput('dhfantasy')
                                       )
                                       
                            ) #end navbarpage
                            
                    ), #end datahub tab item
                    
                    # Draft -----
                    tabItem(tabName = "draft",
                            wellPanel(class = "well",
                                      fluidRow(
                                        column(width=8, h2("Rookie Draft Records")),
                                        column(width=4, selectInput("draftseason", "Season", unique(draft$Season), selected = max(weekly$Season) ))
                                      ),
                                      fluidRow(
                                        column(width=4, reactableOutput('rd1')),
                                        column(width=4, reactableOutput('rd2')),
                                        column(width=4, reactableOutput('rd3'))
                                      )
                            )
                            
                    )
                    
                    
                  )
                  
                  
                )
                
  )
)

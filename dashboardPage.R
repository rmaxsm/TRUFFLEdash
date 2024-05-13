library(shinydashboard)

dashboardPageUI <-
  
  tagList(
    tags$head(includeCSS("www/styles.css")),
    
    # Define UI for application that draws a histogram
    dashboardPage(title="TRUFFLEdash", skin = "black",
                  # Header & Menu Items -----
                  #customizing the header
                  dashboardHeader(title = HTML(" <span style=color:#84A4D8;font-size:24px>truffle</span><span style =color:#8C2E26;font-weight:bold;font-size:40px;font-family:'Audiowide'>dash</span>  ")),
                  
                  #laying out the sidebar menu
                  dashboardSidebar(width = '200px', sidebarMenu(
                    br(),
                    menuItem("Home", tabName = "home", icon = icon("football-ball")),
                    menuItem("Team Portal", tabName = "teamportal", icon = icon("users")),
                    menuItem("Player Portal", tabName = "playerportal", icon = icon("user")),
                    menuItem("Stat Center", tabName = "statcenter", icon = icon("sliders")),
                    menuItem("BYOG", tabName = "byog", icon = icon("chart-line")),
                    menuItem("Trade Machine", tabName = "trademachine" , icon = icon("gears")),
                    menuItem("Fantasy Portal", tabName = "fantasyportal", icon = icon("facebook-f")),
                    menuItem("Cap Corner", tabName = "capcorner", icon = icon("dollar-sign")),
                    menuItem("History Books", tabName = "historybooks", icon = icon("book")),
                    menuItem("Database", tabName = "database", icon = icon("database")),
                    menuItem("Rookie Draft", tabName = "draft", icon = icon("business-time")),
                    menuItem("Constitution", href = "https://docs.google.com/document/d/1wUXsY3VRNCH9NPYeb1Vf9xm8AnJ9HJIiB85rvPTYjgo/", newtab = TRUE, icon = icon("paragraph")),
                    br(),
                    selectInput("homescoring", HTML("<span style=color:#84A4D8;font-size:14px>Scoring System</span>"), c("PPFD", "PPR", "hPPR", "STD"), selected ="PPFD" )
                    #menuItem("Tutorials", tabName = "tutorials", icon = icon("fa-sharp fa-solid fa-info"))
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
                                                 selectInput("homeseason", HTML("<span style=color:#84A4D8;font-size:14px>Season</span>"), sort(unique(c(weekly$Season, currentyr)), decreasing = T), selected = max(unique(c(weekly$Season, currentyr))) )
                                          )
                                          ,
                                          column(width=4, align = "right",
                                                 imageOutput('sponsor',width = '170px',height='70px'))
                                        )
                              ),
                              
                              #Whenever we need bench cup on home page
                              #wellPanel(class = "well",
                              #          htmlOutput("bcgsheet")
                              #),
                              
                              # #awards home page tab for during offseason
                              # wellPanel(class = "well",
                              #           fluidRow(
                              #             column(width=6,
                              #                    h2("Award Winners"),
                              #                    br(),
                              #                    reactableOutput('homeawards')
                              #             ),
                              #             column(width=6,
                              #                    h2("All-TRUFFLE"),
                              #                    em("* denotes unanimous 1st Team selection"),
                              #                    reactableOutput('homeallt1'),
                              #                    reactableOutput('homeallt2')
                              #             )
                              #           )
                              # ),
                              
                              #draft homepage around draft time
                              # wellPanel(class = "well",
                              #           fluidRow(
                              #             column(width=8, h2("Rookie Draft")),
                              #             column(width=4, selectInput("homedraftseason", "Season", sort(unique(draft$Season), decreasing = T), selected = max(draft$Season) ))
                              #           ),
                              #           fluidRow(
                              #             column(width=4, reactableOutput('homerd1')),
                              #             column(width=4, reactableOutput('homerd2')),
                              #             column(width=4, reactableOutput('homerd3'))
                              #           )
                              # ),
                              
                              fluidRow(
                                column(width = 6,
                                       wellPanel(class = "well",
                                                 h2("Standings"),
                                                 tabsetPanel(
                                                   tabPanel("Scoring",
                                                            reactableOutput('hometeamsfantasy', width = "100%")),
                                                   tabPanel("Divisions"
                                                            ),
                                                   tabPanel("Playoffs"
                                                   ),
                                                   tabPanel("Optimal"
                                                   )
                                                   
                                                 )
                                                 
                                       )
                                ),
                                column(width = 6,
                                       wellPanel(class = "well",
                                                 h2("Season Leaders"),
                                                 tabsetPanel(
                                                   tabPanel("Fantasy",
                                                            reactableOutput('homepointsleaders')
                                                            ), #height = "100%"),
                                                   tabPanel("Passing",
                                                            reactableOutput('homepassing')
                                                   ),
                                                   tabPanel("Rushing",
                                                            reactableOutput('homerushing')
                                                   ),
                                                   tabPanel("Receiving",
                                                            reactableOutput('homereceiving')
                                                   ),
                                                   tabPanel("Advanced",
                                                            reactableOutput('homeadvanced')
                                                   ),
                                                   tabPanel("Consistency",
                                                            reactableOutput('homeconsistency')
                                                   )
                                                 )
                                       ))
                                
                              ),
                              wellPanel(class = "well",
                                        fluidRow(
                                          column(width = 10,
                                                 h2("Weekly Leaders")
                                          ),
                                          column(width = 2,
                                                 selectInput("weeklytop5week", "Week", sort(unique(weekly$Week)), selected = max(weekly$Week[weekly$Season == max(weekly$Season)]) )
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
                              wellPanel(class = "well",
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    fluidRow(
                                      column(width = 8, selectInput("tmportaltm",HTML("<span style=color:#84A4D8;font-size:14px>Select Team:</span>"), unique(teams$FullName))),
                                      column(width = 4, selectInput("tmportalyr",HTML("<span style=color:#84A4D8;font-size:14px>Year:</span>"), sort(c(unique(seasons$Season), currentyr), decreasing = T), selected = max(weekly$Season)))       
                                             , style ="padding-top:10px;padding-left:20px;padding-right:20px;z-index:10000;")
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
                                                            reactableOutput('tpcontracts'),
                                                            hr(),
                                                            em("Purple values indicate player is eligible for a Rookie Extension in given year.")
                                                  )
                                         ),
                                         tabPanel("Box Score",
                                                  wellPanel(class = "well",
                                                            reactableOutput('tpboxscore'),
                                                            hr(),
                                                            em("Traditional Passing, Rushing, Receiving scoring stats. Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("Advanced",
                                                  wellPanel(class = "well",
                                                            reactableOutput('tpadvanced'),
                                                            hr(),
                                                            em("Advanced stats on per-touch efficiency, and point source breakdown (Yardage vs. TD vs. First Downs / Rushing vs. Receiving). Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("Consistency",
                                                  wellPanel(class = "well",
                                                            reactableOutput('tpconsistency'),
                                                            hr(),
                                                            em("Week-to-week consistency stats by scoring output and weekly positional rank. Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("Extra Stats",
                                                  wellPanel(class = "well",
                                                            reactableOutput('tpextradash'),
                                                            hr(),
                                                            em("Additional weekly context stats, including Yards per Carry/Catch, 20+ and 40+ yard plays, and others. Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("ESPN xFP/xTD",
                                                  wellPanel(class = "well",
                                                            reactableOutput('tpxfpxtd'),
                                                            hr(),
                                                            em("Expected Fantasy Points and Expected Touchdown stats scraped from ESPN. Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("Snap Share",
                                                  wellPanel(class = "well",
                                                            reactableOutput('tpsnapshare'),
                                                            hr(),
                                                            em("Week-to-week percentage of offensive team snaps played.")
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
                                        selectizeInput('player',h2("Select Player:"), choices = sort(unique(weekly$Player)), selected = NULL, multiple = T),
                                        hr(),
                                        em("Type and select player name(s) to view/compare player stats across all Player Portal tables & charts")
                              ),
                              wellPanel(class = "well",
                                        h2("Player Info"),
                                        reactableOutput('ppbios')
                              ),
                              fluidRow(
                                column(width = 8,
                                       wellPanel(class = "well",
                                                 h2("TRUFFLE Career Stats"),
                                                 reactableOutput('pptrufflecareerstats'),
                                                 hr(),
                                                 em("Statistics only based on stats produced in active TRUFFLE lineups. Expand for career stats by team.")
                                       )
                                ),
                                column(width = 4,
                                       wellPanel(class = "well",
                                                 h2("Contract Details"),
                                                 reactableOutput('ppcontracthistory'),
                                                 hr(),
                                                 em("Expand for year-by-year contract overview.")
                                       )
                                )
                              ),
                              wellPanel(class = "well",
                                        fluidRow(
                                          column(width = 10,
                                                 h2("Stat Center")
                                          ),
                                          column(width = 2,
                                                 selectInput("ppstatcenterseason", "Season", c(as.character(sort(unique(weekly$Season), decreasing = T)), "All"), selected = as.character(max(weekly$Season)) )
                                          )
                                        ),
                                        tabsetPanel(
                                          tabPanel("BoxScore", reactableOutput('ppseasons'),
                                                   hr(),
                                                   em("Traditional Passing, Rushing, Receiving scoring stats. Hover over column header for definition.")),
                                          tabPanel("Advanced", reactableOutput('ppadvanced'),
                                                   hr(),
                                                   em("Advanced stats on per-touch efficiency, and point source breakdown (Yardage vs. TD vs. First Downs / Rushing vs. Receiving). Hover over column header for definition.")),
                                          tabPanel("Consistency", reactableOutput('ppconsistency'),
                                                   hr(),
                                                   em("Week-to-week consistency stats by scoring output and weekly positional rank. Hover over column header for definition.")),
                                          tabPanel("Extra Stats", reactableOutput('ppextradash'),
                                                   hr(),
                                                   em("Additional weekly context stats, including Yards per Carry/Catch, 20+ and 40+ yard plays, and others. Hover over column header for definition.")),
                                          tabPanel("ESPN xFP/xTD", reactableOutput('ppxfpxtd'),
                                                   hr(),
                                                   em("Expected Fantasy Points and Expected Touchdown stats scraped from ESPN. Hover over column header for definition.")),
                                          tabPanel("Snap Share", reactableOutput('ppsnapshare'),
                                                   hr(),
                                                   em("Week-to-week percentage of offensive team snaps played."))
                                        )
                              ),
                              fluidRow(
                                column(width = 4,
                                       wellPanel(class = "well",
                                                 fluidRow(
                                                   column(width = 8,
                                                          h2("Percentile Radar Chart")),
                                                   column(width = 4,
                                                          selectInput("ppradarplotseason", "Season", sort(unique(weekly$Season), decreasing = T), selected = max(weekly$Season)))
                                                 ),
                                                 plotOutput('ppradarplot',
                                                            width = "100%",
                                                            height = "400px"),
                                                 hr(),
                                                 em("Radar chart shows avg. weekly player percentile performance at position.")
                                       )
                                ),
                                column(width = 8,
                                       wellPanel(class = "well",
                                                 fluidRow(
                                                   column(width = 6,
                                                          h2("Week-by-Week")),
                                                   column(width = 3,
                                                          selectInput("ppwbwstat", "Stat", colnames(weekly)[c(8:22, 24)], selected = colnames(weekly)[24])),
                                                   column(width = 3,
                                                          selectInput("ppwbwseason", "Season", sort(unique(weekly$Season), decreasing = T), selected = max(weekly$Season)))
                                                 ),
                                                 plotlyOutput('ppwbw',
                                                              width = "100%",
                                                              height = "400px"),
                                                 hr(),
                                                 em("Weeks missed represented by missing data point, not 0")
                                       )
                                )
                              ),
                              wellPanel(class = "well",
                                        fluidRow(
                                          column(width = 10,
                                                 h2("Game Logs")
                                          ),
                                          column(width = 2,
                                                 selectInput("ppgamelogsseason", "Season", c(as.character(sort(unique(weekly$Season), decreasing = T)), "All"), selected = as.character(max(weekly$Season)) )
                                        )
                                        ),
                                        tabsetPanel(
                                          tabPanel("Weekly", reactableOutput('ppgamelogweekly')),
                                          tabPanel("Fantasy",
                                                   br(),
                                                   p("*Only includes games when player was actively started in TRUFFLE"),
                                                   reactableOutput('ppgamelogfantasy'))
                                        )
                              )
                      ), #end player portal tab item
                      
                      # Fantasy Portal -----
                      tabItem(tabName = "fantasyportal",
                              wellPanel(class = "well",
                                        fluidRow(
                                          column(width = 8,
                                                 h2("Fantasy Portal")
                                          ),
                                          column(width = 2,
                                                 selectInput("fantasyportalseason", "Season", sort(unique(weekly$Season), decreasing = T), selected = max(weekly$Season))
                                          ),
                                          column(width = 2,
                                                 selectInput("totalorperc", "View", c("Totals","Percentages"), selected = "Totals" )
                                          )
                                        ),
                                        reactableOutput('truffleanalysis')
                                        
                              )
                              
                      ), #end fantasy portal tab item
                      # Stat Center -----
                      tabItem(tabName = "statcenter",
                              wellPanel(class = "well",
                                fluidRow(
                                  column(width = 12,
                                         h2("Stat Center"))
                                ),
                                fluidRow(
                                  column(width = 2,
                                         selectInput("scseason", "Season", sort(unique(weekly$Season), decreasing = T), selected = max(weekly$Season) )
                                  ),
                                  column(width = 3,
                                         sliderInput("scweekrange", "Weeks",
                                                     min = 1, max = 18,
                                                     value = c(1,max(weekly$Week[weekly$Season == max(weekly$Season)])),
                                                     step = 1, round = T)
                                  ),
                                  column(width = 3,
                                         checkboxGroupInput("scpositions", "Positions", choices = c("QB","RB","WR","TE"), selected = c("QB","RB","WR","TE"),
                                                            inline = T)
                                  ),
                                  column(width = 2,
                                         checkboxGroupInput("scavailable", "Availability", choices = c("Owned","FA"), selected = c("Owned", "FA"),
                                                            inline = T)
                                  ),
                                  column(width = 2,
                                         sliderInput("scavgmin", "Minimum Average FPts Threshold",
                                                     min = 0, max = 30,
                                                     value = 5,
                                                     step = 1, round = T)
                                  )
                                  
                                )
                              ),
                              navbarPage(title = HTML("<span style=color:#84A4D8>StatCenter</span>"),
                                         tabPanel("Box Score",
                                                  wellPanel(class = "well",
                                                            reactableOutput('scboxscore'),
                                                            hr(),
                                                            em("Traditional Passing, Rushing, Receiving scoring stats. Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("Advanced",
                                                  wellPanel(class = "well",
                                                            reactableOutput('scadvanced'),
                                                            hr(),
                                                            em("Advanced stats on per-touch efficiency, and point source breakdown (Yardage vs. TD vs. First Downs / Rushing vs. Receiving). Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("Consistency",
                                                  wellPanel(class = "well",
                                                            reactableOutput('scconsistency'),
                                                            hr(),
                                                            em("Week-to-week consistency stats by scoring output and weekly positional rank. Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("Extra Stats",
                                                  wellPanel(class = "well",
                                                            reactableOutput('scextradash'),
                                                            hr(),
                                                            em("Additional weekly context stats, including Yards per Carry/Catch, 20+ and 40+ yard plays, and others. Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("ESPN xFP/xTD",
                                                  wellPanel(class = "well",
                                                            reactableOutput('scxfpxtd'),
                                                            hr(),
                                                            em("Expected Fantasy Points and Expected Touchdown stats scraped from ESPN. Hover over column header for definition.")
                                                  )
                                         ),
                                         tabPanel("Snap Share",
                                                  wellPanel(class = "well",
                                                            reactableOutput('scsnapshare'),
                                                            hr(),
                                                            em("Week-to-week percentage of offensive team snaps played.")
                                                  )
                                         )
                                         
                              )
                              
                      ), #end stat center tab item
                      
                      #Build Your Own Graph -----
                      tabItem(tabName = "byog",
                              wellPanel(class = "well",
                                        fluidRow(column(h2("Build Your Own Graph"), width = 12),
                                                 column(p("An interactive graphing tool, designed for you to graphically compare and analyze any two statistics featured on TRUFFLEdash by player season."), width = 12)
                                        )
                                        
                              ),
                              wellPanel(class = "well",
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    fluidRow(
                                      h2("Select Graphing X- & Y-Axis Satistics"),
                                      column(width = 6, selectInput("byogX", "X-Axis", colnames(byog)[7:74], selected = colnames(byog)[69])),
                                      column(width = 6, selectInput("byogY", "Y-Axis", colnames(byog)[7:74], selected = colnames(byog)[23]))    
                                      , style ="padding-top:10px;padding-left:20px;padding-right:20px;z-index:10000;")
                                  ),
                                  mainPanel(br(),
                                            h2("Apply Filters"),
                                            fluidRow(
                                              column(width = 2,
                                                     selectInput("byogseason", "Season", c(sort(unique(weekly$Season), decreasing = T), "All"), selected = max(weekly$Season))),
                                              column(width = 4,
                                                     checkboxGroupInput("byogpositions", "Positions", choices = c("QB","RB","WR","TE"), selected = c("QB","RB","WR","TE"),
                                                                        inline = T)
                                              ),
                                              column(width = 2,
                                                     selectInput("byoggamesmin", "Min. G", choices = c(1:17), selected = 1)),
                                              column(width = 4,
                                                     sliderInput("byogavgmin", "Min. Average FPts",
                                                                 min = 0, max = 30,
                                                                 value = 5,
                                                                 step = 1, round = T)
                                              )
                                            )
                                            )
                                  
                                ), style = "background-color:#FFFFFF;padding-top:20px", #end sidebarLayout
                              #wellPanel(
                                plotlyOutput('byog',
                                  width = "100%",
                                  height = "500px"),
                                hr(),
                                em("Details revealed by hovering over specific data points, color coded by position")
                              )
                      ), #end byog tab item
                      
                      # Trade Machine -----
                      tabItem(tabName = "trademachine",
                              wellPanel(class = "well",
                                        fluidRow(column(h2("Trade Machine"), width = 12),
                                                  column(p("An interactive, and helpful tool, designed to be a peace offering from the Commissioner for making the rules so complicated."), width = 12)
                                                 
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
                                                  reactableOutput('capcornercontracts'),
                                                  hr(),
                                                  em("Purple values indicate player is eligible for a Rookie Extension in given year.")
                                         ),
                                         
                                         tabPanel("Cap Breakdown",
                                                  class = "well",
                                                  #p("Salary Cap Breakdown by Team"),
                                                  plotlyOutput('plot1', height = 555)
                                                  
                                         ),
                                         
                                         tabPanel("Rookie Extensions & Franchise Tag",
                                                  wellPanel(class = "well",
                                                            h2("Rookie Extension Values"),
                                                            p("See the tables below for Rookie Extension contract values by Draft Pick and Position. Owners may trigger any eligible players' rookie extension during the offseason after their initial rookie contract expires. Extended players then receive an additional 2-year contract at the value specified below."),
                                                            hr(),
                                                            fluidRow(
                                                              column(width = 3,
                                                                     reactableOutput('extvalqb')
                                                              ),
                                                              column(width = 3,
                                                                     reactableOutput('extvalrb')
                                                              ),
                                                              column(width = 3,
                                                                     reactableOutput('extvalwr')
                                                              ),
                                                              column(width = 3,
                                                                     reactableOutput('extvalte')
                                                              )
                                                            ),
                                                            hr(),
                                                            em("Expand Rows to see eligible players.")
                                                  ),
                                                  wellPanel(class = "well",
                                                            h2("Franchise Tag Values"),
                                                            p("See the tables below for projected Franchise Tag values for next season by Position. Use the 'Franchise Tag Value Player Lookup' tool to input a player and check his specific tag value."),
                                                            hr(),
                                                            h3("Franchise Tag Rules, as written in the constitution:"),
                                                            p("When a non-rookie player contract expires at season’s end, the player automatically becomes a Free Agent and part of the player pool for the Free Agent Auction, unless the player is Franchise Tagged. 
                                                              Similar to the NFL, a Franchise Tag gives owners the option to automatically extend a player’s contract by 1 year at a positionally predetermined Franchise Tag salary. Each offseason, any team may franchise tag up to 2 players. Any individual player may be franchise tagged 2 times before automatically entering Free Agency."),
                                                            p("The positionally predetermined Franchise Tag salary is calculated as follows:"),
                                                            tags$ul(
                                                              tags$li("If the player in question has not been franchise tagged before, his additional 1-year contract salary becomes the average salary (rounded up) of the top 5 highest paid players at his position from the previous season."),
                                                              tags$li("If the player in question has not been franchise tagged before AND his previous salary exceeded the average salary (rounded up) of the top 5 highest paid players at his position from the previous season, his additional 1-year contract salary becomes $1 more than his previous year salary"),
                                                              tags$li("If the player in question was franchise tagged the previous year, his additional 1-year contract salary becomes $1 more than the previous highest salary in the league at his position.")
                                                            ),
                                                            hr(),
                                                            h3("Positional Franchise Tag Values"),
                                                            fluidRow(
                                                              column(width = 3,
                                                                     reactableOutput('tagvalsqb')
                                                              ),
                                                              column(width = 3,
                                                                     reactableOutput('tagvalsrb')
                                                              ),
                                                              column(width = 3,
                                                                     reactableOutput('tagvalswr')
                                                              ),
                                                              column(width = 3,
                                                                     reactableOutput('tagvalste')
                                                              )
                                                            ),
                                                            hr(),
                                                            em("Expand Rows to see top 5 highest paid players at poision that determine tag value.")
                                                  ),
                                                  wellPanel(class = "well",
                                                            h2("Franchise Tag Value Player Lookup"),
                                                            p("Use the tool below to type in specific player names and view their specific franchise tag value for next year."),
                                                            #wellPanel(
                                                            sidebarLayout(
                                                              sidebarPanel(
                                                                fluidRow(selectizeInput('tagvalplayer',h3("Select Player:"),choices = ft$Player, selected = NULL, multiple = T), style ="padding-top:00px;padding-left:20px;padding-right:20px;z-index:10000;")
                                                              ),
                                                              mainPanel(br(),
                                                                        reactableOutput('tagvalsplayer')
                                                              )
                                                              
                                                            )#, style = "background-color:#FFFFFF;padding-top:20px" ), #end sidebarLayout
                                                            
                                                  )
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
                                                                     selectInput("recordteams", "League / Team", c("TRUFFLE", unique(teams$FullName)), selected = "TRUFFLE" )
                                                              )
                                                            )),
                                                  wellPanel(class = "well",
                                                            h2("Overall"),
                                                            fluidRow(
                                                              column(width = 6,
                                                                     reactableOutput('recordrings')
                                                              ),
                                                              column(width = 6,
                                                                     reactableOutput('recordbenchcups')
                                                              )
                                                            ),
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
                                                                     selectInput("awardseason", "Season", sort(unique(awards$Season), decreasing = T), selected = max(awards$Season))
                                                              )
                                                            )
                                                  ),
                                                  wellPanel(class = "well",
                                                            fluidRow(
                                                              column(width=6,
                                                                     h2("Award Winners"),
                                                                     br(),
                                                                     reactableOutput('historybooksawards')
                                                              ),
                                                              column(width=6,
                                                                     h2("All-TRUFFLE"),
                                                                     em("* denotes unanimous 1st Team selection"),
                                                                     reactableOutput('allt1'),
                                                                     reactableOutput('allt2')
                                                              )
                                                            )
                                                  )
                                         ), #end tabpanel
                                         
                                         tabPanel("Rivalries",
                                                  wellPanel(
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        fluidRow(selectInput("rivalry",HTML("<span style=color:#84A4D8;font-size:14px>Select Rivalry:</span>"), unique(teams$RivalryName)), style ="padding-top:10px;padding-left:20px;padding-right:20px;z-index:10000;")
                                                      ),
                                                      mainPanel(reactableOutput('rivheader'))
                                                      
                                                    ), style = "background-color:#FFFFFF;padding-top:20px"), #end sidebarLayout
                                                  fluidRow(
                                                    column(width = 6,
                                                           wellPanel(class = "well",
                                                                     h2("History"),
                                                                     reactableOutput('rivscores')
                                                           )
                                                    ),
                                                    column(width = 6,
                                                           wellPanel(class = "well",
                                                                     h2("Leading Scorers *"),
                                                                     reactableOutput('rivleaders'),
                                                                     hr(),
                                                                     em("*since 2020")
                                                           ))
                                                  )
                                         ), #end tabpanel
                                         
                                         tabPanel("Bench Cup",
                                                  
                                                  wellPanel(class = "well",
                                                            htmlOutput("bcgsheet")
                                                  )
                                         ) #end tabpanel
                                         
                              ) #end navbarpage
                              
                      ), #end record books tab item
                      
                      # Database -----
                      tabItem(tabName = "database",
                              
                              navbarPage(title = HTML("<span style=color:#84A4D8>Database</span>"),
                                         
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
                                          column(width=4, selectInput("draftseason", "Season", sort(unique(draft$Season), decreasing = T), selected = max(draft$Season) ))
                                        ),
                                        fluidRow(
                                          column(width=4, reactableOutput('rd1')),
                                          column(width=4, reactableOutput('rd2')),
                                          column(width=4, reactableOutput('rd3'))
                                        )
                              )
                              
                      ),
                      
                      #tutorials
                      tabItem(tabName = "tutorials",
                              wellPanel(class = "well",
                                        tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/uU9hTNvvo9A", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=T)
                              )
                              
                      )
                      
                      
                    )
                    
                    
                  )
                  
    )
  )
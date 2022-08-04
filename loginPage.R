library(shiny)

loginPageUI <-
  
  ##### UI code for login page
  fluidPage(
    title = "TRUFFLEdash",
    titlePanel(title="",
               tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"))),
    fluidRow(
      column(width = 4, offset = 4,
             br(), br(), br(), br(),
             uiOutput("uiLogin"),
             uiOutput("pass")
      )
    )
  )
library(shiny)

loginPageUI <-
  
  ##### UI code for login page
  fluidPage(
    fluidRow(
      column(width = 4, offset = 4,
             br(), br(), br(), br(),
             uiOutput("uiLogin"),
             uiOutput("pass")
      )
    )
  )
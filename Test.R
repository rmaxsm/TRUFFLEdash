library(reactable)
library(htmltools)
library(readxl)
library(data.table)

awards <- as.data.table(read_excel("data/awards.xlsx"))
champ <- awards[Award == "Champion"][, c("Logo", "Season", "Award", "TRUFFLE", "Winner")]

reactable(
  teams[, c("FullName", "Owner", "Logo", "Location")],
  columns = list(
    FullName = colDef(minWidth = 300,
      cell = function(value, index) {
        location <- teams$Location[index]
        col <- teams$Primary[index]
        div(
          div(style = list(fontWeight = 600, fontSize=30, color = col), value),
          div(style = list(fontSize = 12), location)
        )
      }
    ),
    Logo = colDef(name = "", 
                  align="center", 
                  minWidth = 80, 
                  cell = function(value) {
                    img_src <- knitr::image_uri(value)
                    image <- img(src = img_src, height = "200px", alt = value)
                    tagList(
                      div(style = list(display = "inline-block"), image)
                    )
                  }),
    Location = colDef(show = FALSE)
  )
  )

data <- starwars %>%
  select(character = name, height, mass, gender, homeworld, species)

reactable(
  awards,
  columns = list(
    Award = colDef(
      # Show species under character names
      cell = function(value, index) {
        winner <- awards$Winner[index]
        div(
          div(style = list(fontWeight = 600, fontSize=14), value),
          div(style = list(fontSize = 12), winner)
        )
      }
    ),
    Winner = colDef(show = FALSE)
  ),
  defaultPageSize = 6,
  theme = reactableTheme(
    # Vertically center cells
    cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
  )
)

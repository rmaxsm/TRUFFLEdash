library(reactable)
library(htmltools)
library(readxl)
library(data.table)

awards <- as.data.table(read_excel("data/awards.xlsx"))
champ <- awards[Award == "Champion"][, c("Logo", "Season", "Award", "TRUFFLE", "Winner")]

reactable(
  champ,
  columns = list(
    Logo = colDef(cell = function(value) {
      img_src <- knitr::image_uri(value)
      image <- img(src = img_src, height = "24px", alt = value)
      tagList(
       div(style = list(display = "inline-block", width = "45px"), image)
      )
    }),
    TRUFFLE = trfDef
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

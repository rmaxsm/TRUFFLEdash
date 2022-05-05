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


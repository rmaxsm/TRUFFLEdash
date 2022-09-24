#### Radarplots ----

library(fmsb)


radarchart(emptyradar,
           #custom polygon
           pcol="grey", pfcol= "white", plwd=1, pty = 32,
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=1,
           
           ylim = c(-1,1.2)
           )

legend(x=0.6, y=1.3, legend = "No Players selected", bty = "n", text.col = 'black', cex=1.2, pt.cex=3)


library(plotly)



fig <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself',
  mode = 'markers'
) 
for (i in 3:4) {
fig <- fig %>%
  add_trace(
    r = as.numeric(testo[i, ]/testo[1, ]),
    theta = colnames(testo),
    name = rownames(testo)[3]
  ) 
}

fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,1)
      )
    )
  )

fig

par(mar=c(0, 0, 0, 0))
radarchart(emptyradar,
           #custom polygon
           pcol="grey", pfcol= "white", plwd=1, pty = 32,
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=1,
           
           ylim = c(-1.5,1.2),
           xlim = c(-1,1)
)

legend(x=-.4, y=-1, legend = "No Players selected", bty = "n", text.col = 'black', cex=1.2, pt.cex=3)


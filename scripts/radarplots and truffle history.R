#### detail expansion ----

scsnaps <- action_mod(snaps, "FRR")

reactable(snaps,
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
            TRUFFLE = trfDef(),
            Pos = posDef(),
            Player = playerDef(filt = T),
            Team = nflDef,
            `Total Snaps` = colDef(minWidth = 150, format = colFormat(percent = F))
          )
)

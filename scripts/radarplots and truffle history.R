#### detail expansion ----

reactable(extradashszn,
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
            Season = seasonDef(),
            TRUFFLE = trfDef(),
            Pos = posDef(),
            Player = playerDef(filt = T),
            G = gDef(),
            `Cmp%` = colDef(header = with_tt("Cmp%", "Completion Percentage"), format = colFormat(percent = T), minWidth = 75),
            Pa20 = colDef(header = with_tt("20+", "Passing Completions of 20+ Yd"), class = "border-left-grey"),
            Pa40 = colDef(header = with_tt("40+", "Passing Completions of 40+ Yd")),
            RuYPC = colDef(header = with_tt("YPC", "Rushing Yards per Carry"), class = "border-left-grey", minWidth = 60),
            Ru20 = colDef(header = with_tt("20+", "Rushes of 20+ Yd")),
            Tar = colDef(header = with_tt("Tar", "Targets"), class = "border-left-grey"),
            `Tar%` = colDef(header = with_tt("Tar%", "Percentage of Team Targets"), minWidth = 70, format = colFormat(suffix = "%")),
            ReYPC = colDef(header = with_tt("YPC", "Yards per Catch"), minWidth = 60),
            Re20 = colDef(header = with_tt("20+", "Receptions of 20+ Yd")),
            Re40 = colDef(header = with_tt("40+", "Receptions of 40+ Yd")),
            `ReFD%` = colDef(header = with_tt("FD%", "Percentage of Receptions resulting in First Down"), minWidth = 65, format = colFormat(percent = T)),
            TotYd = colDef(header = with_tt("TotYd", "Total Passing/Rushing/Receiving Yards"), minWidth = 70)
          ),
          columnGroups = list(
            colGroup(name = "Passing", columns = c("Cmp%","Pa20","Pa40"), align = 'left'),
            colGroup(name = "Rushing", columns = c("RuYPC", "Ru20"), align = 'left'),
            colGroup(name = "Receiving", columns = c("Tar", "Tar%", "ReYPC", "Re20", "Re40", "ReFD%"), align = 'left')
          )
)

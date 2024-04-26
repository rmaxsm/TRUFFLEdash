#### detail expansion ----

reactable(ft[Player %in% c("Dalvin Cook", "Ezekiel Elliott", "Davante Adams")],
          compact = T,
          columns = list(
            Pos = posDef(filt = F),
            Player = playerDef(minW = 125),
            Salary = salaryDefBar(minW = 125),
            Contract = contractDef(filt = F, name = "Yr"),
            TagVal = colDef(header = with_tt("Tag Value", "Player Tag Value for Next Year"),
                            minWidth = 100,
                            align = "right")
          )
          )

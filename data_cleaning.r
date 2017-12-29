


## remove data for shifts which produce multiple IDs on a m/c
## for this, combine the date, shift & m/c column, & retain unique rows
prdcn$DateShiftMc <- paste(prdcn$Date, prdcn$Shift, prdcn$Machine)
tmp <- prdcn[! prdcn$DateShiftMc %in% unique(prdcn[duplicated(prdcn$DateShiftMc), "DateShiftMc"]), ] # unique(prdcn, by = prdcn$DateShift)

## remove rows with negative value in production
tmp <- tmp[!(tmp$OKProductionPcs < 0) , ]


## cleaning production data
## remove rows where the categorical var. x1 value is blank (NA)
## remove rows where the continuous var x2 value is blank (NA)
## remove rows where the kpi is blank (NA) 
## set rows with 'NA' in x3 & x4 to '0'
## remove rows where x4 is negative

setwd("C:/Users/Desktop/data/analysis/")

### read production data for casting 
raw_input <- read.csv("C:/Users/Desktop/data/analysis/production.csv")


###############################################################################
## dealing with NA values : removing rows
raw_input[raw_input==""] <- NA

## detect column no.s for x1, x2 & kpi, which have 'NA' in some rows, & remove these rows  
### Method 1 ###
#col1 <- as.integer(match("x1",names(raw_input)))
#col2 <- as.integer(match("x2",names(raw_input)))
#col3 <- as.integer(match("kpi",names(raw_input)))
#col4 <- as.integer(match("y",names(raw_input)))
## remove rows with NA
#prdcn <- raw_input[complete.cases(raw_input[,col1:col2]),]
#prdcn <- raw_input[complete.cases(raw_input[,col3:col4]),]


### Method 2 ###
prdcn <- raw_input[!(raw_input$x1 == "" | is.na(raw_input$x1) | raw_input$x2 == "" | is.na(raw_input$x2)
                     | raw_input$y == "" | is.na(raw_input$y)), ]
#prdcn <- raw_input[!(raw_input$x2 == "" | is.na(raw_input$x2)), ]
#prdcn <- raw_input[!(raw_input$kpi == "" | is.na(raw_input$kpi)), ]
#prdcn <- raw_input[!(raw_input$y == "" | is.na(raw_input$y)), ]


### Method 3 : remove rows where at least any 1 cell is NA ###
df <- na.omit(df)

######################################################################
### set NA to zero for continuous var x3 & x4 ######
prdcn$x3 <- as.character(prdcn$x3)
prdcn$x4 <- as.character(prdcn$x4)
prdcn$x3[is.na(prdcn$x3)] <- 0
prdcn$x4[is.na(prdcn$x4)] <- 0
prdcn$x3 <- as.integer(prdcn$x3)
prdcn$x4 <- as.integer(prdcn$x4)

# for all cols of df
df[is.na(df)] <- 0

######################################################################
## remove data for shifts which produce multiple IDs on a m/c
## for this, combine the date, shift & m/c column, & retain unique rows
prdcn$DateShiftMc <- paste(prdcn$Date, prdcn$Shift, prdcn$Machine)
tmp <- prdcn[! prdcn$DateShiftMc %in% unique(prdcn[duplicated(prdcn$DateShiftMc), "DateShiftMc"]), ] # unique(prdcn, by = prdcn$DateShift)

## remove rows with negative value in x4
tmp <- tmp[!(tmp$x4 < 0) , ]


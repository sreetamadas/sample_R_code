## cleaning data : deal with NA, INF or missing values

## remove rows where the categorical var. x1 value is blank (NA)
## remove rows where the continuous var x2 value is blank (NA)
## remove rows where the kpi is blank (NA) 
## set rows with 'NA' in x3 & x4 to '0'
## remove rows where x4 is negative

setwd("C:/Users/Desktop/data/analysis/")

### read data 
raw_input <- read.csv("C:/Users/Desktop/data/analysis/data.csv")

################################################################################################
## adding rows corresponding to missing time stamps
# method 1
sub <- subset(T, T$date == dateList[i])
sub <- pad(sub)  # generate rows corresponding to missing time stamps
sub$new_T <- sub$temp   # duplicate the temp column
sub$new_T <- na.locf(sub$new_T, fromLast = FALSE) # for all missing entries, copy from previous value  
new_df <- rbind(new_df, sub)

# method 2
# get list of dateTimes to be filled in
daterange=c(as.POSIXlt(min(T$dateTime)), as.POSIXlt(max(T$dateTime)))
ts <- seq(daterange[1], daterange[2], by="5 min") 
ts <- as.data.frame(ts)
colnames(ts)[1] <- 'dateTime'
 
# add missing dateTime at end of day, replace missing data with zero
# this leads to a daily pattern of zero values, which is being picked by the code as a seasonal pattern; this is unreasonable
new_df <- merge(new_df, ts, by='dateTime', all=TRUE)
new_df$new_T[is.na(new_df$new_T)] <- 0


################################################################################################
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
# remove rows with no dateTime
d <- d[!is.na(d$timestamp),]


### Method 2 ###
prdcn <- raw_input[!(raw_input$x1 == "" | is.na(raw_input$x1) | raw_input$x2 == "" | is.na(raw_input$x2)
                     | raw_input$y == "" | is.na(raw_input$y)), ]
#prdcn <- raw_input[!(raw_input$x2 == "" | is.na(raw_input$x2)), ]
#prdcn <- raw_input[!(raw_input$kpi == "" | is.na(raw_input$kpi)), ]
#prdcn <- raw_input[!(raw_input$y == "" | is.na(raw_input$y)), ]


### Method 3 : remove rows where at least any 1 cell is NA ###
df <- na.omit(df)
## remove NA values
new_df <- new_df[complete.cases(new_df),]

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

# set Inf to zero, or some other value
new_df$col_calc <- new_df$col1/new_df$col2
new_df$col_calc[is.na(new_df$col_calc)] <- -0.01
new_df$col_calc[new_df$col_calc==Inf] <- 0

######################################################################
## filling with values from preceding or succeeding cells
#library(padr)  pad(a)
# https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
# https://stackoverflow.com/questions/40219973/insert-new-series-rows-based-on-time-stamp-in-r
# tidyr::fill
library(zoo)
sel_dat <- na.locf(sel_dat, fromLast = FALSE)  # for all entries, copy from previous value
sel_dat <- na.locf(sel_dat, fromLast = TRUE)  # for 1st entry, copy from next value



##########################################################################################
###########################################################################################
## remove data for shifts which produce multiple IDs on a m/c
## for this, combine the date, shift & m/c column, & retain unique rows
prdcn$DateShiftMc <- paste(prdcn$Date, prdcn$Shift, prdcn$Machine)
tmp <- prdcn[! prdcn$DateShiftMc %in% unique(prdcn[duplicated(prdcn$DateShiftMc), "DateShiftMc"]), ] # unique(prdcn, by = prdcn$DateShift)

## remove rows with negative value in x4
tmp <- tmp[!(tmp$x4 < 0) , ]


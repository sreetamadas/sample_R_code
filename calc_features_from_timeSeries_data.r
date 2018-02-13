#calculate total E, total time & no. of switches/ state changes per day -> these are the features

# handle missing data, duplicates
# add time stamp at the beginning of each hr, to enable correct timedel calculation

#library(readxl)
#library(plyr)
#library(padr)  pad(a)
# library(sqldf)   # array_name = sqldf("select DISTINCT sno as 'sensor ID' from dat")

library(zoo)  # na.locf
library(corrplot)
attach(mtcars, warn.conflicts = FALSE)
library(RColorBrewer)

library(ggplot2)
require(cowplot)
Palette1 <- c('black','green','grey','red')

setwd("C:/Users/Desktop/analysis/") #KPI_CM")
fileLoc2 <- "C:/Users/Desktop/data/"


# input data
dat <- read.csv(paste(fileLoc2,'appliance_events_17_Jan.csv',sep=''))
dat$name <- NULL  ## remove unnecessary cols

# split the ts column into date & time, format & then join
dat$date <- substring(dat$ts, 1, 10)
dat$time <- substring(dat$ts, 12, 19)
dat$date <- format(as.POSIXct(dat$date), format='%Y-%m-%d')
dat$time <- format(as.POSIXct(dat$time, format='%H:%M:%S'), format='%H:%M:%S')

dat$dateTime <- format(as.POSIXct(paste(dat$date, dat$time, sep =' '), format='%Y-%m-%d %H:%M:%S'))
#dat$dateTime <- as.POSIXct(dat$dateTime, format="%Y-%m-%d %H:%M:%S") + 5*60*60 + 30*60  ### add 5 hours 30 min
dat$date <- format(as.POSIXct(dat$dateTime, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d')
dat$time <- format(as.POSIXct(dat$dateTime, format='%Y-%m-%d %H:%M:%S'), format='%H:%M:%S')


## device ID to appliance name mapping
dev_id <- read.csv("deviceID_type_mapping.csv")
IDlist_event <- as.character(sort(unique(dat$sno)))


################################################################
####  get list of dates
dateList <- as.character(sort(unique(dat$date)))   # dateList <- c("2018-01-15","2018-01-16")

## prepare df for adding time stamps at the start of each hr, if missing
dateTime <- c()  #date <- c()

for (k in 1:length(dateList)) {
  
  #sel_dat <- subset(dat, dat$sno == id & dat$date == dateList[k])
  #D <- paste(dateList[k], ' ', sep='')
  
  for (i in 1:24) {
    i <- i-1
    if(i < 10 ) {
      i=paste("0", i, sep = '')
    }
    #date <- append(date, dateList[k])
    dateTime <- append(dateTime,paste(dateList[k],' ',i,':00:00', sep=''))
  }
}
tmp <- data.frame(dateTime)  #, date)
rm(dateTime, i, k)

# remove times which exceed the last entry in dat or other input file
tmp <- subset(tmp, as.integer(as.POSIXct(tmp$dateTime)) <= as.integer(as.POSIXct(dat[nrow(dat),]$dateTime))  )


##############################################################
## generate feature maps for each dev

for (id in 1:length(IDlist_event)) {
  
  # app type
  app <- as.character(dev_id$Appliance.Name[dev_id$Appliance.ID == IDlist_event[id] ])
  print(id)
  
  # subset data for device
  sel_dat <- subset(dat, dat$sno == IDlist_event[id])


  # create a column combining date, from & to
  sel_dat$chk_col <- paste(as.numeric(as.POSIXct(paste(sel_dat$date, sel_dat$time, sep = ' '))), sel_dat$from, sel_dat$to, sep ='') 
  # remove adjacent duplicates
  # https://stackoverflow.com/questions/27022057/removing-only-adjacent-duplicates-in-data-frame-in-r
  sel_dat = sel_dat[with(sel_dat, c(chk_col[-1]!= chk_col[-nrow(sel_dat)], TRUE)),]
  
  # for multiple switchings in a minute, reassign the time (seconds value)
  #sel_dat$new[] <- 
  sel_dat$new <- unlist(sapply(unique(sel_dat$dateTime), function(x) {
    freq <- sum(x == sel_dat$dateTime)
    as.character(seq(as.POSIXct(x, format="%Y-%m-%d %H:%M:%S"), by = 60/freq, length.out = freq))  #(new, by=freq, length.out=freq)
  }))  


  # remove unnecessary cols:
  #sel_dat$time <- NULL
  sel_dat <- sel_dat[ , !names(sel_dat) %in% c("time","hr","chk_col","dateTime")] ## delete multiple cols: works as expected
  names(sel_dat)[names(sel_dat) == "new"] <- "dateTime"
  
  
  ## add time stamps at the start of each hr, if missing
  sel_dat <- merge(sel_dat, tmp, by.x=c('dateTime'), all.x = TRUE, all.y = TRUE)  
  
  #### fill in rows corresponding to added timestamps
  #library(padr)  pad(a)
  # https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
  # https://stackoverflow.com/questions/40219973/insert-new-series-rows-based-on-time-stamp-in-r
  # tidyr::fill
  sel_dat <- na.locf(sel_dat, fromLast = FALSE)  # for all entries, copy from previous value
  sel_dat <- na.locf(sel_dat, fromLast = TRUE)  # for 1st entry, copy from next value
  
  
  ####  calculate intervals between successive events
  c_time <- as.POSIXlt(sel_dat$dateTime, format="%Y-%m-%d %H:%M:%S" )
  timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
  
  ## add a value to end (or start?) of column, as length(timedel) = (No. of rows in DF) - 1 
  # check what value should be added
  timedel <- append(timedel, 60) #'NA') 
  #sel_dat <- sel_dat[-nrow(sel_dat),]   ## remove last row of data, corrs to which there is no time duration
  sel_dat <- cbind(sel_dat,timedel)  ## add the time differences column to the dataframe
  sel_dat$timedel[is.na(sel_dat$timedel)] <- 0  ## this is to take care of entries where the dateTime was modified to date (see above)
  
  ### calculate ON & Off time for each instance
  sel_dat$to <- as.numeric(sel_dat$to)
  sel_dat$from <- as.numeric(sel_dat$from)
  sel_dat$on <- sel_dat$to * sel_dat$timedel   # in secs; divide by 60 to get in min
  sel_dat$off <- (sel_dat$timedel - sel_dat$on)  # in secs; divide by 60 to get in min
    
  ### indicate switches in state
  state_change <- abs(diff(sel_dat$to))
  # add 0 (1?) at the beginning of the vector for no state change (change from previous day?) ******
  state_change <- append(0,state_change)
  # add to df
  sel_dat <- cbind(sel_dat, state_change)
  ## change switch logic : consider 1 -> 0 as a switch
  sel_dat$ch <- 0
  for(r in 1:nrow(sel_dat)) {
    if(sel_dat[r,]$state_change == 1 & sel_dat[r,]$from == 1 & sel_dat[r,]$to == 0) {
      sel_dat[r,]$ch = 1
      }
  }

###########################################################################################
## calculate features for hourly data
  sel_dat$dateHr <- format(as.POSIXct(sel_dat$dateTime, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d %H')
  
  new_df <- data.frame(as.character(levels(as.factor(sel_dat$dateHr))))
  colnames(new_df)[1] <- 'dateHr'
  # calculate switchings
  switch <- tapply(sel_dat$ch, sel_dat$dateHr, FUN=sum)
  ###sel_dat2 <- sel_dat #######
  # remove last row of sel_dat to remove the extra value added in timedel
  ###  sel_dat <- sel_dat[-nrow(sel_dat),]
  # calculate On time
  t_ON <- tapply(sel_dat$on, sel_dat$dateHr, FUN=sum)   # in secs
  # calculate off time
  t_OFF <- tapply(sel_dat$off, sel_dat$dateHr, FUN=sum)   # in secs
  new_df <- cbind(new_df, t_ON, t_OFF, switch)
  new_df$t_OFF <- new_df$t_OFF/60  # in min
  new_df$t_ON <- new_df$t_ON/60  # in min
  new_df$total_time <- new_df$t_ON + new_df$t_OFF
  





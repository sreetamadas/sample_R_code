#### assign day of week  #####
df$date <- format(as.POSIXct(df$txtime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d") # %H:%M:%S
df$day <- as.POSIXlt(df$date)$wday  # as number; sunday -> 0, Monday -> 1 & so on
df$day2 <- weekdays(as.Date(df$txtime))  # as text, ie, by name
             
             
### calculate time differences ######
c_time <- as.POSIXlt(df$s15.txtime )
timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")  
## (OR), timedel2 <- as.numeric(diff(df$s15.txtime))
timedel <- append(timedel,'NA') ## add 'NA' to end of column, as length(timedel) = (No. of rows in DF) - 1 
df <- cbind(df,timedel)  ## add the time differences column to the dataframe


c_time <- as.POSIXlt(df$s15.txtime )
timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
# (OR), timedel2 <- as.numeric(diff(df$s15.txtime))
#timedel <- append(timedel, -999) #'NA') ## add 'NA' to end of column, as length(timedel) = (No. of rows in DF) - 1 
df <- df[-nrow(df),]   ## remove last row of data, corrs to which there is no time duration
df <- cbind(df,timedel)  ## add the time differences column to the dataframe


### split the ts column into date & time
dat$date <- substring(dat$ts, 1, 10)
dat$time <- substring(dat$ts, 12, 19)
                           
                           
### adding time interval to a given time stamp
dat$dateTime <- as.POSIXct(dat$dateTime, format="%Y-%m-%d %H:%M:%S") + 5*60*60 + 30*60  ### add 5 hours 30 min
                           
             
##################################################################################################
### calculate aggregates date wise
new_df <- data.frame(as.character(levels(as.factor(E$date))))
colnames(new_df)[1] <- 'date'
tot <- tapply(E$x, E$date, FUN=sum)
new_df <- cbind(new_df,tot)
new_df$date <- format(as.POSIXct(new_df$date, format="%Y-%m-%d"), format="%Y-%m-%d" )


## if the daily aggregates are to be calculated from a different starting time, instead of 00:00 hrs, offset the time & then do as above
# e.g. from 6am of today to 6am of next day
E$dateTimeOffset <- as.POSIXct(E$DateTime, format="%Y-%m-%d %H:%M:%S") - 6*60*60   ### subtract 6 hours 
E$date_offset <- format(as.POSIXct(E$dateTimeOffset, format = "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")
new_df <- data.frame(as.character(levels(as.factor(E$date_offset))))
colnames(new_df)[1] <- 'date'
#....






## check if there is discontinuity in dates
## could be due to missing data packets, 
## or improper ordering of date stamps
## repeats in date stamps - either mistake, or collected at shorter duration (than specified)

## set directory

## read input data (converted to separate csv from excel file)
library(readxl)
input <- 'file1'
mc <- read_excel(paste("C:/User/Desktop/data/", input, ".xls", sep = ''))

## create a new dataframe for calculations
df <- data.frame(mc$Date)

## format date & time to 24-hr format
df$DateTime <- format(as.POSIXct(mc$Date, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")


## calculate time intervals b/w successive instances of operation
c_time <- as.POSIXlt(df$DateTime, format="%Y-%m-%d %H:%M:%S")
timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)])) #, units="mins")
df <- df[-nrow(df),]   ## remove last row of data, corrs to which there is no time duration
df <- cbind(df,timedel)  ## add the time differences column to the dataframe


## plot time difference vs time stamp to locate gaps
#plot(df$DateTime, df$timedel, xlab="time", ylab="time difference", type='l', lwd=0.6)  ## not working
plot(df$timedel, type='l', lwd=0.6, ylab="time difference b/w successive timestamps")


## write out the time differences
write.csv(df, file=paste(input,"_timeDifferences.csv"))



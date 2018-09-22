## condition monitoring real time data

##########################################################
# 1. use rolling mean computed with anomalies removed
# 2. use higher threshold with MAD 
# 3. use quartiles with modified thresholds
##########################################################

## keep 1 day's data in memory
# as each new data point comes in, push it to the end of the df, & push out elements from the top
# also, add this new data point to a new df
# against each such entry, compute the mean & sd (or median & MAD) using the window  ******
# (moving average == rolling mean)
# flag as anomaly if the deviation is >threshold
# compute hr of day
# check if there is a switch in state; assign a new no. for each such change
# calculate time & cum time - check if cum time is above a threshold

## CHECK THAT
# 1. same results are obtained with the real-time & batch data methods - YES

# 2. check that the method of filling in the missing time points does not affect the anomalies detected;
#    check with & without data imputation; alternately, leave out anomalies at the night time
#    (keep a separate column with flags for imputed temp - anomalies should not be from this period)
#    - less anomalies are obained if we dont impute the data points, or no imputation + shorter rolling window
#    also, less anomalies are obtained with moving/ rolling average & SD; check if median-based approaches &
#    EWMA are required
#    use rolling median + shorter window ???  ****
##   do Q-Q plot : actual temp vs MA temp   ***
### are the energy data (from disaggregation) correct ? try with multi-variate data from some other source  ****


# 3. setting the timedel threshold as 1 hr for anomalies from 11am-5pm will miss out true anomalies for shorter duration
#    - set timedel to a lower threshold

# 4. how to differentiate b/w temp rise due to loading time/ left open vs due to freezer not working properly


### other related methods ###
#https://cran.r-project.org/web/packages/anomalize/anomalize.pdf
# https://www.datascience.com/blog/python-anomaly-detection
# https://anomaly.io/anomaly-detection-moving-median-decomposition/
# https://jalobe.com/doc/tsoutliers.pdf
#https://github.com/robjhyndman/forecast/tree/master/R


################################################################################################################

library(padr)
library(zoo)  # na.locf


path <- "C:/Users/Desktop/data/"
setwd(path)


IDlist_event <- c("10", "20", "30", "40", "50")  # 
a <- c("mc1", "mc2", "mc3", "mc4", "mc5" ) #
dev_id <- data.frame(IDlist_event,a)
rm(a)
colnames(dev_id) <- c('Appliance.ID','Appliance.Name')
id <- 1   #5


# appliance type
app <- as.character(dev_id$Appliance.Name[dev_id$Appliance.ID == IDlist_event[id] ])
## temp data input
T <- read.csv(paste(path,'data_all/temp_cold_rooms/','temp_',IDlist_event[id],'.csv', sep=''), header=TRUE)## CHANGE FILE  *****
## fix dateTime to IST from UTC
T$dateUTC <- as.POSIXct(T$dateUTC, format="%Y-%m-%d %H:%M:%S", tz='UTC') + 5*60*60 + 30*60  ### add 5 hours 30 min
## remove extra columns
T <- T[ , !names(T) %in% c("applId")]
# rename col
names(T)[names(T) == "dateUTC"] <- "dateTime"
## option2 : fill all missing values with prev value
T <- pad(T)  #T$temp_copy <- T$temp
T$temp <- na.locf(T$temp, fromLast = FALSE) # for all missing entries, copy from previous value  

###############################################################################################################
## function definitions

calc_mean <- function(df) {
  avg <- mean(df$temp)
  return(avg)
}

###############################################################################################################

## backup 1 day's data 
rolling_window <- 180  # 180 -> not imputaing missing data;  288 -> imputing missing data every night
thres <- 1.5             # 2.5 ; 3 ; 2

backup <- T[1:rolling_window,]
# assign a new column to label anomalies for incoming data;
#the corresponding data points will not be used for calculatinf sd
backup$row_anom <- 0

## push last 1 data point into a new df
current <- T[rolling_window,]
current$hr <- as.numeric(format(as.POSIXct(current$dateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))
current$timedel <- 0

#current$avg <- mean(backup$temp)  ## change the averaging & thresholding methods to change no. of anomalies
#current$sd <- sd(backup$temp)
#current$anomaly <- ifelse(current$temp - current$avg > thres*current$sd, 1, 0)  # 2, 2.5, 3

current$median <- median(backup$temp)
current$sd <- sd(backup$temp - median(backup$temp))
current$anomaly <- ifelse((current$temp - current$median) > thres*current$sd, 1, 0)

#current$mad <- median(abs(backup$temp - median(backup$temp) ) )
#current$anomaly <- ifelse((current$temp - current$median) > thres*current$mad, 1, 0)
#
#del <- median(abs(backup$temp - mean(backup$temp)) )
#current$anomaly <- ifelse(current$temp - current$avg > 3*del, 1, 0)
#
#current$del <- sqrt(sum((backup$temp - median(backup$temp))**2)/(nrow(backup) - 1))
#current$anomaly <- ifelse(current$temp - current$median > thres*current$del, 1, 0)   # thres=2.5 ; 3

#q1 <- function(x) {quantile(x, na.rm=TRUE)[2]}
#q3 <- function(x) {quantile(x, na.rm=TRUE)[4]}
#current$Q1 <- q1(backup$temp - median(backup$temp))
#current$Q3 <- q3(backup$temp - median(backup$temp))
#current$IQR <- current$Q3 - current$Q1
#current$del <- current$temp - current$median - current$Q3
#current$anomaly <- ifelse(current$temp - current$median - current$Q3 > thres*(current$Q3 - current$Q1), 1, 0)


#current$state_change <- 0
current$csum_time <- 0
current$final_anomaly <- 0
current$final_anomaly30 <- 0
#current$entry <- 1

## how many data points were used to calculate sd
current$sd_row <- rolling_window


index <- 1
#cnt <- 1

# create a loop to simulate reading in new data and checking for anomalies
for(i in (rolling_window + 1):nrow(T) ) {
  # add new data to backup
  backup <- backup[-1,]
  #backup <- rbind(backup, T[i,])
  backup[rolling_window,]$dateTime <- T[i,]$dateTime
  backup[rolling_window,]$temp <- T[i,]$temp
  backup[rolling_window,]$row_anom <- 0
  
  
  # go to new row of current & update
  index <- index + 1
  current[index,]$dateTime <- T[i,]$dateTime
  current[index,]$temp <- T[i,]$temp
  current[index,]$hr <- as.numeric(format(as.POSIXct(current[index,]$dateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))
  current[index,]$timedel <- as.numeric(difftime(as.POSIXlt(current[index,]$dateTime, format="%Y-%m-%d %H:%M:%S" ) , as.POSIXlt(current[(index-1),]$dateTime, format="%Y-%m-%d %H:%M:%S" ), tz = 'UTC'))

  #sub_back <- subset(backup, backup$row_anom == 0)
  #current[index,]$avg <- mean(subset(backup, backup$row_anom == 0)$temp)  #mean(backup$temp)
  #current[index,]$sd <- sd(subset(backup, backup$row_anom == 0)$temp)  #sd(backup$temp)
  current[index,]$median <- median(backup$temp)
  #current[index,]$mad <- median(abs(backup$temp - median(backup$temp) ) )
  #current[index,]$sd <- sd(backup$temp - median(backup$temp))
  current[index,]$sd <- sd(sub_back$temp - median(sub_back$temp))
  
  #current[index,]$Q1 <- q1(backup$temp - median(backup$temp))
  #current[index,]$Q3 <- q3(backup$temp - median(backup$temp))
  #current[index,]$IQR <- current[index,]$Q3 - current[index,]$Q1
  #current[index,]$del <- current[index,]$temp - current[index,]$median - current[index,]$Q3
  
  
  
  
  # 1st layer anomaly
  #      rolling mean, rolling SD
  #current[index,]$anomaly <- ifelse(current[index,]$temp - current[index,]$avg > thres*current[index,]$sd, 1, 0)
  #      rolling mean, rolling MAD
  #del <- median(abs(backup$temp - mean(backup$temp)) )
  #current[index,]$anomaly <- ifelse(current[index,]$temp - current[index,]$avg > 3*del, 1, 0)
  #      rolling median, rolling MAD
  #current[index,]$anomaly <- ifelse(current[index,]$temp - current[index,]$median > thres*current[index,]$mad, 1, 0)
  #      rolling median, rolling SD
  current[index,]$anomaly <- ifelse(current[index,]$temp - current[index,]$median > thres*current[index,]$sd, 1, 0)
  #      rolling median, deviation from median
  #current[index,]$del <- sqrt(sum((sub_back$temp - median(backup$temp))**2)/(nrow(sub_back) - 1))
  #current[index,]$anomaly <- ifelse(current[index,]$temp - current[index,]$median > thres*current[index,]$del, 1, 0)  # thres = 2.5 ; 3
  #      with quartile
  #current[index,]$anomaly <- ifelse(current[index,]$temp - current[index,]$median - current[index,]$Q3 > thres*(current[index,]$Q3 - current[index,]$Q1), 1, 0)
  
  
  ## detect state change
  #current[index,]$state_change <- abs(current[index,]$anomaly - current[index-1,]$anomaly)
  
  # assign a new no. for each change of state
  if(current$anomaly[index] != current$anomaly[index-1]) {
    #cnt <- cnt + 1
    current[index,]$csum_time <- current[index,]$timedel
  }  else {
    current[index,]$csum_time <- current[index,]$timedel + current[(index-1),]$csum_time
  }
  #current$entry[index] <- cnt
  
  
  # 2nd layer anomaly
  current[index,]$final_anomaly <- 0
  # long dur of high temp outside loading hr
  current[index,]$final_anomaly[(current[index,]$hr < 11 | current[index,]$hr > 17) & current[index,]$anomaly == 1 & current[index,]$csum_time > 10] <- 1
  # within loading hr, see high temp > 1hr & more than once
  current[index,]$final_anomaly[(current[index,]$hr >= 11 & current[index,]$hr <= 17) & current[index,]$anomaly == 1 & current[index,]$csum_time > 60] <- 1
  
  current[index,]$final_anomaly30 <- 0
  current[index,]$final_anomaly30[current[index,]$anomaly == 1 & current[index,]$csum_time > 30] <- 1
  
  
  current[index,]$sd_row <- nrow(subset(backup, backup$row_anom == 0))
  ## reassign the row_anom in backup data based on anomaly status in level 1
  backup[rolling_window,]$row_anom <- current[index,]$anomaly
  
  #if(current[index,]$final_anomaly == 1) {
  #  print(current[index,])
  #}
}

s <- subset(current, current$final_anomaly == 1)
write.csv(s, "real_time_anom2.csv")

# 1. use rolling mean computed with anomalies removed
# 2. use higher threshold with MAD 
# 3. use quartiles with modified thresholds

par(mfrow=c(4,1))  # no. of rows, no. of col : this creates 6 figs
par(mar=c(2,5,2,1))
library(ggplot2)
#plot(as.POSIXct(current$dateTime), current$avg, type='l', xlab='', main=paste('rolling median & MAD, thres=',thres,sep=''))
#plot(as.POSIXct(current$dateTime), current$median, type='l', xlab='',  main=paste('rolling median & SD with anomalies removed, thres=',thres,sep=''))
#plot(as.POSIXct(current$dateTime), current$mad, type='l', xlab='')   # current$del
#plot(as.POSIXct(current$dateTime), current$sd, type='l', xlab='')   # current$del
plot(as.POSIXct(current$dateTime), current$median, type='l', xlab='',  main=paste('rolling median & deviation from quartiles, thres=',thres,sep=''))
plot(as.POSIXct(current$dateTime), current$del, type='l', xlab='')   # current$del


# layer 1
current$col[current$anomaly == 0] <- 'green'
current$col[current$anomaly == 1] <- 'red'
plot(as.POSIXct(current$dateTime), current$temp, col=alpha(current$col,0.4), pch=16, xlab='')

# layer 2
current$col2[current$final_anomaly30 == 0] <- 'yellow'
current$col2[current$final_anomaly30 == 1] <- 'red'
plot(as.POSIXct(current$dateTime), current$temp, col=alpha(current$col2,0.4), pch=16, xlab='')



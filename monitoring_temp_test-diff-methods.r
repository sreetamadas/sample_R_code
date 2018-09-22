#####  temp monitoring  ######

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



# 1. bound-based temp anomaly (moving average == rolling mean)
# 2. deviations from temp predictions using GAM/ ARIMA/ LSTM


# try with energy - what happens when time stamps are not regularly placed?
# does arima work for non-seasonal data?
# does arima/ holt's method work for irregular time interval data?


# use a vector of temp, time since start of increase in temp. (from threshold) & time-of-day? , &  ****
# check clustering *******

# try with ON-off data : generate a state variable (1,0) for every instant at 1 min/ 1s/ 500ms interval
#########################################################################################################################

# there are missing time stamp in b/w vs at the end of day - how to handle the 2 separately?
# already got list of start & stop times everyday. fill all NA with last Temp value  (histograms of temp values, with & without padding, 
# show difference in relative freq. of bins, although the peak positions remain same)

# get a second list with time differences b/w successive entries for the original data
#    - select device with least packets lost

## THEORETICAL SOURCE FOR THIS METHOD
## see exact implementation by Jagadeesh (rolling sd or single sd for window?
##                                        effect of changing window size?)
## HOW TO TRY THIS REAL_TIME WITH DATA FLOWING IN ?
## HOW TO TAKE CARE OF DATA MISSING PERIODICALLY ?
# see histogram of temp distribution - bimodal for frozen freezer room



library(padr)
library(zoo)  # na.locf
library(ggplot2)

#library(tidyverse)
#library(anomalize)
#library(dplyr)


path <- "C:/Users/Desktop/data/"
setwd(path)


## device list
IDlist_event <- c("0419001004001", "041900187501", "0419002538301", "0419002899301", "041900824401")  # 
a <- c("Non Veg Freezer Room", "Dairy Cold Room", "F&V Cold Room", "Frozen Freezer Room", "Non Veg Cold Room" ) #
dev_id <- data.frame(IDlist_event,a)
rm(a)
colnames(dev_id) <- c('Appliance.ID','Appliance.Name')


id <- 1   #5


#for (id in 1:length(IDlist_event)) {
  
  # appliance type
  app <- as.character(dev_id$Appliance.Name[dev_id$Appliance.ID == IDlist_event[id] ])
  
  ## temp data input
  T <- read.csv(paste(path,'data_all/temp_cold_rooms/','temp_',IDlist_event[id],'.csv', sep=''), header=TRUE)## CHANGE FILE  *****
  
  
  ## another NV cold room with energy & event data
  #app <- 'nonVegColdRoom'
  #T <- read.csv(paste(path,'data_all/AE_EV_Temp_received15thMarch/','temp_041900824401.csv', sep=''), header=TRUE)## CHANGE FILE  *****
  
  
  ## fix dateTime to IST from UTC
  T$dateUTC <- as.POSIXct(T$dateUTC, format="%Y-%m-%d %H:%M:%S", tz='UTC') + 5*60*60 + 30*60  ### add 5 hours 30 min
  
  ## remove extra columns
  T <- T[ , !names(T) %in% c("applId")]
  
  # rename col
  names(T)[names(T) == "dateUTC"] <- "dateTime"
  
  # convert to tibble
  #T <- as_tibble(T)
  
  
  # get the start & stop points everyday
  #T$date <- format(as.POSIXct(T$dateTime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")
  #dateList <- as.character(sort(unique(T$date)))
  ###new_df <- data.frame(as.character(levels(as.factor(T$date)))) #colnames(new_df)[1] <- 'date'   #new_df$date <- format(as.POSIXct(new_df$date, format="%Y-%m-%d"), format="%Y-%m-%d")
  #t_start <- c()
  #t_end <- c()
  #new_df <- data.frame()
  
  #for(i in 1:length(dateList) ) {   #nrow(new_df)
    ## Getting start & stop points
    #s <- subset(T, T$date == dateList[i])$dateTime[1] #new_df$date[i])$dateTime[1]
    #e <- subset(T, T$date == dateList[i])$dateTime[nrow(subset(T, T$date == dateList[i]))] #new_df$date[i])$dateTime[nrow(subset(T, T$date == new_df$date[i]))]
    #s <- format(as.POSIXct(s, format="%Y-%m-%d %H:%M:%S"), format="%H:%M:%S")
    #e <- format(as.POSIXct(e, format="%Y-%m-%d %H:%M:%S"), format="%H:%M:%S")
    #t_start <- append(t_start,s)
    #t_end <- append(t_end, e)
    
    
    #### option 1
    ## filling missing values in the middle of a day; fill values at end of day separately
    #sub <- subset(T, T$date == dateList[i])
    #sub <- pad(sub)  # generate rows corresponding to missing time stamps
    #sub$new_T <- sub$temp   # duplicate the temp column
    #sub$new_T <- na.locf(sub$new_T, fromLast = FALSE) # for all missing entries, copy from previous value  
    #new_df <- rbind(new_df, sub)
  #}
  #new_df <- data.frame(dateList, t_start, t_end)
  #rm(t_start, t_end, s, e)
  
  
  ## get list of time differences b/w successive time stamps
  ## calculate intervals between successive events
  #c_time <- as.POSIXlt(T$dateTime, format="%Y-%m-%d %H:%M:%S" )
  #timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
  ## add a value to end (or start?) of column, as length(timedel) = (No. of rows in DF) - 1 
  # check what value should be added
  #timedel <- append(timedel, 0) #'NA') 
  #T <- cbind(T,timedel)  ## add the time differences column to the dataframe
  #T$timedel[is.na(T$timedel)] <- 0  ## this is to take care of entries where the dateTime was modified to date (see above)
  #png(paste(path,'analysis/',IDlist_event[id],'_',app,'_timeDel','.png', sep=''), h=400, w=600 )
  #plot(T$timedel, type='l', lwd=0.6, ylab="time difference b/w successive timestamps")
  #dev.off()
  #write.csv(T, paste(IDlist_event[id],'_',app,'_timeDel','.csv', sep=''))
  
  
  # get list of dateTimes to be filled in
  #daterange=c(as.POSIXlt(min(T$dateTime)), as.POSIXlt(max(T$dateTime)))
  #ts <- seq(daterange[1], daterange[2], by="5 min") 
  #ts <- as.data.frame(ts)
  #colnames(ts)[1] <- 'dateTime'
  
  
  # add missing dateTime at end of day, replace missing data with zero
  # this leads to a daily pattern of zero values, which is being picked by the code as a seasonal pattern;
  # this is unreasonable
  #new_df <- merge(new_df, ts, by='dateTime', all=TRUE)
  #new_df$new_T[is.na(new_df$new_T)] <- 0
  
  
  ## option2 : fill all missing values with prev value
  # can use a distribution to fill in instead of using flat values - see how to do this ?
  #	impute missing temp. data with ideal values of temp from a normal distribution within limits - sample & distribute
  T <- pad(T)  #T$temp_copy <- T$temp
  T$temp <- na.locf(T$temp, fromLast = FALSE) # for all missing entries, copy from previous value  
  #write.csv(T, paste(IDlist_event[id],'_',app,'_Temperature','.csv', sep=''))
  
  
  ## find anomalies: 1st layer of filtering - finding this threshold properly is crucial
  ## method 1:vector of temp, time since start of increase in temp. (from threshold) & time-of-day? , &
  # check clustering *******
  
  # set threshold; (can use diff approaches to choose thres -)
  # anomaly : outside 12-5pm, >1hr, after the first instance of 1 hr from 12-5
  #thres <- as.numeric(-5)
  #T$state <- ifelse(T$temp > thres, 1, 0)
  
  ## method 1 : using anomalize
  #df <- T %>%
    # decompose into trend, seasonality & remainder
    #time_decompose(temp) %>% #, method = "stl", frequency = "auto", trend = "auto", message = TRUE) %>%    ## method = stl / twitter
    # anomaly detection on the remainder component using IQR
    #anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%    # method = iqr or gesd
    # create the lower and upper bounds around the "observed" values
    #time_recompose()
  #T <- as_tibble(T)
  #T <- cbind(T, df$anomaly)
  #names(T)[names(T) == "df$anomaly"] <- "anomaly"
  #T$state <- ifelse(T$anomaly == 'Yes', 1, 0)
  #T$col[T$anomaly == 'No'] <- 'green'
  #T$col[T$anomaly == 'Yes'] <- 'red'
  
  
  
  
  ## method 2 : using rolling mean (from zoo package) & standard deviation
  # parameters to be changed: window length for MA
  #                           threshold for anomaly
  # this gives too few anomalies compared to the anomalize method
  rolling_window <- 288  # 180 -> not imputaing missing data;  288 -> imputing missing data every night
  T$roll_median <- rollapply(data = T$temp,  # original series
                           width = rolling_window,  # width of the rolling window  = 12 * 24
                           FUN = median, #na.rm = T,  # Any arbitrary function
                           fill = NA,
                           align='right')
  
  T$roll_sd <- rollapply(data = T$temp,  # original series
                          width = rolling_window,  # width of the rolling window  = 12 * 24
                          FUN = sd, #na.rm = T,  # Any arbitrary function
                          fill = NA,
                          align='right')
  
  # calculate deviation
  #T$dev <- T$temp - T$roll_mean
  T$dev <- T$temp - T$roll_median
  
  # flag as anomaly
  T$anomaly <- ifelse(T$dev > 2.5*T$roll_sd, 1, 0)   # times: 3, 2.5
  
  
  ## method 3: using sd deviation over full data instead of rolling
  #T$anomaly3 <- ifelse(T$dev > 3*sd(T$temp), 1, 0)
  
  
  ## method 4:  using rolling median & MAD
  #T$roll_median <- rollapply(data = T$temp,  # original series
  #                         width = rolling_window,  # width of the rolling window  = 12 * 24
  #                         FUN = median, #na.rm = T,  # Any arbitrary function
  #                         fill = NA,
  #                         align='right')
  # flag as anomaly
  #T$anomaly4 <- ifelse(T$temp > 3*median(abs(T$temp - T$roll_median), na.rm=TRUE), 1, 0)
  
  
  ## method 5: rolling median & median-based deviation (full data) -> less anomalies than above method, more than anomalize
  #T$anomaly6 <- ifelse((T$temp - T$roll_median) > 3*median(abs(T$temp - T$roll_median), na.rm = TRUE), 1, 0)
  
  
  ## method 6: rolling median & median-based deviation by window  (gives too many anomalies)
  #ut <- function(x) {m = median(x); median(x) + 3 * median(abs(x - m))}
  #T$med_dev <- rollapply(T$temp, rolling_window, ut, align="right", fill=NA)
  #T$anomaly5 <- ifelse(T$temp > T$med_dev, 1, 0)
  
  
  
  
  # set NA values as 0
  T$anomaly[is.na(T$anomaly)] <- 0
  #T$anomaly6[is.na(T$anomaly6)] <- 0
  
  T$col[T$anomaly == 0] <- 'green'
  T$col[T$anomaly == 1] <- 'red'
  
  ## check time of day
  T$hr <- as.numeric(format(as.POSIXct(T$dateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))
  
  
  #plot(as.POSIXct(T$dateTime), T$temp, col=T$col)   ## all plots at the end
  #par(mfrow=c(2,1))  # no. of rows, no. of col : this creates 6 figs
  #par(mar=c(5,3,2,1))
  #plot(as.POSIXct(T$dateTime), T$temp, type='l', xlab='')
  #plot(as.POSIXct(T$dateTime), T$roll_mean, type='l', col='blue', xlab='')
  
  
  # check day of week
  #T$date <- format(as.POSIXct(T$dateTime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")
  #T$day <- as.POSIXlt(T$date)$wday # as number; sunday -> 0, Monday -> 1 & so on
  #plot(T$hr, T$temp, col=T$col)
  
  
  # indicate switches in state
  #state_change <- abs(diff(T$state))  # use with anomalize
  #state_change <- abs(diff(T$anomaly))  # use with rolled methods
  # add 0 (1?) at the beginning of the vector for no state change (change from previous day?) ******
  #state_change <- append(0,state_change)
  # add to df
  #T <- cbind(T, state_change)
  
  
  # assign a new no. for each change of state
  T$entry[1] <- 1
  cnt <- 1
  for (i in 2:nrow(T) ) {
    if(T$anomaly[i] != T$anomaly[i-1]) {
    #if(T$state[i] != T$state[i-1]) {
      cnt <- cnt + 1
    }
    T$entry[i] <- cnt
  }
  
   
  ## for each Temp > threshold, add the consecutive time stamps : cumulative time in that state
  # steps : calculate diff time; calculate cumsum
  c_time <- as.POSIXlt(T$dateTime, format="%Y-%m-%d %H:%M:%S" )
  timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
  ## add a value to end of column, as length(timedel) = (No. of rows in DF) - 1 
  timedel <- append(timedel, 0) #'NA') 
  T <- cbind(T,timedel)  ## add the time differences column to the dataframe
  T$csum_time <- ave(T$timedel, T$entry, FUN=cumsum) #cumsum(T$timedel)
  
  
  
  
  ## anomalies : 2nd layer of filtering  ****
  T$final <- 0
  
  # long dur of high temp outside loading hr
  T$final[(T$hr < 11 | T$hr > 17) & T$anomaly == 1 & T$csum_time > 10] <- 1   #T$state == 1 
  # within loading hr, see high temp > 1hr & more than once
  T$final[(T$hr >= 11 & T$hr <= 17) & T$anomaly == 1 & T$csum_time > 60] <- 1  #T$state == 1 
  
  ## alternately, flag all high temp duration > 30 min as anomaly
  T$final30 <- 0
  T$final30[T$anomaly == 1 & T$csum_time > 30] <- 1   #T$state == 1 
  
  
  T$col2[T$final == 0] <- 'yellow'
  T$col2[T$final == 1] <- 'red'
  #plot(T$hr, T$temp, col=T$col2, type='b')
  
  par(mfrow=c(5,1))  # no. of rows, no. of col : this creates 4 figs
  par(mar=c(2,5,2,1))
  plot(as.POSIXct(T$dateTime), T$temp, type='l', xlab='')
  plot(as.POSIXct(T$dateTime), T$roll_mean, type='l', xlab='')
  plot(as.POSIXct(T$dateTime), T$roll_sd, type='l', xlab='')
  plot(as.POSIXct(T$dateTime), T$temp, col=alpha(T$col,0.5), type='b', pch=16, xlab='', ylab='anom 1')
  plot(as.POSIXct(T$dateTime), T$temp, col=alpha(T$col2,0.5), type='b', pch=16, xlab='', ylab='anom 2')  ## DO FACET PLOTS
  
  #s <- subset(T, T$anomaly == 'Yes')  # anomaly from 1st filter
  #s1 <- subset(T, T$col2 == 'red')    # anomaly from 2nd filter
  
  ## subset & print anomalies
  s <- subset(T, T$final == 1)
  write.csv(s, "batch_anom2.csv")
  
  ###############################################################################################
  ## method 2 : find anomalies
  # https://business-science.github.io/anomalize/
  T %>%
    # decompose into trend, seasonality & remainder
    time_decompose(temp) %>% #, method = "stl", frequency = "auto", trend = "auto", message = TRUE) %>%    ## method = stl / twitter
    
    # anomaly detection on the remainder component using IQR
    anomalize(remainder, method = "iqr") %>%  #, alpha = 0.05, max_anoms = 0.2) %>%    # method = iqr or gesd
    
    # create the lower and upper bounds around the "observed" values
    time_recompose() %>%
    
    # plot
    plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
  
    # visualize anomalies superposed on the STL decomposition
    #plot_anomaly_decomposition() +
    #labs(title = "Decomposition of Anomalized temp.")
  
  #Error in mutate_impl(.data, dots) : Evaluation error: invalid 'tz' value  -> fixed by adding tz='UTC'
  #Error: `var` must evaluate to a single number or a column name, not a list 
  #  -> input to time_decompose should be the reqd col from df
  
  
  ##########################################################################################################
  ## method 3 : check deviation from prediction of time series
  
  library(fpp)
  ## check with different deltat / frequency
  # x <- ts(df$Y, deltat = 1/60)  # working value: 1/60, 1/(60*24)  : (this seems better)
  rolling_window <- 288
  x <- ts(new_df$new_T, frequency = 288) # 24 hrs * 12 =  288; # 1day * 24hrs * 60min = 10080   # T$temp
  fit <- stl(x, "periodic")
  plot(fit)
  
  ## accessing stl output
  fit
  fit$time.series[,"trend"]  # fit$time.series[,"remainder]
  
  ## when using weighted moving average for trend calculation/ ARIMA, if the last included point is an anomaly,
  # the future predicted points will start to move away from the actual distribution towards anomalous values 
  
  
  ########################################################################################################
  ## for historical data
  # calculate daily histograms of temp in different bins: show as heatmap (date vs temp bin; colour = time dur)
  
  
#}  

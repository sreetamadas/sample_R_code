###### Measure mc performance scores ######
# 1. for predictive maintenance, we need to define score thresholds (on some basis: heating time?, etc.) 
#    beyond which the machine is said to be at risk
# 2. Also, the scores used need not be at intervals of 1 (i.e slope may be different), and may not be linear
#    (scoring scale should be based on some physical priciples)
# 3. the scores can vary with the time interval spent in a particular state
# 4. Demand could also be taken into account in the scoring system (?)
#
## also, plot scores for successive timepoints

######### METHOD 1 ###########
# 1. subset data for 1 hour intervals
# 2. calculate the total duration for which the system was in a particular state during this 1 hr
#    (irrespective of the lengths of individual intervals)
# 3. scoring system  (CHANGE ACCORDING TO REQUIREMENT/ DOMAIN KNOWLEDGE)
#    (operation at high power produces more heat -> low health score
#     operation at low power is inefficient -> low efficiency score)
#   __________________________________________________
#    FracPower  level  HealthScore  EfficiencyScore
#     < 5%        1        5              1     (should this be 0 when switched off?)
#  >=5 & <30%     2        4              2
#  >=30 & <60%    3        3              3
#  >=60 & <100%   4        2              4
#  >=100%         5        1              5     (should health scores vary between positive & negative?)
#   ___________________________________________________
#
#  Calculate hourly scores of performance - this is given by: sum for 1 hr((health score) * (efficiency score))
#  OR,
#  sum(health score for 1 hr) * sum(efficiency score for 1 hr) ?

######################    METHOD 2, to be added later     ####################################
## multiple scores at each level, depending on time duration of operation
##############################################################################################


setwd("C:/Users/Desktop/data/analysis/")

ratedpower <- as.numeric(132)

## generate dataframe
df <- data.frame(s$txtime, s$realtimepower)  
df$FracRatedPower <- df$s.realtimepower * 100/ratedpower


# new power levels
fac2 <- cut(df$FracRatedPower, c(0, 30, 80, 100, 500), labels=c('low','medium','high','veryHigh'))   # rename levels  ; #c(-10, 5, 30, 60, 100, 500)
df <- cbind(df,fac2)


## calculate time intervals b/w successive instances of operation at different power levels
c_time <- as.POSIXlt(df$s.txtime )
timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
# (OR), timedel2 <- as.numeric(diff(df$sensor15.txtime))
#timedel <- append(timedel, -999) #'NA') ## add 'NA' to end of column, as length(timedel) = (No. of rows in DF) - 1 
df <- df[-nrow(df),]   ## remove last row of data, corrs to which there is no time duration
df <- cbind(df,timedel)  ## add the time differences column to the dataframe


############# method 1 ##############
## assigning scores 
df$health <- 0
df$efficiency <- 0

#   ______________________________________________________________________
#    FracPower  level  HealthScore  EnergyEfficiencyScore    utilization
#  >100%          VH       10              90                    yellow
#  >80 & <=100%   H        100            100                    green
#  >30 & <=80%    M        90              40                    yellow
#  >0 & <=30%     L        60              10                    red
#   _____________________________________________________________________


df$health[df$fac2 == 'low'] <- 60
df$health[df$fac2 == 'medium'] <- 90
df$health[df$fac2 == 'high'] <- 100
df$health[df$fac2 == 'veryHigh'] <- 10

df$efficiency[df$fac2 == 'low'] <- 10
df$efficiency[df$fac2 == 'medium'] <- 40
df$efficiency[df$fac2 == 'high'] <- 100
df$efficiency[df$fac2 == 'veryHigh'] <- 90

df$color[df$fac2 == 'low'] <- 'orange'       #'red'
df$color[df$fac2 == 'medium'] <- 'yellow'
df$color[df$fac2 == 'high'] <- 'green'
df$color[df$fac2 == 'veryHigh'] <- 'red'    #'yellow'


#df = within(df, {#df$HS_per_reading <- ifelse(timedel == 'NA', 'NA', as.numeric(df$health) * as.numeric(df$timedel))
#df$ES_per_reading <- ifelse(timedel == 'NA', 'NA', (df$efficiency * as.numeric(df$timedel)))
#})
#df$HS_per_reading[df$timedel != 'NA'] <- df$health * as.numeric(df$timedel)
#df$HS_per_reading[df$timedel == 'NA'] <- 'NA'

df$HS_per_reading <- df$health * as.numeric(df$timedel)/60             # converting time duration from sec to min
df$ES_per_reading <- df$efficiency * as.numeric(df$timedel)/60         # converting time duration from sec to min
df$score_per_reading <- sqrt(df$HS_per_reading * df$ES_per_reading)    ## this favours medium scores in both scales


### set dates ###
set1 <- c(25:31)
set2 <- c(1:3)
set3 <- c(10:13)
set4 <- c(14:16)
alldates <- c(set1,set2,set3,set4)

library(ggplot2)
require(cowplot)

### plot the scores for each day , & calculate total scores per hour
for (dateofmonth in (alldates)){
  if(dateofmonth < 10 ){
    dateofmonth=paste("0", dateofmonth, sep = '')
  }
  subset1=df[strftime(df$s.txtime,'%d',tz = 'UTC')== dateofmonth,]   ## subsetting by date

  ### plotting daily scores ##
  power <- ggplot(data=subset1, aes(as.POSIXct(subset1$s.txtime), subset1$FracRatedPower)) + 
              geom_point(colour = as.factor(subset1$color)) + labs(x="", y="% of Rated Power") + 
              geom_line(size=0.01, colour = as.factor(subset1$color))  # + ylim(0,180) + geom_line(size=0.01, col="blue")
  
  
  
  ### calculate overall scores per hour  #########
  subset1$s.txtime <- as.POSIXct(subset1$s.txtime, format="%Y-%m-%d %H:%M:%S")
  subset1$HOUR <-format(subset1$s.txtime, format="%H")  # %H:%M
  
  ### tmp_df <- data.frame(levels(as.factor(subset1$HOUR)))
  ## select by hour, then by fac2 level in each hour
  ##hourly_HS <- tapply(subset1$HS_per_reading, subset1$HOUR, FUN=mean)  # sum)
  ##hourly_ES <- tapply(subset1$ES_per_reading, subset1$HOUR, FUN=mean)  # sum)  ## this averages over the number of instances & not the total time: wrong avg obtained if there are large intervals b/w successive data points
  ##hourly_totalScore <- tapply(subset1$score_per_reading, subset1$HOUR, FUN=mean)  # sum)
  ##tmp_df <- cbind(tmp_df,hourly_HS,hourly_ES,hourly_totalScore)
  
  subset1$energy <- subset1$FracRatedPower * as.numeric(subset1$timedel)
  tmp_df <- data.frame(levels(as.factor(subset1$HOUR)))
  hourly_HS <- tapply(subset1$HS_per_reading, subset1$HOUR, FUN=sum)
  hourly_ES <- tapply(subset1$ES_per_reading, subset1$HOUR, FUN=sum)
  hourly_power <- tapply(subset1$energy, subset1$HOUR, FUN=sum)
  # hourly_totalScore <- tapply(subset1$score_per_reading, subset1$HOUR, FUN=sum)
  hourly_dur <- tapply(subset1$timedel, subset1$HOUR, FUN=sum)
  tmp_df <- cbind(tmp_df, hourly_dur, hourly_HS,hourly_ES, hourly_power)# , hourly_totalScore)
  tmp_df$hourly_HS <- tmp_df$hourly_HS * 60/tmp_df$hourly_dur
  tmp_df$hourly_ES <- tmp_df$hourly_ES * 60/tmp_df$hourly_dur
  tmp_df$hourly_power <- tmp_df$hourly_power/tmp_df$hourly_dur
  # tmp_df$hourly_totalScore <- tmp_df$hourly_totalScore * 60/tmp_df$hourly_dur
  
  tmp_df$color[tmp_df$hourly_power >= 1 & tmp_df$hourly_power < 30] <- 'brown' #'red'
  tmp_df$color[tmp_df$hourly_power >= 30 & tmp_df$hourly_power < 80] <- 'gold' #'yellow'
  tmp_df$color[tmp_df$hourly_power >= 80 & tmp_df$hourly_power < 100] <- 'green'
  tmp_df$color[tmp_df$hourly_power >= 100] <- 'red' #'yellow'
  
  colnames(tmp_df)[1] <- "time"
  #totalScr <- ggplot(data=tmp_df, aes(as.POSIXct(tmp_df$time, format="%H"), tmp_df$hourly_totalScore)) + 
  #              geom_point(colour = as.factor(subset1$color)) + labs(x="", y="overall score (per hr)") + 
  #              geom_line(size=0.01, col="blue")  # + ylim(0,6)
  H_power <- ggplot(data=tmp_df, aes(as.POSIXct(tmp_df$time, format="%H"), tmp_df$hourly_power)) + 
                    geom_point(size=6, colour = as.factor(tmp_df$color)) + labs(x="", y="power (per hr)") + 
                    geom_line(size=1.5, colour = as.factor(tmp_df$color))  # + ylim(0,6)
  HHScr <- ggplot(data=tmp_df, aes(as.POSIXct(tmp_df$time, format="%H"), tmp_df$hourly_HS)) + 
                    geom_point(size=6, colour = as.factor(tmp_df$color)) + labs(x="", y="health score (per hr)") + 
                    geom_line(size=1.5, colour = as.factor(tmp_df$color))  # + ylim(0,6)
  HEScr <- ggplot(data=tmp_df, aes(as.POSIXct(tmp_df$time, format="%H"), tmp_df$hourly_ES)) + 
                   geom_point(size=6, colour = as.factor(tmp_df$color)) + labs(x="time (hr)", y="efficiency score (per hr)") + 
                   geom_line(size=1.5, colour = as.factor(tmp_df$color))  # + ylim(0,6)

  f=paste("mc_scores_date-", dateofmonth, ".png", sep = '')
  png(f, h=2000, w=1200)
  print(plot_grid(power, H_power, HHScr, HEScr, ncol=1))  
  # print(plot_grid(power, totalScr, healthScr, HHScr, efficiencyScr, HEScr, ncol = 2))
  dev.off()
}


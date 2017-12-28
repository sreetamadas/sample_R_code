## discord detection, using SAX, on datewise data
# method
# find_discords_hotsax(NumericVector ts, int w_size, int paa_size, int a_size, double n_threshold, int discords_num)
# NumericVector ts: time series data
# int w_size: sliding window, sub-sequence of the time series which is discretised at a time
# int paa_size: no of PAA points/segments into which it (the subsequence) is transformed(?)
# int a_size: no. of alphabets used to describe the levels
# double n_threshold: normalization threshold
# int discords_num: no. of discord to print


library(ggplot2)
library(zoom)
library(jmotif)
library(dplyr)


setwd("C:/User/Desktop/data/")
s10 <- read.csv("C:/User/Desktop/data/rawData.csv")

###########################################################################################
### set dates ###
#set1 <- c(25:31) #set2 <- c(1:3) #set3 <- c(10:13) #set4 <- c(14:16)
#set5 <- c(16:29)  #set6 <- c(1:15) #set7 <- c(16:25)
#alldates <- c(set5) #c(set5) # c(set1,set2,set3,set4)
### select data datewise ###
#for (dateofmonth in alldates){
#  if(dateofmonth < 10 ){
#    dateofmonth=paste("0", dateofmonth, sep = '')
#  }
#}
#  datelist <- split(s10, as.Date(as.POSIXct(s10$txtime))) #-21600))


  ### subset data for a particular day ###
  dateofmonth <- 12
  s1=s10[strftime(s10$txtime,'%d',tz = 'UTC')== dateofmonth,] 

  
  ### subset in an interval ###
  dateofmonth <- 16
  interval <- 21 #14 #7
  Date1 <- paste("2016-12-", dateofmonth, ' 06:00:00', sep = '')
  Date2 <- paste("2016-12-", (dateofmonth + interval - 1), ' 06:00:00', sep = '')
  # Date1 <- "2016-12-16 06:00:00"
  # Date2 <- "2017-01-07 06:00:00"  # 23 day interval
  # Date2 <- "2017-01-09 06:00:00"  # 25 day interval
  # Date2 <- "2017-01-11 06:00:00"  # 27 day interval
  Date1 <- as.integer(as.POSIXct(Date1)) 
  Date2 <- as.integer(as.POSIXct(Date2))
  s1 <- subset(s10, (as.integer(as.POSIXct(s10$txtime)) >= Date1 & as.integer(as.POSIXct(s10$txtime)) < Date2))
  
  
  #####  Find discords #######
  w_size <- 480   # sub-sequence of the time series which is discretised at a time
  paa_size <- 24  # no of PAA points/segments into which it (the subsequence) is transformed
  a_size <- 3     # no. of alphabets used to describe the levels
  n_thres <- 0.01 # normalization threshold
  d_num <- 5      # no. of discord to print
  
  discords = find_discords_hotsax(s1$X, w_size, paa_size, a_size, n_thres, d_num) 
  
  # (480, 24, 3, 0.01, 10) : reduce 1 shift's data 8hrs*60pts/hr=480 data pts to 8hrs*3pts/hr=24 data pts 
  # -> the above works on an interval of 3 weeks
  # reduce 60 min data to 3-4 data points? 60 3 3 0.01 5 
  # (30, 3, 3, 0.01, 5)  # (20, 4, 3, 0.01, 5) (20, 2, 4, 0.01, 5) (10, 2, 4, 0.01, 5) 
  # (100, 10, 4, 0.01, 5)  (100, 4, 4, 0.01, 5)


  ###########################################################################################
  ## alternately, discretise the  full data at 1 go
  ## find discords in entire data, with discretisation of 1 day at a time (for 1 shift at a time, code KEEPS CALCULATING)
  #w_size <- 1440   # sub-sequence of the time series which is discretised at a time
  #paa_size <- 72  # no of PAA points/segments into which it (the subsequence) is transformed
  #a_size <- 3     # no. of alphabets used to describe the levels
  #n_thres <- 0.01 # normalization threshold
  #d_num <- 5      # no. of discord to print
  
  #discords = find_discords_hotsax(s10$X, w_size, paa_size, a_size, n_thres, d_num) 
  ## reduce 1 day's data 24hrs*60pts/hr = 1440 pts to 24hrs*3pts/hr= 72 data pts
  ##############################################################################################
  
  ## PLOTS    
  plot(s1$X, type = "l")
  #library(dplyr)
  start <- arrange(discords,desc(nn_distance))[1,2]
  lines(x=c(start:(start+w_size)), y=s1$X[start:(start+w_size)], col="red") 
  ## color separately the discord, whose start position on X-axis is given by [xth row, 2nd column], & it continues upto y positions, 
  ## since that is the window length considered for calculation => discords[x,2] + y
  #lines(x=c(discords[1,2]:(discords[1,2]+480)), y=s1$x[discords[1,2]:(discords[1,2]+480)], col="red")  
  
  ## zoom out the plot
  zm()

  rm(s1,discords,Date2,start)
  

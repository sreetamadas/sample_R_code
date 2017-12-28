### SUBSET DATA BY DATE ###

### method 1: subset in an interval ###
dateofmonth <- 16
interval <- 21 #14 #7
Date1 <- paste("2016-12-", dateofmonth, ' 06:00:00', sep = '')
Date2 <- paste("2016-12-", (dateofmonth + interval - 1), ' 06:00:00', sep = '')
#Date1 <- "2016-12-16 06:00:00"
#Date2 <- "2017-01-07 06:00:00"  # 23 day interval
Date1 <- as.integer(as.POSIXct(Date1, tz = 'UTC')) 
Date2 <- as.integer(as.POSIXct(Date2, tz = 'UTC'))
s1 <- subset(s10, (as.integer(as.POSIXct(s10$txtime)) >= Date1 & as.integer(as.POSIXct(s10$txtime)) < Date2))
  

######################################################################################
### method 2: selecting data by date & shift ###
# add column for day of week, shift, & additional column marking distinct shifts

setwd("C:/Users/name/Desktop/data/")
df <- read.csv("C:/Users/name/Desktop/data/sample_data.csv") 

## assign day of week
df$date <- format(as.POSIXct(df$txtime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d") # %H:%M:%S
df$day <- as.POSIXlt(df$date)$wday
df$day2 <- weekdays(as.Date(df$txtime))

## assign shift
df$time <- as.integer(format(as.POSIXct(df$txtime, format="%Y-%m-%d %H:%M:%S"), format="%H")) # %H:%M:%S
df$shift <- 0
df$shift[df$time >= 6 & df$time < 14] <- '1'
df$shift[df$time >= 14 & df$time < 22] <- '2'
df$shift[df$time >= 22 | df$time < 6] <- '3'
df$time <- NULL

## assign different factors to each distinct slot
df$entry[1] <- 1
rownum <- nrow(df)
cnt <- 1
for (i in 2:rownum) {
  if(df$shift[i] != df$shift[i-1]) {
    cnt <- cnt + 1
  }
  df$entry[i] <- cnt
}

## subset data by date & shift
for (i in 1:cnt) {
  s15 <- subset(df, df$entry == i)
  DO OTHER CALCULATIONS ...
}


###########################################################################
### method 3: to select by date only, using the above logic

## assign day of week
df$day <- as.POSIXlt(df$date)$wday

## assign different factors to each distinct slot
df$entry[1] <- 1
rownum <- nrow(df)
cnt <- 1
for (i in 2:rownum) {
  if(df$day[i] != df$day[i-1]) {
    cnt <- cnt + 1
  }
  df$entry[i] <- cnt
}


#########################################################################
### method 4:  plot for discontinuous dates ######

set1 <- c(25:31)
set2 <- c(1:3)
set3 <- c(10:16)
alldates <- c(set1,set2,set3)
#print(alldates)

for (dateofmonth in alldates){
  if(dateofmonth < 10 ){
    dateofmonth=paste("0", dateofmonth, sep = '')
  }
  
  subset = df[strftime(df$txtime,'%d',tz = 'UTC')== dateofmonth,]
  date_n_time <- as.POSIXct(subset$txtime)
  
  f=paste("df_date_", dateofmonth, ".png", sep = '')
  png(f, h=706, w=1042, )
  
  plot(date_n_time, subset$Y, type='l', col="red", ylim=c(0,60), ylab="Y")
  dev.off()
}

########################################################################

### plot loads for 31 days, using loop ###
library(readxl)
library(ggplot2)
df <- read_excel("C:/Users/name/Desktop/data/sample_Data.xlsx")
for (dateofmonth in (1:31)){
  if(dateofmonth < 10 ){
  dateofmonth=paste("0", dateofmonth, sep = '')
  }

  subset = df[strftime(df$Date,'%d',tz = 'UTC')== dateofmonth,]
  date_n_time <- as.POSIXct(subset$Date)
  
  f=paste("df_date_", dateofmonth, ".png", sep = '')
  png(f, h=706, w=1042, )
  print(ggplot(data=subset, aes(date_n_time, Y)) + geom_point() + geom_line(size=0.1) + ylim(0,130))
  dev.off()

  #df <- melt(subset, id.vars = c("Y","Y1"), value.name = "value", variable.name="Date")
  #print(ggplot(data=df, aes(x=Date,y=value, group="c")) + geom_point() + geom_line(size=0.1))
}


######################################################################
######################################################################
###### time plot : save data date-wise   #######
df <- read.csv("C:/Users/name/Desktop/data/sample_Data.csv", header=TRUE)
png("boxplot_df.png", h=706, w=1042, )
boxplot(df$Y)
dev.off()


datetime <- as.POSIXct(df$txtime)
ggplot(data=df, aes(x=datetime, y=current)) + geom_point() + scale_x_datetime(date_minor_breaks = "1 day")

df_oct28 <- df[5471:6928,]		## selection of dates
date_n_time <- as.POSIXct(df_oct28$txtime)
ggplot(data=df_oct28, aes(x=date_n_time, y=current)) + geom_point() + scale_x_datetime(date_minor_breaks = "1 day")


############################################################

## another method, but not useful for downstream analysis  ###
load <- read_excel("C:/Users/name/Desktop/data/sample_Data.xlsx")
date_n_time <- as.POSIXct(load$Date)
loadbydate <- split(load, as.Date(date_n_time))


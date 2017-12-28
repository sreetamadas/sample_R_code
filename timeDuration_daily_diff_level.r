## plot by date & shift
## variation in Y, with level (as bar plots)
## also, show time duration of operation in each level
## level definitions: ( <=0), ( >0 to <= 30), (>30 - <=80), (>80 - <=100), (>100)

## set directory
setwd("C:/Users/Desktop/data/")

## read data, CSV format, usually 1 month at a time
s15 <- read.csv("C:/Users/Desktop/data/sample_data.csv")
### or, prompt user to enter data
#filename <- readline(prompt="enter filename for monthly data (full path, e.g. C:/Users/Desktop/data/sample_data.csv): ")
#s15 <- read.csv(filename)


## threshold rating
thres <- as.numeric(132)
# or, prompt user to enter threshold
# thres <- as.numeric(readline(prompt="Enter rating threshold:  "))


## create data frame by rated power
df <- data.frame(s15$time, s15$P, s15$xA, s15$xB, s15$xC) 
df$Frac <- df$P * 100/thres


#########################################################################################################
## assign day of week
df$date <- format(as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d") # %H:%M:%S
df$day <- as.POSIXlt(df$date)$wday
df$day2 <- weekdays(as.Date(df$time))

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

######################################################################################################
## subset data by date, then show distribution in 'x' 

## add libraries
#if (!require("dplyr")) {
#  install.packages("dplyr")
#  library(dplyr)
#}
attach(mtcars, warn.conflicts = FALSE)

for (i in 1:cnt) {
  s15 <- subset(df, df$entry == i)
  
  # calculate imbalance: method 2: 2% imbalance #####
  s15$X <-  (abs(s15$xA) + abs(s15$xB) + abs(s15$xC))/3 
  s15$diff_A <- abs(abs(s15$xA) - abs(s15$X)) 
  s15$diff_B <- abs(abs(s15$xB) - abs(s15$X)) 
  s15$diff_C <- abs(abs(s15$xC) - abs(s15$X))
  s15$diff_x <- ( s15$diff_A + s15$diff_B + s15$diff_C) * 100/(2 * s15$X)
  
  
  
  ## separate data by thres
  fac2 <- cut(s15$Frac, c(-100, 0, 30, 80, 100, 500),labels=c('off', 'low','medium','high','veryHigh')) # rename levels
  s15 <- cbind(s15,fac2)
  vH1 <- subset(s15, s15$fac2 == 'veryHigh')
  h1 <- subset(s15, s15$fac2 == 'high')
  m1 <- subset(s15, s15$fac2 == 'medium')
  l1 <- subset(s15, s15$fac2 == 'low') 
  o1 <- subset(s15, s15$fac2 == 'off')

  
  ### time duration calculation
  # time diff
  c_time <- as.POSIXlt(s15$txtime )
  timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
  s15 <- s15[-nrow(s15),]   ## remove last row of data, corrs to which there is no time duration
  s15 <- cbind(s15,timedel)  ## add the time differences column to the dataframe
  
  # calculate total time duration (per day) spent in a bin
  tmp_df <- data.frame(levels(as.factor(s15$fac2)))
  colnames(tmp_df)[1] <- 'level'
  hourly_dur <- tapply(s15$timedel, s15$fac2, FUN=sum)
  tmp_df <- cbind(tmp_df, hourly_dur)
  tmp_df$hourly_dur[is.na(tmp_df$hourly_dur)] <- 0 ## replace NA values with zero 
  #tmp_df$hourly_dur <- tmp_df$hourly_dur/60  # convert to min
  tmp_df$hourly_dur <- tmp_df$hourly_dur * 100/sum(tmp_df$hourly_dur) # convert to percent time
  
  
  ###### plots #######
  f=paste("date_", s15$date[1],"_",s15$day[1],s15$day2[1], "_shift", s15$shift, ".png", sep = '')
  png(f, h=800, w=1800 )  # png(f, h=500, w=2100 )
  par(mar=c(5,6,2,1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text
  par(mfrow=c(3,2))  # par(mfrow=c(1,3)) <- 1 row, 3 col
  
  ## time duration plot
  barplot(tmp_df$hourly_dur, ylim=c(0,100), xlab="level", ylab="% of total time", cex.axis=1.5, cex.names=1.5, 
          cex.lab=2.0, names.arg=c("off","low","medium","high","veryHigh"), space=0)
  abline(h=10)

  ## Frac vs time plot
  plot(as.POSIXct(s15$txtime, format="%Y-%m-%d %H:%M:%S"), s15$Frac, type="b", 
       lwd=2, col="blue", xlab="time", ylab="% Rated Power", ylim=c(0,150), cex.axis=1.5, cex.lab=2.0)
    
  if(tmp_df$hourly_dur[tmp_df$level == 'off'] < 100) {
    boxplot(l1$xA, l1$xB, l1$xC, m1$xA, m1$xB, m1$xC,
            h1$xA, h1$xB, h1$xC, vH1$xA, vH1$xB, vH1$xC, 
            col=c("red","gold","green"), 
            names=c("","low","","","medium","","","high","","","very High",""), 
            ylab='x', xlab='level', cex.axis=1.5, cex.lab=2.0,
            at=c(1,2,3, 5,6,7, 9,10,11, 13,14,15))
            #at=c(1,2,3, 5,6,7, 9,10,11, 13,14,15, 17,18,19))
    #legend("topleft", legend=c("A","B", "C"), fill=c("red","gold","green"), cex=1.5)  # "topleft" "bottomright"
    
        
    ## deviation in x ##
    boxplot(l1$diff_x, m1$diff_x, h1$diff_x, vH1$diff_x, col="light blue",
            names=c("low","medium","high","very High"),
            ylab='imbalance', xlab='level', cex.axis=1.5, cex.lab=2.0,
            at=c(1, 3, 5, 7))
    abline(h=2, col="red", lwd=3, lty=2)
    
  }
  dev.off()
}    


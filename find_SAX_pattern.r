### find SAX patterns, in shift data  #####
### calculate time spent in a level, in each shift
# add column for day of week, shift, & additional column marking distinct shifts
# some of the shifts lack partial data points; such shifts have been excluded from calculation

## the old 'jmotif' library has some problems with high paa num value ('paa_num' size is invalid): try to install new library

######################################################################################
### read data ###
setwd("C:/User/Desktop/data/")
dat <- read.csv("C:/Users/Desktop/data/sampleData.csv")
s15 <- dat[1:9083,] # less data available for 3rd shift on 22/11/16, so leave out that shift's data
#s15 <- dat[9153:33948,]


## create data frame
thres <- as.numeric(100) 
df <- data.frame(s15$time, s15$p)  
rm(s15)
colnames(df) <- c("txtime","p") 
df$Frac <- df$p * 100/thres

################################################################################################
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

####################################################
## subset data by date & shift, then check for patterns 

library(jmotif)
#library(stringr)
library(stringi)
library(plotrix)
attach(mtcars, warn.conflicts = FALSE)

for (i in 1:cnt) {
  s15 <- subset(df, df$entry == i)
  
  # generate SAX sequences
  x_paa <- paa(znorm(s15$p, 0.01), 96)  # generate 1 paa per 5min, approx. (12 per hr * 8 hrs per shift)
  #x_paa <- paa(s15$p, 96)  # use without normalization; paa = 96, 192
  #x_paa <- paa(znorm(s15$p, 0.01), 192) # 1 paa per 2.5min, 24 per hour * 8hrs = 192
  #x_paa <- paa(znorm(s15$p, 0.01), 220) # 1 paa per 1.25min, 48 per hour * 8hrs = 384
  
  x_sax <- series_to_string(x_paa, 4)  # use 4 (or 3) aplhabet  ### CHANGE HERE
  
 
  # convert to matrix of dimers
  z <- matrix(nrow=4, ncol=4)   ### CHANGE HERE
  #z <- matrix(nrow=3, ncol=3)   ### CHANGE HERE
  z[1,1] <- stri_count_regex(x_sax, '(?=aa)') # aa, str_count(x_sax, pattern = 'aa')
  z[1,2] <- stri_count_regex(x_sax, '(?=ab)') # at
  z[1,3] <- stri_count_regex(x_sax, '(?=ac)') # ca
  z[1,4] <- stri_count_regex(x_sax, '(?=ad)') # cc
  z[2,1] <- stri_count_regex(x_sax, '(?=ba)') # ag
  z[2,2] <- stri_count_regex(x_sax, '(?=bb)') # at
  z[2,3] <- stri_count_regex(x_sax, '(?=bc)') # cg
  z[2,4] <- stri_count_regex(x_sax, '(?=bd)') # ct
  z[3,1] <- stri_count_regex(x_sax, '(?=ca)') # ga
  z[3,2] <- stri_count_regex(x_sax, '(?=cb)') # gc
  z[3,3] <- stri_count_regex(x_sax, '(?=cc)') # ta
  z[3,4] <- stri_count_regex(x_sax, '(?=cd)') # tc
  z[4,1] <- stri_count_regex(x_sax, '(?=da)') # gg
  z[4,2] <- stri_count_regex(x_sax, '(?=db)') # gt
  z[4,3] <- stri_count_regex(x_sax, '(?=dc)') # tg
  z[4,4] <- stri_count_regex(x_sax, '(?=dd)') # tt
  
  
  ### time duration calculation
  # time diff
  c_time <- as.POSIXlt(s15$txtime )
  timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
  s15 <- s15[-nrow(s15),]   ## remove last row of data, corrs to which there is no time duration
  s15 <- cbind(s15,timedel)  ## add the time differences column to the dataframe
  
  # factoring into power value bins
  #fac <- cut(s15$p, breaks=seq(-10,260,by=10), labels=seq(0,260,by=10)) #labels=c(1:27))
  fac <- cut(s15$Frac, breaks=seq(-10,200,by=10), labels=seq(0,200,by=10)) #labels=c(1:27))
  s15 <- cbind(s15,fac)
  
  # calculate total time duration (per day) spent in a power bin
  tmp_df <- data.frame(levels(as.factor(s15$fac)))
  colnames(tmp_df)[1] <- 'level'
  hourly_dur <- tapply(s15$timedel, s15$fac, FUN=sum)
  tmp_df <- cbind(tmp_df, hourly_dur)
  tmp_df$hourly_dur <- tmp_df$hourly_dur/60  # convert to min
  tmp_df$hourly_dur[is.na(tmp_df$hourly_dur)] <- 0 ## replace NA values with zero 
  
  
  ####  plots  #####
  f=paste("s15_PAA_date_", s15$date[1],"_",s15$day[1],s15$day2[1], "_shift", s15$shift, ".png", sep = '')
  png(f, h=1000, w=1000 )  # h=500, w=1500
  
  MIN <- min(s15$p)
  MAX <- max(s15$p)
  if(MIN > 0) {
    MIN <- 0
  }
  par(mfrow=c(2,2))  # par(mfrow=c(1,3))
  
  plot(as.POSIXct(s15$txtime), s15$p, type='l', xlab="time",ylab="var", ylim=c(MIN,MAX))
  
  plot(x_paa, type='l', ylab="PAA")
  
  color2D.matplot(z,c(1,0),c(0,1),c(0,0), show.legend=FALSE,xlab="Column",ylab="Row",do.hex=FALSE,axes=TRUE,show.values=FALSE) 
  # black-green: c(0,0),c(0,1),c(0,0); white-black: c(1,0),c(1,0),c(1,0); white-green: c(1,0),c(1,1),c(1,0) [low - high scales]
  
  #plot(c(0:26), tmp_df$hourly_dur, type='l', xlab="p (x10)", ylab="time (min)")  # for original scale
  #plot(c(0:20), tmp_df$hourly_dur, type='l', xlab="Frac (x10)", ylab="time (min)") # for fractional scale
  
  barplot(tmp_df$hourly_dur, xlab="Frac", ylab="time (min)", space=0) # breaks=seq(0,210,by=10),
  dev.off()
}


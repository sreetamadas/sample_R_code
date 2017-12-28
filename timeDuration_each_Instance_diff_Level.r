# calculating time intervals for each instance in a different state/level


thres <- as.numeric(132)

## read in all data
dataset1 <- read.csv("dat1.csv")
dataset2 <- read.csv("dat2.csv")   
dataset1 <- rbind(dataset1, dataset2)


## separate the data by breaks in dates
s15 <- dataset1[1:12610,]  # round1
#s15 <- dataset1[12611:73497,] # round2  (12611:73497 -> for nov 11 to dec25); (12611:20922-> nov 11-16


## generate dataframe
df <- data.frame(s15$txtime, s15$Y)  
df$Frac <- df$s15.Y * 100/thres

fac2 <- cut(df$Frac, c(-10, 0, 30, 80, 100, 500),labels=c('off','low','medium','high','veryHigh')) # rename levels
df <- cbind(df,fac2)


## assign changes in state to a new dataframe
rownum <- nrow(df)
tmp <- as.data.frame(df[1,])

j <- 2
for (i in 2:rownum) {
  if(as.character(df[i,4]) != as.character(df[i-1,4])) {
    tmp[j,] = df[i,]
    j= j+1
  }
}
tmp[j,] = df[rownum,]


## calculate time duration
c_time <- as.POSIXlt(tmp$s15.txtime, format="%m/%d/%Y %H:%M")
timedur <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
tmp <- tmp[-nrow(tmp),]   ## remove last row of data, corrs to which there is no time duration
tmp <- cbind(tmp,timedur)  ## add the time differences column to the dataframe
#tmp$timedur <- tmp$timedur/60 # converting secs to mins (in larger dataset, time stamp is already in mins)


## check time duration distribution for different states (for 1st csv file, before break in dates)
vH <- subset(tmp, tmp$fac2 == 'veryHigh')
h <- subset(tmp, tmp$fac2 == 'high')
m <- subset(tmp, tmp$fac2 == 'medium')
l <- subset(tmp, tmp$fac2 == 'low')
o <- subset(tmp, tmp$fac2 == 'off')

old_tmp <- tmp # save the 1st set time durations

## append to the existing data frame (for new csv files)
temp_vH <- subset(tmp, tmp$fac2 == 'veryHigh')
vH <- rbind(vH, temp_vH)
temp_h <- subset(tmp, tmp$fac2 == 'high')
h <- rbind(h, temp_h)
temp_m <- subset(tmp, tmp$fac2 == 'medium')
m <- rbind(m, temp_m)
temp_l <- subset(tmp, tmp$fac2 == 'low')
l <- rbind(l, temp_l)
temp_o <- subset(tmp, tmp$fac2 == 'off')
o <- rbind(o, temp_o)



library(ggplot2)
require(cowplot)

vh_plot <- qplot(vH$timedur, geom="histogram", binwidth=2, xlab="time duration (min) veryhigh power")
h_plot <- qplot(h$timedur, geom="histogram", binwidth=2, xlab="time duration (min) high power")
m_plot <- qplot(m$timedur, geom="histogram", binwidth=2, xlab="time duration (min) medium power")
l_plot <- qplot(l$timedur, geom="histogram", binwidth=2, xlab="time duration (min) low power")
#o_plot <- qplot(o$timedur, geom="histogram", binwidth=2, xlab="time duration (min) OFF")


#f=paste("s15_hist", L3, L2, "pc_frac_alldays.png", sep = '-')
png("s15_timeDuration_distr_oct25-dec25_0-30-80-100.png", h=800, w=1000)

print(plot_grid(vh_plot, h_plot, m_plot, l_plot, ncol = 2))  #  o_plot,
dev.off()

# very long ON duration on 2016-11-02 22:42:53 for 'veryHigh' level because there are missing data after that
# correct the data & re-run

## total time duration ##
time_vH <- sum(vH$timedur) #/60 # convert min to hrs
time_h <- sum(h$timedur) #/60
time_m <- sum(m$timedur) #/60
time_l <- sum(l$timedur) #/60
time_o <- sum(o$timedur) #/60

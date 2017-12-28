# test of significance (Mann Whitney / Wilcoxon Rank Sum test)
# use to check if distribution of X in different phases are similar or different
# separate data by shift, then by level
# finally, consider only when the system persists in that level for >10% of the time

# checking on full data

setwd("C:/Users/Desktop/data/") 
s15 <- read.csv("C:/Users/Desktop/data/sample_Data.csv")


## separate data by level
thres <- as.numeric(100)
s15$Frac <- s15$P * 100/thres
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


## significance test
testSig <- data.frame(x=vH1$xA, y='A')
testSig1 <- data.frame(x=vH1$xB, y='B')
testSig <- rbind(testSig, testSig1)
rm(testSig1)
wilcox.test(testSig$x ~ testSig$y)

 
# wilcox.test(vH1$xA, vH1$xB, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F, exact=T, correct=T)
# wilcox.test(vH1$xB, vH1$xC, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)
# kruskal.test(list(m1$xA, m1$xB, m1$xC)) => gives error in df
# kruskal.test(testSig$x ~ testSig$y) => use this

## significance tests on X
wilcox.test(m1$xA, m1$xB, mu=0, conf.int=T)

testA <- data.frame(x=m1$xA, y='A')
testB <- data.frame(x=m1$xB, y='B')
testC <- data.frame(x=m1$xC, y='C')
testA <- rbind(testA,testB,testC)
rm(testB,testC)
kruskal.test(testA$x ~ testA$y)

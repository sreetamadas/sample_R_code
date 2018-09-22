STEPS:
1. select window (window of 4-groove, or 4 separate windows ?)
2. use clustering - detect centroid - unshift the level changes  (does not work well if too many levels - this may not be a problem with shorter windows)
3. smoothen the data


possible solution methods:
0. count the number of peaks & the interval between them by labelling the states as a/b/c (or 1/2/3) & finding length of state 1. *** 
   - the interval can be affected by the selection window 


1. using the individual waves in each waveform to detect the type - wave 1 of 1-groove, 2-groove, 3-groove, 4-groove    -> total 10 events + possible variants
                                                                    wave 2              2-groove, 3-groove, 4-groove
                                                                    wave 3                        3-groove, 4-groove
                                                                         4                                  4-groove

   steps: event detection -> event classification -> sequence identification -> product classification

   event detection did not work on raw data, unshifted data, unshifted + rolled mean data -> subsequent points are changing gradually - need for an event detection filter ?

   event classification requires taking each 'detected wave', binning it & representing by a feature vector where the value of each feature is some number 
   calculated on the bin (median, avg, sd, etc.)


2. calculate cross-correlation between standard waves (for each type) & a window of the data

   steps: a. take a window of data
          b. represent by some bins instead of raw values (since there are many variants of each type e.g, 2-groove & 3-groove)
               - find centroid representation (not good)
               - bin data into histograms & use the histogram bin number as label (too noisy as the bins are affected by the raw values)
               - take SAXS & convert labels a/b/c to numbers 1/2/3  (make appropriate choice of paa length)
            (try SAX based matrices on different groove types with same number of data samples - not much visual difference)

          c. compare with standard patterns
               - clustering
               - cross correlation
              (tried comparing the 4-groove patterns with each other; instead compare with patterns from the remaining data) ***


3. use of fft or periodogram on the window of data

4. SVM based approach: window is 1 vs not 1
                                 2 vs not 2
                                 3 vs not 3
                                 4 vs not 4
                             background vs not background

5. use of LSTM/ RNN approaches

# methods tried:
# 1. checked SAX dimeric content heatmap plots for 2/3/4 groove samples - 
#    same window length vs window for the signal only - not much diference
#    try different color schemes
#    also, check for all standard groove patterns - differences in heatmap for different variants of the same groove
# 2. edge detection methods: not working on raw data (affected by fluctuations & step changes)
#                            not working on unshifted data  (affected by fluctuations)
#                            not working on unshifted + rolled average data (actual changes are graduate)
#                            piecewise median & then difference,  *** - SEE HOW TO USE THIS
#                            difference of ratio, ratio of difference, etc.
# 3. fft/ stft- based approaches
# 4. cross-correlation based approaches  - how to compute cross-correlation with lags ?  ***
#      correlation between selected standard patterns & randomly selected data (4700 points from an image) does not 
#      give proper results (direct cor or on median values)
# 5. LSTM ?
# 6. SVM ?
######################################################################################################################

# signal detection (no of grooves: 1/2/3/4) from junker power data

# methods tried:
# 1. checked SAX dimeric content heatmap plots for 2/3/4 groove samples - 
#    same window length vs window for the signal only - not much diference
#    try different color schemes
#    also, check for all standard groove patterns - differences in heatmap for different variants of the same groove
# 2. edge detection methods: not working on raw data (affected by fluctuations & step changes)
#                            not working on unshifted data  (affected by fluctuations)
#                            not working on unshifted + rolled average data (actual changes are graduate)
#                            piecewise median & then difference,  *** - SEE HOW TO USE THIS
#                            difference of ratio, ratio of difference, etc.
# 3. fft/ stft- based approaches
# 4. cross-correlation based approaches  - how to compute cross-correlation with lags ?  ***
#      correlation between selected standard patterns & randomly selected data (4700 points from an image) does not 
#      give proper results (direct cor or on median values)
# 5. LSTM ?
# 6. SVM ?


path <- "C:/Users/Desktop/data/"
setwd(path)

#### read power data ####
# list.files(path = ".")   # list files in current directory

dat <- read.table(paste(path,'data/rA.dat',sep=''), skip=1) # read.csv(paste(path,))
#dat <- read.table(paste(path,'data/new_dat/rA.txt',sep=''), skip=1) # read.csv(paste(path,))

df <- as.data.frame(dat)
colnames(df)[1] <- 'rA'
rm(dat)

## add time
# data points are sampled at 10 ms -> 500ms from 50 data points
# => 1 s from 100 data points 
# => 1 hr = 360000 data points
# => 8hrs shift = 8*60*60*100 = 2880000 points
#time <- seq(as.POSIXct("2017-12-31 00:14:35", format="%Y-%m-%d %H:%M:%S"), by=0.5, length.out = nrow(df))  #  starttime: 21/12/2017_9:22:59
#df <- cbind(df,time)


################################################################################################################
#### smooth the signal with window size=750 & check differences ####
library(zoo)
rolling_window <- 50  # 750, 300, 100, 150
df$roll_mean50 <- rollapply(data = df$rA,  # original series
                         width = rolling_window,  # width of the rolling window  = 12 * 24
                         FUN = mean, #na.rm = T,  # Any arbitrary function
                         fill = NA,
                         align='right')

## plot
for(i in 1:(round(nrow(df)/100000) + 1) ) {
  t <- data.frame(df[(1 + (i-1)*100000):(i*100000),])
  colnames(t)[1] <- 'rA'
  png(paste(path,'plots/round3/figure',i,'.png', sep=''), h=300, w=1800 )  # h=800
  #par(mfrow=c(3,1))  # no. of rows, no. of col : this creates 6 figs
  par(mar=c(2,5,2,1))
  plot(t$rA, type='l', ylim=c(0, 14000))  # max(t$rA)
  #plot(t$roll_mean300, type='l', ylim=c(0, 14000))  # max(t$roll_mean300)
  #plot(t$roll_mean150, type='l', ylim=c(0, 14000))  # max(t$roll_mean100)
  dev.off()
  rm(t)
  print(i)
}

###########################################################################
## sample plots
# 2 groove
t40 <- data.frame(df[(1 + (40-1)*100000):(40*100000),])
colnames(t40)[1] <- 'rA'
#s40 <- data.frame(t[1500:6500,])  # 2 windows
# s40 <- data.frame(t40[1000:10500,])
s40_2g <- data.frame(t40[1500:6200,])  # 1500:4000
#plot(s40_2g$rA, type='l', ylab='fig 40, ind 1500-4000') # window for 1: 2500
paa_size <- 470  # no of PAA points/segments into which it (the subsequence) is transformed
a_size <- 3     # no. of alphabets used to describe the levels
n_thres <- 0.01 # normalization threshold
library(jmotif)
x_sax <- series_to_string(paa(znorm(s40_2g$rA, n_thres), paa_size), a_size)  #paa_size=250
library(stringi)
library(plotrix)
z <- matrix(nrow=3, ncol=3)   ### CHANGE HERE
z[1,1] <- stri_count_regex(x_sax, '(?=aa)') # aa, str_count(x_sax, pattern = 'aa')
z[1,2] <- stri_count_regex(x_sax, '(?=ab)') # at
z[1,3] <- stri_count_regex(x_sax, '(?=ac)') # ca
z[2,1] <- stri_count_regex(x_sax, '(?=ba)') # ag
z[2,2] <- stri_count_regex(x_sax, '(?=bb)') # at
z[2,3] <- stri_count_regex(x_sax, '(?=bc)') # cg
z[3,1] <- stri_count_regex(x_sax, '(?=ca)') # ga
z[3,2] <- stri_count_regex(x_sax, '(?=cb)') # gc
z[3,3] <- stri_count_regex(x_sax, '(?=cc)') # ta
color2D.matplot(z,c(1,0),c(1,0),c(1,0), show.legend=FALSE,xlab="Column",ylab="Row 2-groove",do.hex=FALSE,axes=TRUE,show.values=FALSE) 
# black-green: c(0,0),c(0,1),c(0,0); white-black: c(1,0),c(1,0),c(1,0); white-green: c(1,0),c(1,1),c(1,0) [low - high scales]


# 4 groove
t41 <- data.frame(df[(1 + (41-1)*100000):(41*100000),])
colnames(t41)[1] <- 'rA'
s41 <- data.frame(t41[42800:47500,])
#s41 <- data.frame(t41[37500:52500,])
colnames(s41)[1] <- 'rA'
#plot(s41$rA, type='l', ylab='fig 41, ind 42800-47500')  # length = 4700
x_sax <- series_to_string(paa(znorm(s41$rA, n_thres), paa_size), a_size)
z <- matrix(nrow=3, ncol=3)   ### CHANGE HERE
z[1,1] <- stri_count_regex(x_sax, '(?=aa)') # aa, str_count(x_sax, pattern = 'aa')
z[1,2] <- stri_count_regex(x_sax, '(?=ab)') # at
z[1,3] <- stri_count_regex(x_sax, '(?=ac)') # ca
z[2,1] <- stri_count_regex(x_sax, '(?=ba)') # ag
z[2,2] <- stri_count_regex(x_sax, '(?=bb)') # at
z[2,3] <- stri_count_regex(x_sax, '(?=bc)') # cg
z[3,1] <- stri_count_regex(x_sax, '(?=ca)') # ga
z[3,2] <- stri_count_regex(x_sax, '(?=cb)') # gc
z[3,3] <- stri_count_regex(x_sax, '(?=cc)') # ta
color2D.matplot(z,c(1,0),c(1,0),c(1,0), show.legend=FALSE,xlab="Column",ylab="Row 4-groove",do.hex=FALSE,axes=TRUE,show.values=FALSE) 
# black-green: c(0,0),c(0,1),c(0,0); white-black: c(1,0),c(1,0),c(1,0); white-green: c(1,0),c(1,1),c(1,0) [low - high scales]


# 3 groove
t28 <- data.frame(df[(1 + (28-1)*100000):(28*100000),])
colnames(t28)[1] <- 'rA'
#s28 <- data.frame(t28[40500:50500,])
#s28 <- data.frame(t28[43500:46000,])
#colnames(s28)[1] <- 'rA'
#plot(s28$rA, type='l', ylab='fig 28, ind 43500-46000')
s28 <- data.frame(t28[43400:48100,])  # 43400:46100
colnames(s28)[1] <- 'rA'
#plot(s28$rA, type='l', ylab='fig 28, ind 43400-46100')  # window length = 2700
x_sax <- series_to_string(paa(znorm(s28$rA, n_thres), paa_size), a_size)  # nrow(s28)/10  paa_size = 250
z <- matrix(nrow=3, ncol=3)   ### CHANGE HERE
z[1,1] <- stri_count_regex(x_sax, '(?=aa)') # aa, str_count(x_sax, pattern = 'aa')
z[1,2] <- stri_count_regex(x_sax, '(?=ab)') # at
z[1,3] <- stri_count_regex(x_sax, '(?=ac)') # ca
z[2,1] <- stri_count_regex(x_sax, '(?=ba)') # ag
z[2,2] <- stri_count_regex(x_sax, '(?=bb)') # at
z[2,3] <- stri_count_regex(x_sax, '(?=bc)') # cg
z[3,1] <- stri_count_regex(x_sax, '(?=ca)') # ga
z[3,2] <- stri_count_regex(x_sax, '(?=cb)') # gc
z[3,3] <- stri_count_regex(x_sax, '(?=cc)') # ta
color2D.matplot(z,c(1,0),c(1,0),c(1,0), show.legend=FALSE,xlab="Column",ylab="Row 3-groove",do.hex=FALSE,axes=TRUE,show.values=FALSE) 
# black-green: c(0,0),c(0,1),c(0,0); white-black: c(1,0),c(1,0),c(1,0); white-green: c(1,0),c(1,1),c(1,0) [low - high scales]



######################################################################################################################
### functions

get_knee <- function (knee_df) {
  
  x_values <- knee_df$X
  y_values <- knee_df$Y
  # y_values <- (knee_df$Y - mean(knee_df$Y))/max(knee_df$Y)  # quick & dirty scaling
  max_x_x <- max(x_values)
  max_x_y <- y_values[which.max(x_values)]
  max_y_y <- max(y_values)
  max_y_x <- x_values[which.max(y_values)]
  max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))
  
  # Creating straight line between the max values
  fitlm <- lm(max_df$y ~ max_df$x)
  
  # Distance from point to line
  distances <- c()
  for(i in 1:length(x_values)) {
    distances <- c(distances, abs(coef(fitlm)[2]*x_values[i] - y_values[i] + coef(fitlm)[1]) / sqrt(coef(fitlm)[2]^2 + 1^2))
  }
  
  # Max distance point
  x_max_dist <- x_values[which.max(distances)]
  y_max_dist <- y_values[which.max(distances)]
  
  return(x_max_dist)   #return(c(x_max_dist, y_max_dist))  # take 1 cluster extra ?
}


#########################################################################################################################

#### count numbers of each type #### 
### STEPS
# 1. take window: final window ~ 3 * 4-groove window: 14500 - 15000 samples
#               or, window = length of a single 4-groove event ? (~4700 samples)
s28 <- data.frame(t28[33500:47000,])
colnames(s28)[1] <- 'rA'
plot(s28$rA, type='l', ylab='fig 28, ind 33500-47000')  # length = 13500



# 2. compute centroid & shift level
## can this be speeded up using a histogram-based approach ? ****
## using clustering
library(clusterSim)
cdf <- data.frame(t28$rA)  # s28$rA
colnames(cdf)[1] <- 'rA'

wss <- (nrow(cdf)-1)*sum(apply(cdf,2,var))
for (i in 2:6) wss[i] <- sum(kmeans(cdf, centers=i)$withinss)

# determine optimal no of clusters from knee plot (AUTOMATE)
plot(1:6, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
knee_df <- data.frame(c(1:6), wss)
colnames(knee_df) <- c('X','Y')
max_clust <- get_knee(knee_df)

# find centroid & unshift the level changes
fit <- kmeans(cdf, max_clust) #+1)  # 3; 2 
cdf <- cbind(cdf, fit$cluster)#, fit$centers)
colnames(cdf)[2] <- 'clust_id'
# assign centroid
cdf$clust_centroid <- fit$centers[cdf$clust_id]
#cdf$clust_centroid <- ifelse(cdf$clust_id == 1, fit$centers[1], fit$centers[2]) 
#for (num_clust in 1:3) {
  #cdf$clust_centroid[cdf$clust_id == num_clust] = fit$centers[num_clust]
#}
# calculate without shift
t28$unshift <- t28$rA - cdf$clust_centroid
#s28$unshift <- s28$rA - cdf$clust_centroid

## using mode (finds only the highest freq. mode; not all modes)
#estimate_mode <- function(x) {
#  if (length(x) > 2) {
#    d <- density(x)
#    d$x[which.max(d$y)]
#  }
#  else 0
#}
#X_mode <- estimate_mode(s28$rA)



# 3. smooth data with a small rolling window
library(zoo)
rolling_window <- 50  # 750, 300, 100, 150
s28$roll_mean50 <- rollapply(data = s28$unshift,  # original series
                             width = rolling_window,  # width of the rolling window  = 12 * 24
                             FUN = mean, #na.rm = T,  # Any arbitrary function
                             fill = NA,
                             align='right')
# remove the first 50 rows with NA
t45 <- na.omit(t45)

par(mfrow=c(4,1))  # no. of rows, no. of col : this creates 6 figs
par(mar=c(2,5,2,1))
plot(s28$rA, type='l', ylab='fig 28, ind 33500-47000')
plot(s28$roll_mean100_n, type='l', ylab='fig 28, ind 33500-47000')
plot(s28$unshift, type='l', ylab='fig 28, ind 33500-47000')
plot(s28$roll_mean100_n2, type='l', ylab='fig 28, ind 33500-47000')




# 4. find type
## A.  use fft frequency ? # https://stackoverflow.com/questions/14010605/fast-fourier-transform-in-r
#library(TSA)
#p <- periodogram(s28$unshift)
#dat = data.frame(freq=p$freq, spec=p$spec)
#order = dat[order(-dat$spec),]
#head(order)

## compute stft
#library(e1071)
#t <- data.frame(df[(1 + (40-1)*100000):(40*100000),])  # 40th image: goove 2 to 4
#stft(X, win=min(80,floor(length(X)/10)), inc=min(24, floor(length(X)/30)), coef=64, wtype="hanning.window")
#FT <- stft(t$rA, win=5000, inc=2000, coef=64, wtype="hanning.window")

## compute stft & plot
#install.packages("seewave")
#library(seewave)
#spectro(s$rA, 100, 2000)  # chek parameter value assignment

## check sushovan's method of smoothing & finding maxima/ minima
# use fft, stft




## B.  use SAX pattern matching ? # sequence-based methods ?
library(jmotif)
# generate SAX sequences
#w_size <- 13501 # sub-sequence of the time series which is discretised at a time
paa_size <- 600  # no of PAA points/segments into which it (the subsequence) is transformed
a_size <- 3     # no. of alphabets used to describe the levels
n_thres <- 0.01 # normalization threshold
#x_paa <- paa(znorm(s15$p, 0.01), 96) # generate 1 paa per 5min, approx. (12 per hr * 8 hrs per shift)
x_sax <- series_to_string(paa(znorm(s28$unshift, n_thres), paa_size), a_size) # use 4 (or 3) aplhabet



# C.  cross correlation with each segment vs standard pattern for 1/2/3/4-groove
# get standard pattern- instead of raw values, convert to sax & replace a/b/c with 1/2/3 -
# then compute correlation or distance



# D.  LSTM ?
# https://stackoverflow.com/questions/11752727/pattern-recognition-in-time-series  




# E.  event detection from changes in raw data & MA data
## check anirban's method of smoothing & locating differences to find changes
# check if the first wave in all waveforms can be used for distinguishing (similar to anirban's prob.)
# detection of events required - 
# edge detection algo required ?

t <- data.frame(df[1:4700,])
colnames(t)[1] <- 'rA'
# m1. difference on raw data
t$diff <- c(0, diff(t$rA))  # plot(t$rA, type='l') # plot(t$diff, type='l')

# difference on levelled data
t$unshift <- t$rA - cdf$clust_centroid
t$diff_uns <- c(0, diff(t$unshift))  # plot(t$unshift, type='l') # plot(t$diff_uns, type='l')

# difference on leveled + moving median data
# t$roll_ma50
rolling_window = 50
t$roll_median50 <- rollapply(data = t$unshift,  # original series
                         width = rolling_window,  # width of the rolling window 
                         FUN = median, #mean, #na.rm = T,  # Any arbitrary function
                         fill = NA,
                         align='right')
t$diff_ma <- c(0, diff(t$roll_median50))
t$diff_ma[is.na(t$diff_ma)] <- 0 # plot(t$diff_ma, type='l') # plot(t$roll_ma50, type='l')


# difference on piecewise median data ******
#tmp <-c()  #data.frame(rA = numeric())
#(rA= numeric, stringsAsFactors=FALSE)
#for(i in 1:94) {
#  tt <- data.frame(t[(1 + (i-1)*50):(i*50),])
#  colnames(tt)[1] <- 'rA'
#  tmp <- append(tmp, median(tt$unshift))   ## median(tt$unshift) , median(tt$rA)
#}
#tmpdiff <- c(0, diff(tmp))

library(clusterSim)

# replace NA
library(zoo)
dfA <- na.locf(dfA, fromLast = FALSE) # for all entries, copy from previous value

for(i in 1:(round(nrow(dfA)/100000) + 1) ) {
  temp <- data.frame(dfA[(1 + (i-1)*100000):(i*100000),])
  colnames(temp)[1] <- 'rA'
  
  # 2. compute centroid & shift level
  unused_block <- function() {
  cdf <- data.frame(temp$rA)  # s28$rA
  colnames(cdf)[1] <- 'rA'
  
  wss <- (nrow(cdf)-1)*sum(apply(cdf,2,var))
  for (k in 2:6) wss[k] <- sum(kmeans(cdf, centers=k)$withinss)
  knee_df <- data.frame(c(1:6), wss)
  colnames(knee_df) <- c('X','Y')
  max_clust <- get_knee(knee_df)
  
  fit <- kmeans(cdf, max_clust) #+1)  # 3; 2 
  cdf <- cbind(cdf, fit$cluster)#, fit$centers)
  colnames(cdf)[2] <- 'clust_id'
  # assign centroid
  cdf$clust_centroid <- fit$centers[cdf$clust_id]
  temp$unshift <- temp$rA - cdf$clust_centroid
  }
  
  
  # 3. compute piecewise median
  tmp <-c()
  for(j in 1:2000) {  # 100000/50 = 2000
    tt <- data.frame(temp[(1 + (j-1)*50):(j*50),])
    colnames(tt)[1] <- 'rA'
    tmp <- append(tmp, median(tt$rA))   ## median(tt$unshift) , median(tt$rA)
  }
  # 4. compute diff
  tmpdiff <- c(0, diff(tmp))
  
  png(paste(path,'plots/round7/figure',i,'.png', sep=''), h=900, w=1800 )  # h=800
  par(mfrow=c(4,1))  # no. of rows, no. of col : this creates 6 figs
  par(mar=c(2,5,2,1))
  plot(temp$rA, type='l', ylab='rA', cex.axis=2, cex.lab=2)  # max(t$rA)
  #plot(temp$unshift, type='l', ylab='rA unshifted', cex.axis=2, cex.lab=2)  # max(t$rA)
  plot(tmp, type='l', ylab='PAA median', cex.axis=2, cex.lab=2)
  plot(tmpdiff, type='l', ylab='diff PAA median', cex.axis=2, cex.lab=2)
  #plot(t$roll_mean300, type='l', ylim=c(0, 14000))  # max(t$roll_mean300)
  #plot(t$roll_mean150, type='l', ylim=c(0, 14000))  # max(t$roll_mean100)
  dev.off()
  rm(temp, tmp, tmpdiff, cdf, fit, knee_df, max_clust, wss, tt)
  print(i)
}




## DO WE NEED A SEPARATE EDGE DETECTION ALGORITHM ?
## edge detection using differences & ratios
dat <- read.table(paste(path,'data/rA.dat',sep=''), skip=1)
dfA <- as.data.frame(dat)
colnames(dfA)[1] <- 'rA'
rm(dat)

dat <- read.table(paste(path,'data/reA.dat',sep=''), skip=1)
dfreA <- as.data.frame(dat)
colnames(dfreA)[1] <- 'reA'
rm(dat)

dfA <- cbind(dfA, dfreA)

dfA$diffA <- c(0, diff(dfA$rA))
dfA$diffreA <- c(0, diff(dfA$reA))
# ratio of differences
dfA$ratio <- dfA$diffA/dfA$diffreA
dfA$ratio[dfA$ratio==Inf] <- 0
dfA$ratio[is.na(dfA$ratio)] <- 0

# ratio of raw data
dfA$raw_ratio <- dfA$rA/dfA$reA
dfA$raw_ratio[dfA$raw_ratio==Inf] <- 0
dfA$raw_ratio[is.na(dfA$raw_ratio)] <- 0

# difference of raw data
dfA$rA_reA <- dfA$rA - dfA$reA

# difference of ratios
dfA$diff_raw_ratio <- c(0, diff(dfA$raw_ratio)) 

# ratio of consecutive values - does not work unless the values change abruptly
#dfA$ratio_con = dfA$rA/lag(dfA$rA, 1)
#datalen=length(dfA$rA)
#dfA$ratio_con[2:datalen]=dfA$rA[1:datalen-1]/dfA$rA[2:datalen]
#dfA$ratio_con[is.na(dfA$ratio_con)] <- dfA$rA
#dfA$ratio_con[dfA$ratio_con==Inf] <- dfA$rA


for(i in 1:(round(nrow(dfA)/100000) + 1) ) {
  temp <- data.frame(dfA[(1 + (i-1)*100000):(i*100000),])
  #colnames(t)[1] <- 'rA'
  png(paste(path,'plots/round6/figure',i,'.png', sep=''), h=900, w=1800 )  # h=800
  par(mfrow=c(6,1))  # no. of rows, no. of col : this creates 6 figs
  par(mar=c(2,5,2,1))
  plot(temp$rA, type='l', ylim=c(0, 14000), ylab='rA', cex.axis=2, cex.lab=2)  # max(t$rA)
  plot(temp$reA, type='l', ylab='reA', cex.axis=2, cex.lab=2)
  plot(temp$rA_reA, type='l', ylab='rA - reA', cex.axis=2, cex.lab=2)
  plot(temp$raw_ratio, type='l', ylab='rA/reA', cex.axis=2, cex.lab=2)
  plot(temp$ratio, type='l', ylab='ratio of diff', cex.axis=2, cex.lab=2)
  plot(temp$diff_raw_ratio, type='l', ylab='diff of ratio', cex.axis=2, cex.lab=2)
  #plot(t$roll_mean300, type='l', ylim=c(0, 14000))  # max(t$roll_mean300)
  #plot(t$roll_mean150, type='l', ylim=c(0, 14000))  # max(t$roll_mean100)
  dev.off()
  rm(temp)
  print(i)
}






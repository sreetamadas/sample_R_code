####  TIME SERIES MODELING  #####
## code needs to be updated

setwd("C:/Users/Desktop/data/")
### enter data ###
df <- read.csv("C:/Users/Desktop/data/sample_Data.csv")

## visualize the data
# plot(df$Y,type='l')


## time series analysis
# http://stackoverflow.com/questions/21123039/error-when-trying-to-use-stl-and-decompose-functions-in-r
# https://www.otexts.org/fpp/6/1
# http://robjhyndman.com/uwafiles/fpp-notes.pdf 
library(fpp)
## check with different deltat / frequency
# x <- ts(df$Y, deltat = 1/60)  # working value: 1/60, 1/(60*24)  : (this seems better)
x <- ts(df$Y, frequency = 10080) # 7day * 24hrs * 60min
fit <- stl(x, "periodic")
plot(fit)


## checking that there is no predictive power in residuals
fc <- rwf(x)
res <- residuals(fc)
plot(res)
hist(res)
#acf(res)  ## gives error


# check AR or MA process from ACF & PACF plots (with 'x' or raw timestamp data?)
acf(x, lag.max = 50)  # can change the value of lag.max
pacf(x, lag.max = 50)
#acf(df$Y, lag.max = 50)
#acf(df$Y, lag.max = 200)
#acf(df$Y, lag.max = 2000)
#acf(df$Y, lag.max = 10100)
#acf(x)
#pacf(df$Y, lag.max = 50)
#pacf(x, lag.max = 50)


# show time plot, ACF plot and lagged scatterplot on the same graph.
tsdisplay(x, plot.type = "scatter")


## check data correlation for different lag values
lag.plot(x, lags=30, do.lines = FALSE)



## using periodogram to find dominant frequency
library(TSA)
p <- periodogram(df$Y)
dat = data.frame(freq=p$freq, spec=p$spec)
order = dat[order(-dat$spec),]
top2 = head(order, 2)

# spectral analysis in R
# discrete fourier transform in R, dft in R
# fft in R
# tsa in R
# interpretation of frequency values in a periodogram in R
# http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html


## data filtering:
# google: how to apply high pass filter in R
# http://stackoverflow.com/questions/7105962/how-do-i-run-a-high-pass-or-low-pass-filter-on-data-points-in-r


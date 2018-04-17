####  TIME SERIES MODELING  #####
# how to take care of missing data ?
# how/ when to smooth data? use of filters?
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


## accessing stl output
fit
fit$time.series[,"trend"]  # fit$time.series[,"remainder]


# augmented Dickey-Fuller (ADF) test is a formal statistical test for stationarity
adf.test(x, alternative = "stationary")


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


## creating differenced data
library(astsa)
xlag1=lag(x,-1)
y=cbind(x,xlag1)
#plot(y[,1], y[,2])


# calculating differences
library(astsa)
diff1=diff(x, 1)
plot(diff1)


### sarima(df$X, 1,0,0,0,1,1,7)
# sarima(diff1, 1,0,0,0,1,1,12)


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


## creating differenced data
library(astsa)
xlag1=lag(x,-1)
y=cbind(x,xlag1)
#plot(y[,1], y[,2])


# calculating differences
library(astsa)
diff1=diff(x, 1)
plot(diff1)

#####################################################################
## arima methods for forecasting

### sarima(Prod$Q, 1,0,0,0,1,1,7)
# sarima(diff1, 1,0,0,0,1,1,12)

#########################################################################
## exponential smoothing methods for forecasting
# https://www.otexts.org/fpp/7/4

## fitting simple exponential smoothing
fit_ses <- ses(x, h = 3)  # predict next 3 points
fit_ses$model
accuracy(fit_ses)
plot(fit_ses, plot.conf=FALSE, fcol="white", type="o")
lines(fitted(fit_ses), col="blue", type="o")
lines(fit_ses$mean, col="blue", type="o")


# holt's damped trend
fit_hwd <- holt(x,damped=TRUE)
plot(fit_hwd, plot.conf=FALSE, fcol="white", type="o")
accuracy(fit_hwd)


# holt winter's (trend + seasonality)
fit_hw <- hw(x,seasonal="additive")
fit_hw$model
plot(fit_hw, plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(fit_hw), col="red", lty=2)
lines(fit_hw$mean, type="o", col="red")
accuracy(fit_hw)

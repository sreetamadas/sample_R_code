## time series modeling

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
x <- ts(df$Y, frequency = 10080) # 7day * 24hrs * 60min
fit <- stl(x, "periodic")
plot(fit)


# show time plot, ACF plot and lagged scatterplot on the same graph.
tsdisplay(x, plot.type = "scatter")

## check data correlation for different lag values
lag.plot(x, lags=30, do.lines = FALSE)


acf(df$Y, lag.max = 50)
acf(df$Y, lag.max = 200)
acf(df$Y, lag.max = 2000)
acf(df$Y, lag.max = 10100)
#acf(x)

pacf(df$Y, lag.max = 50)
pacf(x, lag.max = 50)

## checking that there is no predictive power in residuals
fc <- rwf(x)
res <- residuals(fc)
plot(res)
hist(res)
acf(res)  ## gives error



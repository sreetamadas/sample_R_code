##################################################################
### spectral analysis ###
## to be updated further

install.packages('TSA')
library(TSA)

setwd("C:/User/Desktop/data/")
s10 <- read.csv("C:/User/Desktop/data/rawData.csv")
### select time series length appropriately

#plot(as.POSIXct(s10$txtime), s10$X, type='l', xlab="time",ylab="real power s5", cex.axis=1.5, cex.lab=2, lwd=0.5) #, ylim=c(MIN,MAX))


periodogram(sensor10$X)
periodogram(sensor10$X, xlim=c(0,0.0005))

# spectral analysis in R
# discrete fourier transform in R, dft in R
# fft in R
# tsa in R
# interpretation of frequency values in a periodogram in R
# http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html


## data filtering:
# google: how to apply high pass filter in R
# http://stackoverflow.com/questions/7105962/how-do-i-run-a-high-pass-or-low-pass-filter-on-data-points-in-r

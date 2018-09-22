########################################################################################################
## develop model to predict Y, given the categorical variables x1, x2 & continuous variable x  ###
## use Support Vector regression
########################################################################################################

### read data 
prdcn <- read.csv("sample_data.csv")

## subset data by first categorical variable
w_model <- subset(prdcn, prdcn$x1 == X1) 


## assign colour by shift
w_model$colour[w_model$Shift == '1'] <- 'blue'  # shift 1: 00:00 - 08:00
w_model$colour[w_model$Shift == '2'] <- 'green' # shift 2: 08:00 - 16:00
w_model$colour[w_model$Shift == '3'] <- 'red'   # shift 3: 16:00 - 00:00


## list number of levels of categorical var x2
x2List <-  as.character(sort(unique(w_model$x2))) ## get unique IDs in x2 column


## SVR library
library(e1071)

for (mc in x2List) {
  w_mc <- subset(w_model, w_model$x2 == X2)

  ### fit SVR model
  fit <- svm(w_mc$Y ~ w_mc$X, data=w_mc, type="nu-regression", kernel="radial")
  w_mc$y_predicted <- predict(fit, data=w_mc)
  #summary(fit)
  
  
  ## plot by time for given x1 & x2
  png(paste("plot_",X1,"_",mc, ".png", sep = ''), h=600, w=1200)
  par(mar=c(6,6,2,1))
  par(mfrow=c(1,2))
  # plot data for mc
  plot(w_mc$X, w_mc$Y, xlab=paste('x (', X1, ')'), ylab='Y', col=w_mc$colour, lwd=2, pch=16, cex=0.8, cex.axis=2.5, cex.lab=2.5)
  legend("topright",legend=c("shift 1","shift 2", "shift 3"), fill=c("blue","green","red"), cex=1)
  
  
  # show regression fit
  #par(new=T)
  #plot(w_model$TotalProductPcs, w_model$y_predicted, col='orange', xlab='', ylab='')
  points(w_mc$X, w_mc$y_predicted, col='orange', xlab='', ylab='')
  
  
  ## show regression parameters
  nu <- format(summary(fit)$nu)
  cost <- format(summary(fit)$cost)
  gamma <- format(summary(fit)$gamma)
  
  ## printing parameters
  mtext(paste('nu:', nu,', cost:', cost,', gamma:', gamma), side=3, line=1, cex=1)
  
  
  ## plot residuals
  plot(resid(fit) ~ fitted(fit), cex.axis=2, cex.lab=2, xlab='fitted value', ylab='residual')
  abline(h=0, col="red")
  
  dev.off()
}


## fit SVR model on full data
library(e1071)
fit <- svm(w_model$Y ~ w_model$X, data=w_model, type="nu-regression", kernel="radial")
w_model$y_predicted <- predict(fit, data=w_model)
#summary(fit)


## plot for all m/c(s)
png(paste("plot_", x1, ".png", sep = ''), h=600, w=1200)
par(mar=c(6,6,2,1))
par(mfrow=c(1,2))
plot(w_model$X, w_model$Y, xlab=paste('X (', x1,')'), ylab='Y', col=w_model$colour, lwd=2, pch=16, cex=0.8, cex.axis=2.5, cex.lab=2.5)
legend("topright",legend=c("shift 1","shift 2", "shift 3"), fill=c("blue","green","red"), cex=1)


# show regression fit
#par(new=T)
#plot(w_model$X, w_model$Y, col='orange', xlab='', ylab='')
points(w_model$X, w_model$y_predicted, col='orange', xlab='', ylab='')

## show regression parameters
nu <- format(summary(fit)$nu)
cost <- format(summary(fit)$cost)
gamma <- format(summary(fit)$gamma)

## printing parameters
mtext(paste('nu:', nu,', cost:', cost,', gamma:', gamma), side=3, line=1, cex=1)

## plot residuals
plot(resid(fit) ~ fitted(fit), cex.axis=2, cex.lab=2, xlab='fitted value', ylab='residual')
abline(h=0, col="red")

dev.off()

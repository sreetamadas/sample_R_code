########################################################################################################
## develop model to predict Y, given the categorical variables x1, x2 & continuous variable x  ###
## use linear regression: Y = A + Bx

## alternately, 
## 1. try Y = A + Bx, where Y = log(y)
## for this, transform the data before passing to 'lm' by; 
##  w_mc$Y_new <- log(w_mc$Y)
##  fit <- lm(w_mc$Y_new ~ w_mc$X, data=w_mc)

## 2. try Y = A + BX, where Y = log(y), X = log(x)
## for this, transform: w_mc$Y_new <- log(w_mc$Y)  ;  w_mc$X_new <- log(w_mc$X)

## 3. try Y = A + BX, where Y & X have undergone Box-Cox transformation
###  use the following lines of code in appropriate place  ###
# Box-Cox transform
# library(fpp)  
# x_lambda <- BoxCox.lambda(w_mc$X)  # https://www.otexts.org/fpp/2/4
# y_lambda <- BoxCox.lambda(w_mc$Y)

# fit regression model
# w_mc$Y_new <- BoxCox(w_mc$Y, y_lambda)
# w_mc$X_new <- BoxCox(w_mc$X, x_lambda)
# fit <- lm(w_mc$Y_new ~ w_mc$X_new, data=w_mc)
########################################################################################################

# predicted r2
pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}
PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}
############################################################################################################

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


########  model building ###########
for (mc in x2List) {
  w_mc <- subset(w_model, w_model$x2 == X2)

  ### fit LR model
  fit <- lm(w_mc$Y ~ w_mc$X, data=w_mc)
  w_mc$y_predicted <- predict(fit, data=w_mc)
  #summary(fit)
  # mse
  #mse <- mean(sum((w_mc$Y - w_mc$y_predicted)*(w_mc$Y - w_mc$y_predicted)))
  
  
  ## plot for given x1 & x2
  png(paste("plot_",X1,"_",mc, ".png", sep = ''), h=600, w=1200)
  par(mar=c(6,6,2,1))
  par(mfrow=c(1,2))
  plot(w_mc$X, w_mc$Y, xlab=paste('x (', X1, ')'), ylab='Y', col=w_mc$colour, lwd=2, pch=16, cex=0.8, cex.axis=2.5, cex.lab=2.5)
  legend("topright",legend=c("shift 1","shift 2", "shift 3"), fill=c("blue","green","red"), cex=1)
  
  
  # show prediction intervals
  pred.int =  predict(fit, interval="prediction", level=0.95)
  lines(w_mc$X, pred.int[,2], col="blue", lty=2, lwd=1) 
  lines(w_mc$X, pred.int[,3], col="blue", lty=2, lwd=1) 
  
  
  # show regression fit
  #par(new=T)
  #plot(w_model$TotalProductPcs, w_model$y_predicted, col='orange', xlab='', ylab='')
  abline(coef(fit)[1:2])
  points(w_mc$X, w_mc$y_predicted, col='orange', xlab='', ylab='')
  
  
  ## show regression equation
  ## rounded coefficients for better output
  cf <- round(coef(fit), 2)
  ## R2
  r2 <- format(summary(fit)$r.squared, digits = 3)
  ## sign check to avoid having plus followed by minus for negative coefficients
  eq <- paste0("kWh/pc = ", cf[1],
               ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " total_Pcs ")
               
  ## printing of the equation
  mtext(eq, side=3, line=0)
  ## printing r2
  mtext(bquote(r^2 == .(r2)),adj=1)
  ## printing lambda (for Box-Cox transformation)
  # mtext(paste('x_lambda: ',x_lambda,' y_lambda: ',y_lambda), side=3, line=1)
  
  
  ## plot residuals
  plot(resid(fit) ~ fitted(fit), cex.axis=2, cex.lab=2, xlab='fitted value', ylab='residual')
  abline(h=0, col="red")
  
  
  ## residuals normality
  library(car)
  qqPlot(fit)
  #layout(matrix(c(1,2,3,4),2,2))
  #plot(fit)

  
  dev.off()
}


####################################################################
## fit regression model for full data
fit <- lm(w_model$Y ~ w_model$X, data=w_model)
w_model$y_predicted <- fitted(fit)
#summary(fit)


## rest of the steps as before

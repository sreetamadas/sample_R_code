############  check data normality before & after Box-Cox transformation  ############

attach(mtcars, warn.conflicts = FALSE)

par(mfrow=c(2,2))
hist(log(df$Y))
hist(df$Y)
hist(log(df$X))
hist(df$X)

par(mfrow=c(2,2))
plot( log(df$X), log(df$Y) )
plot( df$X, log(df$Y) )
plot( log(df$X), df$Y )
plot( df$X, df$Y )


## checking normality of data
par(mfrow=c(2,2))
qqnorm( df$X)  # normal probability plot, to check if data fit a normal distribution
qqline( df$X)
qqnorm( log(df$X))
qqline( log(df$X))
qqnorm( df$Y)
qqline( df$Y)
qqnorm( log(df$Y))
qqline( log(df$Y))


## checking distribution of residuals for heteroscedasticity
fit <- lm(df$Y ~ df$X, data=df) ## regression fit
plot( df$X, df$Y )
abline(coef(fit)[1:2])
plot(resid(fit) ~ fitted(fit), cex.axis=2, cex.lab=2) # or, plot(fitted(fit), resid(fit))=> x-axis: predicted values, y: residuals
abline(h=0)
qqnorm(resid(fit))
qqline(resid(fit))


## detecting multiple parameters for heteroscedasticity
## https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/
df$log_Y <- log(df$Y)
df$log_X <- log(df$X)
lmMod <- lm(w_model$log_Y ~ w_model$log_X, data=df)
par(mfrow=c(2,2))
plot(lmMod)


####  Box-Cox transformation  #####
# This test only works for positive data (http://www.statisticshowto.com/box-cox-transformation/). 
# However, Box and Cox did propose a second formula that can be used for negative y-values
library(fpp)  
x_lambda <- BoxCox.lambda(df$X)  # https://www.otexts.org/fpp/2/4
y_lambda <- BoxCox.lambda(df$Y)
x <- BoxCox(df$X, x_lambda)
y <- BoxCox(df$Y, y_lambda)

qqnorm(x)
qqline(x)
qqnorm(y)
qqline(y)

fitBC <- lm(y ~ x, data=df)
plot(x,y)
abline(coef(fitBC)[1:2])
qqnorm(resid(fitBC))     # not normally distributed
qqline(resid(fitBC))
plot(resid(fitBC) ~ fitted(fitBC))  # heteroscdastic
abline(h=0)


#######################################################
> summary(fit)

Call:
  lm(formula = df$Y ~ df$X, data = w_model)

Residuals:
  Min      1Q  Median      3Q     Max 
-8.581  -0.609   0.154   0.525 107.314 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)              9.667349   0.568119   17.02   <2e-16 ***
  df$X                  -0.081737   0.005834  -14.01   <2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 4.325 on 1142 degrees of freedom
Multiple R-squared:  0.1467,	Adjusted R-squared:  0.1459 
F-statistic: 196.3 on 1 and 1142 DF,  p-value: < 2.2e-16

######################################################
> summary(fitBC)

Call:
  lm(formula = y ~ x, data = df)

Residuals:
  Min       1Q   Median       3Q      Max 
-2.30769 -0.08927  0.07202  0.18329  1.44424 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  8.664e-01  3.485e-02   24.86   <2e-16 ***
  x           -1.101e-04  7.001e-06  -15.72   <2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.3608 on 1142 degrees of freedom
Multiple R-squared:  0.1779,	Adjusted R-squared:  0.1772 
F-statistic: 247.1 on 1 and 1142 DF,  p-value: < 2.2e-16

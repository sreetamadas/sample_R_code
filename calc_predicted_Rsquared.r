# calculate predicted r2 for linear regression
# https://www.r-bloggers.com/can-we-do-better-than-r-squared/


#fit <- lm(w_mc$Y ~ w_mc$X, data=w_mc)
#w_mc$y_predicted <- predict(fit, data=w_mc)
#summary(fit)

# r2
#r2 <- format(summary(fit)$r.squared, digits = 3)

# adjusted r2
#r2 <- format(summary(fit)$adj.r.squared, digits = 3)


##########################################################
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

##### usage ######
# pred.r.squared <- pred_r_squared(my.lm)



   # SVM
   library(e1071)
   fit <- svm(fmlaT, cv_train, type="nu-regression", kernel="radial")
   prediction <- predict(fit, newdata=cv_test)
   
   
   ## logistic
   fit_logistic2 <- glm(fmla, data=cv_train, family=binomial(link="logit"))  # change input formula
   ###summary(fit_logistic2)
   ### anova(fit_logistic, test="Chisq")
   glm_prob2 <- predict(fit_logistic2, newdata = cv_test, type='response') # plogis(predict(fit_logistic2, newdata = cv_test))
   prediction <- ifelse(glm_prob2 > cutOff, 1, 0)


   ## neural net
   library(nnet)
   fit_ANN <- nnet(fmlaT, cv_train, size=10) #, softmax=TRUE)
   prediction_ANN_2cl <- predict(fit_ANN, newdata=cv_test[-11], type="class")

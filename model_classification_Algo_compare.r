# https://machinelearningmastery.com/evaluate-machine-learning-algorithms-with-r/

library(caret)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7

metric <- "Accuracy"
dataset <- dTraining2


# linear discriminant analysis
set.seed(seed)
fit.lda <- train(fmlaT, data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(fmlaT, data=dataset, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(fmlaT, data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(fmlaT, data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(fmlaT, data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(fmlaT, data=dataset, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(fmlaT, data=dataset, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed)
fit.c50 <- train(fmlaT, data=dataset, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(fmlaT, data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(fmlaT, data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(fmlaT, data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)



results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, #c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)


# Dot-plot comparison
dotplot(results)

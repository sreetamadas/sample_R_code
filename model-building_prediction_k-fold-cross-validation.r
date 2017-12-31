### model building and cross-validation for regression
## algos to try: RandomForest, Support Vector Regression, Multiple Linear Regression

# input 1: The data frame that you want to split into training, validation, and test
# this should combine production data and scada data (and others, if required)

# https://stackoverflow.com/questions/36068963/r-how-to-split-a-data-frame-into-training-validation-and-test-sets
# Input 2. Set the fractions of the dataframe you want to split into :
# training, validation, and test.
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20
# or (train + test set)
# fractionTraining   <- 0.9
# fractionTest       <- 0.1

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(d))
sampleSizeValidation <- floor(fractionValidation * nrow(d))
sampleSizeTest       <- floor(fractionTest       * nrow(d))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(d)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(d)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)
# or (train + test) 
# indicesTraining    <- sort(sample(seq_len(nrow(d)), size=sampleSizeTraining))
# indicesNotTraining <- setdiff(seq_len(nrow(d)), indicesTraining)
# indicesTest        <- sort(sample(indicesNotTraining, size=sampleSizeTest))

# Finally, output the three dataframes for training, validation and test.
dTraining   <- d[indicesTraining, ]
dValidation <- d[indicesValidation, ]
dTest       <- d[indicesTest, ]


#######################################################################################################
######  build using training set, optimize using validation set, finally, test on training set  #####

fmlaT <- as.formula(paste("Y ~ ", paste(c(colnames(d)[5],colnames(d)[16]), collapse='+')))

#######   partitioning the data for K-fold cross-validation   ###########

# https://www.r-bloggers.com/k-fold-cross-validation-random-forest-vs-gbm/
k = 10
n = floor(nrow(dTraining)/k) # size of each fold
err.vect <- rep(NA,k)     # store the error in this vector

library(randomForest)
require(verification)  ## for roc.area

for (i in 1:k) {
  s1 <- (i-1)*n + 1  # start of subset
  s2 <- i*n          # end of subset
  Subset <- s1:s2    # range of subset
  
  # create training & validation sets
  cv_train <- dTraining[-Subset,]
  cv_test <- dTraining[Subset,]
  
  # build RandomForest model (or, any other method)
  #fit <- randomForest(x=cv_train[,-1], y=as.factor(cv_train[,1]))
  fit <- randomForest(fmla, cv_train, ntree=501, importance=T) 
  #fit <- svm(fmla, cv_train, type="nu-regression", kernel="radial")
  # fit <- lm(Y ~ x1 + x2, cv_train)
  
  # predict using model
  # prediction <- (fit, newdata=cv_test[,-1], type="prob")[,2]   ## use for classification problem
  prediction <- predict(fit, newdata=cv_test)  ## use for random forest regression
  
  # calculate model accuracy for i-th fold
  # for Classification
  #err.vect[i] <- roc.area(cv_test[,1], prediction)$A  ## use for classification; cv_test[,1] = cv_test$class
  #print(paste("AUC for fold ",i,":", err.vect[i]))    ## use for classification
  # r-squared, for regression : R2 = 1 - sum((y-predicted)^2)/sum((y-mean(y))^2)
  #library(miscTools)
  #r2 <- rSquared(dValidation$Y, dValidation$Y - predict(fit, newdata=dValidation))
  r2 <- 1 - sum((cv_test$Y - predict(fit, newdata=cv_test))^2)/sum((cv_test$Y - mean(cv_test$Y))^2)
  r2_train <- 1 - sum((cv_train$Y - predict(fit, data=cv_train))^2)/sum((cv_train$Y - mean(cv_train$Y))^2)

  # MSE
  err.vect[i] <- mean((cv_test$Y - predict(fit, newdata=cv_test))^2)  # error on test set
  mse <- mean((cv_train$Y - predict(fit, data=cv_train))^2)    # error on training set
  
  print(paste("fold ",i,", MSE (test):", err.vect[i],", r2 (test):", r2,", MSE <train>: ",mse,", r2 <train>; ",r2_train)) 
  varImpPlot(fit, sort = T, main=paste("Variable Importance, ntree=",fit$ntree,", fold=",i, sep =' '), n.var=15)
  #plot(cv_test$Y, predict(fit, newdata=cv_test), xlab='Y (test)', ylab=paste('Y predicted, fold ',i))
  #abline(a=0, b=1, col='red')
}

#print(paste("avg AUC: ",mean(err.vect)))     ## use for classification
print(paste("avg MSE: ", mean(err.vect)))     ## use for regression


########################################  END  #####################################################

dTraining$class[dTraining$Y < 2.2] <- 'L'
dTraining$class[dTraining$Y >= 2.2 & dTraining$Y <= 2.6] <- 'M'
dTraining$class[dTraining$Y > 2.6] <- 'H'

dTest$class[dTest$Y < 2.2] <- 'L'
dTest$class[dTest$Y >= 2.2 & dTest$Y <= 2.6] <- 'M'
dTest$class[dTest$Y > 2.6] <- 'H'

####################################################################################################
####################################################################################################

## get 1 tree from the random forest
# getTree(randomForest(fmlaT1, data=d, ntree=1000), k=1, labelVar = TRUE)


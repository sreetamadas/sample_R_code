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
#fmla <- as.formula(paste("kWh_per_piece ~ ", paste(c(colnames(d)[4],colnames(d)[14:(ncol(d)-1)]), collapse='+')))
# no. of pieces, water temp
fmlaT <- as.formula(paste("kWh_per_piece ~ ", paste(c(colnames(d)[5],colnames(d)[16]), collapse='+')))


######  build using training set, optimize using validation set, finally, test on training set  #####

fit <- randomForest(fmlaT, data=dTraining, ntree=501)  # ntree = 250 or 500
prediction <- predict(fit, newdata=dValidation)
library(miscTools)
r2 <- rSquared(dValidation$kWh_per_piece, dValidation$kWh_per_piece - predict(fit, newdata=dValidation))
# or, R2 = 1 - sum((y-predicted)^2)/sum((y-mean(y))^2)
# 1 - sum((dValidation$kWh_per_piece-predict(kWh_forest, newdata=dValidation))^2)/sum((dValidation$kWh_per_piece-mean(dValidation$kWh_per_piece))^2)
mse <- mean((dValidation$kWh_per_piece - predict(fit, newdata=dValidation))^2)
# fit
# summary(fit)
# getTree(randomForest(fmlaT1, data=d, ntree=1000), k=1, labelVar = TRUE)

library(ggplot2)
ggplot(data=data.frame(actual=dValidation$kWh_per_piece, pred=prediction), aes(x=actual, y=pred)) +
  geom_point() + geom_abline(color="red")

#plot(kWh_forest)
#varImpPlot(kWh_forest, sort = T, main=paste("Variable Importance, ntree=",kWh_forest$ntree, sep =' '), n.var=25)


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
  # fit <- lm(kWh_per_piece ~ TotalProductPcs + WATER.TEMP., cv_train)
  
  # predict using model
  # prediction <- (fit, newdata=cv_test[,-1], type="prob")[,2]   ## use for classification problem
  prediction <- predict(fit, newdata=cv_test)  ## use for random forest regression
  
  # calculate model accuracy for i-th fold
  #err.vect[i] <- roc.area(cv_test[,1], prediction)$A  ## use for classification; cv_test[,1] = cv_test$class
  #print(paste("AUC for fold ",i,":", err.vect[i]))    ## use for classification
  # r-squared
  r2 <- 1 - sum((cv_test$kWh_per_piece - predict(fit, newdata=cv_test))^2)/sum((cv_test$kWh_per_piece - mean(cv_test$kWh_per_piece))^2)
  r2_train <- 1 - sum((cv_train$kWh_per_piece - predict(fit, data=cv_train))^2)/sum((cv_train$kWh_per_piece - mean(cv_train$kWh_per_piece))^2)

  # MSE
  err.vect[i] <- mean((cv_test$kWh_per_piece - predict(fit, newdata=cv_test))^2)
  mse <- mean((cv_train$kWh_per_piece - predict(fit, data=cv_train))^2)
  print(paste("fold ",i,", MSE (test):", err.vect[i],", r2 (test):", r2,", MSE <train>: ",mse,", r2 <train>; ",r2_train)) 
  varImpPlot(fit, sort = T, main=paste("Variable Importance, ntree=",fit$ntree,", fold=",i, sep =' '), n.var=15)
  #plot(cv_test$kWh_per_piece, predict(fit, newdata=cv_test), xlab='kWh/pc (test)', ylab=paste('KPI predicted, fold ',i))
  #abline(a=0, b=1, col='red')
}

#print(paste("avg AUC: ",mean(err.vect)))     ## use for classification
print(paste("avg MSE: ", mean(err.vect)))     ## use for regression


########################################  END #####################################################

dTraining$class[dTraining$kWh_per_piece < 2.2] <- 'L'
dTraining$class[dTraining$kWh_per_piece >= 2.2 & dTraining$kWh_per_piece <= 2.6] <- 'M'
dTraining$class[dTraining$kWh_per_piece > 2.6] <- 'H'

dTest$class[dTest$kWh_per_piece < 2.2] <- 'L'
dTest$class[dTest$kWh_per_piece >= 2.2 & dTest$kWh_per_piece <= 2.6] <- 'M'
dTest$class[dTest$kWh_per_piece > 2.6] <- 'H'

####################################################################################################
####################################################################################################

kWh_forest <- randomForest(fmla, dTraining, ntree=251, importance=T)
plot(kWh_forest)
varImpPlot(kWh_forest, sort = T, main=paste("Variable Importance, ntree=",kWh_forest$ntree, sep =' '), n.var=25)
mean((dValidation$kWh_per_piece - predict(kWh_forest, newdata=dValidation))^2)
kwh_forest251 <- kWh_forest

######################################################################################################
#####################################################################################################
#####################################################################################################
### different combinations of features for random forest  ####
# no. of pieces, air 9 flow ACT
# fmlaT4 <- as.formula(paste("kWh_per_piece ~ ", paste(c(colnames(d)[5],colnames(d)[47]), collapse='+')))

# no. of pieces, water temp, air 9 flow ACT
#fmlaT1 <- as.formula(paste("kWh_per_piece ~ ", paste(c(colnames(d)[5],colnames(d)[16], colnames(d)[47]), collapse='+')))

# no. of pieces, water temp, air 9 flow ACT, air 1 flow ACT
#fmlaT2 <- as.formula(paste("kWh_per_piece ~ ", paste(c(colnames(d)[5],colnames(d)[16],colnames(d)[47],colnames(d)[31]), collapse='+')))

# no. of pieces, water temp, air 9 flow ACT, air 1 flow ACT, air 9 cooling time
#fmlaT3 <- as.formula(paste("kWh_per_piece ~ ", paste(c(colnames(d)[5],colnames(d)[16],colnames(d)[47],colnames(d)[31],colnames(d)[48]), collapse='+')))

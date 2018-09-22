### classification : breast cancer -> in this case, sensitivity/recall is the most important (minimise FN)

## 1. how to handle class imbalance  ??? http://r-statistics.co/Logistic-Regression-With-R.html (see 'Check Class bias')
## 2. how to optimise model hyperparameters ?? - try grid ? - code
## 3. variable selection is not showing large improvements here - try with the other breast cancer data
#     feature selection vs feature reduction (is PCA doing feature reduction?)  
## 4. try ensemble techniques
## 5. try multi-class classification (all class as sametime vs layer-wise: merge classes & then classify sub-class)


##  parameters: X(i)
## RF- hyperparameters: ntree, mtry
## logistic- hyperparameters: probability cutoff
## SVM - gamma, cost, epsilon (what is this?)


## good kernels to follow:
# https://www.kaggle.com/uciml/breast-cancer-wisconsin-data/kernels
# https://github.com/sanjaykrishnagouda/UCI-breast-cancer-data   (RF, SVM with diff kernels, logistic, NN)


# https://analyticsindiamag.com/7-types-classification-algorithms/


# GOOGLE: metrics for classification machine learning
# https://medium.com/greyatom/performance-metrics-for-classification-problems-in-machine-learning-part-i-b085d432082b
# https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
# https://www.machinelearningplus.com/machine-learning/evaluation-metrics-classification-models-r/


# GOOGLE: how to calculate ROC from random forest fit in R
# https://stats.stackexchange.com/questions/34363/randomforest-chooses-regression-instead-of-classification
# http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html

# GOOGLE: different confusion matrix from random forest fit vs confusion matrix from caret package
# https://github.com/topepo/caret/issues/622
# https://stackoverflow.com/questions/46201550/different-results-from-confusionmatrix-of-caret-package-and-roc-of-epi-package-i
# https://stackoverflow.com/questions/19871043/r-package-caret-confusionmatrix-with-missing-categories/30538793


# http://r-statistics.co/Logistic-Regression-With-R.html
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/logistic-regression-analysis-r/tutorial/
# gogle: model link function in logistic regression
# use of AIC to compare modes, VIF in multicollinearity, ROC-AUC
# http://scg.sdsu.edu/logit_r/    (discussion of VIF & other tests for model validation)
## GOOGLE: logistic regression is linear or non-linear



path <- "C:/Users/DAR9KOR/Desktop/data/sample_datasets/breast_cancer_data/"
setwd(path)


########   read data  ##########
df <- read.table(paste(path,'breast-cancer-wisconsin.txt',sep=''), sep=',') ## header=TRUE,



##### data pre-processing ######
# apply data labels
colnames(df) <- c('pat_id','Clump_Thickness','Uniformity_of_Cell_Size','Uniformity_of_Cell_Shape',
                   'Marginal_Adhesion', 'Single_Epithelial_Cell_Size', 'Bare_Nuclei',
                   'Bland_Chromatin', 'Normal_Nucleoli', 'Mitoses', 'Class')


# check data: , data col type, 
str(df)
#'data.frame':	699 obs. of  11 variables:
#  $ pat_id                     : int  1000025 1002945 1015425 1016277 1017023 1017122 1018099 1018561 1033078 1033078 ...
#$ Clump_Thickness            : int  5 5 3 6 4 8 1 2 2 4 ...
#$ Uniformity_of_Cell_Size    : int  1 4 1 8 1 10 1 1 1 2 ...
#$ Uniformity_of_Cell_Shape   : int  1 4 1 8 1 10 1 2 1 1 ...
#$ Marginal_Adhesion          : int  1 5 1 1 3 8 1 1 1 1 ...
#$ Single_Epithelial_Cell_Size: int  2 7 2 3 2 7 2 2 2 2 ...
#$ Bare_Nuclei                : Factor w/ 11 levels "?","1","10","2",..: 2 3 4 6 2 3 3 2 2 2 ...
#$ Bland_Chromatin            : int  3 3 3 3 3 9 3 3 1 2 ...
#$ Normal_Nucleoli            : int  1 2 1 7 1 7 1 1 1 1 ...
#$ Mitoses                    : int  1 1 1 1 1 1 1 1 5 1 ...
#$ Class                      : int  2 2 2 2 2 4 2 2 2 2 ... 

summary(df)

# check NA values
any(is.na(df))     # [1] FALSE

# missing values in 'Bare_Nuclei' represented by '?' - remove these 16 rows & render col as integer
clean <- subset(df, df$Bare_Nuclei != '?')
clean$Bare_Nuclei <- as.integer(clean$Bare_Nuclei)

# make the class column as factor instead of integer
clean$Class <- as.factor(clean$Class)

# check data: outliers (?)


# check: data imbalance & how to handle  -> data imbalance is there  *****
#  Class: 2 for benign, 4 for malignant
# or, summary(clean)
library(dplyr)
clean %>% 
  group_by(Class) %>%
  summarise(no_rows = length(Class))
#Class no_rows
#<fct>   <int>
# 2         444
# 4         239


## convert the factor labels- Class: 2 for benign, 4 for malignant
df$Class[df$Class == 2] <- 0
df$Class[df$Class == 4] <- 1
df$Class <- as.factor(df$Class)

sub$Class <- as.numeric(as.character(sub$Class))
sub$Class[sub$Class == 2] <- 0
sub$Class[sub$Class == 4] <- 1
sub$Class <- as.factor(sub$Class)



###### DATA UNDERSTANDING ############ 
# distribution of the variables
library(caret)
x <- clean[,2:10]   # also, check with dTraining & dTest
y <- clean[,11]
scales <- list(x=list(relation="free"), y=list(relation="free"))
# the above results in plots with individual scales for each var;
# remove this option to plot on same scale for all
featurePlot(x=x, y=y, plot="density", scales=scales)


# distribution of pairwise variables
# https://rstudio-pubs-static.s3.amazonaws.com/12556_4e02f5564dc24b57b7a8f6d95d2a5cf7.html
# http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
pairs(clean[,-c(1,11)], pch=16, col=as.factor(clean$Class))
pairs(clean[,-c(1,11)], panel = function(...) smoothScatter(..., add = T))


# boxplot-distribution by class
library(reshape2)
dfmelt <- melt(clean, measure.vars=2:10)

library(ggplot2)
ggplot(dfmelt, aes(Class, value, fill=variable))+
  geom_boxplot() + 
  #geom_jitter(size=0.8) + 
  facet_wrap(~variable)

rm(dfmelt)
## median values for 'Mitoses' are similar for the 2 classes
## median values for 'Bare_Nuclei' are close-by


# correlation among variables
cor(clean[,2:10]) #numeric_data df 
library(corrplot)
corrplot(cor(clean[,2:10]),type="lower", method="color", tl.cex=0.6, cl.cex=0.8)
## several of the variables are highly correlated - select i) uniformity of cell size
#                                                         ii) Bare nuclei
#                                                        iii) mitoses
#                                                         iv) clump thickness (0.64 cc to uniformity of cell size)


# pca  (GOOGLE: how to run pca in R)
t <- clean[,c(2:10)]     # take numeric columns only, not factors
#t <- t[, colSums(t != 0) > 0]    # remove columns with only zeroes
t <- t[,apply(t, 2, var, na.rm=TRUE) != 0] # remove columns with constant, variance = 0
m.pca <- prcomp(t, center = TRUE, scale. = TRUE) # take PCA
plot(m.pca, type='l')
plot(m.pca$x[,1:2], col = clean$Class)
summary(m.pca)
# 1st PC has max contribution (61%) to proportion of variance
# 1st 3 PCs explain 78% of variance, 1st 4 explain 84%
# 2nd & 3rd PCs have higher coefficient for Bare_nuclei & Mitoses
# 4th PC has high coefficient from Clump_thickness


## check VIF : valid for lm & glm models
# see results below model run



########   create validation & test sets  ###########
fractionTraining   <- 0.9
fractionTest       <- 0.1
sampleSizeTraining   <- floor(fractionTraining   * nrow(clean))
sampleSizeTest       <- floor(fractionTest       * nrow(clean))
indicesTraining    <- sort(sample(seq_len(nrow(clean)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(clean)), indicesTraining)
indicesTest        <- sort(sample(indicesNotTraining, size=sampleSizeTest))
dTraining   <- clean[indicesTraining, ]
dTest       <- clean[indicesTest, ]
#rm(sampleSizeTest, sampleSizeTraining, indicesNotTraining, indicesTest, indicesTraining)

dTraining %>% 
  group_by(Class) %>%
  summarise(no_rows = length(Class))
#Class no_rows
#<fct>   <int>
#  2     398
#  4     216

dTest %>% 
  group_by(Class) %>%
  summarise(no_rows = length(Class))
# class labels change: 2 -> 0; 4 -> 1
#  Class no_rows
#<fct>   <int>
# 2        45
# 4        23



####### run random forest with k-fold cross-validation  #######
fmla <- as.formula(paste("Class ~ ", paste(c(colnames(clean)[2:10]), collapse='+')))  ## all variables

fmla2 <- as.formula(paste("Class ~ ", paste(c(colnames(clean)[2:3],colnames(clean)[7],colnames(clean)[10]), collapse='+'))) ## selected variables
fmla3 <- as.formula(paste("Class ~ ", paste(c(colnames(clean)[2:3],colnames(clean)[7]), collapse='+'))) ## selected variables



# partition the data for K-fold cross-validation 
# https://www.r-bloggers.com/k-fold-cross-validation-random-forest-vs-gbm/

library(randomForest)
# where & how to set seed ?

# require(verification)  ## for roc.area
library(caret)

k = 10
n = floor(nrow(dTraining)/k) # size of each fold

#err.vect <- rep(NA,k)     # store the error in this vector
sensitivity.vec <- rep(NA,k)
specificity.vec <- rep(NA,k)
precision.vec <- rep(NA,k)
recall.vec <- rep(NA,k)
F1.vec <- rep(NA,k)
balanced_acc.vec <- rep(NA,k)


## model hyper-parameters
# RF
ntree = 401 # 251  , try 1001?
mtry = 5    # 3 (default) , try 8?

# logistic
cutOff = 0.4  # 0.8 , 0.5
#library(InformationValue)
#optCutOff <- optimalCutoff(cv_test$Class, glm_prob2)[1]   # find optimal cut-off
#optCutOff = 0.52; # however, it did not lead to improvement with fmla2 or fmla,
# cross-validation shows better sensitivity with lower cutOff (tried with 0.4)


for (i in 1:k) {
  s1 <- (i-1)*n + 1  # start of subset
  s2 <- i*n          # end of subset
  Subset <- s1:s2    # range of subset
  
  # create training & validation sets
  cv_train <- dTraining[-Subset,]
  cv_test <- dTraining[Subset,]
  
  
  ### build classification model (or, any other method) ###
  # RandomForest
  ####fit <- randomForest(fmla, cv_train, ntree=251, importance=T)  #fit <- randomForest(x=cv_train[,-1], y=as.factor(cv_train[,1]))
  #fit2 <- randomForest(fmla2, cv_train, ntree=ntree, mtry=mtry, importance=T)
  
  #png(paste('VI_4var_ntree',ntree,'_k',i,'.png', sep=''), h=552, w=803)
  #varImpPlot(fit2, sort = T, main=paste("Variable Importance, ntree=",fit$ntree,", fold=",i," mtry=",mtry, sep =' '))
  #dev.off()
  
  ## error vs #trees
  # GOOGLE: random forest error plot
  # https://stats.stackexchange.com/questions/51629/multiple-curves-when-plotting-a-random-forest
  #plot(fit2)
  
  # predict using model
  #####prediction <- predict(fit, newdata=cv_test[,-11], type="prob")[,2]   ## use for classification problem
  #prediction <- predict(fit2, newdata=cv_test)  ## use for random forest regression
  
  # calculate model accuracy for i-th fold  **** the below line gives error -find way to calculate roc
  #err.vect[i] <- roc.area(cv_test[,11], prediction)$A  ## use for classification; cv_test[,1] = cv_test$class
  #print(paste("AUC for fold ",i,":", err.vect[i]))    ## use for classification
  
  
  # SVM
  #fit <- svm(fmla, cv_train, type="nu-regression", kernel="radial")
  
  
  ## logistic
  fit_logistic2 <- glm(fmla, data=cv_train, family=binomial(link="logit"))  # change input formula
  #summary(fit_logistic2)
  # anova(fit_logistic, test="Chisq")
  glm_prob2 <- predict(fit_logistic2, newdata = cv_test, type='response') # plogis(predict(fit_logistic2, newdata = cv_test))
  prediction <- ifelse(glm_prob2 > cutOff, 1, 0)

  
  
  conf_mat <- confusionMatrix(data = prediction, #cv_test$predicted.response,  
                              reference = cv_test$Class,
                              positive = '1')
  #print(conf_mat$table)
  sensitivity.vec[i] <- conf_mat$byClass[1]
  specificity.vec[i] <- conf_mat$byClass[2]
  precision.vec[i] <- conf_mat$byClass[5]
  recall.vec[i] <- conf_mat$byClass[6]
  F1.vec[i] <- conf_mat$byClass[7]
  balanced_acc.vec[i] <- conf_mat$byClass[11]
  
  rm(conf_mat,Subset)  # prediction, glm_prob2, fit_logistic2
}

#print(paste("avg AUC: ",mean(err.vect)))     ## use for classification
#print(paste("avg MSE: ", mean(err.vect)))     ## use for regression

print(paste('sensitivity','specificity','precision','recall','F1-measure','balanced_acc', sep=' '))
for(i in 1:k) {
  print(paste(sensitivity.vec[i], specificity.vec[i], precision.vec[i], recall.vec[i], F1.vec[i], balanced_acc.vec[i], sep=' '))
}

print("\n")
print(paste('avg sensitivity','specificity','precision','recall','F1-measure','balanced_acc', sep=' '))
print(paste(mean(sensitivity.vec), mean(specificity.vec), mean(precision.vec), mean(recall.vec), mean(F1.vec), mean(balanced_acc.vec), sep=' '))


## ROC curve:
#library(pROC)
#plot(roc(test_set$bad_widget, glm_response_scores, direction="<"))  ## not good?
#library(ROCR)
#perf <- performance(glm_prob2,"tpr","fpr")   # not working
#library(plotROC)
#plotROC(cv_test$Class, prediction)  ## not working


## VIF (variable inflation factor) to check multi-collinear predictors
library(car)
vif(fit_logistic)  #fit_logistic <- glm(fmla, data=cv_train, family=binomial(link="logit"))  # change input formula
vif(fit_logistic2) #fit_logistic2 <- glm(fmla2, data=cv_train, family=binomial(link="logit"))  # change input formula


########################################################################################
# improve with: handle data imbalance, feature selection & PCA, optimise hyper-parameters


# feature selection
# see RF run


## build final model on full training set
ntree = 401 #251
mtry = 5
fit <- randomForest(fmla, dTraining, ntree=ntree, mtry=mtry, importance=T) 
varImpPlot(fit, sort = T, main=paste("Variable Importance, ntree=",fit$ntree,", fold=",i," mtry=",mtry, sep =' '))

fit2 <- randomForest(fmla2, dTraining, ntree=ntree, mtry=mtry, importance=T)
varImpPlot(fit2, sort = T, main=paste("Variable Importance, ntree=",fit$ntree,", fold=",i," mtry=",mtry, sep =' '))


## run on final test set dTest
prediction <- predict(fit, newdata=dTest)  ## use for random forest regression
confusionMatrix(data = prediction, #cv_test$predicted.response,  
                reference = dTest$Class,
                positive = '2')

prediction <- predict(fit2, newdata=dTest)  ## use for random forest regression
confusionMatrix(data = prediction, #cv_test$predicted.response,  
                reference = dTest$Class,
                positive = '2')




## select important attributes using Random forest Classification, regression forest ctree and rtree

## read in data


####  to find features using classification ####
# step 1: convert continuous data to categorical data (class labels) using binning
m1$class[m1$Y <= 37.9 ] <- 'L'
m1$class[m1$Y > 37.9 & m1$Y <= 60.8] <- 'M'
m1$class[m1$Y > 60.8] <- 'H'
m1$class <- as.factor(m1$class)


## using random forest  
library(randomForest)
fmla <- as.formula(paste("class ~ ", paste(colnames(m1)[16:(ncol(m1)-1)], collapse='+')))
fit <- randomForest(fmla, m1, ntree=10)
summary(fit)
importance(fit)

### finding variable importance ###
fit_forest <- randomForest(fmla, dataframe, ntree=250, importance=T)  ## ntree = 501 or 251
plot(fit_forest)
varImpPlot(fit_forest, sort = T, main="Variable Importance", n.var=10)
fit_forest$importance


library(party)
## 1st var
fmla <- as.formula(paste("class ~ ", paste(colnames(m1)[16:(ncol(m1)-1)], collapse='+')))
tree = ctree(fmla, data=m1, controls = ctree_control(stump = TRUE))
plot(tree)

## 2nd var
fmla <- as.formula(paste("class ~ ", paste(c(colnames(m1)[16:27],colnames(m1)[29:(ncol(m1)-1)]), collapse='+')))
tree = ctree(fmla, data=m1, controls = ctree_control(stump = TRUE))
plot(tree)

 
## using rpart
library(rpart)
rtree = rpart(fmla, method='anova', data=dat)
rtree
plotcp(rtree)
plot(rtree)
summary(rtree)



##### using the raw values of continuous variable ####
fmla <- as.formula(paste("continuous_Y ~ ", paste(colnames(dat)[16:(ncol(dat))], collapse='+')))

library("party")
fmla <- as.formula(paste("Total_kW ~ ", paste(colnames(dat)[16:(ncol(dat))], collapse='+')))
tree = ctree(fmla, data=dat)
plot(tree)

library(rpart)
rtree = rpart(fmla, method='anova', data=dat)
rtree
plotcp(rtree)
plot(rtree)
summary(rtree)
# ?rpart # split can be using information or GINI


## if the variable to be predicted is continuous, then the RF implementation is called regression forest

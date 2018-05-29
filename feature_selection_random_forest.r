## select important attributes using Random forest Classification, regression forest ctree and rtree

## read in data

###########################################################################################
##### correlation among different variables #####
t <- df[,c(5,9,14:ncol(df))]
#t <- t[,apply(t, 2, var, na.rm=TRUE) != 0]  # remove columns with constant, variance = 0
library(corrplot)
#par(mar = c(10,4,4,4)) # space on the bottom, left, top, and right.
corrplot(cor(d[,c(5,9,14:ncol(d))]),type="lower", method="color", tl.cex=0.6, cl.cex=0.8)


#### PCA  #####
t <- df[,c(5,14:ncol(df))]     # take numeric columns only, not factors
#t <- t[, colSums(t != 0) > 0]    # remove columns with only zeroes
t <- t[,apply(t, 2, var, na.rm=TRUE) != 0]  # remove columns with constant, variance = 0
#t <- t[,sapply(t, function(v) var(v, na.rm=TRUE)!=0)]
#str(t)  # shows types of data in different columns
m.pca <- prcomp(t, center = TRUE, scale. = TRUE)   # take PCA
#print(m.pca)
plot(m.pca, type='l')
plot(m.pca$x[,1:2],)
summary(m.pca)
##########################################################################################

####  to find features using classification ####
# step 1: convert continuous data to categorical data (class labels) using binning
m1$class[m1$Y <= 37.9 ] <- 'L'
m1$class[m1$Y > 37.9 & m1$Y <= 60.8] <- 'M'
m1$class[m1$Y > 60.8] <- 'H'
m1$class <- as.factor(m1$class)


## using random forest 
#randomForest_R does not take more than 52 values of a categorical variable
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

## get kth tree from the random forest
# getTree(randomForest(fmlaT1, data=d, ntree=1000), k=1, labelVar = TRUE)


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

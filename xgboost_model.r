library(xgboost)

# encode factor data: # convert selected columns to numeric
cols <- c(1:18)
T <- tt
T[cols] <- as.data.frame(lapply(T[cols], as.numeric))
#T <- as.numeric(tt) #as.data.frame(apply(tt[cols],2,function(x)as.numeric(x)))

# to create same train-test set as RF
dTr   <- T[indicesTraining, ]
dTe       <- T[indicesTest, ]

# use the columns used for fmla7
train_X = dTr[,c(4,5,6,7,8,9,10,11,12,14,15,16,17,18)]
train_Y = dTr[,c(1)]
test_X = dTe[,c(4,5,6,7,8,9,10,11,12,14,15,16,17,18)]
test_Y = dTe[,c(1)]

#put into the xgb matrix format
dtrain_gb = xgb.DMatrix(data =  as.matrix(train_X), label = train_Y )
dtest_gb = xgb.DMatrix(data =  as.matrix(test_X), label = test_Y)

# these are the datasets the rmse is evaluated for at each iteration
watchlist = list(train=dtrain_gb, test=dtest_gb)

# try 1 - off a set of paramaters I know work pretty well for most stuff

bst = xgb.train(data = dtrain_gb, 
                max.depth = 8, 
                eta = 0.3, 
                nthread = 2, 
                nround = 1000, 
                watchlist = watchlist, 
                objective = "reg:linear", 
                early_stopping_rounds = 50,
                print_every_n = 500)


#[1]	train-rmse:273.195496	test-rmse:285.724762 
#Multiple eval metrics are present. Will use test_rmse for early stopping.
#Will train until test_rmse hasn't improved in 50 rounds.

#Stopping. Best iteration:
#[11]	train-rmse:84.180618	test-rmse:174.297806
importance_matrix <- xgb.importance(colnames(dtrain_gb), model = bst)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")

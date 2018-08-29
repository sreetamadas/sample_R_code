## check threshold for cropping signals

library(readxl)

path <- "C:/Users/DAR9KOR/Desktop/data/sample_datasets/airfare/"
setwd(path)

################################################################
## original file ## 
D <- read_excel("AirFare_Data.xlsx")

# the following cols can be removed
D <- D[ , !names(D) %in% c('Destination Country Code','Destination Country Name','Destination Region Name',
                           'Destination State-Province Code','Issuing Country Code','Local Currency Code',
                           'Origin Country Code','Origin State-Province Code','Ticket Status Description',
                           'Tickets Used','Total Travel Time','Transaction Count (Air + Rail)','Travel Type Description')] 
summary(D)
#library(dplyr)
#D %>%
#  group_by(11, 26) %>%
#  summarise_all(funs(sum))




######################################################################

#### read data ####
df <- read_excel('top_5_route_data.xls')

### drop columns  ####
df$X__1 <- NULL
df$`Days to Refund` <- NULL
#df <- df[ , !names(df) %in% c("col1","col2","col3","col5")]


#### create new features ####
# travel month
df$travel_month <- format(as.POSIXct(df$`Ticket Departure Date`, format="%Y-%m-%d"), format = "%m")

# travel day of week
df$travel_day <- as.POSIXlt(df$`Ticket Departure Date`)$wday

# booking date vs ticket departure date & ticket return date
df$book_dep_time <- difftime(as.POSIXct(df$`Ticket Departure Date`, format="%Y-%m-%d"), 
                        as.POSIXct(df$`Booking Date`, format="%Y-%m-%d"), units="days")

df$book_ret_time <- difftime(as.POSIXct(df$`Ticket Return Date`, format="%Y-%m-%d"), 
                             as.POSIXct(df$`Booking Date`, format="%Y-%m-%d"), units="days")

df$book_dep_time <- as.numeric(df$book_dep_time)
df$book_ret_time <- as.numeric(df$book_ret_time)

# remove days(=1) with booking date after departure date
df <- subset(df, df$book_dep_time >=0)


###### plots #######
max(df$`Base Fare`)  # max(df$`Local Paid Fare`)  ;  max(df$`Lowest Fare Amount`)
min(df$`Base Fare`)  # min(df$`Local Paid Fare`)  ;  min(df$`Lowest Fare Amount`)
plot(as.POSIXct(df$`Ticket Departure Date`, format="%Y-%m-%d"), df$`Base Fare`, type='l', ylim=c(-700,2400))
par(new=T)
plot(as.POSIXct(df$`Ticket Departure Date`, format="%Y-%m-%d"), df$`Local Paid Fare`, type='l', ylim=c(-700,2400), col='red')
par(new=T)
plot(as.POSIXct(df$`Ticket Departure Date`, format="%Y-%m-%d"), df$`Lowest Fare Amount`, type='l', ylim=c(-700,2400), col='blue')
boxplot(df$`Base Fare`)
#############################

## eliminate 7 data points with -ve values of base, local & lowest fare
df <- subset(df, df$`Base Fare` >= 0)

## for `Transaction Type` == 'EXCHANGE' ; trip miles are zero


###########################################################################################
##### correlation among different variables #####
#t <- df[,c(3,14,15,40,41)]
#t <- t[,apply(t, 2, var, na.rm=TRUE) != 0]  # remove columns with constant, variance = 0
library(corrplot)
#par(mar = c(10,4,4,4)) # space on the bottom, left, top, and right.
corrplot(cor(df[,c(3,14,15,40,41)]),type="lower", method="color", tl.cex=0.6, cl.cex=0.8)



## using random forest 
#randomForest_R does not take more than 52 values of a categorical variable
library(randomForest)
t <- df[,c(14,1,2,5,7,8,10,11,16,22,24,25,26,28,38,39,3,18,32,36,40,41)]

# replace spaces with _ in column names
#colnames(t)[1] <- 'Local_Paid_Fare'
names(t) <- gsub(" ", "_", names(t))  
names(t) <- gsub("-", "_", names(t))  # correct E-ticket

# replace spaces in character strings in multiple columns
cols <- c(2:13)  
##t[cols] <- gsub(" ", '_', t[cols])
#[t == " "] <- "_"
t[cols] <- as.data.frame(apply(t[cols],2,function(x)gsub('\\s+', '_',x)))

# convert selected columns to factor
cols <- c(2:16)
t[cols] <- lapply(t[cols], factor)
summary(t)
str(t)
#  $ Class_by_Coupon             : Factor w/ 366 levels "B","B/B","B/B/Q/Q",..: 162 117 52 297 108 70 206 131 308 28 ...
t$Class_by_Coupon <- NULL

# remove NA values
t <- t[complete.cases(t),] 

# https://stackoverflow.com/questions/10688137/how-to-fix-spaces-in-column-names-of-a-data-frame-remove-spaces-inject-dots
fmla <- as.formula(paste("Local_Paid_Fare ~ ", paste(colnames(t)[2:21], collapse='+')))

fit_forest <- randomForest(fmla, data=t, ntree=251, importance=T)
plot(fit_forest)
varImpPlot(fit_forest, sort = T, main="Variable Importance", n.var=20)
fit_forest$importance
summary(fit_forest)
importance(fit_forest)

# remove base fare & re-run
corrplot(cor(t[,c(1,16,17,18,19,20)]),type="lower", method="color", tl.cex=0.6, cl.cex=0.8)
t$Base_Fare <- NULL
fmla2 <- as.formula(paste("Local_Paid_Fare ~ ", paste(colnames(t)[2:20], collapse='+')))
fit_forest2 <- randomForest(fmla2, data=t, ntree=251, importance=T)
fit_forest3 <- randomForest(fmla2, data=t, ntree=501, importance=T)
plot(fit_forest2)
varImpPlot(fit_forest2, sort = T, main="Variable Importance", n.var=15)
fit_forest2$importance
summary(fit_forest)
importance(fit_forest)

# drop trip_Miles & re-run
tt <- t
tt$Trip_Miles <- NULL
fmla3 <- as.formula(paste("Local_Paid_Fare ~ ", paste(colnames(tt)[2:19], collapse='+')))
fit_forest4 <- randomForest(fmla3, data=tt, ntree=501, importance=T)
plot(fit_forest4)
varImpPlot(fit_forest4, sort = T, main="Variable Importance, ntree=501")

fit_forest5 <- randomForest(fmla3, data=tt, ntree=1001, importance=T)
varImpPlot(fit_forest5, sort = T, main="Variable Importance, ntree=1001")
plot(fit_forest5)

# drop no. of sub-trips
tt$Number_of_Sub_Trips <- NULL
fmla4 <- as.formula(paste("Local_Paid_Fare ~ ", paste(colnames(tt)[2:18], collapse='+')))
fit_forest6 <- randomForest(fmla4, data=tt, ntree=1001, importance=T)
plot(fit_forest6)
varImpPlot(fit_forest6, sort = T, main="Variable Importance, ntree=1001")




## create separate training & test set
fractionTraining   <- 0.9
fractionTest       <- 0.1
sampleSizeTraining   <- floor(fractionTraining   * nrow(tt))
sampleSizeTest       <- floor(fractionTest       * nrow(tt))
indicesTraining    <- sort(sample(seq_len(nrow(tt)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(tt)), indicesTraining)
indicesTest        <- sort(sample(indicesNotTraining, size=sampleSizeTest))
dTraining   <- tt[indicesTraining, ]
dTest       <- tt[indicesTest, ]


## run k-fold cross-validation with different parameters ###
k = 10
n = floor(nrow(dTraining)/k)
err.vect <- rep(NA,k)     # store the error in this vector


## model hyper-parameters
# RF
ntree = 1501 # 501; 1501 ; 251  , try 1001?
mtry = 5  # 6

# key features
fmla11 = as.formula(paste("Local_Paid_Fare ~ ", paste(c(colnames(tt)[5:6],colnames(tt)[9:10],colnames(tt)[14:18]), collapse='+'))) 
fmla12 = as.formula(paste("Local_Paid_Fare ~ ", paste(c(colnames(tt)[5:6],colnames(tt)[9:10],colnames(tt)[14:15],colnames(tt)[17:18]), collapse='+'))) 

# dropped ticket class descr, air booking descr, airline alliance, Main_Airline_Code, Origin & destination airport & city
fmla10 = as.formula(paste("Local_Paid_Fare ~ ", paste(c(colnames(tt)[4],colnames(tt)[7],colnames(tt)[11:12],colnames(tt)[14:18]), collapse='+')))   


# dropped ticket class descr, air booking descr, airline alliance, total_travel_time_in_min
fmla9 = as.formula(paste("Local_Paid_Fare ~ ", paste(c(colnames(tt)[4:12],colnames(tt)[14:15],colnames(tt)[17:18]), collapse='+')))   

# dropped ticket class descr, air booking descr, airline alliance, E_ticket, Overnight_Eligible
fmla8 = as.formula(paste("Local_Paid_Fare ~ ", paste(c(colnames(tt)[4:6],colnames(tt)[8:10],colnames(tt)[12],colnames(tt)[14:18]), collapse='+')))     
# dropped ticket class descr, air booking descr, airline alliance
fmla7 = as.formula(paste("Local_Paid_Fare ~ ", paste(c(colnames(tt)[4:12],colnames(tt)[14:18]), collapse='+')))   
# dropped ticket class descr, air booking descr
fmla6 = as.formula(paste("Local_Paid_Fare ~ ", paste(c(colnames(tt)[3:12],colnames(tt)[14:18]), collapse='+')))    
# dropped ticket class descr
fmla5 = as.formula(paste("Local_Paid_Fare ~ ", paste(c(colnames(tt)[2:12],colnames(tt)[14:18]), collapse='+'))) 

fmla4 = as.formula(paste("Local_Paid_Fare ~ ", paste(colnames(tt)[2:18], collapse='+')))

fmla = fmla12
for (i in 1:k) {
  s1 <- (i-1)*n + 1  # start of subset
  s2 <- i*n          # end of subset
  Subset <- s1:s2    # range of subset
  
  # create training & validation sets
  cv_train <- dTraining[-Subset,]
  cv_test <- dTraining[Subset,]
  
  # explicitly drop the cols
  #cv_train <- cv_train[ , !names(cv_train) %in% c("Ticket_Class_Description","Air_Booking_Description","Airline_Alliance")] # Total_Travel_Time_in_Minutes
  #cv_test <- cv_test[ , !names(cv_test) %in% c("Ticket_Class_Description","Air_Booking_Description","Airline_Alliance")]  # Total_Travel_Time_in_Minutes
  
  
  ### build regression model : RandomForest
  fit <- randomForest(fmla, cv_train, ntree=ntree, importance=TRUE) # mtry=mtry,
  
  #png(paste('VI_4var_ntree',ntree,'_k',i,'.png', sep=''), h=552, w=803)
  #varImpPlot(fit, sort = TRUE, main=paste("VI, ntree=",fit$ntree,", fold=",i," mtry=",mtry,' ', 'fmla12', sep =' '))
  #dev.off()
  
  ## error vs #trees
  # GOOGLE: random forest error plot
  # https://stats.stackexchange.com/questions/51629/multiple-curves-when-plotting-a-random-forest
  #plot(fit2)
  
  # predict using model
  prediction <- predict(fit, newdata=cv_test)  ## use for random forest regression
  
  # r-squared
  r2 <- 1 - sum((cv_test$Local_Paid_Fare - predict(fit, newdata=cv_test))^2)/sum((cv_test$Local_Paid_Fare - mean(cv_test$Local_Paid_Fare))^2)
  r2_train <- 1 - sum((cv_train$Local_Paid_Fare - predict(fit, data=cv_train))^2)/sum((cv_train$Local_Paid_Fare - mean(cv_train$Local_Paid_Fare))^2)
  
  # MSE
  err.vect[i] <- sqrt(mean((cv_test$Local_Paid_Fare - predict(fit, newdata=cv_test))^2))
  rmse <- sqrt(mean((cv_train$Local_Paid_Fare - predict(fit, data=cv_train))^2))
  print(paste("fold ",i,", RMSE (test):", err.vect[i],", r2 (test):", r2,", RMSE <train>: ",rmse,", r2 <train>; ",r2_train)) 
}
print(paste("avg RMSE: ", mean(err.vect)))     ## use for regression


#### xgboost ###

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


##################################################################################

# train RF using all data
fmla = fmla12
cv_train <- dTraining[,c(1,5,6,9,10,14,15,17,18)]
cv_test <- dTest[,c(1,5,6,9,10,14,15,17,18)]
cv_train$travel_day <- as.numeric(cv_train$travel_day)
cv_train$travel_month <- as.numeric(cv_train$travel_month)
cv_test$travel_day <- as.numeric(cv_test$travel_day)
cv_test$travel_month <- as.numeric(cv_test$travel_month)


fit <- randomForest(fmla, cv_train, ntree=ntree, mtry=2) #, importance=TRUE)
prediction <- predict(fit, newdata=cv_test)  ## use for random forest regression

# r-squared
r2 <- 1 - sum((cv_test$Local_Paid_Fare - predict(fit, newdata=cv_test))^2)/sum((cv_test$Local_Paid_Fare - mean(cv_test$Local_Paid_Fare))^2)
r2_train <- 1 - sum((cv_train$Local_Paid_Fare - predict(fit, data=cv_train))^2)/sum((cv_train$Local_Paid_Fare - mean(cv_train$Local_Paid_Fare))^2)
rmse_test <- sqrt(mean((cv_test$Local_Paid_Fare - predict(fit, newdata=cv_test))^2))
rmse_train <- sqrt(mean((cv_train$Local_Paid_Fare - predict(fit, data=cv_train))^2))

######################################################################

## predict on new data: BOSTON (BOS) - NEW YORK (EWR, JFK, LGA), travel month, travel day
#Destination_City_Name <- c('BOSTON')
#Origin_City_Name <- c('NEW_YORK')
#Destination_Airport_Code <- c('BOS')
#Origin_Airport_Code <- c('EWR','JFK','LGA')
travel_month <- seq(1,12, length.out = 12)
travel_day <- seq(0,6, length.out = 7)
book_dep_time <- c(1,30,60)
book_ret_time <- c(1,30,60,5,35,65)
#Local_Paid_fare <- c(0)
#require(utils)
#test <- expand.grid(Local_Paid_fare,Destination_Airport_Code,Destination_City_Name,Origin_Airport_Code,Origin_City_Name,
#                    travel_month,travel_day,book_dep_time,book_ret_time, KEEP.OUT.ATTRS = FALSE)
#colnames(test) = c('Local_Paid_Fare','Destination_Airport_Code','Destination_City_Name','Origin_Airport_Code','Origin_City_Name',
#                   'travel_month','travel_day','book_dep_time','book_ret_time')
#test$travel_day <- as.factor(test$travel_day)
#test$travel_month <- as.factor(test$travel_month)
#test$Local_Paid_Fare <- 0


#test <- subset(dTest, dTest$Destination_City_Name == 'BOSTON' & dTest$Origin_City_Name == 'NEW_YORK')
#test <- test[,c(1,5,6,9,10,14,15,17,18)]
#test$travel_day <- as.numeric(test$travel_day)
#test$travel_month <- as.numeric(test$travel_month)


test <- subset(dTest, dTest$Destination_City_Name == 'BOSTON' & dTest$Origin_City_Name == 'NEW_YORK')
test1 <- test[,c(1,5,6,9,10)]
test1 <- test1[13:15,]
tmp1 <- data.frame()
for(i in 1:12) {
  tmp1 <- rbind(tmp1, test1)
}
tmp1 <- cbind(tmp1,travel_month)

tmp2 <- data.frame()
for (i in 0:6) {
  tmp2 <- rbind(tmp2, tmp1)
}
tmp2 <- cbind(tmp2, travel_day)

tmp1 <- data.frame()
for(i in 1:3) {
  tmp1 <- rbind(tmp1, tmp2)
}
tmp1 <- cbind(tmp1,book_dep_time)

tmp2 <- data.frame()
for (i in 1:6) {
  tmp2 <- rbind(tmp2, tmp1)
}
tmp2 <- cbind(tmp2, book_ret_time)

## predict ##
test_pred <- predict(fit, newdata=tmp2)
test <- cbind(tmp2,test_pred)

##############################################################################################
### check NA values
Are there any NA values in col?
> any(is.na(df$sel_col))
[1] TRUE

How many NA values in col?
> sum(is.na(df$sel_col))
[1] 1

Which index has NA value?
> which(is.na(df$sel_col))
[1] 146308

###################################################################################################
## use of if-else condition, without using loop
#DF1$Activity <- ifelse(DF2$NAME == DF1$NAME, DF2$Activity, NA)
df$from <- ifelse(df$to == 0, 1, 0)  # meaning: if to==0,from = 1; if to==1, from=0

#####################################################################################################
## calculate max & min in a row of data ##

1. create a dataframe with the columns (from each row) which should be included in the calculation
2. run the following code on it

df <- data.frame(subset$XA, subset$XB, subset$XC)	# creating data frame
df$min=apply(df,1,function(x) min(x))				      # select min in the row
df$max=apply(df,1,function(x) max(x))				      # select max in the row
df$diff <- 0.04*df$min - (df$max - df$min)				# calculation on selected data

#######################################################################################################
#########  calculate max & min along column  ########
> min(vh$A)
[1] 201.01
> max(vh$A)  
[1] 264.59
> sd(vh$A)
[1] 13.23705
> mean(vh$A)
[1] 230.304

max(df$y, na.rm=T)
sort(df$y, decreasing = TRUE)

####################################################################################################
## append data to a list
# at the beginning
df <- append(0,df)             

# at the end
df <- append(df,0)             

####################################################################################################
## say, a dataframe has multiple values (taken at short intervals) of temp, pr etc per shift
## create a new dataframe with averaged values for these parameters

df_avg <- summaryBy(. ~ Date + Shift, FUN=c(mean, median, sd), data=df, na.rm=TRUE)   ## median affected by NA values

######################################################################################################
             
## combine dataframes
dat <- merge(w_mc, df_avg, by = "DateTime")    # inner join
#dat <- merge(w_mc, df_avg, by = "DateTime", all.x = TRUE)  # check left/right/outer/inner join
#sel_dat <- merge(sel_dat, tmp, by.x='dateTime', all.x = TRUE, all.y = TRUE)

######################################################################################################
             
## calculate new col based on condition

# NAs introduced by coercion since the numerator & denominator are blank for some rows
# so use the if-else condition
for (i in 1:nrow((prdf))) {
  if(prdf$x[i] == 0) {
    prdf$y[i] <- 0
  }
  else{
    prdf$y[i] <- as.numeric(as.character(prdf$y[i]))/prdf$x[i]
  }
}

#####################################################################################################
####### correlation among variables   ##############
numeric_data <- data[,4:17]  # create new df with only the numeric columns
cor(numeric_data)           # calculate corr
library(corrplot)
corrplot(cor(numeric_data),type="lower", method="color", tl.cex=0.6, cl.cex=0.8)

##########################################################################################################
### PCA ###
t <- d[,c(5,14:ncol(d))]     # take numeric columns only, not factors

#t <- t[, colSums(t != 0) > 0]    # remove columns with only zeroes
t <- t[,apply(t, 2, var, na.rm=TRUE) != 0]  # remove columns with constant, variance = 0
#t <- t[,sapply(t, function(v) var(v, na.rm=TRUE)!=0)]

#str(t)  # shows types of data in different columns

m.pca <- prcomp(t, center = TRUE, scale. = TRUE)   # take PCA
#print(m.pca)
plot(m.pca, type='l')
plot(m.pca$x[,1:2],)
summary(m.pca)

#####################################################################################################
###  normalize data  ############

subset=s110[strftime(s110$txtime,'%d',tz = 'UTC')== dateofmonth,]
library(clusterSim)
Y_z   <- data.Normalization(subset$Y,type="n1",normalization="column")

###########################################################################################################
             
### combine multiple conditions into a data frame / subsetting data
# http://stackoverflow.com/questions/4935479/how-to-combine-multiple-conditions-to-subset-a-data-frame-using-or

my.df <- subset(data , V1 > 2 | V2 < 4)
my.df <- data[(data$V1 > 2) | (data$V2 < 4), ]	## using OR

new_df <- subset(df, (df$Y >= L2) & (df$Y < L1))		## using AND

df <- subset(dat, subset = df$cat_X %in% c('M01','M02','M03','M04','M05') & df$Y == 'K' )

new_df <- df[,c(5,14:ncol(df))]     # take selected columns by column no.

########################################################################################################
             
### using data table to subset data ###
## for each value of X, subset data with the min. value of Y
library(data.table)
DT <- data.table(df)

#select <- DT[ , .SD[which.min(Y)], by = X]  
## alternately, keep multiple cases with same min value
select2 <- DT[ , .SD[Y == min(Y)], by = X]
## another alternate
#select3c <- DT[DT[, .I[Y == min(Y)], by = X]$V1]

########################################################################################################
## bind multiple data frame 
dataset1 <- read.csv("C:/Users/Desktop/data/data1.csv")
dataset2 <- read.csv("C:/Users/Desktop/data/data2.csv")   
dataset1 <- rbind(dataset1, dataset2)

###################################################################################################
### concatenate data from separate files to a single file

setwd("C:/Users/Desktop/data/")
file_list <- list.files()	## this list is not ordered by date

library(readxl)
for (file in file_list){
    f=paste("C:/Users/Desktop/data/",file,sep = '')

    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
        dataset <- read_excel(f)	# read_excel(file)
    }
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
        temp_dataset <-read_excel(f)
        dataset<-rbind(dataset, temp_dataset)
        rm(temp_dataset)
    }
}
num <- 1
write.csv(dataset, paste('filenew',num,'_.csv'))               

###################################################################################################
######## rename column of dataframe  #########
             
colnames(tmp_df)[1] <- "time"
colnames(df) <- c("time","col1","col2","Y1","Y2")
names(df)[names(df) == "oldColumnName"] <- "NewColumnName"
             
             
### remove columns of a dataframe
df$col_to_remove <- NULL
df <- df[ , !names(df) %in% c("col1","col2","col3","col5")] ## works as expected
              

###########################################################################
             
## fill in rows corresponding to added timestamps
#library(padr)  pad(a)
df <- na.locf(df, fromLast = TRUE)
              
###################################################################################
             
## binning data ###
#fac <- cut(df$subset.Y, c(-10, 20, 50, 80, 110, 140, 170, 200, 230, 260),labels=c('1','2','3','4', '5','6','7','8','9')) # rename levels
#fac <- cut(df$subset.Y, c(-10,20,40,60,80,100,120,140,160,180,200,220,240,260),labels=c(1:13)) # rename levels
fac <- cut(df$subset.Y, breaks=seq(-10,260,by=10), labels=c(1:27))
df <- cbind(df,fac)

########################################################################################################
### rename factors ####
fac2 <- cut(df$Y, c(-10, 5, 30, 60, 100, 500),labels=c('off','low','medium','high','veryHigh'))
df <- cbind(df,fac2)

new_factor <- factor(df$data, levels=c("A","B","AB","O"), labels=c("BT_A","BT_B","BT_AB","BT_O"))

#######################################################################################################
##### assign values to columns by factors ######
df$score[df$fac2 == 'low'] <- 60

#########################################################################################################
## find factors values in a col, & save them i) in a new dataframe , ii) as list
## method 1: as dataframe
new_df <- data.frame(as.character(levels(as.factor(df$ID))))
colnames(new_df)[1] <- 'ID'
total <- tapply(df$TotalPcs, df$ID, FUN=sum)
sd_T <- tapply(df$TotalPcs, df$ID, FUN=sd)  # sample FUN : sum, mean, median, sd
num_of_instances <- tapply(df$someVariable, df$ID, function(x) length(unique(x))) # count no. of occurrences of each ID
new_df <- cbind(new_df, total, sd_T)
## remove NA values
new_df <- new_df[complete.cases(new_df),]             
             

## method 2: as list             
idList <-  as.character(sort(unique(df$ID))) ## get unique IDs in column
            
## method 3:
## finding unique entries
library(sqldf)
array_name = sqldf("select DISTINCT sno as 'sensor ID' from dat")
                          
                           
########################################################################################################
## calculate statistical parameters for data grouped by factors ###

df <- data.frame(subset1$txtime, subset1$Y, subset1$xA, subset1$xB, subset1$xC)   
df$Y_n <- df$subset1.Y * 100/constant
fac <- cut(df$Y_n, c(5, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500))
df <- cbind(df,fac)
df$ab <- df$subset1.xA - df$subset1.xB

average_ab <- tapply(df$ab, df$fac, FUN=mean)
range_ab <- tapply(df$ab, df$fac, FUN=range)
sd_ab <- tapply(df$ab, df$fac, FUN=sd)
max_ab <- tapply(df$ab, df$fac, FUN=max)
min_ab <- tapply(df$ab, df$fac, FUN=min)

tmp_df <- cbind(tmp_df, sd_ab, max_ab, min_ab) 


#######################################################################################################
## assign day of week
df$date <- format(as.POSIXct(df$txtime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d") # %H:%M:%S
df$day <- as.POSIXlt(df$date)$wday  # as number; sunday -> 0, Monday -> 1 & so on
df$day2 <- weekdays(as.Date(df$txtime))  # as text, ie, by name
             
             
### calculate time differences ######
c_time <- as.POSIXlt(df$s15.txtime )
timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")  
## (OR), timedel2 <- as.numeric(diff(df$s15.txtime))
timedel <- append(timedel,'NA') ## add 'NA' to end of column, as length(timedel) = (No. of rows in DF) - 1 
df <- cbind(df,timedel)  ## add the time differences column to the dataframe


c_time <- as.POSIXlt(df$s15.txtime )
timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
# (OR), timedel2 <- as.numeric(diff(df$s15.txtime))
#timedel <- append(timedel, -999) #'NA') ## add 'NA' to end of column, as length(timedel) = (No. of rows in DF) - 1 
df <- df[-nrow(df),]   ## remove last row of data, corrs to which there is no time duration
df <- cbind(df,timedel)  ## add the time differences column to the dataframe


### split the ts column into date & time
dat$date <- substring(dat$ts, 1, 10)
dat$time <- substring(dat$ts, 12, 19)
                           
                           
### adding time interval to a given time stamp
dat$dateTime <- as.POSIXct(dat$dateTime, format="%Y-%m-%d %H:%M:%S") + 5*60*60 + 30*60  ### add 5 hours 30 min
                           
             
##################################################################################################
                           
## check no. of repeats                           
library(plyr)
sel_dat <- join(sel_dat, count(sel_dat, "dateTime"))
             
                           
                           
## techniques to remove repeats
# for repeats, do either of i, ii or iii
#   i. single entry in each 1-min interval
sel_dat = sel_dat[with(sel_dat, c(ts[-1]!= ts[-nrow(sel_dat)], TRUE)),]
  
#  ii. unique entries (1 -> 0) & (0 -> 1) in each 1-min interval 
sel_dat$chk_col <- paste(as.numeric(as.POSIXct(paste(sel_dat$date, sel_dat$time, sep = ' '))), sel_dat$from, sel_dat$to, sep ='') 
sel_dat <- sel_dat[with(sel_dat, order(chk_col)), ] #sel_dat <- sel_dat[order(chk_col),]
sel_dat = sel_dat[with(sel_dat, c(chk_col[-1]!= chk_col[-nrow(sel_dat)], TRUE)),]
  
# iii. remove only adjacent duplicates
# for this, create a column combining date, from & to
sel_dat$chk_col <- paste(as.numeric(as.POSIXct(paste(sel_dat$date, sel_dat$time, sep = ' '))), sel_dat$from, sel_dat$to, sep ='') 
# remove adjacent duplicates
# https://stackoverflow.com/questions/27022057/removing-only-adjacent-duplicates-in-data-frame-in-r
sel_dat = sel_dat[with(sel_dat, c(chk_col[-1]!= chk_col[-nrow(sel_dat)], TRUE)),]
                           
                           
                           
### for multiple entries in a minute, reassign the time (seconds value)
#sel_dat$new[] <- 
sel_dat$new <- unlist(sapply(unique(sel_dat$dateTime), function(x) {
    freq <- sum(x == sel_dat$dateTime)
    as.character(seq(as.POSIXct(x), by = 60/freq, length.out = freq))  #(new, by=freq, length.out=freq)
  }))
                           
                           

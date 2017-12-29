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

####################################################################################################
## say, a dataframe has multiple values (taken at short intervals) of temp, pr etc per shift
## create a new dataframe with averaged values for these parameters

df_avg <- summaryBy(. ~ Date + Shift, FUN=c(mean, median, sd), data=df, na.rm=TRUE)   ## median affected by NA values

######################################################################################################
## combine dataframes
dat <- merge(w_mc, df_avg, by = "DateTime")    # inner join
#dat <- merge(w_mc, df_avg, by = "DateTime", all.x = TRUE)  # check left/right/outer/inner join


#####################################################################################################
####### correlation among variables   ##############
numeric_data <- data[,4:17]  # create new df with only the numeric columns
cor(sensor111data)           # calculate corr


##########################################################################
### combine multiple conditions into a data frame

my.data.frame <- subset(data , V1 > 2 | V2 < 4)
my.data.frame <- data[(data$V1 > 2) | (data$V2 < 4), ]	## using OR
prC <- subset(df, (df$Y >= L2) & (df$Y < L1))		## using AND

http://stackoverflow.com/questions/4935479/how-to-combine-multiple-conditions-to-subset-a-data-frame-using-or

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
               
#####################################################################################################
###  normalize data  ############

subset=s110[strftime(s110$txtime,'%d',tz = 'UTC')== dateofmonth,]
library(clusterSim)
Y_z   <- data.Normalization(subset$Y,type="n1",normalization="column")

###################################################################################################
######## rename column of dataframe  #########
colnames(tmp_df)[1] <- "time"
colnames(df) <- c("time","col1","col2","Y1","Y2")

###########################################################################
## binning data ###
#fac <- cut(df$subset.Y, c(-10, 20, 50, 80, 110, 140, 170, 200, 230, 260),labels=c('1','2','3','4', '5','6','7','8','9')) # rename levels
#fac <- cut(df$subset.Y, c(-10,20,40,60,80,100,120,140,160,180,200,220,240,260),labels=c(1:13)) # rename levels
fac <- cut(df$subset.Y, breaks=seq(-10,260,by=10), labels=c(1:27))
df <- cbind(df,fac)

########################################################################################################
### rename factors ####
fac2 <- cut(df$Y, c(-10, 5, 30, 60, 100, 500),labels=c('off','low','medium','high','veryHigh'))
df <- cbind(df,fac2)

########################################################################################################
##### assign values to columns by factors ######
df$score[df$fac2 == 'low'] <- 60

#########################################################################################################
## find factors values in a col, & save them i) in a new dataframe , ii) as list
## method 1: as dataframe
new_df <- data.frame(as.character(levels(as.factor(df$ID))))
colnames(new_df)[1] <- 'ID'
total <- tapply(df$TotalPcs, df$ID, FUN=sum)
sd_T <- tapply(df$TotalPcs, df$ID, FUN=sd)  # sample FUN : sum, mean, median, sd
new_df <- cbind(new_df, total, sd_T)
## remove NA values
new_df <- new_df[complete.cases(new_df),]             
             

## method 2: as list             
idList <-  as.character(sort(unique(df$ID))) ## get unique IDs in column
            
             
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
df$day <- as.POSIXlt(df$date)$wday
df$day2 <- weekdays(as.Date(df$txtime))
             
             
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





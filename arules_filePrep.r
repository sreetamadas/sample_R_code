### create a single column of transactions/ states from values (0,1) in the columns of the individual items
# steps
# store the colnames (excluding dateTime) into a vector called cn
# df[,-1] != 0 returns a matrix of TRUE/FALSE being TRUE where different from zero
# use apply for each row of the previous matrix to subset cn using the values TRUE/FALSE,
# and we collapse the resulting colnames into one string
# then bind the previous data.frame (only the dateTime column actually) with the new values into a column called D

# remove unnecessary columns
df$D1 <- NULL
df$day <- NULL

# format data appropriately
cn <- colnames(df)[-(1:5)]
new_df <- cbind(df['time'], #df['id'],df['A'],df['B'],df['C'],
                device=apply( df[,-(1:5)] != 0, 1 , function(x) paste(cn[x],collapse=',')))

## remove timeDate, & empty rows in device col
new_df$time <- NULL
new_df <- data.frame(new_df[!(new_df$device == "") ,])
colnames(new_df)[1] <- 'device'

                             
                             
## reshaping data: from long to wide format                             
#library(reshape2)                             
#cast.df <- dcast(original_df, formula = dateTime ~ ID, value.var = "to")   
# this means we get 1 column for each ID, 1 row at each timeStamp, & the value in the cell corresponds to the value in 'to' column 
# of original dataframe

## fill empty cells: NA values with last known state
#cast.dev <- na.locf(cast.dev, fromLast = FALSE)  # for all entries, copy from previous value
#cast.dev <- na.locf(cast.dev, fromLast = TRUE)  # for 1st entry, copy from next value                             
                             
# convert the data to numeric format
#for (i in 2:6) {
#  cast.dev[,i] <- as.numeric(cast.dev[,i])
#}                             
                             
########################################################################################
### association rule mining
# http://r-statistics.co/Association-Mining-With-R.html                             
# http://blog.hackerearth.com/beginners-tutorial-apriori-algorithm-data-mining-r-implementation

library(arules)
library(arulesViz)

## convert to transactions format
tData <- as (new_df, "transactions") 
#inspect(head(tData, 3))

## get the most frequent items
frequentItems <- eclat (tData, parameter = list(supp = 0.07, maxlen = 5)) # calculates support for frequent items
inspect(frequentItems)                             
                             
                             
# Create an item frequency plot for the top 5 items
itemFrequencyPlot(dat,topN=5,type="absolute")  # not working, plotly not installed properly ; missing libraries
                             
                             

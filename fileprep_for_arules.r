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

# reshape data
cn <- colnames(df)[-(1:5)]
new_df <- cbind(df['time'], #df['id'],df['A'],df['B'],df['C'],
                device=apply( df[,-(1:5)] != 0, 1 , function(x) paste(cn[x],collapse=',')))

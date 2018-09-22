###### correlation among variables   #######
             
numeric_data <- data[,4:17]  # create new df with only the numeric columns
cor(numeric_data)           # calculate corr
library(corrplot)
corrplot(cor(numeric_data),type="lower", method="color", tl.cex=0.6, cl.cex=0.8)


##########################################################################################################
# how to visualize clustering of multi dimensional data
# https://stats.stackexchange.com/questions/52625/visually-plotting-multi-dimensional-cluster-data

##########################################################################################################
####  PCA  #####
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/

t <- d[,c(5,14:ncol(d))]     # take numeric columns only, not factors

#t <- t[, colSums(t != 0) > 0]    # remove columns with only zeroes
t <- t[,apply(t, 2, var, na.rm=TRUE) != 0]  # remove columns with constant, variance = 0
#t <- t[,sapply(t, function(v) var(v, na.rm=TRUE)!=0)]

#str(t)  # shows types of data in different columns

m.pca <- prcomp(t, center = TRUE, scale. = TRUE)   # take PCA
#print(m.pca)  # prints the PCs for all the components in the input vector
summary(m.pca)  # prints importance of the PCs - standard deviation, proportion of variance, & cumulative proportion

plot(m.pca, type='l')  # plots PC vs variance
plot(m.pca$x[,1:2],)   # plots PC1 vs PC2

smoothScatter(m.pca$x[,1:2])  # gives a smoothened distribution with color-coding by density

#####################################################################################################
#####  normalize data  #######
subset=s110[strftime(s110$txtime,'%d',tz = 'UTC')== dateofmonth,]
library(clusterSim)
Y_z   <- data.Normalization(subset$Y,type="n1",normalization="column")


# how to normalize all columns in one go
scaled_df <- data.frame(scale(df))
scaled_df <- scaled_df[,!(sapply(scaled_df, function(x) all(is.nan(x))))]




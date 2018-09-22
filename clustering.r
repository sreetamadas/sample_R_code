# color by day-of-week: 
new_df$day <- as.POSIXlt(new_df$Date)$wday
new_df$col[new_df$day == 1| new_df$day == 2| new_df$day == 3 | new_df$day == 4| new_df$day == 5| new_df$day == 6] <- 'green'
new_df$col[new_df$day == 0] <- 'red'  # sunday

# plot
library(rgl)
plot3d(new_df$t_ON, new_df$e_per_tON, new_df$s_per_tON, col=new_df$col )


### dataframe with only relevant columns for clustering
# normalize the data ?  ************
library(clusterSim)
new_df$tONz   <- data.Normalization(new_df$t_ON,type="n1",normalization="column")
new_df$stONz <- data.Normalization(new_df$s_per_tON,type="n1",normalization="column")
new_df$etONz <- data.Normalization(new_df$e_per_tON,type="n1",normalization="column")

df <- data.frame(new_df$tONz, new_df$etONz, new_df$stONz)
colnames(df) <- c("t_ON","e_per_tON","s_per_tON")


### find optimal number of clusters ; plot upto 15
# https://www.statmethods.net/advstats/cluster.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html   <- see for random selection of initial cluster centres
wss <- (nrow(df)-1)*sum(apply(df,2,var))
n_clust <- 15  ## SPECIFY NO> OF CLUSTERS
for (i in 2:n_clust) wss[i] <- sum(kmeans(df, centers=i)$withinss)
plot(1:n_clust, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


### K-Means Cluster Analysis (, or any other method ? ********) 
fit <- kmeans(df, 4) # 3 cluster solution
# get cluster means
aggregate(df,by=list(fit$cluster),FUN=mean)


# add cluster id & centroid details for each data point
df <- cbind(df, fit$cluster)#, fit$centers)
colnames(df)[2] <- 'clust_id'
df$clust_centroid <- fit$centers[df$clust_id]  # this won't work if the centroid is a vector & not a scalar


### Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
clusplot(df, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
# Centroid Plot against 1st 2 discriminant functions
#library(fpc)
#plotcluster(df, fit$cluster) 

# how to visualize clustering of multi dimensional data
# https://stats.stackexchange.com/questions/52625/visually-plotting-multi-dimensional-cluster-data


### 3D plot
df <- cbind(df, fit$cluster)
plot3d(df$t_ON, df$e_per_tON, df$s_per_tON, col= as.factor(df$`fit$cluster`))


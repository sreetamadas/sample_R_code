###### correlation among variables   #######
             
numeric_data <- data[,4:17]  # create new df with only the numeric columns
cor(numeric_data)           # calculate corr
library(corrplot)
corrplot(cor(numeric_data),type="lower", method="color", tl.cex=0.6, cl.cex=0.8)

##########################################################################################################
####  PCA  #####
             
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
#####  normalize data  #######
subset=s110[strftime(s110$txtime,'%d',tz = 'UTC')== dateofmonth,]
library(clusterSim)
Y_z   <- data.Normalization(subset$Y,type="n1",normalization="column")

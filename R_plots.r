##################################################
##### plots in R ###


### set the layout of a plot
par(mfrow=c(3,2))  # no. of rows, no. of col
par(mar=c(5,4.5,2,1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text
#layout(matrix(c(1,2,2,2,3,3), 1, 6, byrow = TRUE))


### boxplot (comparison of variables X, Y, Z in dataframes l0, lL, lM, lH, lvH)
boxplot( l0$x, l0$y,  l0$z, 
         lL$x, lL$y, lL$z,
         lM$x, lM$y, lM$z, 
         lH$x, lH$y, lH$z, 
         lvH$x, lvH$y, lvH$z, 
         col=c("red","gold","green"), 
         names=c("","off","","","L","","","M","","","H","","","vH",""), 
         ylab='variable', xlab='Level', cex.axis=1, cex.lab=1.5,
         at=c(1,2,3, 5,6,7, 9,10,11, 13,14,15, 17,18,19))
         legend("topleft", legend=c("X","Y","Z"), fill=c("red","gold","green"), cex=1.0)
## add a horizontal line to the plot at a specified y value 
abline(h=10, col="red")


### barplot
# make a vector of the values to be used as input to barplot
bin = c(value_0, value_L, value_M, value_H, value_vH)
# plot
barplot(bin, ylim=c(0,100), xlab="Level", ylab="% of total instances", cex.axis=1.5, cex.names=1.5, cex.lab=1.5, names.arg=c("off","10","30","50","70","90","110","vH"), space=0)
abline(h=10, col="red")



library(ggplot2)
print(ggplot(df, aes(as.POSIXct(df$DateTime), comprG$x)) + geom_point(size=0.2, colour=as.factor(df$colour)) + geom_line(size=0.1, colour=as.factor(df$colour)) + labs(x="time",y="X") + geom_hline(yintercept=10) + theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))) 

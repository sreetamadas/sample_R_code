##################################################
##### plots in R: base graphics, ggplot ####

#####################################################################
### set the layout of a plot 
# method 1: in base graphics (for multiple plots)
png('figure1.png', h=1200, w=2600 )
attach(mtcars, warn.conflicts = FALSE)
par(mfrow=c(3,2))  # no. of rows, no. of col : this creates 6 figs
par(mar=c(5,4.5,2,1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text
#layout(matrix(c(1,2,2,2,3,3), 1, 6, byrow = TRUE))
plot(df$time, df$x) 
# etc figs ...
dev.off()

# method 2: in ggplot
library(ggplot2)
require(cowplot)
png('figure1.png', h=1200, w=2600 )
p1 <- ggplot(data=df, aes(df$time,df$x)) + geom_point() + labs(x="", y="X") 
p2 <- ggplot(data=df, aes(df$time,df$y)) + geom_point() + labs(x="", y="Y")
p3 <- ggplot(data=df, aes(df$time,df$z)) + geom_point() + labs(x="time", y="Z")
print(plot_grid(p1, p2, p3, ncol = 1)) #, align = 'v'))
dev.off()


######################################################################
# create the dataframe (http://www.dummies.com/programming/r/how-to-create-a-data-frame-from-scratch-in-r/)
X <- c(110, 90, 62, 20) # mean of data in bin, used bas input
Y <- c(14, 10, 25, 17)  # SD of data in the bin
size <- c(7026, 2115, 244, 27)  # no. of datapoints in the bin
id <- c(1, 1, 2, 2)
time <- as.Date(c('2017-01-01 08:00:00','2017-01-01 10:00:00','2017-01-02 09:00:00','2017-01-03 08:00:00'))
df <- data.frame(time, X, Y, size)


### scatter plot
# changing line width (lwd) & type (lty) =>  http://www.statmethods.net/advgraphs/parameters.html
# method 1: line plot
plot(as.POSIXct(df$time), df$y, type='l', lwd=0.5, xlab="time", ylab="var. Y", cex.axis=1.5, cex.lab=2) #, ylim=c(MIN,MAX))

# method 2: line plot in ggplot
library(ggplot2)
ggplot(data=df, aes(as.POSIXct(df$time), df$y)) + geom_point(size=0.2, colour=as.factor(df$colour)) +  # color by factor
  labs(x="time", y="Y") + ylim(0,180) + 
  geom_line(size=0.1, colour=as.factor(df$colour)) +  # colour="blue"
  geom_hline(yintercept=10) +  # adding horizontal line to plot
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))) 


## scatter plot for all variables in a datafarme
new <- df[,5:8]  # save columns 5-8 with numeric data fram df
pairs(new)  # plots the paired scatterplots
cor(new)  # check correlation among the variables



### 2D scatter plot with transparency
plot(df$x, df$y, col=rgb(0,100,0,50,maxColorValue=255), pch=16, xlab="X", ylab="Y") 


### 3D scatterplots ###
# method 1
library(scatterplot3d)
scatterplot3d(df$X, df$Y, df$size)
# method 2
library(rgl)
plot3d(df$X, df$Y, df$size)
#     method 2, with colours changed
df$colour[df$size < 500] <- 1
df$colour[df$size >= 500] <- 2
plot3d(df$X, df$Y, df$size, col=as.numeric(df$colour))
#      method 2, with text labels added
with(df, plot3d(df$X, df$Y, df$size, col=as.numeric(df$colour)))
with(df, text3d('X', 'Y', 'size'))
# method 3
library(car)
scatter3d(df$X, df$Y, df$size)



### histogram & density plots
# method 1
hist(df$y, xlab="var. Y", breaks=seq(0,175,by=5), cex.main=1.5, cex.lab=1.5, cex.axis=1.5)  # changing label size
# density plot
hist(df$y, freq=FALSE, xlab="var. Y")  
# method 2: histogram using ggplot
qplot(df$y, geom="histogram", binwidth=2, xlab="var.Y)")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + 
  xlim(min(df$y), (min(df$y) + 150))  
# density plot in ggplot
qplot(df$y, geom="density", xlab="var.Y")


### boxplot (comparison of variables X, Y, Z in dataframes l0, lL, lM, lH, lvH)
# method 1
boxplot( l0$x, l0$y,  l0$z, 
         lL$x, lL$y, lL$z,
         lM$x, lM$y, lM$z, 
         lH$x, lH$y, lH$z, 
         lvH$x, lvH$y, lvH$z, 
         col=c("red","gold","green"), 
         names=c("","off","","","L","","","M","","","H","","","vH",""), 
         ylab='variable', xlab='Level', cex.axis=1, cex.lab=1.5,
         at=c(1,2,3, 5,6,7, 9,10,11, 13,14,15, 17,18,19))
         legend("topleft", legend=c("X","Y","Z"), fill=c("red","gold","green"), cex=1.0) #, notch=TRUE)
## add a horizontal line to the plot at a specified y value 
abline(h=10, col="red") # adding horizontal line at y=10
abline(a=0,b=1) # adding line a+bx
# method 2: using ggplot
ggplot(tmp_df, aes(tmp_df$day_of_week, tmp_df$y)) + 
  geom_boxplot(width=0.4, fill='green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='day', y='Y') + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16))  



### violin plot (check bean plot: http://exploringdatablog.blogspot.in/2011/03/boxplots-beyond-iv-beanplots.html)
# method 1
library(vioplot)
vioplot(l0$x, l0$y,  l0$z, 
         lL$x, lL$y, lL$z,
         lM$x, lM$y, lM$z, 
         lH$x, lH$y, lH$z, 
         lvH$x, lvH$y, lvH$z,
        col=c("red","gold","green"))
# method 2: ggplot
ggplot(tmp_df, aes(tmp_df$day_of_week, tmp_df$Y)) + 
  geom_violin(scale="count", trim=FALSE, 
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='', y='Y') + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) 



### barplot
# make a vector of the values to be used as input to barplot
bin = c(value_0, value_L, value_M, value_H, value_vH)
# method 1: plot
barplot(bin, ylim=c(0,100), xlab="Level", ylab="% of total instances", 
        cex.axis=1.5, cex.names=1.5, cex.lab=1.5, 
        names.arg=c("off","10","30","50","70","90","110","vH"), space=0)  # breaks=seq(0,140,by=20),
abline(h=10, col="red")
# method 2: ggplot
ggplot(tmp_df, aes(as.POSIXct(tmp_df$Date), tmp_df$y)) + 
  geom_bar(stat="identity", fill=as.factor(tmp_df$colour)) +
  labs(x='time', y='Y') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16))
# method 2, sorted by the variable on X-axis
ggplot(df, aes(x = reorder(df$Model, -df$totalcount), df$y)) +
  geom_bar(stat="identity", width = 0.5) + #geom_point() + geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x='model', y='cumulative count (%)') +
  geom_hline(data=df, aes(yintercept=80), linetype='dashed') +
  geom_hline(data=df, aes(yintercept=50), linetype='dashed') +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=12))




### bubble plot 
symbols(df$X, df$Y, circles=df$size)
radius <- sqrt( df$size/ pi )
symbols(df$X, df$Y, circles=radius)
#symbols(df$X, df$Y, circles=radius, bg=as.numeric(df$X))
symbols(df$X, df$Y, circles=radius, bg=as.numeric(df$size), xlab= "bin", ylab= "SD")


### plot a matrix z by color
library(plotrix)
attach(mtcars, warn.conflicts = FALSE)
color2D.matplot(z,c(1,0),c(0,1),c(0,0), show.legend=FALSE,xlab="Column",ylab="Row",do.hex=FALSE,axes=TRUE,show.values=FALSE) 
# black-green: c(0,0),c(0,1),c(0,0); white-black: c(1,0),c(1,0),c(1,0); white-green: c(1,0),c(1,1),c(1,0) [low - high scales]


## plot heatmap
# method 1 : mat
library(corrplot)
corrplot(t(mat),is.corr = FALSE, method='square')
#corrplot(t(mat),is.corr = FALSE, method='color')
# method 2
heatmap(t(mat), Colv = F) #, scale= 'none')
# method 3
library(RColorBrewer)
corrplot(t(mat),is.corr = FALSE, method='color', col=brewer.pal(n=9, name='Blues'))
# method 4
library(plotrix)
color2D.matplot(t(mat), show.legend=FALSE,do.hex=FALSE,axes=TRUE,show.values=FALSE) 
#color2D.matplot(t(mat),c(1,0),c(0,1),c(0,0), show.legend=FALSE,do.hex=FALSE,axes=TRUE,show.values=FALSE) 


###############################################################
### color & shape by factor
# method 1
Palette1 <- c('red','blue','violet','black')

library(ggplot2)
require(cowplot)
library(car)
library(rgl)
fac <- cut(df$y, c(5, 40, 80, 100, 500))  # bin data
df <- cbind(df,fac)
plot1 <- ggplot(data=df, aes(df$x, df$y, color=fac)) + 
  geom_point(alpha=0.3) + 
  labs(x="X", y="Y") + 
  scale_colour_manual(values=Palette1) + 
  xlim(-45,45) + ylim(-45,45) + 
  scale_colour_discrete(drop=TRUE, limits = levels(fac)) # + geom_line(size=0.01, col="blue")


# method 2
fac2 <- cut(df$y, c(1, 30, 80, 100, 500),labels=c('low','medium','high','veryHigh')) # rename levels
df <- cbind(df,fac2)
df$color[df$fac2 == 'low'] <- 'red'
df$color[df$fac2 == 'medium'] <- 'yellow'
df$color[df$fac2 == 'high'] <- 'green'
df$color[df$fac2 == 'veryHigh'] <- 'yellow'
plot2 <- ggplot(data=df, aes(as.POSIXct(df$x), df$y)) + 
  geom_point(colour = as.factor(df$color)) + 
  labs(x="x", y="y") + 
  geom_line(size=0.01, colour = as.factor(df$color))  # + ylim(0,180) + geom_line(size=0.01, col="blue")

ggplot(data=df, aes(x=time, y=y, color=as.factor(id), shape=as.factor(id))) + 
  geom_point() + geom_line(size=0.1) + scale_x_datetime(date_minor_breaks = "1 day")


# method 3: if using plot() and coloring by factors, use type='b' and not lineplot i.e. type='l'
### changing line width (lwd) & type (lty) =>  http://www.statmethods.net/advgraphs/parameters.html

##########################################################
## Dual axis plot 
par(mar=c(5,4,4,5)+.1)  # sets bottom, left, top and right margins respectively of the plot region in number of lines of text
plot(x,y1,type="l", lwd=2, col="red", xlab="",ylab="", ylim=c(0,250))
par(new=T)
lines(x,y2,type="l", lwd=2, col="blue", xlab="",ylab="")
par(new=T)
lines(x,y3,type="l", lwd=2, col="green", xlab="",ylab="")
par(new=T)
plot(x,Z1,type="l",col="black", lty=2, lwd=3, xaxt="n",yaxt="n",xlab="",ylab="", ylim=c(0,100))
axis(4)
mtext("Y",side=2,line=2)
mtext("Z",side=4,line=3)
legend("bottomleft",col=c("red","blue","green","black"),lty=1,legend=c("Y1","Y2","Y3","Z"))

############################################################
## multiple line plots on the same figure
plot(df$time, df$x, type='l', col="red", ylim=c(0,250), ylab="x") ## change ylim
par(new=T)
lines(df$time, df$y, type='l', col="blue", ylim=c(0,250), ylab="") ## change ylim
par(new=T)
lines(df$time, df$z, type='l', col="green", ylim=c(0,250), ylab="")  ## change ylim
legend("topleft",col=c("red","blue","green"), lty=1, legend=c("x","y","z"))
par(new=F)
dev.off()


############################################################
####  zoom into a plot ####
install.packages("zoom")
library(zoom)
zm()

###########################################################
#### place tick marks ##
plot(..., xaxp  = c(x1, x2, n))

plot(1:10, 1:10, axes = FALSE)
axis(side = 1, at = c(1,5,10))
axis(side = 2, at = c(1,3,7,10))
box()

##################################################
##### plots in R: base graphics, ggplot #####
# venn diagram
# scatterplot (2D, 3D)
# histogram & density plot, 
# boxplot, violinPlot, 
# barplot, stacked barchart
# bubblePlot
# heatmap & matrices
# multiple plots, dual axes, facet wrap & grid, cowplot, shape & color changes

#####################################################################
### set the layout of a plot 
# method 1: in base graphics (for multiple plots)
png('figure1.png', h=1200, w=2600 )
attach(mtcars, warn.conflicts = FALSE)
par(mfrow=c(3,2))  # no. of rows, no. of col : this creates 6 figs
par(mar=c(5,4.5,2,1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text
#layout(matrix(c(1,2,2,2,3,3), 1, 6, byrow = TRUE))
plot(df$time, df$x) ## color & shape of points can be changed
# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
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


#############################################################################################
## plot multiple variables vs time from a dataframe  ###

for(i in 4:ncol(df)) {
  png(paste('param_',i,'.png') ,h=500, w=600)
  print(plot(as.POSIXct(df$Time, format="%Y-%m-%d %H:%M:%S"), df[,i], type='b'))
  dev.off()
}


###############################################################################################
### venn diagram  ###
# https://stackoverflow.com/questions/8713994/venn-diagram-proportional-and-color-shading-with-semi-transparency
# packages : venneuler, eulerr
# method 1
library(venneuler)  # not showing overlaps correctly
v <- venneuler(c(A=30, B=50, "A&B"=20 ))
plot(v)
# method 2
library(eulerr)   # not showing overlap correctly
v <- euler(c(A=30, B=50, "A&B"=20))
plot(v)




### scatter plot  ###
# changing line width (lwd) & type (lty) =>  http://www.statmethods.net/advgraphs/parameters.html
# method 1: line plot
plot(as.POSIXct(df$time), df$y, type='l', lwd=0.5, xlab="time", ylab="var. Y", cex.axis=1.5, cex.lab=2) #, ylim=c(MIN,MAX))

# method 2: line plot in ggplot
library(ggplot2)
ggplot(data=df, aes(as.POSIXct(df$time), df$y)) + geom_point(size=0.2, colour=as.factor(df$colour)) +  # color by factor
  labs(x="time", y="Y") + ylim(0,180) + 
  geom_line(size=0.1, colour=as.factor(df$colour)) +  # colour="blue"
  geom_hline(data=df, aes(yintercept=10), linetype='dashed') +  # adding horizontal line to plot
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))) 


### scatter plot for all variables in a datafarme
new <- df[,5:8]  # save columns 5-8 with numeric data fram df
pairs(new)  # plots the paired scatterplots
cor(new)  # check correlation among the variables


### 2D scatter plot with transparency  ###
# method 1
#library(clusterSim)  # may be reqd for rgb
#attach(mtcars, warn.conflicts = FALSE)
plot(df$x, df$y, col=rgb(0,100,0,50,maxColorValue=255), pch=16, xlab="X", ylab="Y") 

# method 2 : (alpha sets the transparency)
library(ggplot2)
ggplot(data=df, aes(df$ab, df$bc, color=fac)) + geom_point(alpha=0.3) +
    labs(x="var A-B", y="var B-C")   


### 3D scatterplots ###
# method 1
library(scatterplot3d)
scatterplot3d(df$X, df$Y, df$size, col=df$color, pch=as.numeric(df$shape), angle=50) # both color & shape (pch) can be changed; 
# angle: to change viewing angle 
# adding grid & legend
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
addgrids3d(df$X, df$Y, df$size, grid = c("xy", "xz", "yz"), angle=50)
legend('right',legend = c('ID 1','ID 2','ID 3','ID 4' ), pch=c(1,2,3,4))
#http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization

# method 2
library(rgl)
plot3d(df$X, df$Y, df$size)
#     method 2, with colours changed
df$colour[df$size < 500] <- 1
df$colour[df$size >= 500] <- 2
plot3d(df$X, df$Y, df$size, col=as.numeric(df$colour))  # only color of points can be changed 
#      method 2, with text labels added
with(df, plot3d(df$X, df$Y, df$size, col=as.numeric(df$colour)))
with(df, text3d('X', 'Y', 'size'))
# method 3
library(car)
scatter3d(df$X, df$Y, df$size)



### histogram & density plots ###
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


### boxplot (comparison of variables X, Y, Z in dataframes l0, lL, lM, lH, lvH)  ###
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
# add a horizontal line to the plot at a specified y value 
abline(h=10, col="red") # adding horizontal line at y=10
abline(a=0,b=1) # adding line a+bx
# method 2: using ggplot
ggplot(tmp_df, aes(tmp_df$day_of_week, tmp_df$y)) + 
  geom_boxplot(width=0.4, fill='green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='day', y='Y') + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16))  + 
  coord_cartesian(ylim=c(0,100)) # used to zoom into a selected section
## show number of data points in the box-plot
f <- function(y) 
     c(label=length(y), y=median(y))
ggplot(tmp, aes(tmp$categorical, tmp$Y)) + geom_boxplot(width=0.4, fill='green') +
      stat_summary(fun.data=f, geom="text", vjust=-0.5, col="blue")



### violin plot (check bean plot: http://exploringdatablog.blogspot.in/2011/03/boxplots-beyond-iv-beanplots.html) ###
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
## show number of data points in the box-plot
f <- function(y) 
     c(label=length(y), y=median(y))
ggplot(tmp, aes(tmp$categorical, tmp$Y)) +  geom_violin(scale="count", trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
      stat_summary(fun.data=f, geom="text", vjust=-0.5, col="blue")



### barplot  ###
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
# method 3, bars colored by gradient, according to value
ggplot(df, aes(x= reorder(df$Model,-df$value), df$value, fill=df$value)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x='model', y='value') + geom_hline(data=df, aes(yintercept=median(df$value)), linetype='dashed', color='black') +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16)) + 
  scale_fill_gradient2(low='blue', mid='white', high='red', space='Lab')
# method 4, horizontal bars
ggplot(df, aes(x= reorder(df$Model,-df$value), df$value, fill=df$value)) + 
  geom_bar(stat="identity") + coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x='model', y='value')


### stacked barchart  ###
## create stacked bars for: multiple shifts data (for a continuous var.) on a date & for multiple categorical variables
ggplot(data = df, aes(as.POSIXct(df$Date), df$continuous_var, colour=as.factor(df$categorical_var))) + 
          geom_bar(stat="identity", aes(fill=as.factor(df$categorical_var))) + scale_fill_brewer(palette = 12)



### bubble plot ###
symbols(df$X, df$Y, circles=df$size)
radius <- sqrt( df$size/ pi )
symbols(df$X, df$Y, circles=radius)
#symbols(df$X, df$Y, circles=radius, bg=as.numeric(df$X))
symbols(df$X, df$Y, circles=radius, bg=as.numeric(df$size), xlab= "bin", ylab= "SD")


### plot a matrix z by color ###
library(plotrix)
attach(mtcars, warn.conflicts = FALSE)
color2D.matplot(z,c(1,0),c(0,1),c(0,0), show.legend=FALSE,xlab="Column",ylab="Row",do.hex=FALSE,axes=TRUE,show.values=FALSE) 
# black-green: c(0,0),c(0,1),c(0,0); white-black: c(1,0),c(1,0),c(1,0); white-green: c(1,0),c(1,1),c(1,0) [low - high scales]


## plot heatmap ###
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


# method 3: if the no. of factors > 6, above won't work; set up colour & shape scale
set_col <- c('M1' = 'purple', 'M2' = 'cyan3', 'M3' = 'green', 'M4' = 'hotpink',
             'M5' = 'purple', 'M6' = 'cyan3', 'M7' = 'green', 'M8' = 'hotpink',
             'M9' = 'purple', 'M10' = 'cyan3', 'M11' = 'green', 'M12' = 'hotpink',
             'M13' = 'purple', 'M14' = 'cyan3', 'M15' = 'green', 'M16' = 'hotpink',
             'M17' = 'purple', 'M18' = 'cyan3', 'M19' = 'green', 'M20' = 'hotpink',
             'M21' = 'purple', 'M22' = 'cyan3', 'M23' = 'green', 'M24' = 'hotpink',
             'M25' = 'purple', 'M26' = 'cyan3', 'M27' = 'green', 'M28' = 'hotpink',
             'M29' = 'purple', 'M30' = 'cyan3', 'M31' = 'green', 'M32' = 'hotpink')
set_shape <- c('M1' = 23, 'M2' = 19, 'M3' = 22, 'M4' = 24,
               'M5' = 19, 'M6' = 22, 'M7' = 24, 'M8' = 23,
               'M9' = 22, 'M10' = 24, 'M11' = 23, 'M12' = 19,
               'M13' = 24, 'M14' = 23, 'M15' = 19, 'M16' = 22,
               'M17' = 4, 'M18' = 7, 'M19' = 8, 'M20' = 10,
               'M21' = 7, 'M22' = 8, 'M23' = 10, 'M24' = 4,
               'M25' = 8, 'M26' = 10, 'M27' = 4, 'M28' = 7,
               'M29' = 10, 'M30' = 4, 'M31' = 7, 'M32' = 8)
ggplot(data = df, aes(x = df$x, y = df$y, 
                           colour = df$id, shape = df$id)) + 
  geom_line(size=0.2) + geom_point(size=2) + #coord_cartesian(ylim = c(0,50)) + # used to zoom into a selected section
  scale_color_manual(values=set_col) +
  scale_shape_manual(values=set_shape) + 
  geom_hline(data=df, aes(yintercept=10), linetype='dashed') +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18))) 


# method 4: if using plot() and coloring by factors, use type='b' and not lineplot i.e. type='l'
### changing line width (lwd) & type (lty) =>  http://www.statmethods.net/advgraphs/parameters.html


# method 5: using direct label
library(directlabels)
library(ggplot2)
ggplot(x, aes(x$DateTime, x$continuous_var, group = x$categorical_var, colour=as.factor(x$categorical_var))) + 
    geom_line() + geom_point() + 
    geom_dl(aes(label = x$categorical_var), method = list(dl.combine("first.points", "last.points"), cex = 2))


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
### use of facet wrap & facet grid to plot data for multiple groups (factors or IDs)
ggplot(data = df, aes(x = df$x, y = df$y)) + 
  geom_line(size=0.05) + geom_point(size=0.8) + coord_cartesian(ylim = c(0,5)) + 
  facet_wrap(~ df$id) + labs(x='X', y='Y') + 
  geom_hline(data= df, aes(yintercept = 2)) +
  geom_hline(data= df, aes(yintercept = 1))

ggplot(data = df, aes(x = df$x, y = df$y)) + 
  geom_line() + geom_point() + coord_cartesian(ylim = c(0,3)) + facet_grid(df$id~.)

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

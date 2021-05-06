# heat-map

library(readxl)

path <- "C:/Users//Desktop/data/paper_PLots/" #analysis/" 
setwd(path)


df <- read_excel("Exhaustive Feature and Model combinations - Results.xlsx", sheet="non_aug_nonWhiten_15")  # Sheet2
#df <- read_excel("Exhaustive Feature and Model combinations2.xlsx", sheet="aug_5")  # Sheet2
#df <- read_excel("Exhaustive Feature and Model combinations - Results.xlsx", sheet="non_aug_whitenTrain_15")


# create list of methods & features
#methodList <- as.character(sort(unique(df$Method)))   #df3$date
methodList <- c("Lasso" , "Elastic Net" , "Ridge" , "SVR" , "Ada Boost" , "Stacked Regressor")

#featureList <- df$Features[1:30] #("Attn")
featureList <- c("PS","Age","c","S","Attn", 
                 "Age + PS","c + PS","c + Age","S + PS","S + Age","c + S","Attn + PS","Attn + Age","Attn + c","Attn + S",
                 "c + Age + PS","S + Age + PS","c + S + PS","c + S + Age","Attn + c + PS","Attn + c + Age","Attn + Age + PS","Attn + c + S","Attn + S + PS",
                 "Attn + S + Age", "c + S + Age+ PS","Attn + c + S + PS","Attn + c + S + Age","Attn + S + Age +PS","Attn + c + S + Age + PS")

## setup matrices
reg_A <- matrix(nrow=length(featureList), ncol=length(methodList))
reg_B <- matrix(nrow=length(featureList), ncol=length(methodList))
reg_C <- matrix(nrow=length(featureList), ncol=length(methodList))
rms_m <- matrix(nrow=length(featureList), ncol=length(methodList))
cor_m <- matrix(nrow=length(featureList), ncol=length(methodList))

for(i in 1:length(featureList) ) {
  for(j in 1:length(methodList) ) {
    ss <- subset(df, df$Method == methodList[j] & df$Features == featureList[i])
    reg_A[i,j] <- ss$`Region-A` 
    reg_B[i,j] <- ss$`Region-B` 
    reg_C[i,j] <- ss$`Region-C` 
    rms_m[i,j] <- ss$`RMSE (g/dL)` 
    cor_m[i,j] <- ss$`Correlation` 
  }
}

#rownames(reg_A) <- featureList #
colnames(reg_A) <- methodList
#rownames(reg_B) <- featureList #
colnames(reg_B) <- methodList
#rownames(reg_C) <- featureList #
colnames(reg_C) <- methodList
rownames(rms_m) <- featureList
colnames(rms_m) <- methodList
#rownames(cor_m) <- featureList #
colnames(cor_m) <- methodList

library(corrplot)
library(RColorBrewer)
#library(ggplot2)
attach(mtcars, warn.conflicts = FALSE)
## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf    # list of available colors

png("clarke_nonaug_nonWhiten_4.png", h= 1400, w = 1900) #w=1400
par(mfrow=c(3,1))
#png("plot.png", h= 2000, w = 1200)
#par(mfrow=c(5,1))
par(mar=c(1,1,2,2))
#corrplot(t(reg_A),is.corr = FALSE, method='color', col=brewer.pal(n=9, name='Blues'), title='region A', mar=c(0,0,0.8,0), tl.cex=0.8, cl.cex=0.7 )
#corrplot(t(reg_A),is.corr = FALSE, method='square', col=brewer.pal(n=9, name='Greens'), tl.cex=1.7, cl.cex=1.7, tl.col = "black", title='region A', mar=c(0,0,0.8,0)) 
#par(mar=c(3,3,4,2))
#corrplot(t(reg_B),is.corr = FALSE, method='square', col=brewer.pal(n=9, name='Oranges'), tl.cex=1.7, cl.cex=1.7, tl.col = "black", title='region B', mar=c(0,0,0.8,0))
#par(mar=c(3,3,4,2))
#corrplot(t(reg_C),is.corr = FALSE, method='square', col=brewer.pal(n=9, name='Reds'), tl.cex=1.7, cl.cex=1.7, tl.col = "black", title='region C', mar=c(0,0,0.8,0))
corrplot(t(reg_A),is.corr = FALSE, method='color', 
         col=colorRampPalette(c("white","green1","green3","green4","dark green"))(20), #"yellow","yellow green","green"
         mar=c(0,0,0.8,0), tl.cex=1.8, cl.cex=1.8, #title='region A', 
         tl.col = "black", addgrid.col="white", addCoef.col="white", number.cex=2 )  # addCoef.col="black"
corrplot(t(reg_B),is.corr = FALSE, method='color', 
         col=colorRampPalette(c("white","lightgoldenrod","gold2","goldenrod3","goldenrod4"))(20), # "palegoldenrod"
         mar=c(0,0,0.8,0), tl.cex=1.8, cl.cex=1.8,  #title='region B', 
         tl.col = "black", addgrid.col="white", addCoef.col="white", number.cex=2 )
corrplot(t(reg_C),is.corr = FALSE, method='color', 
         col=colorRampPalette(c("white","pink","lightsalmon2","red","red4"))(20), #"white","seashell1","pink",
         mar=c(0,0,0.8,0), tl.cex=1.8, cl.cex=1.8, #title='region C', 
         tl.col = "black", addgrid.col="white", addCoef.col="white", number.cex=2 )
dev.off()


#png("rmse2_corr.png", h=800, w= 1000)
#par(mfrow=c(2,1))
#par(mar=c(2,3,3,2))
#corrplot(t(rms_m),is.corr = FALSE, method='square', col=brewer.pal(n=9, name='Reds'), title='RMSE (g/dL)', mar=c(0,0,0.8,0), tl.cex=1.1, cl.cex=1.3, tl.col = "black" )
#corrplot(t(cor_m),is.corr = FALSE, method='square', col=brewer.pal(n=9, name='Greens'), title='Correlation', mar=c(0,0,0.8,0), tl.cex=1.1, cl.cex=1.3, tl.col = "black" )
#dev.off()


png("rmse_corr_nonaug_nonWhiten_2.png", h=800, w= 1000)
par(mfrow=c(2,1))
par(mar=c(2,3,3,2))
corrplot(t(rms_m),is.corr = FALSE, method='color', 
         col=colorRampPalette(c("white","pink","lightsalmon2","red","red4"))(20), # 15
         mar=c(0,0,0.8,0), tl.cex=1.1, cl.cex=1.0, #title='RMSE (g/dL)',
         tl.col = "black", addgrid.col="white", addCoef.col="white", number.cex=0.75 )  #addCoef.col="black"
corrplot(t(cor_m),is.corr = FALSE, method='color', 
         col=colorRampPalette(c("white","green1","green3","green4","dark green"))(20), # "yellow","yellow green","green",
         mar=c(0,0,0.8,0), tl.cex=1.1, cl.cex=1.0, #title='Correlation',
         tl.col = "black", addgrid.col="white", addCoef.col="white", number.cex=0.75 )
dev.off()



## combi
png("combi.png", h=1800, w=1500) #h=2000, w=1200
par(mfrow=c(5,1))
par(mar=c(0.5,2,2,1))  #0.5,2,8,1

corrplot(t(rms_m),is.corr = FALSE, method='color', 
         #col=colorRampPalette(c("white","pink","lightsalmon2","red","red4"))(20),addCoef.col="white",
         col=colorRampPalette(c("white","seashell1","pink","lightsalmon2","red"))(20),addCoef.col="black",
         mar=c(0,0,8,0), tl.cex=1.1, cl.cex=1.0, #title='RMSE (g/dL)',
         tl.col = "black", addgrid.col="white",  number.cex=0.75) #, number.digits = 1 )  

corrplot(t(cor_m),is.corr = FALSE, method='color', 
         col=colorRampPalette(c("white","green1","green3","green4","dark green"))(20), addCoef.col="white",
         #col=colorRampPalette(c("white","yellow","yellow green","green3","green4"))(20),addCoef.col="black",
         mar=c(0,0,0.8,0), tl.cex=1.1, cl.cex=1.0, #title='Correlation',
         tl.col = "black", addgrid.col="white",  number.cex=0.75)#, number.digits = 1 )

corrplot(t(reg_A),is.corr = FALSE, method='color', 
         col=colorRampPalette(c("white","green1","green3","green4","dark green"))(20), addCoef.col="white",
         #col=colorRampPalette(c("white","yellow","yellow green","green3","green4"))(20),addCoef.col="black",
         mar=c(0,0,0.8,0), tl.cex=1.1, cl.cex=1.0, #title='region A', 
         tl.col = "black", addgrid.col="white",  number.cex=0.75, number.digits = 1 )  # addCoef.col="black"

corrplot(t(reg_B),is.corr = FALSE, method='color', 
         col=colorRampPalette(c("white","lightgoldenrod","gold2","goldenrod3","goldenrod4"))(20),addCoef.col="white",
         mar=c(0,0,0.8,0), tl.cex=1.1, cl.cex=1.0,  #title='region B', 
         tl.col = "black", addgrid.col="white",  number.cex=0.75, number.digits = 1 )

corrplot(t(reg_C),is.corr = FALSE, method='color', 
         #col=colorRampPalette(c("white","pink","lightsalmon2","red","red4"))(20), addCoef.col="white",
         col=colorRampPalette(c("white","seashell1","pink","lightsalmon2","red"))(20),addCoef.col="black",
         mar=c(0,0,0.8,0), tl.cex=1.1, cl.cex=1.0, #title='region C', 
         tl.col = "black", addgrid.col="white",  number.cex=0.75, number.digits = 1 )

dev.off()





> library(plotrix)
> color2D.matplot(t(rms_m), show.legend=FALSE,do.hex=FALSE,axes=FALSE,show.values=FALSE,)
> axis(1,at=0.5:29.5,labels=rownames(rms_m), las=2) 
> ?color2D.matplot
# https://stackoverflow.com/questions/1828742/rotating-axis-labels-in-r

# https://stackoverflow.com/questions/30743983/how-to-change-color-scheme-in-corrplot

# https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/





#######################################################################
#######################################################################

## bar-plot with error bar
# https://www.r-graph-gallery.com/4-barplot-with-error-bar/
# https://heuristically.wordpress.com/2013/10/20/bar-plot-with-error-bars-r/

# google: how to calculate 95% confidence interval error value in excel
# https://www.cbgs.k12.va.us/cbgs-document/research/Guide_to_Error_Bars_in_Excel.pdf
# http://pages.mtu.edu/~fmorriso/cm3215/2012WordFigureErrorBars95CI.pdf



library(readxl)

df <- read_excel("Exhaustive Feature and Model combinations - Results.xlsx", sheet="non_aug_nonWhiten_15")
#####################################################################################
# get avg. RMSE & corr for each model
new <- data.frame(as.character(levels(as.factor(df$Method))))   # df3$date
colnames(new)[1] <- 'Method'

avg_RMSE <- tapply(df$`RMSE (g/dL)`, df$Method, FUN=mean)
new <- cbind(new, avg_RMSE)

avg_corr <- tapply(df$Correlation, df$Method, FUN=mean)
new <- cbind(new, avg_corr)

library(ggplot2)
#ggplot(data = new, aes(new$Method, new$avg_RMSE)) +
#  geom_bar(stat = "identity", width = 0.5) + labs(x='method',y='avg RMSE') +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  geom_bar(aes(y = new$avg_corr*2, group = 1), stat = "identity", width = 0.5) + 
#  scale_y_continuous(sec.axis = sec_axis(trans = ~ ./2 , name="avg corr"))


# convert to long format - not working
#library(reshape2)
#long <- melt(new, id.vars = c("Method"))

#ggplot(df, aes(x= reorder(new$Method,-new$avg_RMSE), df$value, fill=df$value)) + 

#################################################################################################


# Data: RMSE & CORR
library(dplyr)


data <- df %>% select(Method, `RMSE (g/dL)`) 
# Calculates mean, sd, se and IC
my_sum <- data %>%
  group_by(Method) %>%
  summarise( 
    n=n(),
    mean=mean(`RMSE (g/dL)`),
    sd=sd(`RMSE (g/dL)`)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
my_sum$group <- 'mean RMSE'


data2 <- df %>% select(Method, Correlation) 
# Calculates mean, sd, se and IC
my_sum2 <- data2 %>%
  group_by(Method) %>%
  summarise( 
    n=n(),
    mean=mean(Correlation),
    sd=sd(Correlation)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
  #mutate( ic2=se * qt(1-0.05/2, n))
my_sum2$group <- 'mean correlation'

# https://www.r-graph-gallery.com/4-barplot-with-error-bar/   *** used this link
# https://heuristically.wordpress.com/2013/10/20/bar-plot-with-error-bars-r/
# https://stackoverflow.com/questions/44872951/how-do-i-add-se-error-bars-to-my-barplot-in-ggplot2
# http://stulp.gmw.rug.nl/ggplotworkshop/comparinggroupstatistics.html




## merge the two dataframes
long <- rbind(my_sum, my_sum2)

write.csv(long, "mean_RMSE_corr.csv")


# Confidence Interval
library(ggplot2)
my_sum$Method <- factor(my_sum$Method, 
                      levels = c("Lasso" , "Elastic Net" , "Ridge" , "Ada Boost" , "SVR" , "Stacked Regressor"))

ggplot(my_sum) +
  geom_bar( aes(x=Method, y=mean), stat="identity", fill="turquoise", alpha=0.5) + 
  labs(x='', y='mean RMSE') + 
  geom_errorbar( aes(x=Method, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="black", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")


ggplot(my_sum2) +
  labs(x='', y='mean corr') + 
  geom_bar( aes(x=Method, y=mean), stat="identity", fill="blue", alpha=0.5) +
  geom_errorbar( aes(x=Method, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")



long$Method <- factor(long$Method, 
                      levels = c("Lasso" , "Elastic Net" , "Ridge" , "Ada Boost" , "SVR" , "Stacked Regressor"))
# aes(x=reorder(long$Method, -long$mean)
ggplot(long, aes(Method, mean, group = group, fill = group)) +
  geom_bar( stat="identity",  position = "dodge") + # alpha=0.5,
  geom_errorbar( aes(x=Method, ymin=mean-ic, ymax=mean+ic), 
                 width=0.2, colour="black", alpha=0.9, size=1.5, position=position_dodge(.9)) + 
  geom_text(aes(label = round(mean+ic, digits=3), y = round(mean+ic, digits=3)), vjust = -.5) +
  geom_text(aes(label = round(mean-ic, digits=3), y = round(mean-ic, digits=3)), vjust = 1.5) +
  labs(x='', y='mean') + 
  scale_fill_manual(values = c("chocolate2","deepskyblue3")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16), 
        legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank(), legend.text=element_text(size=12))
  
# https://stackoverflow.com/questions/28816467/ggplot-position-legend-in-top-left  

# https://stackoverflow.com/questions/22053139/r-ggplot-placing-labels-on-error-bars
# https://stackoverflow.com/questions/6455088/how-to-put-labels-over-geom-bar-in-r-with-ggplot2
# https://stackoverflow.com/questions/44872951/how-do-i-add-se-error-bars-to-my-barplot-in-ggplot2
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization




#########################################################
library(dplyr)


dataA <- df %>% select(Method, `Region-A`) 
# Calculates mean, sd, se and IC
my_sumA <- dataA %>%
  group_by(Method) %>%
  summarise( 
    n=n(),
    mean=mean(`Region-A`),
    sd=sd(`Region-A`)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
my_sumA$group <- 'Region-A'


dataB <- df %>% select(Method, `Region-B`) 
# Calculates mean, sd, se and IC
my_sumB <- dataB %>%
  group_by(Method) %>%
  summarise( 
    n=n(),
    mean=mean(`Region-B`),
    sd=sd(`Region-B`)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
my_sumB$group <- 'Region-B'


dataC <- df %>% select(Method, `Region-C`) 
# Calculates mean, sd, se and IC
my_sumC <- dataC %>%
  group_by(Method) %>%
  summarise( 
    n=n(),
    mean=mean(`Region-C`),
    sd=sd(`Region-C`)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
my_sumC$group <- 'Region-C'


long2 <- rbind(my_sumA, my_sumB, my_sumC)

long2$Method <- factor(long2$Method, 
                      levels = c("Lasso" , "Elastic Net" , "Ridge" , "Ada Boost" , "SVR" , "Stacked Regressor"))

library(ggplot2)
# aes(x=reorder(long$Method, -long$mean)
ggplot(long2, aes(Method, mean, group = group, fill = group)) +
  geom_bar( stat="identity",  position = "dodge") + # alpha=0.5,
  #geom_errorbar( aes(x=Method, ymin=mean-ic, ymax=mean+ic), 
  #               width=0.2, colour="black", alpha=0.9, size=1.5, position=position_dodge(.9)) +
  labs(x='', y='mean') + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16), legend.justification=c(1,1),legend.position=c(1,2),legend.title=element_blank(), legend.text=element_text(size=12)) + 
  scale_fill_manual(values = c("salmon","turquoise","blue"))
  



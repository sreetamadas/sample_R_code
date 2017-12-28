### hypothesis testing, check if the mean values in the 3 phases are significantly different
## assumption: data distributed normally
## use of paired t-test

setwd("C:/Users/Desktop/data/analysis/analysis2/")
load("C:/Users/Desktop/data/analysis/analysis2/.RData")

df1 <- data.frame(s15$P, s15$xA, s15$xB, s15$xC, s15$txtime)       

## select on-state data
df <- subset(df1, df1$s15.P > 0)  # hist(df$s15.P)

## min & max of data
cA_min <- min(as.numeric(df$xA))
cA_max <- max(as.numeric(df$xA))
cB_min <- min(as.numeric(df$xB))
cB_max <- max(as.numeric(df$xB))
cC_min <- min(as.numeric(df$xC))
cC_max <- max(as.numeric(df$xC))
P_min <- min(as.numeric(df$s15.P))
P_max <- max(as.numeric(df$s15.P))
#sort(df$s15.P, decreasing=FALSE)

## mean & standard deviation of phase variable A, B, C
xA_mean <- mean(df$xA)
xA_sd <- sd(df$xA)
xB_mean <- mean(df$xB)
xB_sd <- sd(df$xB)
xC_mean <- mean(df$xC)
xC_sd <- sd(df$xC)


## checking data distribution
hist(df$xA)
hist(df$xB)
hist(df$xC)
boxplot(df$xA, df$xB)
boxplot(df$xB, df$xC)
boxplot(df$xC, df$xA)

mean(df$xA) - mean(df$xB)
mean(df$xB) - mean(df$xC)
mean(df$xC) - mean(df$xA)

plot(df$xA,df$xB, col=rgb(0,100,0,50,maxColorValue=255))
abline(a=0,b=1)
plot(df$xB,df$xC, col=rgb(0,100,0,50,maxColorValue=255))
abline(a=0,b=1)
plot(df$xC,df$xA, col=rgb(0,100,0,50,maxColorValue=255))
abline(a=0,b=1)

## check normality of the differences
df$ab <- df$xA - df$xB
df$bc <- df$xB - df$xC
df$ca <- df$xC - df$xA

boxplot(df$ab)  # or, plot histogram
boxplot(df$bc)
boxplot(df$ca)
# assumption for paired t-test: The differences of the pairs follow a normal distribution or 
# the number of pairs is large (note here that if the number of pairs is < 30, we need to check 
# whether the differences are normal, but we do not need to check for the normality of each population)

qqnorm(df$ab)  ## these show that data are not normally distributed
qqline(df$ab)
qqnorm(df$bc)
qqline(df$bc)
qqnorm(df$ca)
qqline(df$ca)

# https://heuristically.wordpress.com/2011/09/28/paired-sample-t-test-in-r/
shapiro.test(powerAboveZero$ab) # Error in shapiro.test(powerAboveZero$ab) : sample size must be between 3 and 5000


### paired t-test ###
t.test(df$xA,df$xB,paired=TRUE) # mu=0, alt="two.sided", paired=TRUE, conf.level=0.99   
t.test(df$xB,df$xC,paired=TRUE) # mu=0, alt="two.sided", paired=TRUE, conf.level=0.99   
t.test(df$xC,df$xA,paired=TRUE) # mu=0, alt="two.sided", paired=TRUE, conf.level=0.99   

############  IMBALANCE CALCULATION  #####################
## method 1: (Vmax - Vmin) < 4% of Vmin, where Vmax & Vmin are chosen from the 3 phase voltages at each time  ####
## http://ecmweb.com/content/basics-voltage-imbalance 
## https://zenatix.com/current-and-voltage-unbalance-causes-and-counter-measures/
df$minV=apply(df,1,function(x) min(x))
df$maxV=apply(df,1,function(x) max(x))
#df$diff_m1 <- (df$maxV - df$minV) * 100/(4 * df$minV)
df$diff_m1 <- (df$maxV - df$minV) * 100/(df$minV) # 0.04*df$minV - (df$maxV - df$minV)
  
  
## method 2: 2% imbalance from avg voltage  #####
## http://ecmweb.com/content/basics-voltage-imbalance
# sum | V(i) - <V>| / (2 * <V>)  < 2%
df <- cbind(df, subset$Voltage)
colnames(df)[7] <- "subset.Voltage"
df$diffA <- abs(df$subset.voltageA - df$subset.Voltage)
df$diffB <- abs(df$subset.voltageB - df$subset.Voltage)
df$diffC <- abs(df$subset.voltageC - df$subset.Voltage)
#df$diff_m2 <- ( df$diffA + df$diffB + df$diffC) * 100/(2 * df$subset.Voltage)
df$diff_m2 <- ( df$diffA + df$diffB + df$diffC) * 100/(df$subset.Voltage)
  
  
## method 3: Max |V(i) - <V>| < 2% of <V>   ####
## http://www.achrnews.com/articles/93649-three-phase-motor-voltage-unbalance 
tmp_df <- data.frame(df$diffA, df$diffB, df$diffC)
tmp_df$maxDiff=apply(tmp_df,1,function(x) max(x))
#df$diff_m3 <- tmp_df$maxDiff * 100/(2 * df$subset.Voltage)
df$diff_m3 <- tmp_df$maxDiff * 100/(df$subset.Voltage)
  

                     
############################################################################   
# calculate reactive power
df1$apparentPower <- (df1$currentA*df1$voltageA + df1$currentB*df1$voltageB + df1$currentC*df1$voltageC)/1000
df1$reactivePower <- sqrt(df1$apparentPower*df1$apparentPower - df1$realtimepower*df1$realtimepower)
df1$ratio <- df1$realtimepower/df1$apparentPower

fac2 <- cut(df1$FracRatedPower, c(-10, 0, 30, 80, 100, 500),labels=c('off', 'low','medium','high','veryHigh')) # rename levels
df1 <- cbind(df1,fac2)

vH1 <- subset(df1, df1$fac2 == 'veryHigh')
h1 <- subset(df1, df1$fac2 == 'high')

### impedance calculation
vH1$impedanceA <- vH1$voltageA/vH1$currentA
vH1$impedanceB <- vH1$voltageB/vH1$currentB
vH1$impedanceC <- vH1$voltageC/vH1$currentC


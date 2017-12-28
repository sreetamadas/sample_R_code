
## method 1: (Vmax - Vmin) < 4% of Vmin, where Vmax & Vmin are chosen from the 3 phase voltages at each time  ####
## http://ecmweb.com/content/basics-voltage-imbalance 
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
  

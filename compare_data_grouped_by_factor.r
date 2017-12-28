## compare time-usage & total nos on different machines : using barplot & box-plot

## read data

## aggregate by factors
new_df <- data.frame(as.character(levels(as.factor(df$ID))))
colnames(new_df)[1] <- 'ID'
total <- tapply(df$TotalPcs, df$ID, FUN=sum)     ## summing over cell values
usage <- tapply(w_model$TotalProductPcs, w_model$Machine, function(x) length(x))    ## summing over instances/ no. of occurrences **
new_df <- cbind(new_df, total, usage)
## remove NA values
new_df <- new_df[complete.cases(new_df),] 


# usage plot
f <-
ggplot(new_df, aes(new_df$ID, new_df$usage)) + geom_bar(stat="identity") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x='', y='no. of shifts') +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) 


# distribution of production
h <-
ggplot(df, aes(df$ID, df$TotalProductPcs)) + geom_boxplot(width=0.4, fill='gold') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='machine', y='total Product (pcs)') + theme(axis.text=element_text(size=14), axis.title=element_text(size=16))  



#####  **  alternate way of counting instances #########
l0 <- subset(mc, mc$fac == '0')
l10 <- subset(mc, mc$fac == '10')
l30 <- subset(mc, mc$fac == '30')
l50 <- subset(mc, mc$fac == '50')
l70 <- subset(mc, mc$fac == '70')
l90 <- subset(mc, mc$fac == '90')
l110 <- subset(mc, mc$fac == '110')
l130 <- subset(mc, mc$fac == '130')


## count instances in each bin
count_l0 <- nrow(l0) * 100/nrow(mc)
count_l10 <- nrow(l10) * 100/nrow(mc)
count_l30 <- nrow(l30) * 100/nrow(mc)
count_l50 <- nrow(l50) * 100/nrow(mc)
count_l70 <- nrow(l70) * 100/nrow(mc)
count_l90 <- nrow(l90) * 100/nrow(mc)
count_l110 <- nrow(l110) * 100/nrow(mc)
count_l130 <- nrow(l130) * 100/nrow(mc)

## 1. compare time-usage & total nos on different machines : using barplot & box-plot

## read data

## aggregate by factors
new_df <- data.frame(as.character(levels(as.factor(df$ID))))
colnames(new_df)[1] <- 'ID'
total <- tapply(df$TotalPcs, df$ID, FUN=sum)     ## summing over cell values
usage <- tapply(df$TotalPcs, df$ID, function(x) length(x))    ## summing over instances/ no. of occurrences **
median_pcsPerShift <- tapply(df$TotalPcs, df$ID, FUN=median)
new_df <- cbind(new_df, total, usage, median_pcsPerShift)
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
l30 <- subset(mc, mc$fac == '30')
l50 <- subset(mc, mc$fac == '50')
## etc ...

## count instances in each bin
count_l30 <- nrow(l30) * 100/nrow(mc)
count_l50 <- nrow(l50) * 100/nrow(mc)
# etc ...
                
#################################################################################################
## calculate cumultive sum by factor & sort by total
           
model_TotalProd <- data.frame(as.character(levels(as.factor(tmp$ModelNo))))
colnames(model_TotalProd)[1] <- 'Model'
totalProd <- tapply(tmp$TotalProductPcs, tmp$ModelNo, FUN=sum)
model_TotalProd <- cbind(model_TotalProd, totalProd)
### set NA to zero ######
model_TotalProd$totalProd[is.na(model_TotalProd$totalProd)] <- 0

### plot cumulative values (total what % do the top models contribute?)
model_TotalProd$c <- model_TotalProd$totalProd * 100/sum(model_TotalProd$totalProd)    # calc %
model_TotalProd <- model_TotalProd[with(model_TotalProd, order(-totalProd, Model)), ]  # sort by total production, descending order
model_TotalProd$csum <- cumsum(model_TotalProd$c)      # calculate cumulative sum
#  model_TotalProd$csum <- ave(model_TotalProd$totalProd, model_TotalProd$Model, FUN=cumsum) # this is giving cumsum fo each model, not useful here

               
                
                
                
                

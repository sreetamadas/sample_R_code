## 1. calculate cumultive sum by factor & sort by total
           
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

               

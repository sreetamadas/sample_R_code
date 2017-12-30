# heatmap to compare, across different values of 2 categorical variables,
# several continuous variables

library(ggplot2)
require(cowplot)

###############################################
## data input

## data cleaning

###############################################################

## list of x1 ###
x1List <-  as.character(sort(unique(tmp$x1))) ## get unique IDs 

## list of x2 ### 
x2List <-  as.character(sort(unique(tmp$x2)))

# find mode
estimate_mode <- function(x) {
  if (length(x) > 2) {
  d <- density(x)
  d$x[which.max(d$y)]
  }
  else 0
}

shift <- matrix(nrow=length(modelList), ncol=length(machineList))
production <- matrix(nrow=length(modelList), ncol=length(machineList))
kpi <- matrix(nrow=length(modelList), ncol=length(machineList))
  
for(i in 1:length(x1List)) {
  #ntotal <- nrow(subset(tmp, tmp$x1 == x1List[i]))
  #w_model <- (subset(tmp, tmp$x1 == x1List[i]))
  #prod_total <- sum(w_model$TotalProductPcs)
  
  for(j in 1:length(x2List)) {
    
    # using mode
    w_mc <- subset(tmp, tmp$x1 == x1List[i] & tmp$x2 == x2List[j])
    kpi[i,j] <- estimate_mode(w_mc$kpi)  # for KPI, we take the most frequent value for a m/c-model pair
    production[i,j] <- estimate_mode(w_mc$TotalProductPcs)
    kpi[is.na(kpi)] <- -0.3
    production[is.na(production)] <- 0
    
    # using median over narrow production range
    #w_mc <- subset(tmp, tmp$x1 == x1List[i] & tmp$x2 == x2List[j] & tmp$TotalProductPcs >= 80 & tmp$TotalProductPcs <= 100)
    #w_mc <- subset(tmp, tmp$x1 == x1List[i] & tmp$x2 == x2List[j])  # median over full data
    #kpi[i,j] <- median(w_mc$kpi)
    #production[i,j] <- median(w_mc$TotalProductPcs)
    #kpi[is.na(kpi)] <- -0.3
    #production[is.na(production)] <- 0
    
    # using min value
    w_mc <- subset(tmp, tmp$x1 == x1List[i] & tmp$x2 == x2List[j])
    if(nrow(w_mc) == 0) {
      kpi[i,j] <- -0.3
      production[i,j] <- 0
    }
    else {
      kpi[i,j] <- min(w_mc$kpi)
      production[i,j] <- w_mc$TotalProductPcs[which.min(w_mc$kpi)]
    }
    
    #shift[i,j] <- nrow(w_mc) #/ntotal 
    #production[i,j] <- sum(w_mc$TotalProductPcs) #/prod_total ## dividing gives high values only if most pcs for a model were produced on a single (or few) m/cs.
  }
}
  
rownames(shift) <- modelList
rownames(production) <- modelList
rownames(kpi) <- modelList

##### Plots #####
# method 1
library(corrplot)
#corrplot(t(kpi),is.corr = FALSE, method='square')
#corrplot(t(kpi),is.corr = FALSE, method='color')
# kpi[kpi > 4] <- 0
# kpi[kpi > 4] <- -4
# kpi[kpi > 3 & kpi <= 4] <- -3
# kpi[kpi > 2 & kpi <= 3] <- -2
# kpi[kpi > 1.5 & kpi <= 2] <- -1.5
## plot for narrow bins of KPI
kpi_b <- kpi # backup
kpi[kpi > 1.0 | kpi <= 0.5] <- -0.2  # reset values outside of bin
corrplot(t(kpi),is.corr = FALSE, method='square')
rm(kpi)

# method 2
heatmap(t(kpi), Colv = F) #, scale= 'none')

# method 3
library(RColorBrewer)
corrplot(t(kpi),is.corr = FALSE, method='color', col=brewer.pal(n=9, name='Blues'))

# method 4
library(plotrix)
color2D.matplot(t(kpi), show.legend=FALSE,do.hex=FALSE,axes=TRUE,show.values=FALSE) 
#color2D.matplot(t(kpi),c(1,0),c(0,1),c(0,0), show.legend=FALSE,do.hex=FALSE,axes=TRUE,show.values=FALSE) 


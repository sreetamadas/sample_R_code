## method 2 : plot X per ID, per month; as a matrix / heat map

## read data

IDList <-  as.character(sort(unique(df$ID)))    ## get unique IDs in categorical column

df$month <- format(as.POSIXct(df$Date, format="%d.%m.%Y"), format="%Y-%m")
monthList <- as.character(sort(unique(df$month)))

# declare matrix for saving data
production <- matrix(nrow=length(IDList), ncol=length(monthList))     # ncol=length(monthList)+ 1


for(i in 1:length(IDList)) {
  #total <- 0  ####
  for(j in 1:length(monthList)) {
    w_mc <- subset(df, df$ID == IDList[i] & df$month == monthList[j])
    
    production[i,j] <- sum(w_mc$X)   
    #/prod_total ## dividing gives high values only if most pcs for a model were produced on a single (or few) m/cs.
    #total <- total + production[i,j] ### 
    }
  #production[i,(length(monthList)+1)] <- total  ### extra column for overall data
}

rownames(production) <- IDList
colnames(production) <- monthList

library(corrplot)
corrplot(t(production),is.corr = FALSE, method='square')

library(RColorBrewer)
corrplot(t(production),is.corr = FALSE, method='color', col=brewer.pal(n=9, name='Blues'))  
## the top 25 are highlighted in the matrix


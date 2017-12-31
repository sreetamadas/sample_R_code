## plot X per ID, per month; as a matrix / heat map

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

###########################################################################################
### plotting daily data for a month as matrix

# find month, week and day of week for each entry
### day of week
mc$day <- as.POSIXlt(mc$Date)$wday
## week number
mc$week <- format(as.POSIXct(mc$Date, format='%Y-%m-%d'), format="%U")
## month
mc$month <- format(as.POSIXct(mc$Date, format='%Y-%m-%d'), format="%m")

## extract data for a selected month
selected_month <- 10

temp <- subset(mc, mc$month == selected_month)

# create list of week number & day-of-week in selected data
dowList <-  sort(unique(temp$day)) ## get unique IDs in machine column
weekList <- as.integer(sort(unique(temp$week)))

# generate matrix of data for the month
new_dat <- matrix(nrow=length(weekList), ncol=length(dowList))

for(i in 1:length(weekList) ) {
  for(j in 1:length(dowList) ) {
    t <- subset(temp, temp$week == weekList[i] & temp$day == dowList[j])
    
    if(nrow(t) == 0) {
      new_dat[i,j] <- -1
    }
    else {
      new_dat[i,j] <- t$Y
    }
  }
}


# plot as matrix
library(corrplot)
corrplot(energy,is.corr = FALSE, method='square')






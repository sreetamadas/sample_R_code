#### counting occurrences  ####
## method 1: count all factors together
new_df <- data.frame(as.character(levels(as.factor(df$ID))))
num_of_instances <- tapply(df$someVariable, df$ID, function(x) length(unique(x))) # count no. of occurrences of each ID

                           
## method 2: count all factors together
library(dplyr)
dev_occurrence <- new_df %>% 
          group_by(device) %>%
          summarise(counts = length(device))


### method 3: counts for individual factors (in wide format data, where there is separate col for each factor)
# count the no. of cells where value in 1                           
for(i in 1:5) {
  s <- sum(df[,i+1] == 1)
  print(paste(i,s,sep=' '))
}                           
                           
                           
###############################################################################
### method 4:
w_mc$Date <- format(as.POSIXct(w_mc$Date, format='%d.%m.%Y'), format="%Y-%m-%d")
w_mc$Time[w_mc$Shift == '1'] <- '04:00:00'
w_mc$Time[w_mc$Shift == '2'] <- '12:00:00'
w_mc$Time[w_mc$Shift == '3'] <- '20:00:00'
w_mc$DateTime <- as.POSIXct(paste(w_mc$Date, w_mc$Time), format="%Y-%m-%d %H:%M:%S")
w_mc$Time <- NULL
w_mc$DateShiftMc <- NULL
#w_mc$DateShift <- paste(w_mc$Date, w_mc$Shift)

##  step 2: find no. of models in each shift  ##
n_occur <- data.frame(table(w_mc$DateShift))
l <- rep(0, n_occur$Freq[1])

for (i in 2:nrow(n_occur)) {
  l <- c(l, rep(n_occur$Freq[i-1], n_occur$Freq[i]))
}
w_mc <- cbind(w_mc, l)

##  step 3: find model in previous shift  ##
m <- rep(0, n_occur$Freq[1])

for (i in (n_occur$Freq[1]+1):nrow(w_mc)) {
  if(w_mc$l[i] == 1) {m <- c(m, as.character(w_mc$ModelNo[i-1]))}
  else {m <- c(m, 2)}
}
w_mc <- cbind(w_mc, m)


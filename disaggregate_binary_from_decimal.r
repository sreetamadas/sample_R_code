# get states (binary) from decimal nos

library(dplyr)
setwd("C:/Users/Desktop/analysis/")

## data
fileLoc2 <- "C:/Users/Desktop/data/"  

## read event chart in decimal
A <- read.table(paste(fileLoc2,'A.dat',sep=''), skip=1)     # header=TRUE,
B <- read.table(paste(fileLoc2,'B.dat',sep=''), skip=1) 
C <- read.table(paste(fileLoc2,'C.dat',sep=''), skip=1)

## create combined df
df <- data.frame()
df <- as.data.frame(A)
colnames(df)[1] <- 'A'
df <- cbind(df,B,C)
colnames(df)[2] <- 'B'
colnames(df)[3] <- 'C'

### add time information
#time <- seq(as.POSIXct("2017-12-20 15:37:42", format="%Y-%m-%d %H:%M:%S"), by=0.5, length.out = 335544)  #  starttime: 21/12/2017_9:22:59
time <- seq(as.POSIXct("2017-12-21 09:22:59", format="%Y-%m-%d %H:%M:%S"), by=0.5, length.out = 335544)  #  starttime: 21/12/2017_9:22:59
#time <- seq(as.POSIXct("2017-12-23 09:35:44", format="%Y-%m-%d %H:%M:%S"), by=0.5, length.out = 335544)  #  starttime: 21/12/2017_9:22:59
df <- cbind(df,time)

## add row id
df$id  <- 1:nrow(df)


####################################################################################
## get the paired-list for all possible keys & the corresponding states
list_of_paired_keys <- levels(as.factor(paste(df$B,df$C,df$A,sep=' ')))

key_value_df <- data.frame(A=numeric(), # factor(),
                      B=numeric(),
                      C=numeric(),
                      D1=numeric(),
                      D2=numeric(),
                      D3=numeric(),
                      D4=numeric(),
                      D5=numeric(),
                      D6=numeric(),
                      D7=numeric(),
                      D8=numeric(),
                      D9=numeric(),
                      D10=numeric(),
                      D11=numeric(),
                      D12=numeric(),
                      D13=numeric(),
                      D14=numeric(),
                      D15=numeric(),
                      D16=numeric(),  
                      D17=numeric(), 
                      stringsAsFactors=FALSE
)
#key_value_df <- data.frame()

for (i in 1:length(list_of_paired_keys)) {
  tt <- unlist(strsplit(list_of_paired_keys[i], " "))  # saves the 3 keys as separate
  
  key1 <- intToBits(tt[1])  
  key2 <- intToBits(tt[2])    #print(paste(tt[1],' ',tt[2]))
  key3 <- intToBits(tt[3])  
  
  stateVec <- rep(0, 17)   # c(length=15)  initialize with a vector of zeroes with length 17
  
  for (j in 1:15) {
    if(key1[j] == 01) {stateVec[j] <- 1}   
    if(key2[j] == 01) {stateVec[j] <- 1}   
  }
  for(j in 1:2) {
    if(key3[j] == 01) {stateVec[15+j] <- 1}
  }
  
  key_value_df[i,]$A <- as.integer(tt[3])
  key_value_df[i,]$B <- as.integer(tt[1])    #stateVec[is.na(stateVec)] <- 0
  key_value_df[i,]$C <- as.integer(tt[2])
  key_value_df[i,]$D1 <- stateVec[1]
  key_value_df[i,]$D2 <- stateVec[2]
  key_value_df[i,]$D3 <- stateVec[3]
  key_value_df[i,]$D4 <- stateVec[4]
  key_value_df[i,]$D5 <- stateVec[5]
  key_value_df[i,]$D6 <- stateVec[6]
  key_value_df[i,]$D7 <- stateVec[7]
  key_value_df[i,]$D8 <- stateVec[8]
  key_value_df[i,]$D9 <- stateVec[9]
  key_value_df[i,]$D10 <- stateVec[10]
  key_value_df[i,]$D11 <- stateVec[11]
  key_value_df[i,]$D12 <- stateVec[12]
  key_value_df[i,]$D13 <- stateVec[13]
  key_value_df[i,]$D14 <- stateVec[14]
  key_value_df[i,]$D15 <- stateVec[15]
  key_value_df[i,]$D16 <- stateVec[16]
  key_value_df[i,]$D17 <- stateVec[17]
  
  #temp_vec <- as.list(as.integer(tt[1]), as.integer(tt[2]), stateVec)
  #key_value_df <- rbind(key_value_df, as.data.frame(temp_vec))
}


## add extra cols for states to original df
df <- merge(df, key_value_df, by=c('A','B','C'), all.x = TRUE)  
# this step is rearranging the rows in the df, so sort by id 


### sort the df by time
#df <- df[ order(df$time , decreasing = FALSE ),]
# sort df by row id, since the time stamps are repeated as they are 500ms apart
df <- df[order(df$id), ]


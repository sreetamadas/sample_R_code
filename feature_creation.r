### create features & push into a new df ###

# set path
setwd("C:/Users/Desktop/data/analysis/")

# read data
df <- read.csv()

# clean data - check & fix missing values & outliers
# subset, if required
# correct date & time formats; add day of week & time of day, if reqd


#https://stackoverflow.com/questions/10689055/create-an-empty-data-frame
feature <- data.frame(Machine=character(), # factor(),
                      datapts=integer(),
                      Date=as.Date(character()),
                      shift=integer(),
                      day=integer(),
                      TotalProductPcs=integer(),
                      kpi=numeric(),
                      time_off=numeric(),
                      time_L=numeric(),
                      time_M=numeric(),
                      time_H=numeric(),
                      time_vH=numeric(),
                      time_on=numeric(),
                      stringsAsFactors=FALSE
                      )
                      
flag <- 1

for (i in idList) {
  f <- paste('2MCT', substring(i, 6),'.xls', sep='')
  elec <- read_excel(paste(fileLoc2,fileloc1,f, sep =''))
  
  # format date Time in param file
  #elecc$DateTime <- elec$Date
  names(elec)[names(elec) == "Date"] <- "DateTime"
  #elec$Date <- format(as.POSIXct(elec$Date, format='%Y-%m-%d %H:%M:%S'), format="%Y-%m-%d")
  
  ## express as Fraction of rated
  rated <- 30 
  elec$Frac <- elec$k * 100/rated
  
  ##  factoring into power value bins
  #fac <- cut(mc$Frac, breaks=seq(0,140,by=10), labels=seq(5,135,by=10)) # 14 groups
  fac <- cut(elec$Frac, c(-100, 0, 30, 80, 100, 500),labels=c('0', 'L','M','H', 'vH')) # rename levels
  elec <- cbind(elec,fac)
  
  
  ## for each mc, get data for corrs. shift
  # check if data loss is there - data cleaning required ?
  w_mc <- subset(w_mc_all, w_mc_all$Machine == i)
  
  for (j in 1:nrow(w_mc)) {
    s <- data.frame()
    
    if(w_mc[j,]$Shift == 1) {
      start <- paste(w_mc[j,]$Date,'00:00:00', sep = ' ')
      end <- paste(w_mc[j,]$Date,'08:00:00', sep = ' ')
      
      s <- subset(elec, as.integer(as.POSIXct(elec$DateTime)) >= as.integer(as.POSIXct(start, tz = 'UTC')) 
                  & as.integer(as.POSIXct(elec$DateTime)) < as.integer(as.POSIXct(end, tz = 'UTC'))  )
    }
    if(w_mc[j,]$Shift == 2) {
      start <- paste(w_mc[j,]$Date,'08:00:00', sep = ' ')
      end <- paste(w_mc[j,]$Date,'16:00:00', sep = ' ')
      
      s <- subset(elec, as.integer(as.POSIXct(elec$DateTime)) >= as.integer(as.POSIXct(start, tz = 'UTC')) 
                  & as.integer(as.POSIXct(elec$DateTime)) < as.integer(as.POSIXct(end, tz = 'UTC'))  )
    }
    if(w_mc[j,]$Shift == 3) {
      start <- paste(w_mc[j,]$Date,'16:00:00', sep = ' ')
      end <- paste(w_mc[j,]$Date,'23:59:59', sep = ' ')
      
      s <- subset(elec, as.integer(as.POSIXct(elec$DateTime)) >= as.integer(as.POSIXct(start, tz = 'UTC')) 
                  & as.integer(as.POSIXct(elec$DateTime)) <= as.integer(as.POSIXct(end, tz = 'UTC'))  )
      
    }
    
    # feature set:  calculate parameters ; states: '0', 'L','M','H', 'vH'
    feature[flag,]$Machine <- as.character(w_mc[j,]$Machine)
    feature[flag,]$datapts <- nrow(s)
    feature[flag,]$Date <- w_mc[j,]$Date
    feature[flag,]$shift <- w_mc[j,]$Shift
    feature[flag,]$day <- w_mc[j,]$day
    feature[flag,]$TotalProductPcs <- w_mc[j,]$TotalProductPcs
    feature[flag,]$kpi <- w_mc[j,]$kpi
    feature[flag,]$time_off <- 100 * nrow(subset(s, s$fac == '0' ))/nrow(s)
    feature[flag,]$time_L <- 100 * nrow(subset(s, s$fac == 'L' ))/nrow(s)
    feature[flag,]$time_M <- 100 * nrow(subset(s, s$fac == 'M' ))/nrow(s)
    feature[flag,]$time_H <- 100 * nrow(subset(s, s$fac == 'H' ))/nrow(s)
    feature[flag,]$time_vH <- 100 * nrow(subset(s, s$fac == 'vH' ))/nrow(s)
    feature[flag,]$time_on <- feature[flag,]$time_L + feature[flag,]$time_M + feature[flag,]$time_H + feature[flag,]$time_vH
    
    ## update flag for next entry to feature set :
    flag <- flag + 1
    
  }

}

feature$time_0nL <- feature$time_off + feature$time_L
feature$time_MHvH <- feature$time_M + feature$time_H + feature$time_vH
feature$time_HvH <- feature$time_H + feature$time_vH



  
  
  df <- data.frame(subset$txtime, subset$Y)   
  df$Frac <- df$subset.Y * 100/thres

  ## time difference between successive events
  c_time <- as.POSIXlt(df$subset.txtime )
  timedel <- as.numeric(difftime(c_time[2:length(c_time)] , c_time[1:(length(c_time)-1)], tz = 'UTC')) #, units="mins")
  df <- df[-nrow(df),]   ## remove last row of data, corrs to which there is no time duration
  df <- cbind(df,timedel)  ## add the time differences column to the dataframe

  ## factoring into bins
  #fac2 <- cut(df$Frac, c(-10, 30, 80, 100, 150, 200, 500),labels=c('1','2','3','4', '5','6')) # rename levels
  #df <- cbind(df,fac2)
  #fac <- cut(df$subset.Y, c(-10, 20, 50, 80, 110, 140, 170, 200, 230, 260),labels=c('1','2','3','4', '5','6','7','8','9')) # rename levels
  #fac <- cut(df$subset.Y, c(-10,20,40,60,80,100,120,140,160,180,200,220,240,260),labels=c(1:13)) # rename levels
  fac <- cut(df$subset.Y, breaks=seq(-10,260,by=10), labels=seq(0,260,by=10)) #labels=c(1:27))
  df <- cbind(df,fac)
  
  ## calculate total time duration (per day) spent in a power bin
  tmp_df <- data.frame(levels(as.factor(df$fac)))
  colnames(tmp_df)[1] <- 'level'
  hourly_dur <- tapply(df$timedel, df$fac, FUN=sum)
  tmp_df <- cbind(tmp_df, hourly_dur)
  tmp_df$hourly_dur <- tmp_df$hourly_dur/60  # convert to min
  tmp_df$hourly_dur[is.na(tmp_df$hourly_dur)] <- 0 ## replace NA values with zero 
  

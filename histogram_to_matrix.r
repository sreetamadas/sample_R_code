## save histograms of daily data as matrix

  mat <- matrix(nrow=length(dateList), ncol=80)
  # dateList : list of dates
  
  for(i in 1:length(dateList) ) {   
    ss <- subset(df, df$date == dateList[i] )
    mat[i,] <- hist(ss$on, breaks=seq(0,400,by=5), plot=FALSE)$counts  # or, $density
    #print(paste(dateList[i],' '))
    #print(hist(ss$on, breaks=seq(0,80,by=5), plot=FALSE)$counts)
    
  }
  write.csv(mat, file=paste(path,'event_','_conti_ON_time_instances_hist.csv', sep='')) #, sep = "\n")
 

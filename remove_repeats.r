#### check no. of repeats  ####                          
library(plyr)
sel_dat <- join(sel_dat, count(sel_dat, "dateTime"))
             
                           
                           
## techniques to remove repeats
# for repeats, do either of i, ii or iii
#   i. single entry in each 1-min interval
sel_dat = sel_dat[with(sel_dat, c(ts[-1]!= ts[-nrow(sel_dat)], TRUE)),]
  
#  ii. unique entries (1 -> 0) & (0 -> 1) in each 1-min interval 
sel_dat$chk_col <- paste(as.numeric(as.POSIXct(paste(sel_dat$date, sel_dat$time, sep = ' '))), sel_dat$from, sel_dat$to, sep ='') 
sel_dat <- sel_dat[with(sel_dat, order(chk_col)), ] #sel_dat <- sel_dat[order(chk_col),]
sel_dat = sel_dat[with(sel_dat, c(chk_col[-1]!= chk_col[-nrow(sel_dat)], TRUE)),]
  
# iii. remove only adjacent duplicates
# for this, create a column combining date, from & to
sel_dat$chk_col <- paste(as.numeric(as.POSIXct(paste(sel_dat$date, sel_dat$time, sep = ' '))), sel_dat$from, sel_dat$to, sep ='') 
# remove adjacent duplicates
# https://stackoverflow.com/questions/27022057/removing-only-adjacent-duplicates-in-data-frame-in-r
sel_dat = sel_dat[with(sel_dat, c(chk_col[-1]!= chk_col[-nrow(sel_dat)], TRUE)),]
                           
                           
                           
### for multiple entries in a minute, reassign the time (seconds value)
#sel_dat$new[] <- 
sel_dat$new <- unlist(sapply(unique(sel_dat$dateTime), function(x) {
    freq <- sum(x == sel_dat$dateTime)
    as.character(seq(as.POSIXct(x), by = 60/freq, length.out = freq))  #(new, by=freq, length.out=freq)
  }))
         

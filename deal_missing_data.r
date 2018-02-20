# method 1:  fill values in cells with NA or INF
new_df$col_calc <- new_df$col1/new_df$col2
new_df$col_calc[is.na(new_df$col_calc)] <- -0.01
new_df$col_calc[new_df$col_calc==Inf] <- -0.01 


# method 2: filling with values from preceding or succeeding cells
#library(padr)  pad(a)
# https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
# https://stackoverflow.com/questions/40219973/insert-new-series-rows-based-on-time-stamp-in-r
# tidyr::fill
library(zoo)
sel_dat <- na.locf(sel_dat, fromLast = FALSE)  # for all entries, copy from previous value
sel_dat <- na.locf(sel_dat, fromLast = TRUE)  # for 1st entry, copy from next value

 

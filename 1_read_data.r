#install.packages("RCurl")
#library(RCurl)
#https://www.r-bloggers.com/getting-data-from-an-online-source/



## get sorted filelist in the path / location
# https://stackoverflow.com/questions/14496325/natural-sort-order-human-sort-order-in-r-list-files
library(gtools)
filepath <- "C:/Users/Desktop/data"
setwd(filepath)
details <- list.files(filepath)
details <- mixedsort(details, decreasing=FALSE)
# http://r.789695.n4.nabble.com/How-to-get-the-last-modified-time-of-a-file-from-R-td3772025.html
# https://stackoverflow.com/questions/13762224/how-to-sort-files-list-by-date



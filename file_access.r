## METHOD 1: getting file from a networked drive location

## read files from a networked drive
# https://stackoverflow.com/questions/39296526/reading-a-file-on-a-network-in-r
filepath <- "\\\\xyz.com\\Dir1\\sDir2\\sdir3\\Raw Data\\location\\"
dat <- read.csv(paste(path,"sample_data.csv", sep=''), header=FALSE)  # set header=FALSE if file has no header info


## METHOD 2: getting data from a location on the web
#install.packages("RCurl")
#library(RCurl)
#https://www.r-bloggers.com/getting-data-from-an-online-source/



## access all filenames in a location 9equivalent to 'ls' on linux)
details <- list.files(filepath)

## get sorted (by name) filelist in the path / location
# https://stackoverflow.com/questions/14496325/natural-sort-order-human-sort-order-in-r-list-files
library(gtools)
details <- list.files(filepath)
details <- mixedsort(details, decreasing=FALSE)
## to sort by date, see following links
# http://r.789695.n4.nabble.com/How-to-get-the-last-modified-time-of-a-file-from-R-td3772025.html
# https://stackoverflow.com/questions/13762224/how-to-sort-files-list-by-date




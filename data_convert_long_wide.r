# https://www.r-bloggers.com/reshape-and-aggregate-data-with-the-r-package-reshape2/
# https://www.r-bloggers.com/introducing-tidyr/
# https://www.r-bloggers.com/melt/
# https://rpubs.com/bradleyboehmke/data_wrangling
# spread in tidyr, how to use melt in R , how to use cast in reshape in r


## convert from long to wide
## sample data
# datetime    sno    to
# 1           101     1
# 1           102     1
# 1           103     0
# ....................

library(reshape2)
cast.dev <- dcast(combi, formula = dateTime ~ sno, value.var = "to")


## convert wide to long
## sample data
# date   ID    Q1   Q2   Q3   Q4
# 1      101   50   60   40   30
# 1      102   53   29   42   87
# 2 .....

library(reshape2)
long <- melt(df, id.vars = c("ID", "date"))  
# all columns in the table, which are not listed in above command, will be put into a single column in the long format

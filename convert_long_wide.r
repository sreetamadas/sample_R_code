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

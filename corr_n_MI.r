
pairs(df)
cor(df)


cor(df$X, df$Y)  

#install.packages("mpmi")  #https://cran.r-project.org/web/packages/mpmi/mpmi.pdf
library(mpmi)
cmi.pw(df$X, df$Y)         

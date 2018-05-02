## pearson's coeff: to compute correlation coeff b/w linearly related variables
# Kendall Tau or Spearman rank: to compute correlation b/w non-linearly related variables

pairs(df)
cor(df)


cor(df$X, df$Y)  

#install.packages("mpmi")  #https://cran.r-project.org/web/packages/mpmi/mpmi.pdf
library(mpmi)
cmi.pw(df$X, df$Y)         

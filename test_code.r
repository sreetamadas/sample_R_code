

#########################################
### check NA values

Are there any NA values in col?
> any(is.na(df$sel_col))
[1] TRUE

How many NA values in col?
> sum(is.na(df$sel_col))
[1] 1

Which index has NA value?
> which(is.na(df$sel_col))
[1] 146308







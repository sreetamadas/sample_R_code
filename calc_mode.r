# find mode

estimate_mode <- function(x) {
  if (length(x) > 2) {
  d <- density(x)
  d$x[which.max(d$y)]
  }
  else 0
}


X_mode <- estimate_mode(df$X) 


## find anomaly
#  input to time_decompose should be the reqd col from df

library(tidyverse)
library(anomalize)

# https://business-science.github.io/anomalize/
T %>%
  time_decompose(temp) %>% #, method = "stl", frequency = "auto", trend = "auto", message = TRUE) %>%    ## method = stl / twitter
  anomalize(remainder, method = "iqr") %>%    # method = iqr or gesd
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
  #plot_anomaly_decomposition() +
  #labs(title = "Decomposition of Anomalized temp.")


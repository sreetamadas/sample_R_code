## find anomaly
# this would work properly with data which are expected to be normally distributed, & not skewed
#  input to time_decompose should be the reqd col from df

library(tidyverse)
library(anomalize)

T <- as_tibble(data)
# https://business-science.github.io/anomalize/
T %>%
   # decompose into trend, seasonality & remainder
  time_decompose(temp) %>% #, method = "stl", frequency = "auto", trend = "auto", message = TRUE) %>%    ## method = stl / twitter

  # anomaly detection on the remainder component using IQR
  anomalize(remainder, method = "iqr") %>%    # method = iqr or gesd  ; default: alpha = 0.05, max_anoms = 0.2

  # create the lower and upper bounds around the “observed” values
  time_recompose() %>%

  # plot
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

  # visualize anomalies superposed on the STL decomposition
  #plot_anomaly_decomposition() +
  #labs(title = "Decomposition of Anomalized temp.")


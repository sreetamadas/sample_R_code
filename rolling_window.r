## calculate sd using points which are not anomalies ****
rolling_window <- 288

## calculating quartiles with T$Temp
q1 <- function(x) {quantile(x, na.rm=TRUE)[2]}
T$roll_q1 <- rollapply(data = T$temp,  # original series
                       width = rolling_window,  # width of the rolling window  = 12 * 24
                       FUN = q1, #na.rm = T,  # Any arbitrary function
                       fill = NA,
                       align='right')
q3 <- function(x) {quantile(x, na.rm=TRUE)[4]}
T$roll_q3 <- rollapply(data = T$temp,  # original series
                       width = rolling_window,  # width of the rolling window  = 12 * 24
                       FUN = q3, #na.rm = T,  # Any arbitrary function
                       fill = NA,
                       align='right')
T$anom <- ifelse(T$temp - T$roll_q3 > 3*(T$roll_q3 - T$roll_q1), 1, 0)  # 1.5

T$anom[is.na(T$anom)] <- 0
#T$anomaly6[is.na(T$anomaly6)] <- 0

T$col1[T$anom == 0] <- 'green'
T$col1[T$anom == 1] <- 'red'

#############################################
## calculate quartiles on T$temp -T$roll_median

T$roll_median <- rollapply(data = T$temp,  # original series
                           width = rolling_window,  # width of the rolling window  = 12 * 24
                           FUN = median, #na.rm = T,  # Any arbitrary function
                           fill = NA,
                           align='right')


T$roll_median <- na.locf(T$roll_median, fromLast = TRUE)

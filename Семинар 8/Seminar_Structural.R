library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(midasr)
library(strucchange)
library(quantmod)
library(rusquant)
library(dplyr)
library(tsbox)


### Structural breaks


# CUSUM type 

dailyData = getSymbols.Moex('SBER', period = "day", from = "2020-01-01")

sber_close <- xts(x = dailyData$close, 
                  order.by = as.Date(dailyData$timestamp))

sber_close <- ts_ts(sber_close)


CUSUM_result <- efp(sber_close ~ 1)
MOSUM_result <- efp(sber_close ~ 1, type = "Rec-MOSUM")

plot(CUSUM_result)
plot(MOSUM_result)

CUSUM_result$process <- ts_ts(xts(x = CUSUM_result$process, 
                                  order.by = as.Date(dailyData$timestamp)))
plot(CUSUM_result, ylim = c(-10, 10))



# Models

dailyData2 = getSymbols.Moex('MOEX', period = "day", from = "2020-01-01")
moex_close <- xts(x = dailyData2$close, 
                  order.by = as.Date(dailyData2$timestamp))
moex_close <- ts_ts(moex_close)

dailyData3 = getSymbols.Moex('VTBR', period = "day", from = "2020-01-01")
vtb_close <- xts(x = dailyData3$close, 
                  order.by = as.Date(dailyData3$timestamp))
vtb_close <- ts_ts(vtb_close)


ME_result <- efp(sber_close ~ vtb_close + moex_close, type = "ME")
plot(ME_result)
plot(ME_result, functional = NULL)


### F

F_result <- Fstats(sber_close ~ vtb_close + moex_close)
plot(F_result)


# Monitoring

df <- data.frame(sber_close, 
                 vtb_close, 
                 moex_close)

tmp <- df[1:500, ]

Monitor_result <- mefp(sber_close ~ vtb_close + moex_close, 
                       data = tmp,
                       type = "ME")

tmp <- df[1:600, ]
Monitor_result <- monitor(Monitor_result)
Monitor_result

plot(Monitor_result)




# Bai-Perron

bObj <- breakpoints(sber_close ~ vtb_close + moex_close, 
                    h = 0.05)

na.omit(summary(bObj)$breakpoints[13,])
dailyData3$timestamp[na.omit(summary(bObj)$breakpoints[13,])]

sber_close <- xts(x = dailyData$close, 
                  order.by = as.Date(dailyData$timestamp))
plot(sber_close)

shifts <- xts(rep("SHIFT", 13), 
              dailyData3$timestamp[na.omit(summary(bObj)$breakpoints[13,])])

addEventLines(shifts, col = "red")





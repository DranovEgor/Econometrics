library(forecast)
library(tseries)
library(rusquant)


gazp1 <- getSymbols.Moex("GAZP", from = "2024-10-01", period = "1min")
plot(gazp1$close)
plot(ts(gazp1$close))

m <- auto.arima(ts(gazp1$close))
summary(m)
plot(forecast(m, h = 100))


m2 <- arima(ts(gazp1$close), order = c(3, 1, 3))
summary(m2)
plot(forecast(m2, h = 100))

twoForecasts <- data.frame(auto = forecast(m, h = 100)$mean,
                           arima313 = forecast(m2, h = 100)$mean)
plot(x = twoForecasts$auto,
     y = twoForecasts$arima313)

checkresiduals(m2)

#Плохой пример
linTrend <- 1:100 + rnorm(100, sd = 10)
mMA <-arima(ts(linTrend), order = c(0, 0, 1))
checkresiduals(mMA)
plot(ts(linTrend))


####### FORECASTING

maxHorizon = 5
testLast = 10

errors_autoARIMA <- matrix(nrow = maxHorizon, ncol = testLast)
errors_autoARIMA -> errors_ARIMA111
errors_autoARIMA -> errors_Naive
errors_autoARIMA -> errors_ETS

for(k in 1:testLast){
  
  tmpData <- ts(gazp1$close[1:(nrow(gazp1) - k - maxHorizon + 1)])
  m_autoARIMA <- auto.arima(tmpData, allowdrift = F)
  m_arima111 <- arima(tmpData, order = c(1, 1, 1))
  m_ETS <- ets(tmpData, model = 'ZZZ')
  m_naive <- naive(tmpData)
  fact <- ts(gazp1$close[(nrow(gazp1) - k - maxHorizon + 2):(nrow(gazp1) - k + 1)])
  
  errors_autoARIMA[,k] <- c(fact) - c(forecast(m_autoARIMA, h = 5)$mean)
  errors_ARIMA111[,k] <- c(fact) - c(forecast(m_arima111, h = 5)$mean)
  errors_ETS[,k] <- c(fact) - c(forecast(m_ETS, h = 5)$mean)
  errors_Naive[,k] <- c(fact) - c(forecast(m_naive, h = 5)$mean)
  print(k)
}


apply(errors_ARIMA111, 1, function(x) mean(abs(x)))

rowMeans(abs(errors_autoARIMA))
rowMeans(abs(errors_ARIMA111))
rowMeans(abs(errors_ETS))
rowMeans(abs(errors_Naive))



library(forecast)
library(tseries)
library(rusquant)
library(quantmod)

gazp <- getSymbols.Moex("GAZP", from = "2024-09-01", period = "1day")
lkoh <- getSymbols.Moex("LKOH", from = "2024-09-01", period = "1day")

dd = data.frame(gazp = gazp$close,
                lk = lkoh$close)

m_ARIMAX <- arima(ts(dd$gazp), order = c(2, 1, 2), 
                  xreg = ts(dd$lk))

summary(m_ARIMAX)

newdata = data.frame(gazp = rep(133, 10), 
                     lk = c(6861:6870))

predict(m_ARIMAX, newxreg = newdata$lk)

################

library(lmtest)

grangertest(x = dd$gazp, y = dd$lk)

grangertest(x = dd$gazp, y = dd$lk, order = 5)


#########
install.packages('vars')



library(vars)

mVAR <- VAR(dd, p = 3)

summary(mVAR)
AIC(mVAR)
BIC(mVAR)

plot(irf(mVAR))
predict(mVAR)




























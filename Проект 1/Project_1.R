library(forecast)
library(tseries)
library(rusquant)

# Берем данные по стоимости акций группы ПИК
pik <- getSymbols.Moex("PIKK", from = "2021-01-01", to = "2021-09-26", period = "day")
pik_df = data.frame(pik = pik$close)
pik_date = data.frame(date = pik$timestamp, pik = pik$close)


# Строим график 
plot(ts(pik$close), col = 'blue')

# Считаем функции ACF, PACF от ряда и проверяем его стационарность с помощью ADF, KPSS
acf(ts(pik$close))
pacf(ts(pik$close))
adf.test(pik$close)
kpss.test(pik$close)


# Рассматриваем различные модели 
model_arima <- auto.arima(ts(pik$close))
model_arima_515 <- arima(ts(pik$close), order = c(0, 1, 5))
model_ar_1 <- arima(ts(pik$close), order = c(1, 0, 0))
model_ma_1 <- arima(ts(pik$close), order = c(0, 0, 1))

summary(model_arima_515)


model_arima <- auto.arima(ts(pik$close))
model_arima_515 <- arima(ts(pik$close), order = c(0, 1, 5))
model_ar_1 <- arima(ts(pik$close), order = c(1, 0, 0))
model_ma_1 <- arima(ts(pik$close), order = c(0, 0, 1))

summary(model_arima_515)


# Добавляем величину 'наличие льготной ипотеки'  
df_1 <- data.frame(is_exist = rep(1, 125))
df_2 <- data.frame(is_exist = rep(0, 63))
mort <- rbind(df_1, df_2) 


# Строим оба графика
plot(ts(pik$close), col = 'blue')
par(new=TRUE)
plot(ts(mort), col = 'red')


# Проверяем значимость по Гранжеру
#library(lmtest)
#grangertest(x = ts(mort) , y = ts(pik_df))

# Строим модель с учетом льготной ипотеки
model_arimax <- arima(ts(pik$close),order = c(5, 1, 5), xreg = ts(mort))
summary(model_arimax)

# Строим предсказание  
df_forecast <- data.frame(is_exist = rep(0, 10))
plot(forecast(model_arima_515, h = 10))


pik_test <- getSymbols.Moex("PIKK", from = "2024-01-01", to = "2024-10-06", period = "day")
pik_df_test = data.frame(pik = pik_test$close)

plot(ts(pik_test$close), col = 'red')
par(new=TRUE)
plot(forecast(model_arima_515, h = 10), col = 'blue')
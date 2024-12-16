library(forecast)
library(tseries)
library(openxlsx)
library(urca)
library(vars)

dd <- read.xlsx("/Users/egordranovmacbook/Desktop/Main/Programms/Эконометрика/Семинар 4/Data_ts.xlsx")
View(dd)

PMI <- dd$PMI_USA
MSCI <- dd$MSCI_USA
IntRate <- dd$IntRate_USA
Brent <- dd$Brent

#Проверим стационарнотсть 
adf.test(PMI)
adf.test(MSCI)
adf.test(IntRate)
adf.test(Brent)

PMI <- exp(diff(log(PMI))) - 1
PMI[2:length(PMI)] / PMI[1:length(PMI) - 1] - 1

MSCI <- exp(diff(log(MSCI))) - 1
IntRate <- diff(IntRate)
Brent <- exp(diff(log(Brent))) - 1

adf.test(PMI)
adf.test(MSCI)
adf.test(IntRate)
adf.test(Brent)
kpss.test(IntRate)



dd_for_VAR <- data.frame(Brent = Brent,
                         MSCI = MSCI,
                         IntRate = IntRate,
                         PMI = PMI)

m <- VAR(dd_for_VAR, p = 1) # p - число лагов
BIC(m) # Подбираем p по BIC

plot(irf(m)) # Для переключения графика нажать Enter
plot(irf(m, impulse = "Brent", response = "PMI"))


plot(fevd(m)) # Еще олин способ, кроме irf



#################################################


PMI <- dd$PMI_USA
MSCI <- dd$MSCI_USA
IntRate <- dd$IntRate_USA
Brent <- dd$Brent


dd_for_VAR <- data.frame(Brent = Brent,
                         MSCI = MSCI,
                         IntRate = IntRate,
                         PMI = PMI)

m <- lm(MSCI ~ ., data = dd_for_VAR)
adf.test(ts(m$residuals)) # Коинтеграции нет, так как не отвергается гипотеза



dd_for_VAR <- data.frame(Brent = Brent,
                         MSCI = MSCI,
                         PMI = PMI)

m <- lm(MSCI ~ ., data = dd_for_VAR)
adf.test(ts(m$residuals))

J_obj <- ca.jo(dd_for_VAR) # Тест Johansen 
summary(J_obj) # Есть коинтаграционное соотношение, значит нужен VECM


m <- vec2var(J_obj)
plot(irf(m))
plot(irf(m, impulse = "Brent", response = "PMI"))
# Есть еще кумулятивные IRF

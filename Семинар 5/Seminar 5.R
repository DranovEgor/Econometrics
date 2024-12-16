install.packages('BVAR')


library(forecast)
library(tseries)
library(openxlsx)
library(urca)
library(vars)
library(BVAR)

dd <- read.xlsx("/Users/egordranovmacbook/Desktop/Main/Programms/Эконометрика/Семинар 4/Data_ts.xlsx")
View(dd)

PMI <- dd$PMI_USA
MSCI <- dd$MSCI_USA
IntRate <- dd$IntRate_USA
Brent <- dd$Brent

PMI <- exp(diff(log(PMI))) - 1
MSCI <- exp(diff(log(MSCI))) - 1
IntRate <- diff(IntRate)
Brent <- exp(diff(log(Brent))) - 1

dd_for_VAR <- data.frame(Brent = Brent,
                         MSCI = MSCI,
                         IntRate = IntRate,
                         PMI = PMI)

A_matrix <- diag(4)
A_matrix[1, 2] <- NA #Influence of MSCI on Brent
A_matrix[2:4, 1] <- NA #Brent influence on everything
A_matrix[4, 2] <- NA #MSCI influence PMI
A_matrix[4, 3] <- NA #Rate influence PMI

m1 <- VAR(dd_for_VAR, p = 2)
m2 <- SVAR(m1, estemethod = 'direct', Amat = A_matrix)
m2

plot(vars::irf(m2))
plot(vars::irf(m2, impulse = 'Brent', response = 'PMI'))



B_matrix <- diag(4)
B_matrix[2:4, 1] <- NA
B_matrix[4, 2:3] <- NA
B_matrix[2, 3] <- NA


m3 <- SVAR(m1, estimhod = 'direct',
           Bmat = B_matrix)
m3

plot(vars::irf(m3, impulse = 'Brent', response = 'PMI'))


###################
###### BVAR #######
###################

min_prior <- bv_minnesota(
  lambda = bv_lambda(mode = 0.5, sd = 2),
  alpha = bv_alpha(mode = 1, sd = 1),
  psi = bv_psi(scale = 0.04, shape = 0.04)
)


prior_Obj <- bv_priors(mn = min_prior) 


m <- bvar(data = dd_for_VAR, lags = 4,
          priors = prior_Obj,
          n_thin = 10,
          n_draw = 20000,
          n_burn = 10000) 
summary(m)

hist(m$beta[,2,1]) #Histogram of coefficient 2, 1

plot(irf(m))


pred <- predict(m, h = 3)
str(pred)
brentPredictions <- pred$fcast[,,1]
dim(brentPredictions)
medianForecast <- apply(brentPredictions, 2, median)







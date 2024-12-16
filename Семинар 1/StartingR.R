x = rnorm(100, mean = 15, sd = 3)
x = ts(x, start = c(2000, 1), frequency = 12)
x
plot(x)
acf(x)
pacf(x)


for (k in 3:100) {
  x[k] =  x[k - 1] + + rnorm(1, mean = 0, sd =1)
}

x = ts(x, start = c(2000, 1), frequency = 12)
plot(x)


aa <- list(a1 = c(1:5),
           a2 = "kjvokv",
           a3 = mean)

aa$a2
aa[[2]]

#Функция может возвращать функцию
bb = function(x){
  cc = function(y){
    x^y
  }
  return(cc)
}

bb(5)(2)




x <- arima.sim(model = list(ar = 0.1,
                            ma = runif(33, min = -0.5, max = 0.5),
                            order = c(1, 1, 33)),
               rand.gen = function(n) rnorm(n, 0, 1),
               n = 100)
plot(x)

adf.test(x)
kpss.test(x)
acf(x)
pacf(x)
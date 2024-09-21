#stationary and non  stationary tiem series
n = 100
t1 = rnorm(n,0,1)
t2 = cumsum(t1)
z = t1 +0.5
t3 = cumsum(z)
plot.ts(t1,ylim=c(-15,45),ylab = "stationay and non stationary")
lines(t2,lty=6,col =5)
lines(t3,col =3)

# acf plot
n = 100 
t1 = rnorm(n,0,1)
t2 = cumsum(t1)
z = t1 + 0.5
t3 = cumsum(z)
acf(t1,type = "correlation",plot = T)
acf(t2,type = "correlation",plot = T)
acf(t3,type = "correlation",plot = T)

#ACF plot for red wines
library(itsmr)
plotc(wine)
acf(wine,type = "correlation" , plot = T)

#MA(q)
n = 500
ma1 = arima.sim(list(order= c(0,0,1),ma = 0.7),n)
ts.plot(ma1)
acf(ma1,type = "correlation",plot = T)
pacf(ma1)
ma2 = arima.sim(list(order = c(0,0,2),ma = c(-1.3,0.8)),n)
ts.plot(ma2)
acf(ma2,type = "correlation",plot = T)
pacf(ma2)

#AR(q)
n = 500
ar1 = arima.sim(list(order = c(1,0,0), ar = 0.9),n)
ts.plot(ar1)
acf(ar1, type = "correlation",plot = T)
pacf(ar1)
ar2 = arima.sim(list(order = c(2,0,0),ar = c(0.8,-0.2)),n)
ts.plot(ar2)
acf(ar2, type = "correlation",plot = T)
pacf(ar2)

#ARMA(p,q)
n = 100
p = 1
d = 0
q = 1
arma1 = arima.sim(list(order = c(p,d,q),ar = 0.3,ma = 0.8),n)
ts.plot(arma1)
acf(arma1,type = "correlation",plot = T)
pacf(arma1)

#arma(2,2)
n = 100
p = 2
d = 0
q = 2
arma2 = arima.sim(list(order = c(p,d,q),ma = c(0.5,0.7),ar = c(0.6,0.2)),n)
ts.plot(arma1)
acf(arma2,type = "correlation",plot = T)
pacf(arma2)













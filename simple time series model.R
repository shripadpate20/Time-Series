#white noise
wn = arima.sim(model=list(order=c(0,0,0)),n=200)
plot.ts(wn,col=3,main="white noise series")

#simple random walk
set.seed(123)
tt = 100
xx = ww = rnorm(n = tt,mean = 0,sd = 1)
for(t in 2:100){
      xx[t] = xx[t-1] +  ww[t]
}
par(mfrow = c(1,2))
plot.ts(xx,ylab = expression(italic(x[t])))

#Trend
set.seed(123)
n = 100
y = rnorm(n,0,1)
xy = y
xt = 0.1*(1:n) +y
plot.ts(xy , ylim=c(-5,35),ylab="trend")
lines(xt)
lines(0.1*(1:n),col=2,lty="dashed")

#seasonality
set.seed(12)
n = 100
yt = rnorm(n,mean = 0,sd = 1)
xtr = 0.2*(1:n)
xt = 0.2*(1:n) + 2*sin(2.5*(1:n)*pi) +yt
plot.ts(xt,ylim=c(-10,33),ylab = "Seasonality")
lines(xtr,col=2)
lines(2*sin(2.5*(1:n)*pi),col = 3)

#smoothing with a finite moving avg 
install.packages("fpp2")
library(fpp2)
ma(elecsales,5)
autoplot(elecsales,series = "data") + autolayer(ma(elecsales,5),series = "5-MA")

#simple exponential smoothing
library(fpp2)
fc = ses(elecsales,alpha = 0.8)
autoplot(elecsales,series = "data") + autolayer(fitted(fc),series= "SEC")

#additive model
library(fpp2)
autoplot(a10)
plot(decompose(a10,type = "additive"))

#multiplicative model
library(fpp2)
autoplot(euretail)
plot(decompose(euretail,type = "multiplicative"))

#ACF and PACF
install.packages("itsmr")
library(itsmr)
plotc(wine)
M = c("log","season",12,"trend",1)
e = Resid(wine,M)
test(e)
install.packages("feasts")
library(feasts)
e = Resid(wine,M)
box_pierce(e)
















library(fpp)
############################################# 
# Simple exponential smoothing
oildata <- window(oil, start = 1996, end = 2007)
plot(oildata, ylab = "Oil (millions of tonnes)", xlab = "Year") 
fit1 <- ses(oildata, alpha = 0.2, initial = "simple", h = 3) 
fit2 <- ses(oildata, alpha = 0.6, initial = "simple", h = 3) 
fit3 <- ses(oildata, h = 3)

plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)", xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), c("data", expression(alpha == 0.2), expression(alpha == 0.6),expression(alpha == 0.89)),pch=1)

############################################## 
#Forecasting of data with a trend
air <- window(ausair, start = 1990, end = 2004)
fit1 <- holt(air, alpha = 0.8, beta = 0.2, initial = "simple", h = 5)
fit2 <- holt(air, alpha = 0.8, beta = 0.2, initial = "simple", exponential = TRUE, h = 5) 
fit3 <- holt(air, alpha = 0.8, beta = 0.2, damped = TRUE, initial = "simple", h = 5)

plot(fit2, type = "o", ylab = "Air passengers in Australia (millions)", xlab = "Year", fcol = "white", plot.conf = FALSE)
lines(fitted(fit1), col = "blue")
lines(fitted(fit2), col = "red")
lines(fitted(fit3), col = "green")
lines(fit1$mean, col = "blue", type = "o")
lines(fit2$mean, col = "red", type = "o")
lines(fit3$mean, col = "green", type = "o")
legend("topleft", lty = 1, col = c("black", "blue", "red", "green"),
       legend=c("Data", "Holt's linear trend", "Exponential trend", "Additive damped trend"),cex=0.5)

########################################### 
# Comparing the forecasting performance

livestock2 <- window(livestock, start = 1970, end = 2000) 
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2,exponential = TRUE)
fit4 <- holt(livestock2, damped = TRUE)
fit5 <- holt(livestock2,exponential = TRUE, damped = TRUE)

fit1$model
accuracy(fit1) # training set 
accuracy(fit1,livestock) # test set 
fit1$model$par

plot(fit3, type="o", ylab="Livestock, sheep in Asia (millions)", flwd=1, plot.conf=FALSE)
lines(window(livestock,start=2001),type="o") 
lines(fit1$mean,col=2) 
lines(fit2$mean,col=3) 
lines(fit4$mean,col=5) 
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential","Additive Damped","Multiplicative Damped"),cex=0.5)

########################################### 
#Holt-Winters seasonal method
# International tourist visitor nights in Australia 
aust <- window(austourists, start=2005)
plot(aust)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
plot(fit2,ylab="International visitor night in Australia (millions)", plot.conf=FALSE,type="o",fcol="white",xlab="Year")
lines(fitted(fit1),col="red",lty=2)
lines(fitted(fit2),col="green",lty=2)
lines(fit1$mean,type="o",col="red")
lines(fit2$mean,type="o",col="green")
legend("topleft",lty=1,pch=1,col=c(1,2,3),
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"),cex=0.5)

########################################### 
#Forecasting with ETS models

oildata <- window(oil, start = 1996, end = 2007)
fit <- ets(oildata, model = "ANN")
plot(forecast(fit, h=3), ylab="Oil (millions of tones)") 
summary(fit)
ls(fit) 

#list names of the objects in the specified environment 
fit$par
vndata <- window(austourists, start = 2005) 
fit <- ets(vndata)
summary(fit)
plot(fit)
plot(forecast(fit, h = 8), ylab = "International visitor night in Australia (millions)")


?ets
############################################ 
#Stationarity and differencing
#Can you identify which series are stationary? b, f and g
par(mfrow = c(3, 3))
plot(dj, main = "(a)", xlab = "Day") 
plot(diff(dj), main = "(b)", xlab = "Day") 
plot(strikes, main = "(c)", xlab = "Year") 
plot(hsales, main = "(d)", xlab = "Year") 
plot(eggs, main = "(e)", xlab = "Year") 
plot(pigs, main = "(f)", xlab = "Year") 
plot(lynx, main = "(g)", xlab = "Year") 
plot(beer, main = "(h)", xlab = "Year") 
plot(elec, main = "(i)", xlab = "Year")

# The ACF of the Dow-Jones index and the daily changes in the Dow-Jones index 
par(mfrow = c(1, 2))
Acf(dj)
Acf(diff(dj))

# The Ljung-Box test for examining the null hypothesis of independence based on the 10th lag autocorrelation coefficient 
Box.test(diff(dj), type = "L", lag = 10)

# Plot the seasonal difference of the logarithm of the monthly scripts for a10 
par(mfrow=c(3,1))
plot(a10,xlab="Year")
plot(log(a10),xlab="Year")
plot(diff(log(a10),12), xlab="Year",ylab="Annual change in monthly log A10 sales")

# Plot the US net electricity generation, the log of it, t
# the seasonal difference and then apply a first difference on the data 
par(mfrow=c(2,2))
plot(usmelec)
plot(log(usmelec))
plot(diff(log(usmelec),12))
plot(diff(diff(log(usmelec),12),1))

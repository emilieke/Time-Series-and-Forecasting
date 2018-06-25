library(fpp);
###################################
#auto.arima()
plot(usconsumption[, 1], xlab = "Year", ylab = "Quarterly percentage change", 
     main = "US consumption") 
auto.arima(usconsumption[,1], seasonal=FALSE)
fit <- auto.arima(usconsumption[,1], seasonal=FALSE)
plot(forecast(fit, h=5))

################################# 
#acf and pacf

par(mfrow=c(1,2)) 
Acf(usconsumption[,1],main="") 
Pacf(usconsumption[,1],main="")

################################# 
#applying the procedure

#plot the data
eeadj <- seasadj(stl(elecequip, s.window="periodic"))
plot(eeadj, ylab = "Seasonally adjusted new orders index", xlab = "Year")

#no need for Box-Cox transformation to stabilize the data 
#data are not stationary, so we try differencing 
tsdisplay(diff(eeadj), main="")

#use unit-root test to make sure that the data are stationary 
adf.test(diff(eeadj),alternative = "stationary")
kpss.test(diff(eeadj))

#find the appropriate number of fist differences required
#ATTENSION, the following code only applies to non-seasonal time series 
#nd will hold the degree of first differencing
#xstar will hold the appropriately differenced time series
ns <- nsdiffs(eeadj)
if (ns>0) {
  xstar <- diff(eeadj, lag=frequency(eeadj),differences=ns) }else{
    xstar <- eeadj
  }
nd <- ndiffs(xstar)
if(nd > 0){
  xstar <- diff(xstar,differences = nd) }

#after understanding that fist difference of the data is all we need #we analyze again the ACF and PACF data
tsdisplay(diff(eeadj), main="")

#the ARIMA(3,1,0) is indicated from the PACF #trying different variations
Arima(eeadj, c(3, 1, 0))
Arima(eeadj, c(3, 1, 1))
Arima(eeadj, c(4, 1, 0))
Arima(eeadj, c(2, 1, 0))

#the ARIMA(3,1,1) has slightly smaller AIC_c 
fit <- Arima(eeadj, order=c(3,1,1)) 
summary(fit)

#checking the residuals for autocorrelation and doing a portmanteau test 
Acf(residuals(fit))
Box.test(residuals(fit), lag=24, fitdf=4, type="Ljung") # checks for autocorrelation

#ploting the forecast
plot(forecast(fit))

#using the auto.arima() 
auto.arima(seasadj(stl(elecequip, s.window="periodic")))

################################################### 
# Applying the procedure to a seasonal time series

#plotting the data for the quarterly European retail trade from 1996 to 2011 
plot(euretail, ylab="Retail index", xlab="Year")
#step1 
tsdisplay(diff(euretail,4)) # 4 because quartely daata
#step2 
tsdisplay(diff(diff(euretail,4)))
#step3
fit <- Arima(euretail, order=c(0,1,1), seasonal=c(0,1,1));fit
# 1 because significance in spike 4 and because we have performed one differentiation
#step4 
tsdisplay(residuals(fit))
# there might be some hidden information in the trend, because the spikes in ACD and PACF indicate that 
# that the model is not stationary

#step5
fit2 <- Arima(euretail, order = c(0, 1, 2), seasonal = c(0, 1, 1));fit2
fit3 <- Arima(euretail, order = c(0, 1, 3), seasonal = c(0, 1, 1));fit3
fit4 <- Arima(euretail, order = c(1, 1, 1), seasonal = c(0, 1, 1));fit4
#step6
# fit3 the best with the smallest AIC_c
Acf(res)
res <- residuals(fit3)
tsdisplay(res, main = "")
Box.test(res, lag=16, fitdf=4, type="Ljung")
?Box.test
# This p-value is stronger 
#step7
plot(forecast(fit3, h=12))

# from the plot we see that this method looks more sophisticated than the simple models
# it seems to follow the seasonality and trend

#step8 
auto.arima(euretail)

# the autoarime is not performing as well as us

#step9
auto.arima(euretail, stepwise=FALSE, approximation=FALSE) #turning the short-cuts off
# setting this restrictions makes the model run slower but creates a better model

########################################################## 
#take a look at it too

lh02 <- log(h02)
par(mfrow=c(2,1))
plot(h02, ylab="H02 sales (million scripts)", xlab="Year") plot(lh02, ylab="Log H02 sales", xlab="Year")
tsdisplay(diff(lh02,12), main="Seasonally differenced H02 scripts", xlab="Year")
auto.arima(h02, lambda = 0)
Arima(h02, order = c(3, 0, 0), seasonal = c(2, 1, 0), lambda = 0) 
Arima(h02, order = c(3, 0, 1), seasonal = c(2, 1, 0), lambda = 0) 
Arima(h02, order = c(3, 0, 2), seasonal = c(2, 1, 0), lambda = 0) 
Arima(h02, order = c(3, 0, 1), seasonal = c(1, 1, 0), lambda = 0) 
Arima(h02, order = c(3, 0, 1), seasonal = c(0, 1, 1), lambda = 0) 
Arima(h02, order = c(3, 0, 1), seasonal = c(0, 1, 2), lambda = 0) 
Arima(h02, order = c(3, 0, 1), seasonal = c(1, 1, 1), lambda = 0)
fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)

tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=6, type="Ljung")
fit <- auto.arima(h02, lambda=0, d=0, D=1, max.order=9, stepwise=FALSE, approximation=FALSE)
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=8, type="Ljung")
getrmse <- function(x,h,...) {
  train.end <- time(x)[length(x)-h] 
  test.start <- time(x)[length(x)-h+1] 
  train <- window(x,end=train.end) 
  test <- window(x,start=test.start) 
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"]) 
}

getrmse(h02,h=24,order=c(3,0,0),seasonal=c(2,1,0),lambda=0) 
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(2,1,0),lambda=0) 
getrmse(h02,h=24,order=c(3,0,2),seasonal=c(2,1,0),lambda=0) 
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(1,1,0),lambda=0) 
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(0,1,1),lambda=0) 
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(0,1,2),lambda=0) #best
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(1,1,1),lambda=0) 
getrmse(h02,h=24,order=c(4,0,3),seasonal=c(0,1,1),lambda=0) 
getrmse(h02,h=24,order=c(3,0,3),seasonal=c(0,1,1),lambda=0) 
getrmse(h02,h=24,order=c(4,0,2),seasonal=c(0,1,1),lambda=0) 
getrmse(h02,h=24,order=c(3,0,2),seasonal=c(0,1,1),lambda=0) 
getrmse(h02,h=24,order=c(2,1,3),seasonal=c(0,1,1),lambda=0) 
getrmse(h02,h=24,order=c(2,1,4),seasonal=c(0,1,1),lambda=0) 
getrmse(h02,h=24,order=c(2,1,5),seasonal=c(0,1,1),lambda=0)
fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0) 
plot(forecast(fit), ylab="H02 sales (million scripts)", xlab="Year")


# Example 1

?mcopper
#plotting the data for the quarterly European retail trade from 1996 to 2011 
plot(mcopper)

# large variance in end of time serier --> need to transform the data

# the Box-Cox transformation
lambda <- BoxCox.lambda(mcopper)
plot(BoxCox(mcopper,lambda))

#step1 
tsdisplay(diff(mcopper,12))
#step2 
tsdisplay(diff(diff(mcopper,12)))
#step3
fit <- Arima(mcopper, order=c(0,1,1),lambda=lambda);fit
# 1 because significance in spike 4 and because we have performed one differentiation
#step4 
tsdisplay(residuals(fit))
# there might be some hidden information in the trend, because the spikes in ACD and PACF indicate that 
# that the model is not stationary

#step5
fit2 <- Arima(euretail, order = c(0, 1, 2), seasonal = c(0, 1, 1));fit2
fit3 <- Arima(euretail, order = c(0, 1, 3), seasonal = c(0, 1, 1));fit3
fit4 <- Arima(euretail, order = c(1, 1, 1), seasonal = c(0, 1, 1));fit4
#step6
# fit3 the best with the smallest AIC_c
res <- residuals(fit3)
tsdisplay(res, main = "")
Box.test(res, lag=16, fitdf=4, type="Ljung")

# This p-value is stronger 
#step7
plot(forecast(fit3, h=12))

# from the plot we see that this method looks more sophisticated than the simple models
# it seems to follow the seasonality and trend

#step8 
auto.arima(mcopper,lambda=lambda)


# the autoarime is not performing as well as us

#step9
auto.arima(euretail, stepwise=FALSE, approximation=FALSE) #turning the short-cuts off
# setting this restrictions makes the model run slower but creates a better model

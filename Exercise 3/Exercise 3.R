# ----------------------------------------------------------------------------------------------------
# PROBLEM SET 3
# Emilie Engen, 100356077

# ----------------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Time Series and Forecasting/Exercise 3")

# ----------------------------------------------------------------------------------------------------
# Install packages
install.packages('fpp')

# Include libraries
library(fpp)

# ----------------------------------------------------------------------------------------------------
# EXERCISE 1A
# ----------------------------------------------------------------------------------------------------
# For the following series, find an appropriate Box-Cox transformation and order of differencing 
# in order to obtain stationary data. 

# Investigate the dataset
?enplanements

# Load dataset into memory
data(enplanements)

# Type of object
is(enplanements)

# Dimension
dim(enplanements)

# Have a look a it
head(enplanements)

# Summary
summary(enplanements)

# ----------------------------------------------------------------------------------------------------
# Step 1: Plot the data. Identify unusual observations. Understand patterns
dev.off()
# Plot time series
plot(enplanements,main='',ylab='Revenue ($ Million)',xlab='Year')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Seasonal plot
seasonplot(enplanements,main='',ylab='Revenue ($ Million)',xlab='Month',year.labels=TRUE, 
           year.labels.left=TRUE,col=1:7, pch=19)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Month plot
monthplot(enplanements,main='',ylab='Revenue ($ Million)',xlab='Month')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# Step 2: If necessary, use a Box-Cox transformation to stabilize the variance

# The variance is increasing towards the end of the time series, therefore a Box-Cox transformation 
# is necessary

# the Box-Cox transformation
lambda <- BoxCox.lambda(enplanements)
bc_enplanements <- BoxCox(enplanements,lambda)

plot(bc_enplanements,ylab='Revenue ($ Million)')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

tsdisplay(bc_enplanements,main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# Step 3: If necessary, difference the data until it appears stationary. Use unit-root tests
# if you are unsure

# The data is not stationary due to the seasonal pattern. 
# Differencing with lag 12 is required
tsdisplay(diff(bc_enplanements,12),main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Unit-root test
adf.test(diff(bc_enplanements,12), alternative = "stationary")

# There is still a lot of autocorrelation left in the data. 
# ACF and PACF graphs suggest one more differencing.
tsdisplay(diff(diff(bc_enplanements,12)),main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Unit-root test
adf.test(diff(diff(bc_enplanements,12)), alternative = "stationary")

# ----------------------------------------------------------------------------------------------------
# EXERCISE 1B
# ----------------------------------------------------------------------------------------------------
# For the following series, find an appropriate Box-Cox transformation and order of differencing 
# in order to obtain stationary data. 

# Investigate the dataset
?visitors

# Load dataset into memory
data(visitors)

# Type of object
is(visitors)

# Dimension
dim(visitors)

# Have a look a it
head(visitors)

# Summary
summary(visitors)

# ----------------------------------------------------------------------------------------------------
# Step 1: Plot the data. Identify unusual observations. Understand patterns

# Plot time series
plot(visitors,main='',ylab='Number of visitors (Thousands)',xlab='Year')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Seasonal plot
seasonplot(visitors,main='',ylab='Number of visitors (Thousands)',xlab='Month',year.labels=TRUE, 
           year.labels.left=TRUE,col=1:7, pch=19)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Month plot
monthplot(visitors,main='',ylab='Number of visitors (Thousands)',xlab='Month')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# Step 2: If necessary, use a Box-Cox transformation to stabilize the variance

# The variance is increasing towards the end of the time series, therefore a Box-Cox transformation 
# is necessary
tsdisplay(visitors,main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# the Box-Cox transformation
lambda <- BoxCox.lambda(visitors)
bc_visitors <- BoxCox(visitors,lambda)

plot(bc_visitors,ylab='Number of visitors (Thousands)')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

tsdisplay(bc_visitors,main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# Step 3: If necessary, difference the data until it appears stationary. Use unit-root tests
# if you are unsure

# The data is not stationary due to the seasonal pattern. 

# Differencing with lag 12 is required
tsdisplay(diff(bc_visitors,12),main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

adf.test(diff(bc_visitors,12), alternative = "stationary")

# There is still a lot of autocorrelation left in the data. 
# ACF and PACF graphs suggest one more differencing.
tsdisplay(diff(diff(bc_visitors,12)),main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

adf.test(diff(diff(bc_visitors,12)), alternative = "stationary")


# ----------------------------------------------------------------------------------------------------
# EXERCISE 2
# ----------------------------------------------------------------------------------------------------

# Investigate the dataset
?hsales

# Load dataset into memory
data(hsales)

# Type of object
is(hsales)

# Dimension
dim(hsales)

# Have a look a it
head(hsales)

# Summary
summary(hsales)

# ----------------------------------------------------------------------------------------------------
# Step 1: Plot the data. Identify unusual observations. Understand patterns

# Plot time series
plot(hsales,main='',ylab='House Sales',xlab='Year')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

plot(stl(hsales,'periodic'),main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (a) Step 2: If necessary, use a Box-Cox transformation to stabilize the variance

lambda <- BoxCox.lambda(hsales)
bc_hsales <- BoxCox(hsales,lambda)

plot(log(hsales))
# The data does not need any transformation

# ----------------------------------------------------------------------------------------------------
# (b) Step 3: If necessary, difference the data until it appears stationary. Use unit-root tests
# if you are unsure

# The data is not stationary due to the seasonal pattern. 

# First order differencing
tsdisplay(diff(hsales,12),main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Unit-root test
adf.test(diff(hsales,12), alternative = "stationary")

# There is still a lot of autocorrelation left in the data.
# ACF and PACF graphs suggest one more differencing.

# Second order differencing
tsdisplay(diff(diff(hsales,12)),main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Unit-root test
adf.test(diff(diff(hsales,12)), alternative = "stationary")

# ----------------------------------------------------------------------------------------------------
# (c) Step 4: Plot ACF/PACF of differenced data and try to determine different candidate models

# ----------------------------------------------------------------------------------------------------
# (c) Step 5: Try your chosen models and use AIC(c) to search for a better model
fit1  <- Arima(hsales, order = c(0, 1, 2), seasonal = c(0, 1, 1))
fit2  <- Arima(hsales, order = c(0, 1, 1), seasonal = c(0, 1, 1))
fit3  <- Arima(hsales, order = c(0, 1, 3), seasonal = c(0, 1, 1))
fit4  <- Arima(hsales, order = c(1, 1, 2), seasonal = c(0, 1, 1))
fit5  <- Arima(hsales, order = c(1, 1, 1), seasonal = c(0, 1, 1))
fit6  <- Arima(hsales, order = c(1, 1, 3), seasonal = c(0, 1, 1))
fit7  <- Arima(hsales, order = c(4, 1, 0), seasonal = c(0, 1, 1))
fit8  <- Arima(hsales, order = c(3, 1, 0), seasonal = c(0, 1, 1))
fit9  <- Arima(hsales, order = c(5, 1, 0), seasonal = c(0, 1, 1))
fit10 <- Arima(hsales, order = c(4, 1, 1), seasonal = c(0, 1, 1))
fit11 <- Arima(hsales, order = c(3, 1, 1), seasonal = c(0, 1, 1))
fit12 <- Arima(hsales, order = c(5, 1, 1), seasonal = c(0, 1, 1))

# Comparing the AIC_c of the different models
c(fit1$aicc, fit2$aicc, fit3$aicc, fit4$aicc, fit5$aicc, fit6$aicc, fit7$aicc, fit8$aicc, 
  fit9$aicc, fit10$aicc, fit11$aicc, fit12$aicc)

# ----------------------------------------------------------------------------------------------------
# (d) Step 6: Check the residuals from your chosen model by plotting the ACF of the residuals, and
# doing portmanteau test of the residuals

# Parameter estimation
summary(fit5)

res <- residuals(fit5)
tsdisplay(res, main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

hist(res, xlab='Residuals',main='', breaks=20, col='light blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Portmanteau test
Box.test(residuals(fit5), lag=24, fitdf=4, type="Ljung") # checks for autocorrelation

getrmse <- function(x,h,...) {
  train.end <- time(x)[length(x)-h] 
  test.start <- time(x)[length(x)-h+1] 
  train <- window(x,end=train.end) 
  test <- window(x,start=test.start) 
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"]) 
}

getrmse(hsales, h=24, order = c(0, 1, 2), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(0, 1, 1), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(0, 1, 3), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(1, 1, 2), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(1, 1, 1), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(1, 1, 3), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(4, 1, 0), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(3, 1, 0), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(5, 1, 0), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(4, 1, 1), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(3, 1, 1), seasonal = c(0, 1, 1))
getrmse(hsales, h=24, order = c(5, 1, 1), seasonal = c(0, 1, 1))


summary(fit7)
tsdisplay(residuals(fit7))
Box.test(residuals(fit7), lag=24, fitdf=1, type="Ljung")

# ----------------------------------------------------------------------------------------------------
# (e) Step 7: If the residuals look like white noise, calculate the forecast

fcast <- forecast(fit5, h=24)
plot(fcast,ylab='House Sales',xlab='Year',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (f) Compare the forecasts obtained using ets().

fit_ets<- ets(hsales)
fcast_ets = forecast(fit_ets, h=24)
plot(fcast_ets,ylab='House Sales',xlab='Year',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Portmanteau test
Box.test(residuals(fit_ets), lag=24, fitdf=4, type="Ljung") # checks for autocorrelation

par(mfrow=c(1,2))

plot(fcast, ylim = c(20,100), ylab='House Sales',xlab='Year',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

plot(fcast_ets, ylim = c(20,100),ylab='House Sales',xlab='Year',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (g) Try using a non-seasonal model applied to the seasonally adjusted data obtained from STL. 
# The stlf() function will make the calculations easy (with method=???arima???). Compare the forecasts 
# with those obtained in the previous steps. Which do you think is the best approach?
fit_stl <- stlf(hsales, robust=TRUE, method=c('arima'))
fcast_stl <- forecast(fit_stl, h=24)

# Portmanteau test
Box.test(residuals(fit_ets), lag=24, fitdf=4, type="Ljung") # checks for autocorrelation

par(mfrow=c(1,3))

plot(fcast, ylim = c(20,100),ylab='House Sales',xlab='Year',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

plot(fcast_ets, ylim = c(20,100),ylab='House Sales',xlab='Year',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

plot(fcast_stl, ylim = c(20,100),ylab='House Sales',xlab='Year',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

res <- residuals(fit5)
tsdisplay(res, main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)
hist(res, xlab='Residuals',main='', breaks=20, col='light blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

res_ets <- residuals(fit_ets)
tsdisplay(res_ets, main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)
hist(res_ets, xlab='Residuals',main='', breaks=20, col='light blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

res_stl <- residuals(fit_stl)
tsdisplay(res_stl, main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)
hist(res_stl, xlab='Residuals',main='', breaks=20, col='light blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

summary(fit5)
summary(fit_ets)
summary(fit_stl)

# ----------------------------------------------------------------------------------------------------
# EXERCISE 3
# ----------------------------------------------------------------------------------------------------

# (a) Use the following R code to generate data from an AR(1) model with ??1=0.6 and ??2=1.
# The process starts with ??0=0.

set.seed(0)
e <- rnorm(100)
y <- ts(numeric(100))
y1 <- ts(numeric(100))
y2 <- ts(numeric(100))
y3 <- ts(numeric(100))
y4 <- ts(numeric(100))
y5 <- ts(numeric(100))


for(i in 2:100)
  y[i] <- 0.6*y[i-1] + e[i]

for(i in 2:100)
  y1[i] <- -0.8*y1[i-1] + e[i]
for(i in 2:100)
  y2[i] <- -0.4*y2[i-1] + e[i]
for(i in 2:100)
  y3[i] <- 0*y3[i-1] + e[i]
for(i in 2:100)
  y4[i] <- 0.4*y4[i-1] + e[i]
for(i in 2:100)
  y5[i] <- 0.8*y5[i-1] + e[i]

# ----------------------------------------------------------------------------------------------------
# (b) Produce a time plot for the series. How does the plot change as you change ??1?

plot(y,main='',ylim=c(-3,3))
lines(y1, col=5, lty=2)
lines(y2, col=3, lty=2)
lines(y3, col=4, lty=2)
lines(y4, col=2, lty=2)
lines(y5, col=6, lty=2)
legend('topleft',lty=1, col=c(1,5,3,4,2,6), c(expression(phi == 0.6), 
      expression(phi == -0.8), expression(phi == -0.4), expression(phi == 0.0), 
      expression(phi == 0.4), expression(phi == 0.8)), pch=1, cex=0.8)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (c) Write your own code to generate data from an MA(1) model with ??1 =0.6 and ??2=1.

y <- ts(numeric(100))
y1 <- ts(numeric(100))
y2 <- ts(numeric(100))
y3 <- ts(numeric(100))
y4 <- ts(numeric(100))
y5 <- ts(numeric(100))

for(i in 2:100)
  y[i] <- e[i] + 0.6*e[i-1]
for(i in 2:100)
  y1[i] <- e[i] - 0.8*e[i-1]
for(i in 2:100)
  y2[i] <- e[i] - 0.4*e[i-1]
for(i in 2:100)
  y3[i] <- e[i] + 0*e[i-1]
for(i in 2:100)
  y4[i] <- e[i] + 0.4*e[i-1]
for(i in 2:100)
  y5[i] <- e[i] + 0.8*e[i-1]

# ----------------------------------------------------------------------------------------------------
# (d) Produce a time plot for the series. How does the plot change as you change ??1?

plot(y,main='',ylim=c(-3,3))
lines(y1, col=5, lty=2)
lines(y2, col=3, lty=2)
lines(y3, col=4, lty=2)
lines(y4, col=2, lty=2)
lines(y5, col=6, lty=2)
legend('topleft',lty=1, col=c(1,5,3,4,2,6), c(expression(theta == 0.6), 
      expression(theta == -0.8), expression(theta == -0.4), expression(theta == 0.0), 
      expression(theta == 0.4), expression(theta == 0.8)), pch=1, cex=0.8)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (e) Generate data from an ARMA(1,1) model with ??1=0.6 and ??1=0.6 and ??2=1. 

y_arma <- ts(numeric(100))

for(i in 2:100)
  y_arma[i] <- e[i] + 0.6*e[i-1] + 0.6*y_arma[i-1]

# ----------------------------------------------------------------------------------------------------
# (f) Generate data from an AR(2) model with ??1=-0.8 and ??2=0.3 and ??2=1. 
# (Note that these parameters will give a non-stationary series.)

y_ar <- ts(numeric(100))

for(i in 3:100)
  y_ar[i] <- -0.8*y_ar[i-1] + 0.3*y_ar[i-2] + e[i]


# ----------------------------------------------------------------------------------------------------
# (g) Graph the latter two series and compare them. 

par(mfrow=c(1,2)) 
plot(y_arma,ylab='ARMA(1,1)',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

plot(y_ar,ylab='AR(2)',main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)


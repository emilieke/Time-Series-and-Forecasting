# ----------------------------------------------------------------------------------------------------
# PROBLEM SET 1 
# Emilie Engen, 100356077

# ----------------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Time Series and Forecasting/Exercise 1")

# ----------------------------------------------------------------------------------------------------
# Install packages
install.packages('fpp')

# Include libraries
library(fpp)

# ----------------------------------------------------------------------------------------------------
# EXERCISE 1A
# ----------------------------------------------------------------------------------------------------
# Investigate the dataset
?chicken

# Load dataset into memory
data(chicken)

# Type of object
is(chicken)

# Dimension
dim(chicken)

# Have a look a it
head(chicken)

# Summary
summary(chicken)

# ----------------------------------------------------------------------------------------------------
# Initial plots
plot(chicken, xlab = 'Year', ylab = 'Price', main = '')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# Simple forecasting methods

# Creating the prediction series using drift method
chicken.fit <-rwf(chicken, h=5, drift=TRUE)

# Plot the forecasts
plot(chicken.fit, plot.conf=TRUE, xlab = 'Year', ylab = 'Price', main = '') 
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# EXERCISE 1B
# ----------------------------------------------------------------------------------------------------
# Investigate the dataset
?usdeaths

# Load dataset into memory
data(usdeaths)

# Type of object
is(usdeaths)

# Dimension
dim(usdeaths)

# Have a look a it
head(usdeaths)

# Summary
summary(usdeaths)

# ----------------------------------------------------------------------------------------------------
# Initial plots
plot(usdeaths, xlab = 'Year', ylab = 'Number of deaths', main = '')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

seasonplot(usdeaths, xlab = 'Month', ylab = 'Number of deaths', main = '', year.labels=TRUE, col=1:7, 
           pch=19)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# Simple forecasting methods

# Creating the prediction series using the seasonal naive method 
usdeaths.fit <- snaive(usdeaths, h=12) 

# Plot the forecasts
plot(usdeaths.fit, plot.conf=TRUE, xlab = 'Month', ylab = 'Number of deaths', main = '') 
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# EXERCISE 2
# ----------------------------------------------------------------------------------------------------
# Investigate the dataset
?ibmclose

# Load dataset into memory
data(ibmclose)

# Type of object
is(ibmclose)

# Dimension
dim(ibmclose)

# Have a look a it
head(ibmclose)

# Summary
summary(ibmclose)

# ----------------------------------------------------------------------------------------------------
# Initial plots
plot(ibmclose, xlab = 'Day', ylab = 'Closing price $', main = '')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

tsdisplay(ibmclose)

# ----------------------------------------------------------------------------------------------------
# Simple forecasting methods

# Create train set
ibmclose_train <- window(ibmclose,start=1,end=300) 

# Applying different forecasting methods to the test set
ibmclose_train.fit1 <- meanf(ibmclose_train, h=69)
ibmclose_train.fit2 <- naive(ibmclose_train), h=69) 
ibmclose_train.fit3 <- snaive(ibmclose_train, h=69) 
ibmclose_train.fit4 <-rwf(ibmclose_train, h=69,drift=TRUE)

# Plot the forecasts
plot(ibmclose_train.fit1, plot.conf=FALSE, xlab = 'Day', ylab = 'Closing price $', main='', ylim=c(306,603)) 
lines(ibmclose_train.fit2$mean, col=2)
lines(ibmclose_train.fit3$mean, col=3)
lines(ibmclose_train.fit4$mean, col=5) 
lines(ibmclose)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)
legend('topright',lty=1,cex = 0.8,col=c(4,2,3,5), 
       legend=c('Mean method','Naive method','Seasonal naive','Drift method'))

# Measure the accuracy
ibmclose_test <- window(ibmclose, start=301) 
a1<-accuracy(ibmclose_train.fit1, ibmclose_test)
a2<-accuracy(ibmclose_train.fit2, ibmclose_test) 
a3<-accuracy(ibmclose_train.fit3, ibmclose_test)
a4<-accuracy(ibmclose_train.fit4, ibmclose_test)

# Create latex tables
library(xtable)
xtable(t(a1))
xtable(t(a2))
xtable(t(a3))
xtable(t(a4))

# ----------------------------------------------------------------------------------------------------
# Residual diagnostics

# Residuals plot
res_drift <- residuals(rwf(ibmclose));res_drift
plot(res_drift, main='',ylab='Residuals', xlab='Day')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ACF of residuals plot
Acf(res_drift, main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Histogram of residuals
hist(res_drift, nclass='FD', main='',xlab='Residuals',col='light blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Residuals plot, ACF an PACF from naive method
tsdisplay(res_drift, main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# EXERCISE 3
# ----------------------------------------------------------------------------------------------------

?plastics

# Load dataset into memory
data(plastics)

# Type of object
is(plastics)

# Dimension
dim(plastics)

# Have a look a it
head(plastics)

# Summary
summary(plastics)

# ----------------------------------------------------------------------------------------------------
# (a) Plot the time series of sales of product A.

# Initial plots
plot(plastics, xlab = 'Year', ylab = 'Monthly sales', main = '')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Seasonal plot
seasonplot(plastics, xlab = 'Month', ylab = 'Monthly sales', main = '', 
           year.labels=TRUE, year.labels.left=TRUE, col=2:6, pch=19)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Seasonal plot
monthplot(plastics, xlab = 'Month', ylab = 'Monthly sales', main = '')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Plot with forecast
plot(forecast(plastics), xlab = 'Year', ylab = 'Monthly sales', main = '')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# %(b) Use an STL decomposition to calculate the trend-cycle and seasonal indices. 
# (Experiment with having fixed or changing seasonality).

# Time series decomposition

# STL decomposition
# seasonal component does not change over time
fit <- stl(plastics, t.window=12, s.window='periodic', robust=TRUE)
plot(fit)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# seasonal component can change over time
fit <- stl(plastics, t.window=12, s.window=12, robust=TRUE) 
plot(fit)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)


# seasonal component can change over time
fit <- stl(plastics, t.window=12, s.window=24, robust=TRUE) 
plot(fit)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)


# seasonal component can change over time
fit <- stl(plastics, t.window=12, s.window=36, robust=TRUE) 
plot(fit)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)


# The trend-cycle component ignoring noise and seasonality
plot(plastics, xlab = 'Year', ylab = 'Monthly sales', main = '')
lines(fit$time.series[,2],col="blue",ylab="Trend")
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (d) Compute and plot the seasonally adjusted data.

# Seasonally adjusted data
plastics_seasadj <- seasadj(fit)
plot(plastics, xlab = 'Year', ylab = 'Monthly sales', main = '')
lines(plastics_seasadj,col='blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (e) Use a random walk to produce forecasts of the seasonally adjusted data. 

# Forecast on seasonal adjusted data using random walk
adj_fit <- naive(plastics_seasadj,h=12)
plot(adj_fit, plot.conf=TRUE, xlab = 'Year', ylab = 'Monthly sales', main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

plot(forecast(plastics_seasadj,h=12), plot.conf=TRUE, xlab = 'Year', ylab = 'Monthly sales', main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)


# ----------------------------------------------------------------------------------------------------
# (f) Reseasonilize the results to give forecasts on the original scale.

# We put back the seasonality into the forcasting data through the forecast method 
fcast <- forecast(fit, method="naive")
plot(fcast, xlab = 'Year', ylab = 'Monthly sales', main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------







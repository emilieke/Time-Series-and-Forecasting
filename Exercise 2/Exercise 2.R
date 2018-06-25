# ----------------------------------------------------------------------------------------------------
# PROBLEM SET 2 
# Emilie Engen, 100356077

# ----------------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Time Series and Forecasting/Exercise 2")

# ----------------------------------------------------------------------------------------------------
# Install packages
install.packages('fpp')

# Include libraries
library(fpp)

# ----------------------------------------------------------------------------------------------------
# EXERCISE 1
# ----------------------------------------------------------------------------------------------------
# Investigate the dataset
?books

# Load dataset into memory
data(books)

# Type of object
is(books)

# Dimension
dim(books)

# Have a look a it
head(books)

# Summary
summary(books)

# ----------------------------------------------------------------------------------------------------
# (a) Plot the series and discuss the main features of the data.
plot(books[,1], xlab = 'Day', ylab = 'Number of sales', main = '')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

plot(books[,2], xlab = 'Day', ylab = 'Number of sales', main = '')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (b) Use simple exponential smoothing with the ses function (setting initial=???simple???) and explore 
# different values of ?? for the paperback series.

# Define paperback books
paperback <- books[,1]

# Fit a forecasting model with exponential smoothing using different alphas
fit1 <- ses(paperback, alpha = 0.0, initial = 'simple', h = 4)
fit2 <- ses(paperback, alpha = 0.2, initial = 'simple', h = 4) 
fit3 <- ses(paperback, alpha = 0.4, initial = 'simple', h = 4)
fit4 <- ses(paperback, alpha = 0.6, initial = 'simple', h = 4) 
fit5 <- ses(paperback, alpha = 0.8, initial = 'simple', h = 4)
fit6 <- ses(paperback, alpha = 1.0, initial = 'simple', h = 4)

# Plot the model
plot(fit1, plot.conf=FALSE, ylab='Number of sales', xlab='Day', main='', fcol='white', type='o')
lines(fitted(fit1), col=5, type='o')
lines(fitted(fit2), col=3, type='o')
lines(fitted(fit3), col=4, type='o')
lines(fitted(fit4), col=2, type='o')
lines(fitted(fit5), col=6, type='o')
lines(fitted(fit6), col=7, type='o')

# Plot the forecasts
lines(fit1$mean, col=5, type='o')
lines(fit2$mean, col=3, type='o')
lines(fit3$mean, col=4, type='o')
lines(fit4$mean, col=2, type='o')
lines(fit5$mean, col=6, type='o')
lines(fit6$mean, col=7, type='o')
legend('topleft',lty=1, col=c(1,5,3,4,2,6,7), c('data', expression(alpha == 0.0), expression(alpha == 0.2), 
      expression(alpha == 0.4), expression(alpha == 0.6), expression(alpha == 0.8), 
      expression(alpha == 1)),pch=1,cex=0.8)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Compare the results
fit1$model
fit2$model
fit3$model
fit4$model
fit5$model
fit6$model

# Record the within-sample SSE for the one-step forecasts. Plot SSE against ?? and find which value 
# of ?? works best. What is the effect of ?? on the forecasts?

# Compute the SSEs
sse1 = fit1$model$SSE;sse1
sse2 = fit2$model$SSE;sse2
sse3 = fit3$model$SSE;sse3
sse4 = fit4$model$SSE;sse4
sse5 = fit5$model$SSE;sse5
sse6 = fit6$model$SSE;sse6

sse_matrix = c(sse1,sse2,sse3,sse4,sse5,sse6)
alpha_matrix = c(0,0.2,0.4,0.6,0.8,1)

# Plot the SSE values with respect to alpha
plot(alpha_matrix,sse_matrix, ylab='SSE', xlab='The smoothing parameter, alpha',col='blue',pch=21,
     bg='blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (c) Now let ses select the optimal value of ??. Use this value to generate forecasts for the next 
# four days. Compare your results with (b).

fit_opt <- ses(paperback, initial = 'simple', h = 4)
summary(fit_opt)

plot(fit1, plot.conf=FALSE, ylab='Number of sales', xlab='Day', main='', fcol='white', type='o')
lines(fitted(fit_opt), col=2, type='o')

lines(fit_opt$mean, col=2, type='o')
legend('topleft',lty=1, col=c(1,2), c('data', expression(alpha == 0.2125)),pch=1,cex=0.6)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

sse_opt = fit_opt$model$SSE;sse_opt
plot(alpha_matrix,sse_matrix, ylab='SSE', xlab='The smoothing parameter, alpha',col='blue',pch=21,
     bg='blue')
points(0.2125,sse_opt,pch=21,col=2,bg=2)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)


# ----------------------------------------------------------------------------------------------------
# EXERCISE 2
# ----------------------------------------------------------------------------------------------------
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
# (a) Make a time plot of your data and describe the main features of the series.

# Plot of overseas visitors to Australia
plot(visitors,main='',ylab='Thousands of people',xlab='Year')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Seasonal plot
seasonplot(visitors,main='',ylab='Thousands of people',xlab='Month', year.labels=TRUE, 
           year.labels.left=TRUE,col=1:7, pch=19)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Month plot
monthplot(visitors,main='',ylab='Thousands of people',xlab='Month')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (b) Forecast the next two years using Holt-Winters' multiplicative method.

# Fit a forecasting model with HW multiplicative method
fit1 <- hw(visitors,seasonal='multiplicative') 

# Plot the model
plot(fit1, plot.conf=FALSE, ylab='Thousands of people', xlab='Year', main='', fcol='white')
lines(fitted(fit1), col=4, lty=2)
lines(fit1$mean, col=4)
legend('topleft',lty=1, col=c(1,4), c('data', 'Holt-Winters Multiplicative'),pch=1,cex=0.8)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Results from the model
fit1$model
accuracy(fit1)

# ----------------------------------------------------------------------------------------------------
# (d) Experiment with making the trend exponential and/or damped. 

fit1 <- hw(visitors,seasonal='multiplicative') 
fit2 <- hw(visitors,seasonal='multiplicative',exponential=TRUE) 
fit3 <- hw(visitors,seasonal='multiplicative',damped=TRUE) 
fit4 <- hw(visitors,seasonal='multiplicative',exponential=TRUE,damped=TRUE) 

plot(fit1, plot.conf=FALSE, ylab='Thousands of people', xlab='Year', main='', fcol='white')
lines(fitted(fit1), col=4, lty=2)
lines(fitted(fit2), col=5, lty=2)
lines(fitted(fit3), col=3, lty=2)
lines(fitted(fit4), col=2, lty=2)

lines(fit1$mean, col=4)
lines(fit2$mean, col=5)
lines(fit3$mean, col=3)
lines(fit4$mean, col=2)
legend('topleft',lty=1, col=c(1,4,5,3,2), c('data','Holt-Winters Multiplicative','Exponential Trend',
                                            'Damped Trend','Exp and Damped Trend'),pch=1,cex=0.8)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Compare the results
fit1$model
fit2$model
fit3$model
fit4$model

a1<-accuracy(fit1);a1
a2<-accuracy(fit2);a2
a3<-accuracy(fit3);a3
a4<-accuracy(fit4);a4

# Create latex tables
library(xtable)
xtable(t(a1))
xtable(t(a2))
xtable(t(a3))
xtable(t(a4))


# ----------------------------------------------------------------------------------------------------
# (e) Now fit each of the following models to the same data:

# (1) an ETS model
fit5 <- ets(visitors);fit5

# (2) an additive ETS model applied to a Box-Cox transformed series
lambda <- BoxCox.lambda(visitors)
fit6 <- ets(visitors, model='AAZ', lambda=lambda);fit6

# (3) an STL decomposition applied to the Box-Cox transformed data followed by an ETS
# model applied to the seasonally adjusted (transformed) data. 
stl <- stlm(visitors, robust=TRUE, method=c('ets'), lambda=lambda);stl
fit7 <- forecast(stl, lambda=stl$lambda);fit7
dev.off()
# ----------------------------------------------------------------------------------------------------
# (f) Plot all the forecasts together.
plot(forecast(fit5, h=24), plot.conf=FALSE, ylab='Thousands of people', xlab='Year', main='', fcol='white')
lines(fitted(fit5), col=4, lty=2)
lines(fitted(fit6), col=5, lty=2)
lines(fitted(fit7), col=2, lty=2)

lines(forecast(fit5)$mean, col=4)
lines(forecast(fit6)$mean, col=5)
lines(fit7$mean, col=2)
legend('topleft',lty=1, col=c(1,4,5,2), c('data','ETS','Additive ETS','STL and ETS'),pch=1,cex=0.8)
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ----------------------------------------------------------------------------------------------------
# (g) For each model, look at the residual diagnostics and compare the forecasts for the next two years. 
# Which do you prefer?

# Residual diagnostics

# (1) an ETS model
# Residuals plot
res5 <- residuals(fit5);res5
plot(res5, main='',ylab='Residuals', xlab='Year')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ACF of residuals plot
Acf(res5, main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Residuals plot, ACF an PACF for ETS
res5 <- residuals(fit5);res5
tsdisplay(res5, main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# (2) an additive ETS model applied to a Box-Cox transformed series
# Residuals plot
res6 <- residuals(fit6);res6
plot(res6, main='',ylab='Residuals', xlab='Year')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ACF of residuals plot
Acf(res6, main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Residuals plot, ACF an PACF from additive ETS
res6 <- residuals(fit6);res6
tsdisplay(res6, main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# (3) an STL decomposition applied to the Box-Cox transformed data followed by an ETS
# model applied to the seasonally adjusted (transformed) data. 
# Residuals plot
res7 <- residuals(fit7);res7
plot(res7, main='',ylab='Residuals', xlab='Year')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# ACF of residuals plot
Acf(res7, main='')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Residuals plot, ACF an PACF for STL and ETS
res7 <- residuals(fit7);res7
tsdisplay(res7, main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

par(mfrow = c(1, 3))
# Histogram of residuals ETS
hist(res5, breaks=20, main='',xlab='Residuals',col='light blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Histogram of residuals additive ETS 
hist(res6, breaks=15, main='',xlab='Residuals',col='light blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)

# Histogram of residuals STL and ETS
hist(res7, breaks=15, main='',xlab='Residuals',col='light blue')
title(main=paste('Emilie Engen, Timestamp:',Sys.time()),cex.main=1)
# ----------------------------------------------------------------------------------------------------
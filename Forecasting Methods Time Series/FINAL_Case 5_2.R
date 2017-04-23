#import libraries
library(forecast)
library(car)
library(lmtest)
options(max.print=999999) #Avoid the max.print limit

#Read data Clients with 1 lag
data = read.csv("lag_client.csv")
data

#Use only relevant variables
myvars = c("New.Clients..Yt.", "New.Clients..Yt.1.")
data = data[myvars]

#Convert data in timeseries
timeseries = ts (data, frequency = 12, start = c(1985))

#Regression
fit <- lm(New.Clients..Yt. ~ New.Clients..Yt.1.,
           data = timeseries)
fit
summary(fit)

#Lag-client is a significant variable
#R^2 = 0.2484 ==> 24.8% of the variability is expained by the lag-client variable

#Check autocorellation of residuals
residuals = fit$residuals
acf(residuals, main="Autocorrelation Residuals lag-client model", lag.max = 24)
print(acf(residuals, lag.max = 24))

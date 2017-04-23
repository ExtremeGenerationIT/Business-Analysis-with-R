#import libraries
library(forecast)
library(car)
library(lmtest)
options(max.print=999999) #Avoid the max.print limit

full_data = read.csv("full_dataset.csv")

###1) Regression with all variables used by Dorothy

#Start from year 1989 (no Food.Stamps before)
data1 = subset(full_data, Year>=1989)

#Consider only these variables
myvars = c("New.Clients", "Food.Stamps", "BA.Index", "Bankrup.", "Permits.Issued")
data1 = data1[myvars]

#Convert in a TimeSeries
timeseries1 = ts(data1, frequency = 12, start=c(1989))

#Correlation Matrix
cor(timeseries1)

#Regression
fit1 <- lm(New.Clients ~ Food.Stamps 
          + BA.Index
          + Bankrup. 
          + Permits.Issued,
          data = timeseries1)
fit1
summary(fit)


###2) Regression WITHOUT "Food.Stamps" variable
#People.on.Food.Stamps isn't significant
data2 = subset(full_data, Year>=1986)
myvars2 = c("New.Clients", "BA.Index", "Bankrup.", "Permits.Issued")
data2 = data2[myvars2]

#The time series can start from 1986 (we have data starting from 1986)
timeseries2 = ts(data2, frequency = 12, start=c(1986))

#Correlation Matrix
cor(timeseries2)

#Regression
fit2 <- lm(New.Clients ~ BA.Index
           + Bankrup. 
           + Permits.Issued,
           data = timeseries2)
fit2
summary(fit2)

#Perform Durbin-Watson test for autocorrelation
#H0: no autocorrelation ; H1: positive correlation
#Rejection H0 if D>D(lower bound) | do not reject H0 if D<D(upper bound)
#Test inconclusive if D(low) <= D <= D(up)
#k=3 | n=84
durbinWatsonTest(fit2,  alternative = c("positive"))
#Result: 1.56< 1.603019 < 1.72 ==> Test Inconclusive!

#autocorrelation residuals
residuals = fit2$residuals
acf(residuals, main="Autocorrelation Residuals")
#Print autocorrelation values by timeLag
print(acf(residuals))


###3) Seasonal Regression Model

#3.1) Model with all variables
data3 = subset(full_data, Year>=1989)

myvars3 = c("New.Clients", "Food.Stamps", "BA.Index", "Bankrup.", "Permits.Issued",
            "Dummy.2", "Dummy.3", "Dummy.4","Dummy.5","Dummy.6","Dummy.7","Dummy.8","Dummy.9",
            "Dummy.10","Dummy.11","Dummy.12")
data3 = data3[myvars3]

timeseries3 = ts(data3, frequency = 12, start=c(1989))

#Regression
fit3 =  lm(New.Clients ~ Food.Stamps + BA.Index + Bankrup. + Permits.Issued
           + Dummy.2 + Dummy.3 + Dummy.4 + Dummy.5 + Dummy.6 + Dummy.7 + Dummy.8 + Dummy.9 + Dummy.10 + Dummy.11 + Dummy.12,
           data = timeseries3)
fit3
summary(fit3)

#Given the insignificance of Food.Stamps || Bankrup. || Permits.Issued; those are removed from the model

#3.2) Model without Food.Stamps, Bankrup., Permits.Issued
#data4 = read.csv("full_dataset_BA_index.csv")
data4 = full_data
myvars4 = c("New.Clients", "BA.Index",
            "Dummy.2", "Dummy.3", "Dummy.4","Dummy.5","Dummy.6","Dummy.7","Dummy.8","Dummy.9",
            "Dummy.10","Dummy.11","Dummy.12")
data4 = data4[myvars4]

#Since we use only BA.Index, we can start from 1985
timeseries4 = ts (data4, frequency = 12, start=c(1985))

fit4 = lm(New.Clients ~  BA.Index 
          + Dummy.2 + Dummy.3 + Dummy.4 + Dummy.5 + Dummy.6 + Dummy.7 + Dummy.8 + Dummy.9 + Dummy.10 + Dummy.11 + Dummy.12,
          data = timeseries4)
fit4
summary(fit4)

#Only Sesonal 2,3,10 are insignificant, hence we can remove them from the model
#Perform Durbin-Watson test for autocorrelation
#H0: no autocorrelation ; H1: positive correlation
#Rejection H0 if D>D(lower bound) | do not reject H0 if D<D(upper bound)
#Test inconclusive if D(low) <= D <= D(up)
#k=12 | n=95
durbinWatsonTest(fit4,  alternative = c("positive"))
#Result: DW Statistic=1.335816; D(low)=1.39;  D(up)=1.96 ==> DW<D(lower) ==> Reject H0; there's evidence of autocorrelation!

#Check autocorrelation residuals
residuals2 = fit4$residuals
acf(residuals2, lag.max = 24, main="Residuals Seasonal Regression Model")
#Print autocorrelation values by timeLag
print(acf(residuals))
#Get the coefficient for the prediction
coef(fit4)


###4) Forecast 3 periods (h=3) with the sesonal model
forecast_data = read.csv("3_period_data.csv")

#Forecast the new Clients for the Jan, Feb, March 1993
forecast.lm(fit4, h=3, newdata = forecast_data)

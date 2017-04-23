#Import Forecast Library
library("forecast")

#Read DataSet
data <- read.csv("case3.csv")
#Create the Time Series
series <- ts(data, frequency = 12, start = c(1985))
#Show the series on a graph
plot.ts(series, main="Number of New clients, 1985-1993",  ylab="Number of clients", xlab="Year")

#Decompose the Time Series in its components
decomposedSeries <- decompose(series)
plot (decomposedSeries)

#######We need to forecast 9 periods##########

#Explore some Naive Forecasting Methods
#Despite the fact we developed our "Naive model" with Excel, we wanted to explore some Naive methods with R

#Naive stationary model
stationarynaive <- naive(series, h=9)
stationarynaive
plot(stationarynaive)
accuracy(stationarynaive)

#Naive sesonal model
seasonalnaive <- snaive(series, h=9)
seasonalnaive
plot(seasonalnaive)
accuracy(seasonalnaive)


#Smoothing Exponential
seriesforecast <- HoltWinters(series, beta=FALSE, gamma=FALSE)
seriesforecast
#compare the graphs
plot(seriesforecast)
# forecast
forecast_smoothing_exp <- forecast.HoltWinters(seriesforecast, h=9)
forecast_smoothing_exp
plot(forecast_smoothing_exp)
accuracy(forecast_smoothing_exp)

#Holt's Exponential Smoothing (trend; no seasonality)
seriesforecast3 <- HoltWinters(series, gamma = FALSE)
seriesforecast3
plot(seriesforecast3)
# forecast
forecast_holts <- forecast.HoltWinters(seriesforecast3, h=9)
forecast_holts
plot(forecast_holts)
accuracy(forecast_holts)


#CHOSEN MODEL: Winters' Exponential Smoothing
#Winters Exponential Smoothing (increasing/decreasing trend and seasonability)
seriesforecast4 <- HoltWinters(series)
seriesforecast4
plot(seriesforecast4)
# forecast
forecast_winters<- forecast.HoltWinters(seriesforecast4, h=9)
forecast_winters
#Show a projection of the forecasting, with interval of confidence of .95 and .8
plot(forecast_winters)
#Get the accuracy socre
accuracy(forecast_winters)

#Correlogram function of Residuals
residuals <- na.exclude(forecast_winters$residuals) #Exclude NA values
residuals
acf(residuals, lag.max = 24, main="Autocorrelation Residual Function")



#########################
#Ultimately, even though not required by the assignemnt, let the software choose the best forecasting model
#R can find the best forecasting thanks to the function forecast()
best_forecast <- forecast(series, h=9)
best_forecast
plot(best_forecast)
accuracy(best_forecast)


#Forecasting new cases

#Importing data:
library(readxl)
df <- read_excel("/cloud/covid_agreguoti_duomenys.xlsx")
View(df)

#install.packages("ggplot2")
#install.packages("forecast")
#install.packages("fpp2")
#install.packages("tseries")
library(fpp2)
library(forecast)
library(ggplot2)
library(tseries)

#making time series
tsData_cases = ts(df$`new cases`, 1)
ts.plot(tsData_cases) 
ts.plot(tsData_cases, xlab = " day ",
ylab = " Number of new cases ", main = "")
title ( main = " Number of new cases 2020 05 01 - 2021 11 12", line = 0.3)

#differencing
tsData_cases<-diff(tsData_cases)
ts.plot(tsData_cases, xlab = " day ",
ylab = " Differenced number of new case ", main = "")
title ( main = " Differenced number of new cases 2020 05 01 - 2021 11 12",
line = 0.3)

adf.test(tsData_cases, alternative = "stationary", k=0)
#p-value smaller than printed p-value

par(mfrow=c(1,2))
acf(tsData_cases, main="ACF for new cases")
#alternative positive and negative spikes decay to 0
pacf(tsData_cases, main="PACF for new cases") 
#PACF "cuts off" after the 7th lag.

#Ljung_box test
Box.test(tsData_cases, type="Ljung-Box")
# p - value<0.05

# ARIMA modelling
fit = auto.arima(tsData_cases, seasonal=TRUE)
checkresiduals(fit) #p-value<0.05, need to fix model
fitfix<-arima(tsData_cases, order=c(7,1,1))
checkresiduals(fitfix) 
forecast_arima = forecast(fitfix, h=14)
plot(forecast_arima)
autoplot(tsData_cases, series=" Historical data",
ylab = " Number of new cases ") +
  autolayer(forecast_arima, series=" ARIMA Forecast") +
  ggtitle(" ARIMA forecasting") +
  theme(plot.title = element_text(size=8))

# Holt modelling
fit1 =  HoltWinters(tsData_cases, gamma=FALSE, beta=TRUE) 
#Holt-Winters exponential smoothing with trend and without seasonal component.
forecast_hw = forecast(fit1, h=14)
autoplot(tsData_cases, series="Historical data") + 
  autolayer(forecast_hw, series="Holt-Winter forecast") +
  ggtitle("HW Exponential Smoothing") +
  theme(plot.title = element_text(size=8))

# Evaluation
# ARIMA
forecast_arima['model']
accuracy(forecast_arima)
# Holt's
forecast_hw['model']
accuracy(forecast_hw)


#Forecasting deaths

library(readxl)
df <- read_excel("/cloud/covid_agreguoti_duomenys.xlsx")
View(df)

library(fpp2)
library(forecast)
library(ggplot2)
library(tseries)

tsData_deaths = ts(df$deaths, frequency =1)
ts.plot(tsData_deaths, xlab = " day ",
ylab = " Number of deaths ", main = "")
title ( main = " Number of deaths 2020 05 01 - 2021 11 12",
line = 0.3)

tsData_deaths<-diff(tsData_deaths)
ts.plot(tsData_deaths, xlab = " day ",
ylab = "Differenced number of deaths ", main = "")
title ( main = "Differenced number of deaths 2020 05 01 - 2021 11 12",
line = 0.3)

adf.test(tsData_deaths, alternative = "stationary", k=0)
#p-value smaller than printed p-value

par(mfrow=c(1,2))
acf(tsData_deaths, main="ACF for deaths")
pacf(tsData_deaths, main="PACF for deaths")

# ARIMA modelling
fit1 = auto.arima(tsData_deaths, seasonal=FALSE,
stepwise = FALSE, approximation = FALSE) 
forecast_arima1 = forecast(fit1, h=14)
checkresiduals(fit1)
autoplot(tsData_deaths, series=" Historical data",
ylab = " Number of deaths ") +
  autolayer(forecast_arima1, series=" ARIMA Forecast") +
  ggtitle(" ARIMA forecasting") +
  theme(plot.title = element_text(size=8))

# Holt modelling
fit2 =  HoltWinters(tsData_deaths, gamma = FALSE)
forecast_hw2 = forecast(fit2, h=14)
autoplot(tsData_deaths, series="Historical data") + 
  autolayer(forecast_hw2, series="Holt-Winter forecast") +
  ggtitle("HW Exponential Smoothing") +
  theme(plot.title = element_text(size=8))

# Evaluation
# ARIMA
forecast_arima1['model']
accuracy(forecast_arima1)
# Holt's model
forecast_hw2['model']
accuracy(forecast_hw2)
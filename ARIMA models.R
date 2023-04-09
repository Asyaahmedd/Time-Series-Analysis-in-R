# ARIMA models
#--------------------------------------------
#Import
rm(list=ls()) #Removes all items in Environment!
library(forecast) 
#---------------------------------------------
set.seed(250)
timeseries=arima.sim(list(order = c(1,1,2), ma=c(0.32,0.47), ar=0.8),
                     n = 50)+20


plot.ts(timeseries)
acf(timeseries)

pacf(timeseries)
#---------------------------------
windows() ; par(mfrow=c(3,1))
plot (timeseries,col =2  )
acf(timeseries)
pacf(timeseries)
#--------------------------
# 1- Finding d
#--------------------------
D.timeseries = diff(timeseries, lag=1)
windows() ; par(mfrow=c(3,1))
plot (timeseries,col =2  ) ; plot(D.timeseries)

#Augmented Dickey-Fuller Test
#------------------------------
library(urca)  #install.packages("urca")
df=ur.df(timeseries,type="none",lags=2)
summary(df) 

df=ur.df(D.timeseries,type="none",lags=2)
summary(df) 
#---------------
library("tseries")
adf.test(timeseries, k=2)
adf.test(D.timeseries, k=2)

#--------------------------
# 2- Finding p and q
#--------------------------
par(mfrow=c(1,1))
acf(D.timeseries)
pacf(D.timeseries)
#-----------------------
# 3- Model building  
#--------------------------
## partition into train and test
train_series=timeseries[1:40]
test_series=timeseries[41:50]

## make arima models
arimaModel_1=arima(train_series, order=c(0,1,2))
arimaModel_2=arima(train_series, order=c(1,1,0))
arimaModel_3=arima(train_series, order=c(1,1,2))

## look at the parameters
print(arimaModel_1);print(arimaModel_2);print(arimaModel_3)

AIC(arimaModel_1,arimaModel_2,arimaModel_3)

#------------------------------
# 4- Diagnostic for Time-Series 
#------------------------------
hist(arimaModel_1$residuals)
qqnorm(arimaModel_1$residuals) ; qqline(arimaModel_1$residuals)
shapiro.test(arimaModel_1$residuals)

hist(arimaModel_2$residuals)
qqnorm(arimaModel_2$residuals) ; qqline(arimaModel_2$residuals)
shapiro.test(arimaModel_2$residuals)

hist(arimaModel_3$residuals)
qqnorm(arimaModel_3$residuals) ; qqline(arimaModel_3$residuals)
shapiro.test(arimaModel_3$residuals)

#Box-Pierce and Ljung-Box Tests
Box.test(arimaModel_1$residuals,type = "Ljung-Box")
Box.test(arimaModel_2$residuals, type = "Ljung-Box")
Box.test(arimaModel_3$residuals, type = "Ljung-Box")
#-------------------
par(mfrow=c(1,1))
tsdiag(arimaModel_1)
tsdiag(arimaModel_2)
tsdiag(arimaModel_3)

#------------------------------
# 5- Forecasting
#------------------------------
forecast1=predict(arimaModel_1, 10)
forecast2=predict(arimaModel_2, 10)
forecast3=predict(arimaModel_3, 10)
#-------------------

library(DMwR)# Data mining with R
##============
accmeasures1=regr.eval(test_series, forecast1$pred)
accmeasures2=regr.eval(test_series, forecast2$pred)
accmeasures3=regr.eval(test_series, forecast3$pred)
accMeasure=rbind(accmeasures1,accmeasures2,accmeasures3)
print(accMeasure)

#-----------------------
## Using auto.arima function
#-----------------------
library(forecast)
AutoArimaModel=auto.arima(train_series)
AutoArimaModel

forecast(AutoArimaModel)

f20=forecast(AutoArimaModel,h=20 )
plot(f20)
#---------------------------------

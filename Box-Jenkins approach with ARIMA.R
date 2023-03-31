#ARIMA Example
##--------------------------------------------
rm(list=ls()) #Removes all items in Environment!
install.packages('forecast', dependencies = TRUE)
library(forecast) 
#---------------------------------------------
#- Box-Jenkins (1970) Approach
#--------------------------------
#step(1): Model Identification
#step(2): Model Estimation
#step(3): Model Checking
#step(4): Forecasting
#--------------------------------
file.choose()
X=read.csv("D:\\DS Master's\\second term\\DS611 - Time Series\\lec5\\price.csv")
X=as.ts(X) # convert x from dataframe to timesereis data
class(X)
#---------------------------------
windows() ; par(mfrow=c(3,1))
plot.ts (X,col =3)
acf(X)
pacf(X)
# the data is non stationary so it need transformation
#--------------------------
#step(1): Model Identification
#--------------------------
# 1- Finding d
#--------------------------
D.X = diff(X, lag=1)
windows() ; par(mfrow=c(3,1))
plot.ts (X,col =2  ) ; plot.ts(D.X)

#Augmented Dickey-Fuller Test
#------------------------------
#---------------
# ADF Test is a common statistical test used to
#test whether a given Time series is stationary or not .
library("tseries")
adf.test(X, k=2)
# the p-value > 0.05 so we accept the null hypothesis(the series is nonstationary)
adf.test(D.X, k=2)
# the p-value < 0.05 so we reject the null hypothesis and the series is stationary
#--------------------------
# 2- Finding p and q
#--------------------------
par(mfrow=c(1,2))
acf(D.X)
pacf(D.X)
#-----------------------
#step(2): Model Estimation
#--------------------------

## make arima models
arimaModel_1=arima(X, order=c(1,1,0))
arimaModel_2=arima(X, order=c(1,1,1))
arimaModel_3=arima(X, order=c(2,1,0))

## look at the parameters
print(arimaModel_1);print(arimaModel_2);print(arimaModel_3)

AIC(arimaModel_1,arimaModel_2,arimaModel_3)

CI=confint(arimaModel_3) #95% CI on coefs

#------------------------------
#step(3): Model Checking 
#------------------------------
par(mfrow=c(1,2))

hist(arimaModel_3$residuals)
qqnorm(arimaModel_3$residuals) ; qqline(arimaModel_3$residuals)
shapiro.test(arimaModel_3$residuals)

#Box-Pierce and Ljung-Box Tests
Box.test(arimaModel_3$residuals, type = "Ljung-Box")
#-------------------
#------------------------------
#step(4):Forecasting
#------------------------------
library(forecast)
f=forecast(arimaModel_3,h=30 )
par(mfrow=c(1,1))
plot(f)
summary(f)
#---------------------------------


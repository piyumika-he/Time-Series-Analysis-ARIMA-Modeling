#Name: Piyumika Herath Samarasekara


library(TSA)
library(lmtest)

#reading the data
data<-read.table("E:\\TUNI\\Semester2\\Time Series Analysis\\V32.txt", sep="\t", dec=".", header=TRUE)
attach(data)

#original series
Yt = data$V32

par(mfrow =c(1 ,2))

#plotting original series along with acf plot
plot.ts(Yt, main = 'Time Series - column 32')
acf(Yt,lag.max=40,type ="correlation",main = " " )

#obtaining the first difference series
data.diff1<- diff(Yt,1)
#plotting first difference series along with acf plot
plot.ts(data.diff1)
acf(data.diff1,lag.max=40,type = "correlation", main = " " )

#obtaining the second difference series
data.diff2<- diff(Yt,2)
#plotting second difference series along with acf plot
plot.ts(data.diff2)
acf(data.diff2,lag.max=40,type = "correlation", main = " " )

#Conducting unit root test
library(tseries)
adf.test(Yt)
adf.test(data.diff1)
adf.test(data.diff2)

##Step1 - 3
#Plotting ACF and PACF for selected series
acf(data.diff1)
pacf(data.diff1)


##Step2 -1
#Fitting models

mod1 = arima(Yt, order = c(4,1,1))
res1=rstandard(mod1)
mod1
BIC(mod1)

mod2 = arima(Yt, order = c(4,1,2))
res2=rstandard(mod2)
mod2
BIC(mod2)

mod3 = arima(Yt, order = c(4,1,3))
res3=rstandard(mod3)
mod3
BIC(mod3)

mod4 = arima(Yt, order = c(4,1,4))
res4=rstandard(mod4)
mod4
BIC(mod4)

mod5 = arima(Yt, order = c(3,1,1))
res5=rstandard(mod5)
mod5
BIC(mod5)

mod6 = arima(Yt, order = c(3,1,2))
res6=rstandard(mod6)
BIC(mod6)
mod6

mod7 = arima(Yt, order = c(3,1,3))
res7=rstandard(mod7)
mod7
BIC(mod7)

mod8 = arima(Yt, order = c(3,1,4))
res8=rstandard(mod8)
mod8
BIC(mod8)

mod9 = arima(Yt, order = c(2,1,1))
res9=rstandard(mod9)
mod9
BIC(mod9)

mod10 = arima(Yt, order = c(2,1,2))
res10=rstandard(mod10)
mod10
BIC(mod10)

mod11 = arima(Yt, order = c(2,1,3))
res11=rstandard(mod11)
mod11
BIC(mod11)

mod12 = arima(Yt, order = c(2,1,4))
res12=rstandard(mod12)
mod12
BIC(mod12)

mod13 = arima(Yt, order = c(1,1,1))
res13=rstandard(mod13)
mod13
BIC(mod13)

mod14 = arima(Yt, order = c(1,1,2))
res14=rstandard(mod14)
mod14
BIC(mod14)

mod15 = arima(Yt, order = c(1,1,3))
res15=rstandard(mod15)
mod15
BIC(mod15)

mod16 = arima(Yt, order = c(1,1,4))
res16=rstandard(mod16)
mod16
BIC(mod16)

#Extimates of the best models
coeftest(mod5)
coeftest(mod10)
coeftest(mod12)


##Step3 - 1

#Ljung-Box test for the selected 3 models with ACF and PACF plots
#ARIMA(2,1,4)
Box.test(res12,lag =10,type="Ljung-Box")
acf(residuals(mod12),main= "ARIMA(2,1,4)  Model ",ylab="ACF of Residuals")
pacf(residuals(mod12),main= "ARIMA(2,1,4)  Model ",ylab="PACF of Residuals")

#ARIMA(3,1,1)
Box.test(res5,lag =10,type="Ljung-Box")
acf(residuals(mod5),main= "ARIMA(3,1,1)  Model ",ylab="ACF of Residuals")
pacf(residuals(mod5),main= "ARIMA(3,1,1)  Model ",ylab="PACF of Residuals")

#ARIMA(2,1,2)
Box.test(res10,lag =10,type="Ljung-Box")
acf(residuals(mod10),main= "ARIMA(2,1,2)  Model ",ylab="ACF of Residuals")
pacf(residuals(mod10),main= "ARIMA(2,1,2)  Model ",ylab="PACF of Residuals")


##Step3 - 2
##histograms, QQ plots, and Shapiro-Wilk’s test

#ARIMA(2,1,4)
qqnorm(residuals(mod12)); qqline(rstandard(mod12))
hist(rstandard(mod12),xlab ="Histogram of Residuals")
shapiro.test(residuals(mod12))

#ARIMA(3,1,1)
qqnorm(residuals(mod5)); qqline(rstandard(mod5))
hist(rstandard(mod5),xlab ="Histogram of Residuals")
shapiro.test(residuals(mod5))

#ARIMA(2,1,2)
qqnorm(residuals(mod10)); qqline(rstandard(mod10))
hist(rstandard(mod10),xlab ="Histogram of Residuals")
shapiro.test(residuals(mod10))


##Step3 - 4
# Extract fitted values and residuals
fitted_values <- fitted(mod5)

# Plot the original dataset, fitted values, and residuals
plot(Yt, type = "l", col = "blue", ylim = range(Yt, fitted_values))
lines(fitted_values, col = "red")

# Add legend
legend("topright", legend = c("Original", "Fitted"), col = c("blue", "red"), lty = 1)


##Step4 - 1
##forecasting

#When h = 10
pred <- predict(mod5,10)

plot.ts(Yt,xlim =c (0 ,120), main = '10 - step prediction')

#Plotting the forecasted data with 95% CI
lines(pred$pred,lty=2,lwd=2)
lines(pred$pred-1.96*pred$se,lty=3,lwd=2)
lines(pred$pred+1.96*pred$se,lty =3,lwd =2) 

#When h = 25
pred <- predict(mod5,25)

plot.ts(Yt,xlim =c (0 ,125), main = '25 - step prediction')

#Plotting the forecasted data with 95% CI
lines(pred$pred,lty=2,lwd=2)
lines(pred$pred-1.96*pred$se,lty=3,lwd=2)
lines(pred$pred+1.96*pred$se,lty =3,lwd =2)




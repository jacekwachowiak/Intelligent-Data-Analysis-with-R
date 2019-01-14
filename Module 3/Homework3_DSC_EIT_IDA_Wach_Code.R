library('xlsx')
library("forecast")
library("fpp")
library("fpp2")
library("astsa")
library("tseries")
library(moments)
library(nortest)
library(e1071)
library(nlreg)
data = read.xlsx("data_g5.xlsx", sheetIndex = 1)
myts <- ts(data[,2], start=c(1990, 1), end=c(2007, 12), frequency=12)
# 1 plot
plot(myts)
# trend/seasonality/stationarity?
# no easy trend or seasonality, some sine shape can be seen, maybe cyclic every 10+ years

# 2 decomp, 
myts_stl = stl(myts, s.window="periodic")
plot(myts_stl)
myts_dec = decompose(myts) #type additive
plot(myts_dec) #the same plot as before
# trend not linear or any other simple function, seasonal component every year, remainder surely not white noise - decreasing in amplitude
myts_for = forecast(myts_stl)
plot(myts_for)
# forecast almost stable, low increase, does not look really like the previous shape
myts_acf = acf2(myts)
# ACF almost 1 at the beginning, going down steadily - non stationary, should be differenced to appply ARMA
# PACF almost 1 at the beginning, much above the limit, again the same conclusions, plus seasonality

# 3a
par(mfrow=c(1,2))
plot1=plot(myts)
plot2=plot(log(myts))
par(mfrow=c(1,2))
tsdisplay(myts)
tsdisplay(log(myts), max.lag = 20)

# not good enough, still above the limits, no big change, let's work on original
# additionally for arima - log transf works better for multiplicative, we have additive

# 3b
tsdisplay(myts, plot.type="spectrum")
# seasonal=yes
monthplot(myts)
# no

# 3c
# d and D, lets take diffs
par(mfrow=c(1,4))
plot(myts)
plot(diff(myts))
plot(diff(myts, 12))
plot(diff(diff(myts, 12)))
sd(myts)
sd(diff(myts))
sd(diff(myts, 12))
sd(diff(diff(myts, 12)))
# best result = diff(myts) - first-order difference - lowest standard deviation
acf2(myts)
acf2(diff(myts)) #still best
acf2(diff(myts, 12))
acf2(diff(diff(myts, 12)))
# some tests p should be <0.05
adf.test(diff(myts))
pp.test(diff(myts))
kpss.test(diff(myts))
# without changes
adf.test(myts)
pp.test(myts)
kpss.test(myts)
# automatic detection of necessary diffs
ndiffs(myts) #first differences = 1 d
nsdiffs(myts) #seasonal differences = 0 D
# looks ok
# d=1 non-seasonal differences

# 3d
# p and q, P and Q for seasonal
#p=autoregressive terms
#q=moving-average terms

#auto attempt
# on myts
arima_auto=auto.arima(myts,d=1,max.p=10,max.q=10)
summary(arima_auto)
tsdiag(arima_auto)
checkresiduals(arima_auto)
#automodel says
# ARIMA(2,1,1)(2,0,1)[12] 
# 
# Coefficients:
#   ar1     ar2      ma1     sar1     sar2    sma1
# 0.8473  0.0453  -0.6688  -1.2161  -0.3549  0.6251
# s.e.  0.0024  0.0032   0.0032   0.0029   0.0028     NaN
# 
# sigma^2 estimated as 0.2358:  log likelihood=-148.8
# AIC=311.61   AICc=312.15   BIC=335.2
# 
# Training set error measures:
#   ME      RMSE       MAE         MPE     MAPE      MASE        ACF1
# Training set -0.01427911 0.4776134 0.3199109 -0.07991563 2.061981 0.1248075 -0.03094282
#
#but lets try from 0
model.1=Arima(myts,order=c(0,1,0))
summary(model.1)
tsdiag(model.1)
checkresiduals(model.1)
#bad
model.2=Arima(myts,order=c(1,1,0))
summary(model.2)
tsdiag(model.2)
checkresiduals(model.2)
# aic not much better, res yes
model.3=Arima(myts,order=c(1,1,1))
summary(model.3)
tsdiag(model.3)
checkresiduals(model.3)
#still bad, res better
model.4=Arima(myts,order=c(2,1,1))
summary(model.4)
tsdiag(model.4)
checkresiduals(model.4)
#no change, res better
model.5=Arima(myts,order=c(1,1,2))
summary(model.5)
tsdiag(model.5)
checkresiduals(model.5)
#again no change
#last try with only one part of arima
model.6=Arima(myts,order=c(2,1,2))
summary(model.6)
tsdiag(model.6)
checkresiduals(model.6)
#worse res
#lets try once more
model.7=Arima(myts,order=c(3,1,3))
summary(model.7)
tsdiag(model.7)
checkresiduals(model.7)
#better aic, residuals not
# two candidates (2,1,1) and (1,1,2)
#introducing second brackets
model.8=Arima(myts, order=c(2,1,1), seasonal = list(order=c(1,0,0),period=12))
summary(model.8)
tsdiag(model.8)
checkresiduals(model.8)
#AIC improved 308, res improved, rmse 0.48
model.9=Arima(myts, order=c(2,1,1), seasonal = list(order=c(0,0,1),period=12))
summary(model.9)
tsdiag(model.9)
checkresiduals(model.9)
# better acf 296 and rmse 0.45, worse residuals, let's combine
model.10=Arima(myts, order=c(2,1,1), seasonal = list(order=c(1,0,1),period=12)) #GOOD
summary(model.10)
tsdiag(model.10)
checkresiduals(model.10)
# aic 296, rmse 0.45, res a bit better, lets continue
#lets try with the second model
model.11=Arima(myts, order=c(1,1,2), seasonal = list(order=c(1,0,1),period=12))
summary(model.11)
tsdiag(model.11)
checkresiduals(model.11)
# aic 296, rmse 0.45, res no difference at all
model.12=Arima(myts, order=c(2,1,1), seasonal = list(order=c(2,0,1),period=12))
summary(model.12)
tsdiag(model.12)
checkresiduals(model.12)
# aic 312, rmse 0.48, res worse - automatic model is outperformed
model.13=Arima(myts, order=c(2,1,1), seasonal = list(order=c(1,0,2),period=12)) #GOOD - we select this one
summary(model.13)
tsdiag(model.13)
checkresiduals(model.13)
#checking the correlation
cov2cor(model.13$var.coef)
# aic 286, rmse 0.44, res - only one above acf limit
# once again the second model from the first part
model.14=Arima(myts, order=c(1,1,2), seasonal = list(order=c(1,0,2),period=12))
summary(model.14)
tsdiag(model.14)
checkresiduals(model.14)
# minimally worse in residuals distribution
# increasing Q
model.15=Arima(myts, order=c(2,1,1), seasonal = list(order=c(1,0,3),period=12)) #GOOD but not worth it
summary(model.15)
tsdiag(model.15)
checkresiduals(model.15)
# aic 284, rmse 0.43, Ljung-Box slightly better
# lets increase the last argument still
model.16=Arima(myts, order=c(2,1,1), seasonal = list(order=c(1,0,4),period=12))
summary(model.16)
tsdiag(model.16)
checkresiduals(model.16)
# aic 285, rmse 0.43, acf better for small results but two too big
#final model = model.13
#args pdq=(2,1,1), PDQ=(1,0,2) eventually PDQ=(1,0,1) cos change is slight

# 3e
#correlations
acf2(model.13$residuals)
Box.test(model.13$residuals, lag=12, fitdf=6) # autocorrelation for the lag=12 found, pvalue=0.039
#zero mean
checkresiduals(model.13) # last graph shows a nice normal distribution with no significant skeweness
mean(model.13$residuals) #-0.012 - very small, lets consider it as zero
#normality
shapiro.test(model.13$residuals) #pvalue=2*10^-11 really small, not normally distributed
qqnorm(model.13$residuals) 
qqline(model.13$residuals) #some extreme values but that can still mean normal distribution - test gives a better answer
ad.test(model.13$residuals) #another test saying: not normally distributed

# 3f
#forecasts
par(mfrow=c(1,2))
plot(forecast(model.13, h=24)) #2y
plot(myts_for) #comparison to without model

# 3g done earlier



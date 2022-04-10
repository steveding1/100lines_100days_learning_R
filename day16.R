# understand garch
library(fpp)
library(forecast)
library(quantmod)
library(fGarch)
data= hsales
plot(data)
?hsales 
#Monthly sales of new one-family houses sold in the USA since 1973.

#ADF Test is for testing stationary or not
adf.test(data)
transData=(diff(data))
adf.test(transData)
acf(data)
pacf(data)
SeasonalDiff=diff(data,12,lag.max=50)
acf(SeasonalDiff)
pacf(SeasonalDiff)
tsdisplay(transData)
acf(transData,lag.max=150)
acf(data,lag.max=150)

auto.arima(data, stationary = TRUE, seasonal = TRUE, stepwise=FALSE, approximation=FALSE, max.p= 3, max.q = 3, max.P=3, max.Q=3, max.d = 1, max.D = 1, max.order = 6, parallel = TRUE, allowmean = FALSE, test="kpss")
#ARIMA(1,0,0)(3,0,0)[12] with zero mean 
#AIC=1683.65   AICc=1683.88   BIC=1701.74
model2=arima(data,order=c(1,0,0),seasonal=list(order=c(2,1,0),period=12),method='ML')
summary(model2)
# aic = 1614.89
tsdiag(model2)
Box.test(model2$residuals)
adf.test(model2$residuals)
plot.ts(model2$residuals)
pred=forecast(model2, h=12)
print(pred)
plot(pred, include=10)

# generate a simulated AR(3)-GARCH(2,2)
spec = garchSpec(
  model = list(omega = 0.001, 
               ar = c(0.5,0.2,-0.1), 
               alpha = c(0.3, 0.2), 
               beta = c(0.2,0.1)
  ))
process = garchSim(spec, n = 500)
plot(process)

acf(process)
pacf(process)
process_squared <- process^2
acf(process_squared)
pacf(process_squared)

#Fit an ARCH(3) model
model1 <- garchFit(
  formula = ~ garch(3,0) , 
  data = process, 
  trace = F)
summary(model1)
#Fit the correct AR(3)-GARCH(2,2) model:
model2 <- garchFit(
  formula = ~ arma(3,0) + garch(2,2) , 
  data = process, 
  trace = F)
summary(model2)

#Simulate 500 points of a GARCH(1,1) 
spec = garchSpec(
  model = list(alpha = c(0.3), 
               beta = c(0.2)
  ))
process = garchSim(spec, n = 500)
plot(process)

acf(process^2)

install.packages('readxl','vars','timeSeries')

#####################VAR########################
library(readxl)
library(vars)
library(timeSeries)

eurodollarrates <- read_excel(
  "D:/learn/r/100days/eurodollarrates.xlsx",
  sheet = "Weekly,_Ending_Friday")
d_1_month_rate <- diff(eurodollarrates$WED1,trim=TRUE)
d_3_month_rate <- diff(eurodollarrates$WED3,trim=TRUE)
d_6_month_rate <- diff(eurodollarrates$WED6,trim=TRUE)
d_rates <- cbind(d_1_month_rate,d_3_month_rate,d_6_month_rate)

cor(d_rates) #all >= 0.67

VAR_model <- VAR(d_rates, lag.max=12, type = "none", ic = "AIC")
summary(VAR_model)

#Impulse response function
var_irf <- irf(VAR_model, n.ahead = 13,boot = TRUE, ci = 0.95)
plot(var_irf)

# compute and plot the forecast error variance decomposition
VAR_fevd <- fevd(VAR_model,n.ahead = 13)
plot(VAR_fevd)

# obtain residuals:
resids = residuals(VAR_model)
resid1 = resids[,1]
resid2 = resids[,2]
# view cross correlogram:
ccf(resid1, resid2, lag.max = 13, type = "correlation", plot = TRUE)

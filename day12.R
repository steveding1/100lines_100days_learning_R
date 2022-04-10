# learning R: 100 lines of code * 100 days
# day 12: 
# MScFE 610 Econometrics
# MODULE 3: UNIVARIATE TIME SERIES MODELS
# ningding100@gmail.com

?arima.sim
set.seed(1)
simulated_x <- arima.sim(n=2000, model = list(ar=0.45, ma=-0.45))
plot(simulated_x)
acf(simulated_x)
pacf(simulated_x)

library(readxl)
Au_Resid_Pri_Ind <- 
  cbind.data.frame(
    read_excel("MScFE 610 ECON_Data M3.xlsx", 
               sheet = "Data1", 
               col_names = c("Date"),
               range = 'A11:A55'),
    read_excel("MScFE 610 ECON_Data M3.xlsx", 
               sheet = "Data1", 
               col_names = c("CapitalsInx"),
               col_types = "numeric",
               range = 'J11:J55')
)
Au_Resid_Pri_Ind$Date <- as.Date(Au_Resid_Pri_Ind$Date)
View(Au_Resid_Pri_Ind)
?plot
plot(Au_Resid_Pri_Ind,type='l',xlab='Figuer 8',ylab='',
     main="Residential Property Price Index Austrlia 2014\n Weighted average of eight capital cities",
     sub='Source: Australian Bureau of Statistics, our analysis')
#Apply natural logarithm to the data
Ln_Return <- 
  diff(log(Au_Resid_Pri_Ind$CapitalsInx),lag = 1)
plot(Ln_Return)
sum(Ln_Return>0)/length(Ln_Return)
#Apply the first difference in order to obtain stationarity
Ln_Return_Diff <- 
  diff(Ln_Return,lag = 1)
plot(Ln_Return_Diff)
sum(Ln_Return_Diff>0)/length(Ln_Return_Diff)

library(tseries)
library(stats)
library(forecast)
acf(Ln_Return,main='ACF')
pacf(Ln_Return,main='PACF')

fit <- auto.arima(
  Au_Resid_Pri_Ind$CapitalsInx,
  stationary = TRUE,
  seasonal = FALSE, 
  trace=TRUE)

plot(forecast(fit,h=20))
plot(decompose(Au_Resid_Pri_Ind$CapitalsInx))

#model the US CPI inflation rate in compounded annual rate of change
CPIAUCSL <- read_excel(
  "CPIAUCSL.xls", 
  range = "A11:B637",
  col_types = c("date", "numeric")
  )
View(CPIAUCSL)
colnames(CPIAUCSL)

library(ggplot2)
ggplot(CPIAUCSL,aes(x=observation_date,y=CPIAUCSL))+
  geom_line(col='darkblue') +
  scale_x_continuous(breaks = pretty((CPIAUCSL$observation_date),n = 15))
# Convert the data frame to a time series object
CPI_percent_change <- 
  ts(CPIAUCSL$CPIAUCSL, 
     start=1970, 
     frequency=12, 
     end = 2013)
# Truncate the time series object
CPI_percent_change_truncated <- 
  window(CPI_percent_change, 
         start = 1992, 
         end = 2013)
# construct the ACF for the full and truncated sample
Acf(CPI_percent_change)
Acf(CPI_percent_change_truncated)

Pacf(CPI_percent_change)
Pacf(CPI_percent_change_truncated)

adf.test(CPI_percent_change)
adf.test(CPI_percent_change_truncated)

kpss.test(CPI_percent_change)
kpss.test(CPI_percent_change_truncated)

# fit a simple AR model with 12 lags, no differencing, no moving average terms 
#??? i.e. an ARIMA(12,0,0) model:
  AR_model1 <- arima(window(CPI_percent_change,start=1990,end = 2013),
                     order=c(12,0,0), method = "ML")
summary(AR_model1)
#Let???s check if it has white noise errors:
Box.test(AR_model1$residuals, lag = 12)

set.seed(1)
simulated_x<- arima.sim(n=2000, model=list(ar=0.45, ma=-0.45))
plot(simulated_x)
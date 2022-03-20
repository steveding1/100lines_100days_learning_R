# learning R: 100 lines * 100 days
# day 7: linear regression model
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
epsilon <- rnorm(100)*0.8# unrelated random variable 
y = 0.5 + 0.5*x1 - 0.3*x2 + 0.4*x3 + epsilon
regression = lm(formula = y~x1+x2+x3+x4)
summary(regression) #tvalue = estimate / std error

#Formulate the Fama-French regression 
library(tibble)
library(dplyr)
library(tidyquant)

#readcsv
fama_factors <- read.csv('fama_factors_2019.csv')
fama_factors <- tibble(fama_factors)
fama_factors$yyyymmdd <- as.Date(fama_factors$yyyymmdd,format = "%m/%d/%Y")

# Downloading Apple price using quantmod
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

getSymbols("AAPL", from = min(fama_factors$yyyymmdd),
           to = max(fama_factors$yyyymmdd),warnings = FALSE,
           auto.assign = TRUE)

#caculate daily returns
AAPL.return <- diff(AAPL$AAPL.Adjusted)/lag(AAPL$AAPL.Adjusted)
str(AAPL.return)
AAPL.return <- AAPL.return[!is.na(AAPL.return)]
length(AAPL.return)-length(fama_factors$yyyymmdd)

AAPL.return=cbind(yyyymmdd = index(AAPL.return),as.data.frame(AAPL.return))
AAPL.return=tibble(AAPL.return)
FFAAPL <- inner_join(fama_factors,tibble(AAPL.return),by="yyyymmdd")
lm(formula = (FFAAPL$AAPL.Adjusted-FFAAPL$RF)~FFAAPL$Mkt.RF+FFAAPL$SMB+FFAAPL$HML)

Y <- -0.007432 +FFAAPL$Mkt.RF*-0.002666+0.014745*FFAAPL$SMB+FFAAPL$HML*-0.001722+FFAAPL$RF
plot(AAPL.return$AAPL.Adjusted,col='red',type = "b",pch = 18,alpha=0.2)
lines(Y, col = "blue", type = "b", lty = 2)
legend("bottomright", legend=c("real", "FF model"),
       col=c("red", "blue"), lty = 1:2, cex=1)

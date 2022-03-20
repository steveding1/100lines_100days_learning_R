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
fama_factors <- tibble(fama_factors)
fama_factors$yyyymmdd <- as.Date(fama_factors$yyyymmdd,format = "%m/%d/%Y")

# Downloading Apple price using quantmod
library(tidyquant)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

getSymbols("AAPL", from = min(fama_factors$yyyymmdd),
           to = max(fama_factors$yyyymmdd),warnings = FALSE,
           auto.assign = TRUE)
length(AAPL$AAPL.Adjusted)==length(fama_factors$yyyymmdd)
library(dplyr)
typeof(AAPL)
str(as.data.frame(AAPL)["AAPL.Adjusted"])
AAPL=cbind(yyyymmdd = index(AAPL),as.data.frame(AAPL)["AAPL.Adjusted"])
FFAAPL <- inner_join(fama_factors,tibble(AAPL),by="yyyymmdd")
lm(formula = (FFAAPL$AAPL.Adjusted-FFAAPL$RF)~FFAAPL$Mkt.RF+FFAAPL$SMB+FFAAPL$HML)

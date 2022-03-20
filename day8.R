# learning R: 100 lines * 100 days
# day 8: Formulate the Fama-French regression 
library(tibble)
library(dplyr)
#library(tidyverse)
library(purrr)
library(timetk)
library(broom)

#readcsv
fama_factors <- 
  read.csv('fama_factors_2019.csv') %>% 
  tibble()
names(fama_factors)[1] <- "date"
fama_factors$date <- as.Date(fama_factors$date,format = "%m/%d/%Y")
fama_factors[,2:5] = fama_factors[,2:5]/100

# Downloading Apple price using quantmod
#http://www.reproduciblefinance.com/2017/09/25/asset-prices-to-log-returns/
#https://rviews.rstudio.com/2018/04/11/introduction-to-fama-french/
AAPL <- 
  getSymbols("AAPL", 
             from = min(fama_factors$date),
             to = max(fama_factors$date),
             warnings = TRUE,
             auto.assign = TRUE) %>% 
  #To isolate the adjusted price, we use the map function from the purrr package and apply Ad(get(.)) to the imported prices. This will ???get??? the adjusted price from each of our individual price objects.
  map(~Ad(get(.))) %>% 
  #The map function returns a list by default.
  #reduce(merge) function will allow us to merge the lists into one object and coerce back to an xts structure.
  reduce(merge) %>% 
  `colnames<-`('AAPL')

#caculate daily returns
AAPL.return <- 
  Return.calculate(AAPL, method = "log") %>% #A xts
  na.omit() %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  tibble()

FFAAPL <- inner_join(fama_factors,AAPL.return,by="date")

model <- 
  lm(
    formula = 
      (FFAAPL$AAPL-FFAAPL$RF)~
      FFAAPL$Mkt.RF+
      FFAAPL$SMB+
      FFAAPL$HML)

summary(model)
qqnorm(model$resid)
qqline(model$resid)

Y <- model$coefficients['(Intercept)']  + 
  model$coefficients['FFAAPL$Mkt.RF'] * FFAAPL$Mkt.RF - 
  model$coefficients['FFAAPL$SMB'] * FFAAPL$SMB+
  model$coefficients['FFAAPL$HML'] * FFAAPL$HML+
  FFAAPL$RF
plot(AAPL.return$AAPL,col='red',type = "b",pch = 18,alpha=0.2)
lines(Y, col = "blue", type = "b", lty = 2)
legend("bottomright", legend=c("AAPL log return", "FF model simulation"),
       col=c("red", "blue"), lty = 1:2, cex=1)

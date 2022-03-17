# learning R: 100 lines of code * 100 days
# day 5: 
# ningding100@gmail.com
#Understanding Q-Q Plots https://data.library.virginia.edu/understanding-q-q-plots/
# a standard Normal distribution from 0.01 to 0.99 by increments of 0.01:
plot(qnorm(seq(0.01,0.99,0.01)),seq(0.01,0.99,0.01))
# a sample of size 200 and find the quantiles for 0.01 to 0.99
quant.rnorm <- quantile(rnorm(200),probs = seq(0.01,0.99,0.01))
typeof(quant.rnorm)
plot(quant.rnorm)
?qqnorm
qqnorm(quant.rnorm)

#Q-Q plots take your sample data, sort it in ascending order, 
#and then plot them versus quantiles calculated from a theoretical distribution. 
#Data trees provides measurements of the girth, height and volume of timber in 31 trees.
trees
qqnorm(trees$Height)
?randu
install.packages('plotly')
library(plotly)
colnames(randu)
plot_ly(x=randu$x,y=randu$y,z=randu$z,type="scatter3d", mode="markers", color=randu$z)
y <- qunif(ppoints(length(randu$x)))
y=qunif(ppoints(length(randu$x)))
#qqplot produces a QQ plot of two datasets.
qqplot(x=randu$x,y=y)
?qchisq
plot(qchisq(ppoints(30),df=3))
qqplot(qnorm(ppoints(30)), qchisq(ppoints(30),df=3))
#“heavy tails” qcauchy

FinData <- read_excel("D:/MScFE 610 ECON_Data M1 and M2.xlsx", 
                      col_types = c("date", "numeric", "numeric"
                                    , "numeric", "numeric", "numeric"
                                    ,"numeric", "numeric", "numeric"))

ggplot(data = FinData,aes(x=Date,y=MSFT)) +
  geom_line(col='blue')

#quadratic graph y=1/x
x=seq(1,12)
plot(x,1/x,type = "b")
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width 
?plot
#reciprocal graph

#calculate AAPL expect return using CAPM.
install.packages('tidyquant')
library(tidyquant)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("AAPL", from = '2021-03-11',
           to = "2022-03-10",warnings = FALSE,
           auto.assign = TRUE)

return.aapl=
  (tail(AAPL$AAPL.Adjusted,1)[[1]]-AAPL$AAPL.Adjusted[[1]])/
  AAPL$AAPL.Adjusted[[1]]

getSymbols("SPY", from = '2021-03-11',
           to = "2022-03-10",warnings = FALSE,
           auto.assign = TRUE)

return.spy=
  (tail(SPY$SPY.Adjusted,1)[[1]]-SPY$SPY.Adjusted[[1]])/
  SPY$SPY.Adjusted[[1]]

#Rf+βi(ERm−Rf)
Rf = 0.78/100
Er = Rf+(return.spy-Rf)*1.19#AAPL's BETA
cat('Expected AAPL one year return is (should be larger than)', 
    Er * 100,  "%")

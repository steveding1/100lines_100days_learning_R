chooseCRANmirror()
install.packages("tidyverse") # for all modules
install.packages("tseries") # for module 3
install.packages("forecast") # for most modules
install.packages("fGarch") # for module 4
install.packages("vars") # for module 5
install.packages("evir") # for module 6
install.packages("copula") # for module 7

set.seed(10)
norm03 = sort(rnorm(30,0,3))
barplot(norm03)
hist(norm03)
plot(norm03)
set.seed(10)
norm11=rnorm(30,1,1)
barplot(sort(norm11))
hist(norm11)
plot(norm11)

#rnorm(n, mean = , sd = ) vs runif(n, min = , max = ) -uniform
set.seed(10)
hist(rnorm(100,0,1))
install.packages("ggplot2")
library("ggplot2")
set.seed(10)
x <- data.frame(rnorm(100,0,1))
colnames(x) <- 'x'
names(x)
ggplot(x, aes(x = x))+
  geom_density(fill='orange')
set.seed(10)
y <- data.frame(runif(100,-1,1))
colnames(y) <- 'y'
names(y)
ggplot(y, aes(x = y))+
  geom_density(fill='red')

a=rnorm(3)
b=rnorm(3)
c=cbind(a,b)
c+2
c*c
#transpose and inner product
t(c)%*%c 
c%*%t(c)

FinData <- read_excel("D:/MScFE 610 ECON_Data M1 and M2.xlsx", 
                      col_types = c("date", "numeric", "numeric"
                                    , "numeric", "numeric", "numeric"
                                    ,"numeric", "numeric", "numeric"))

msft_std = (FinData$MSFT_lr - mean(FinData$MSFT_lr))/sd(FinData$MSFT_lr)
  
qqnorm(msft_std,main = "Normal QQ plot",plot.it = 1, datax = 1)
qqline(msft_std,datax = 0,distribution = qnorm, probs = c(0.2,0.8),qtype = 7)

library(tidyverse)
# view time plots of the closing price of Microsoft:
ggplot(data = FinData) +
  geom_point(mapping = aes(x = Date,y = MSFT),
             color = "darkblue") +
  labs(x = "year", y = "Microsoft - Daily Closing Price")

# generate plots for each of the log return graphs of the four assets
ggplot(data = FinData) +
  geom_line(mapping = aes(x=Date,y=MSFT_lr),col='red') +
  labs(x='years',y='MSFT - Log Daily Return')

ggplot(data = FinData)+
  geom_line(mapping = aes(x = Date,y = INTC_lr),
           color ="darkgreen")
labs(x = "year",y = "Intel - Log Daily Return")

ggplot(data = FinData)+ 
  geom_line(mapping = aes(x = Date, y = IBM_lr),
            color = "darkcyan")
+labs(x = "year",y = "IBM - Log Daily Return")

plot(1:12,log(1:12,base=exp(1)))
plot(1:12,1/(1:12), lwd = 2,"b",xlab = "")

#ln() in library("SciViews")
max(FinData$Date)
min(FinData$Date)
Msft <- FinData$MSFT
install.packages('tseries')
library(tseries)
sharpe(Msft, r = 0.001, scale = sqrt(252)) #-2.185062
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
#annualized return - risk free return / annualized volatility
((tail(Msft,1)/Msft[1])^(1/11)-1-0.001)/
    (sqrt(252)*sd(((shift(Msft,1)-Msft)/Msft)[1:2767]))

std <- function(v){
  return (sqrt(sum((v-mean(v))^2/(length(v)-1))))
}
std(((shift(Msft,1)-Msft)/Msft)[1:2767])

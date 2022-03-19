#found an awsome site for R books https://bookdown.org/
a <- dnorm(3)
rnorm(3)
?dnorm
a = c(1,2,3)
b = c(4,5,6)
A = cbind(a,b)
A%*%t(A)
c(12, 5, 27)[a=1]

library(tidyverse)
#statistical transformation
ggplot(diamonds)+
  stat_count(aes(cut))
# every geom has a default stat
ggplot(diamonds)+
  geom_bar(aes(cut))
#geom_bar did an auto value counts
barplot(table(diamonds$cut))
#override with the proportion
ggplot(data = diamonds) + 
  geom_bar(aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median)

?stat_bin

ggplot(diamonds)+
  geom_pointrange(
    aes(x=cut,y=depth),
    stat = 'summary'
    )

#What does geom_col() do? How is it different to geom_bar()?
ggplot(data = diamonds) + 
  geom_col(aes(x=color,y=x))#y represent the bar height.
ggplot(data = diamonds) + 
  geom_bar(aes(x=color))#stat_count

colnames(diamonds)
#What variables does stat_smooth() compute? What parameters control its behavior?
ggplot(cars,aes(speed,dist))+
  geom_point()+
  stat_smooth(na.rm=TRUE)

#heights of the bars need to be normalized.
ggplot(data = diamonds) +
  geom_bar(aes(x = cut, y = ..count.. / sum(..count..), fill = color))
#position = "identity" will place each object exactly where it falls in the context of the graph.
for (posi in c('identity','dodge','fill')){
  ggplot(data = diamonds) + 
    geom_bar(aes(x = cut, fill = color,alpha = 1/5), position = posi)
}

for(i in 1:10){print(i)}
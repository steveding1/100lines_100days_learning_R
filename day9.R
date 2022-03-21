#Future value with simple interest)
V = 1000 
R = 0.03 
years = c(1,5,10)
V*(1+R)^years
#the rule of 70 - the number of years to double
Rates = seq(0.01,0.1,0.01)
nRule70 = 0.7/Rates
nExact = log(2)/log(1+Rates)
cbind(rates,nExact,nRule70)
#Multiple compounding periods
V = 1000 
R = 0.1 
m = c(1, 2, 4, 356, 10000) 
Fv=V*(1+R/m)^m
cbind(m,Fv)
V*exp(R)

#3.6.1 Exercises
#draw a line chart? A boxplot? A histogram? An area chart?
library(ggplot2)
plot(iris[iris$Species=="setosa",]$Petal.Width,type='l')
ggplot(data=iris)+
  geom_dotplot(aes(x=Species,y=Petal.Width, fill = Species),
               binaxis = "y",binwidth = 0.05,
               stackdir = 'center',stackratio = 0.7)
ggplot(data=iris)+
  geom_boxplot(aes(x=Species,y=Petal.Width, fill = Species))

library(dplyr) #for using pipeline "%>%"
norm = 
  sort(rnorm(1000))%>%
  data.frame(.,c(1:1000))
colnames(norm)=c('value','index')
head(norm)
str(norm)
ggplot(data=(norm))+
  geom_area(aes(y=value,x=index))

hist(rnorm(1000))
ggplot(data=(norm))+
  geom_histogram(aes(x=value))

ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy, color = drv),
       show.legend = FALSE) + 
  geom_point() + 
  geom_smooth(se = FALSE)#standard error bands to the lines.

ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy))+
  geom_point(size=5)+
  geom_smooth(se=0,aes(group = drv))
      

ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy))+
  geom_point(aes(col=drv),size=3)+
  geom_smooth(se=0,aes(linetype=drv))

ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy))+
  geom_point(col="white",size=5)+
  geom_point(aes(col=drv),size=2)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))
ggplot(data = diamonds) + 
  stat_summary(mapping = aes(x = cut,y=depth),
               fun.min = min,
               fun.max = max,
               fun = mean)
ggplot(data = diamonds) + 
  geom_pointrange(
    mapping = aes(x = cut,y=depth),
    stat = "summary",
    fun.min = min,
    fun.max = max,
    fun = mean)

ggplot(data = diamonds) + 
  geom_bar(aes(x = cut))
ggplot(data = diamonds) + 
  geom_col(aes(x = cut,y=1))

ggplot(data = diamonds) + 
  geom_bar(aes(x = cut,y=..count../sum(..count..),fill=color))
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)
             , position = "jitter"
             )
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position = 'jitter',hight=0)

ggplot(data = mpg, 
       mapping = aes(x = cty, y = hwy, color = class),
       alpha=0.3) +
  geom_count()

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")+
  coord_quickmap()


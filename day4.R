# learning R: 100 lines * 100 days
# day 2: 
# R for Data Science - 3 Data visualisation 
# https://r4ds.had.co.nz/data-visualisation.html

#library(tidyverse)
#parkage::function() like python's package.function
ggplot2::ggplot()
#Fuel economy data from 1999 to 2008 for 38 popular models of cars
?mpg
unique(mpg$year)
# only two different year. we can use 2 shapes to differentiate points
ggplot(data = mpg,mapping = aes(x=displ,y=hwy),legend()) + 
  geom_point(shape=mpg$year-1993,col=c('dark red')) +
  geom_smooth(method = "gam") + #"lm", "glm", "gam", "loess"
  theme_light() +
  labs(title = 'Engine size vs Fuel efficiency.',
       x="engine displacement (litres)",
       y="highway miles per gallon")
?geom_smooth

#Run ggplot(data = mpg). What do you see?
ggplot(data = mpg)

#How many rows are in mpg? How many columns?
nrow(mpg) 
ncol(mpg) 
glimpse(mpg)
str(mpg)

#What does the drv variable describe? Read the help for ?mpg to find out.
?mpg #the type of drive train, where f = front-wheel drive, r = rear wheel drive, 4 = 4wd

#Make a scatterplot of hwy vs cyl.
ggplot(data = mpg,mapping = aes(x=hwy,y=cyl))+
  geom_point()
#What happens if you make a scatterplot of class vs drv? 
#Why is the plot not useful?
#both are categorical variables.
ggplot(data = mpg,mapping = aes(x=class,y=drv))+
  geom_point()
ggplot(data = mpg,mapping = aes(x=class,y=drv))+
  geom_count()

mpg %>%
  count(class,drv) %>%
  complete(class, drv, fill = list(n = 0)) %>%
  ggplot(aes(x=class,y=drv)) + geom_tile(aes(fill=n))
#"2seaters" fall outside of the linear trend. 
?stat_ellipse
ggplot(data = mpg,mapping = aes(x=displ,y=hwy,color=class))+
  geom_point() + 
  stat_ellipse(type = "t",
               level = 0.88,
               segments = 51,
               show.legend=FALSE)
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) 

ggplot(mpg) + 
  geom_point(aes(x=displ,y=hwy)) +
  facet_wrap(~class,nrow=2)
ggplot(mpg) + 
  geom_point(aes(x=displ,y=hwy)) +
  facet_grid(.~class)

#using 2 variable facet
ggplot(mpg) + 
  geom_point(aes(x=displ,y=hwy)) +
  facet_wrap(drv~cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#What happens if you facet on a continuous variable?
ggplot(mpg, aes(x = displ, y = hwy)) +
geom_point() +
  facet_grid(. ~ cty)

#What are the advantages to using faceting instead of the colour aesthetic? 
#clear - easy to compare within categories, not feasable using more than 6-8 colors
#What are the disadvantages? How might the balance change if you had a larger dataset?
#not comparable betweencategories
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,color=class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg,mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(mpg,aes(x = displ, y = hwy))+
  geom_point(aes(col=drv))+
  geom_smooth(data = filter(mpg,class=="subcompact"),se=FALSE)

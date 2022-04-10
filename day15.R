#examine the distribution of a categorical variable, use a bar chart
library(tidyverse)
library(ggplot2)
diamonds
ggplot(diamonds)+
  geom_bar(mapping=aes(x=cut))

count(diamonds,cut)
diamonds %>% 
  count(cut)

#examine the distribution of a continuous variable, use a histogram:
diamonds
ggplot(diamonds)+
  geom_histogram(aes(carat))

diamonds %>% 
  count(cut_width(carat,0.5))

# for many in the same plot using geom_freqpoly()
ggplot(diamonds)+
  geom_freqpoly(aes(carat,col=cut),bandwidth=0.1)

smaller = diamonds%>%
  filter(carat<3)

ggplot(smaller,aes(carat))+
  geom_histogram(binwidth=0.01)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)+
  coord_cartesian(ylim=c(0,10))

unusual <- diamonds %>% 
  filter(y>15|y<3) %>% 
  select(price,x,y,z) %>% 
  arrange(y)

summary(select(diamonds,x,y,z))

ggplot(diamonds, aes(x = x, y = z)) +
  geom_point()+
  geom_smooth()

diamonds %>%
  filter(carat >= 0.9, carat <= 1.1) %>%
  count(carat) %>%
  print(n=Inf)

diamonds %>%
  filter(carat >= 0.7, carat <= 1.1) %>% 
  ggplot(aes(carat))+
  geom_bar()

diamonds2 <- 
  diamonds %>% 
  mutate(y = ifelse(y<0|y>20,NA,y))

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    hour=sched_dep_time %/%100,
    min=sched_dep_time %%100,
    dep_time=hour + min / 60
  ) %>% 
  ggplot(aes(x=dep_time,color=cancelled))+
  geom_freqpoly(binwidth = 1/4)

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = mpg) + 
  geom_boxplot(aes(reorder(class,hwy,median),hwy))+
  coord_flip()

colnames(nycflights13::flights)
nycflights13::flights %>%
  mutate(
    cancelled=is.na(dep_time),
    hour=sched_dep_time%/%100,
    min=sched_dep_time%%100,
    dep_time=hour+min/60
  ) %>% 
  ggplot()+
  geom_boxplot(aes(y=dep_time,x=cancelled))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), orientation = "x")

diamonds %>%
  mutate(color = fct_rev(color)) %>%
  ggplot(aes(x = color, y = price)) +
  geom_boxplot()

diamonds %>%
  ggplot(aes(x = clarity, y = price)) +
  geom_boxplot()

install.packages('lvplot')
library(lvplot)
ggplot(diamonds, aes(x = cut, y = price,fill=cut)) +
  geom_lv()

ggplot(diamonds, aes(x = cut, y = price,fill=cut)) +
  geom_violin()

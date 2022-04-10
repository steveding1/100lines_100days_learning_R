library(nycflights13)
library(tidyverse)
colnames(flights)

ymd <- 
flights %>% 
  select(c("year","month","day" )) %>% 
  filter(month==1|month==3) %>% 
  distinct(year,month,day) 

lead(ymd,2)$day+lag(ymd,2)$day

ranks <- 
ymd %>% 
  group_by(c("month")) %>% 
  mutate(rank=dense_rank(day)) %>% 
  arrange(rank)
view(ranks)

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>% 
  mutate(x_mean=mean(x)) %>% 
  group_by(group) %>% 
  mutate(x_grp_mean=mean(x)) 

summary(ranks)

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>% 
  mutate(x_mean=mean(x)) %>% 
  group_by(group) %>% 
  mutate(csum=cumsum(x),cmean=cummean(x))

#average delay minutes per tailnum that flew >= 20 flights
flights %>% 
  filter(!is.na(tailnum)&(arr_delay>=0)) %>%
  group_by(tailnum) %>% 
  summarise(arr_delay_m = mean(arr_delay),n=n()) %>% 
  filter(n>=20) %>% 
  arrange(desc(arr_delay_m)) %>% 
  head(3)
  
#the proportion of not-cancelled and on-time flights.
flights %>% 
  filter(!is.na(tailnum)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>% 
  summarise(on_time=mean(on_time),n=n()) %>% 
  filter(min_rank(on_time)==1)

quantile(count(flights,tailnum)$n)

#delays by hours 
flights %>% 
  filter(arr_delay>0) %>% 
  summarise(arr_delay=mean(arr_delay,na.rm=TRUE)) %>%
  arrange(arr_delay)

flights %>% 
  filter(str_detect(dest,'^O')) %>% 
  select('dest')

LAXORD <- flights %>% 
  filter(dest=='LAX'&origin=='JFK'&month==3&
           !is.na(carrier))

LAXORD %>% 
  ggplot(aes(x=hour,y=arr_delay))+
  geom_point(aes(color=carrier))+
  geom_jitter()

colnames(LAXORD)

diamonds %>% 
  count(cut_width(carat,0.5))

smaller <- 
  diamonds %>% 
  filter(carat<=3)

ggplot(smaller,aes(x=carat,color=cut))+
  geom_freqpoly(binwidth=0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

xplot=ggplot(diamonds,aes(x=x))+
  geom_histogram(bins=50)
yplot=ggplot(diamonds,aes(x=y))+
  geom_histogram(bins=50)
zplot=ggplot(diamonds,aes(x=z))+
  geom_histogram(bins=50)

install.packages('ggpubr')
library(ggpubr)
ggarrange(xplot,yplot,zplot)

diamonds1 <- 
  filter(
    diamonds,
    between(x,3,10)&
    between(y,3,10)&
    between(z,2,5.5)
      )
xplot=ggplot(diamonds1,aes(x=x))+
  geom_histogram(bins=100)
yplot=ggplot(diamonds1,aes(x=y))+
  geom_histogram(bins=100)
zplot=ggplot(diamonds1,aes(x=z))+
  geom_histogram(bins=100)
ggarrange(xplot,yplot,zplot)


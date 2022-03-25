library(nycflights13)
library(tidyverse)
colnames(flights)
summarise(flights, mewvarname = mean(dep_delay, na.rm = TRUE))

dep_delay1 <-
summarise(group_by(flights,year,month,day),
          dep_delay = mean(dep_delay, na.rm = TRUE))
dep_delay2 <- 
  summarise(group_by(flights,year,month,day),
            mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(
  by_dest,
  count=n(),
  dist=mean(distance,na.rm=TRUE),
  delay = mean(dep_delay, na.rm = TRUE)
)
delay <- filter(delay,dest!="HNL",count>20)

ggplot(data=delay,aes(x=dist,y=delay))+
  geom_point(aes(size=count),alpha=0.3)+
  geom_smooth(se=FALSE)

by_dest <- 
delay <- flights %>% 
  group_by(dest) %>% 
  summarise(.,
    count=n(),
    dist=mean(distance,na.rm=TRUE),
    delay = mean(dep_delay, na.rm = TRUE)
    ) %>% 
    filter(dest!="HNL",count>20)

ggplot(data=delay,aes(x=dist,y=delay))+
  geom_point(aes(size=count),alpha=0.3)+
  geom_smooth(se=FALSE)

delays <- flights %>% 
  group_by(tailnum) %>% 
  summarise(.,
            delay=mean(arr_delay,na.rm = TRUE)
            ) %>% 
  filter(!is.na(delay))
  
ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)


delays <- flights %>% 
  group_by(tailnum) %>% 
  summarise(.,
            delay=mean(arr_delay,na.rm = TRUE),
            counts=n()) %>% 
  filter(!is.na(delay))

ggplot(data = delays, 
       mapping = aes(x=counts,y = delay)) + 
  geom_point(alpha=0.2)
  
install.packages("Lahman")
library(Lahman)
batting <- as_tibble(Lahman::Batting)
batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H,na.rm = TRUE) / sum(AB,na.rm = TRUE),
    ab =  sum(AB, na.rm = TRUE)
  ) %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba))+
  geom_point()+
  geom_smooth(se=FALSE)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year,month,day) %>% 
  summarise(
    avg_delay1= mean(arr_delay),
    avg_delay2= mean(arr_delay[arr_delay>0]),
    med_delay= median(arr_delay[arr_delay>0])
  )


not_cancelled %>% 
  group_by(year,month,day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time)))%>% 
  filter(r %in% range(r))

# What proportion of flights are delayed by more than an hour?

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean(arr_delay>60))

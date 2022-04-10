library(nycflights13)
library(tidyverse)
colnames(flights)

flights$dep_time
transmute(
  flights,
  dep_hour=dep_time%/%100,
  dep_min=dep_time%%100)

colnames(flights)

rankme <- tibble(x=c(10,5,1,5,5,3,3))
rankedme <- 
mutate(rankme,
       row_num=row_number(x),
       min_rank=min_rank(x),
       dense=dense_rank(x))
arrange(rankedme,row_num)

flights_delayed <- 
  flights %>% 
  mutate(
         dep_delay_min_rank = min_rank(desc(dep_delay)),
         dep_delay_row_number = row_number(desc(dep_delay)),
         dep_delay_dense_rank = dense_rank(desc(dep_delay))
         ) %>% 
  select(.,starts_with("dep_delay")) %>% 
  arrange(.,dep_delay_row_number) %>% 
  filter(!(dep_delay_row_number>10|dep_delay_row_number>10|dep_delay_dense_rank>10))

flights %>% 
  select(month, day, carrier, flight, dep_delay) %>% 
  arrange(desc(dep_delay)) %>% 
  slice(1:10)

flights %>% 
  select(month, day, carrier, flight, dep_delay) %>% 
  top_n(10,dep_delay)

cos(pi)
atan(pi)

flights %>% 
  select(flight, dest)

# not works - too many dots
#ggplot(flights,aes(distance,arr_delay))+
#  geom_point()

delay <- 
  group_by(flights,dest) %>% 
  summarise(
            count=n(),
            dist=mean(distance,na.rm=TRUE),
            delay=mean(arr_delay,na.rm=TRUE)
            )

ggplot(delay,aes(dist,delay))+
    geom_point(aes(size=count), alpha = 1/3)

view(head(flights))

#A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.

summarise(
  group_by(flights,tailnum),
  early15=mean(arr_delay<(-15))) %>% 
  filter(.,early15>0.5)

filter(flights,tailnum=='N1602') %>% 
select(arr_delay) %>% 
arrange(arr_delay)

#99% of the time a flight is on time(arr late less than 5 min).
ontime99 <- 
summarise(
  group_by(flights,tailnum),
  late_rate=mean(arr_delay<5),
  total_flights=n()) %>% 
  filter(.,late_rate>=0.99)

view(ontime99)
filter(flights,tailnum=='N20904')$arr_delay

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% count(dest)

summarise(
  group_by(flights,dest),
  n=n()
)

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

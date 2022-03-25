install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)

nycflights13::flights
filter(flights,month==1,day==1)

(sqrt(2) ^ 2 == 2)
near(sqrt(2) ^ 2,2)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1| is.na(x))

#Had an arrival delay of two or more hours
filter(flights,dep_delay>=120)
#Flew to Houston (IAH or HOU)
colnames(flights)
filter(flights,dest=='IAH'|dest=='HOU')
filter(flights,carrier %in% c("AA", "DL", "UA"))
#Departed in summer (July, August, and September)
filter(flights,month %in% 7:9)
filter(flights,between(month,7,9))
#Departed between midnight and 6am (inclusive)
summary(flights$dep_time)
filter(flights, dep_time %% 2400 <= 600)
sum(is.na(flights$dep_time))
NA ^ 0 == 1
NA | TRUE
FALSE & NA
Inf * 0
arrange(flights, desc(dep_delay))
arrange(flights, desc(is.na(dep_time)),dep_time)
arrange(flights, desc(dep_delay),dep_time )
arrange(flights, desc(distance/(arr_time-dep_time)) )

select(flights, time_hour, air_time, everything())
#Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with('dep_'))
select(flights, matches("^(dep|arr)_(time|delay)$"))
select(flights, contains("TIME", ignore.case = FALSE))
transmute(flights,dep_hour=dep_time%/%100,dep_min=dep_time%%100) 
1:3 + 1:10
top_n(flights,10,dep_delay)

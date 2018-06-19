library(tidyverse)
library(nycflights13)
planes %>% 
  count(tailnum) %>% 
  filter(n>1)

weather %>% 
  count(year,month,day,hour,origin) %>% 
  filter(n>1)

flights %>% 
  count(year,month,day,flight) %>% 
  filter(n>1)

flights %>% 
  count(year,month,day,flight,tailnum) %>% 
  filter(n>1)

flights2 <- flights %>% 
  select(year:day,hour,origin,dest,tailnum,carrier)
flights2

flights2 %>% 
  select(-origin,-dest) %>% 
  left_join(airlines,by='carrier')

flights2 %>% 
  select(-origin,-dest) %>% 
  mutate(name=airlines$name[match(carrier,airlines$carrier)])

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x %>% 
  inner_join(y,by='key')














library(tidyverse)
table1 <- tibble(
  country=c('Afghanistan','Afghanistan','Brazil','Brazil','China','China'),
  year=c(1999,2000,1999,2000,1999,2000),
  cases=c(745,2666,37737,80488,212258,213766),
  population=c(19987071,20595360,172006362,174504898,1272915272,1280428583)
)
table1 %>% 
  count(year,wt=cases)

ggplot(table1,aes(year,cases))+
  geom_line(aes(group=country),color='grey50')+
  geom_line(aes(color=country))

tidy4a <- table4a %>% 
  gather('1999','2000',key='year',value = 'cases')

tidy4b <- table4b %>% 
  gather('1999','2000',key='year',value = 'population')

left_join(tidy4a,tidy4b)

spread(table2,key = type,value = count)

table3 %>% 
  separate(rate,into=c('cases','population'))

table5 %>% unite(new,century,year)
table5 %>% unite(new,century,year,sep='')

stocks <- tibble(
  year
  = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr
  = c(
    1,
    2,
    3,
    4,
    2,
    3,
    4),
  return = c(1.88, 0.59, 0.35,
             NA, 0.92, 0.17, 2.66)
)

stocks %>% 
  spread(year,return)

stocks %>% 
  spread(year,return) %>% 
  gather(year,return,'2015':'2016',na.rm = T)

stocks %>% 
  complete(year,qtr)

treatment <- tribble(
  ~ person,            ~treatment, ~response,
  "Derrick Whitmore", 1,             7,
  NA,                 2,             10,
  NA,                 3,             9,
  "Katherine Burke",  1,             4
)

treatment %>% 
  fill(person)

View(who)

who1 <- who %>% 
  gather(new_sp_m014:newrel_f65,key='key',value = 'cases',na.rm = T)

who1 %>% 
  count(key)

who2 <- who1%>% 
  mutate(key=stringr::str_replace(key,'newrel','new_rel'))

who3 <- who2%>% 
  separate(key,c('new','type','sexage'),sep='_')
who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new,-iso2,-iso3)

who %>% 
  gather(code,value,new_sp_m014:newrel_f65,na.rm = T) %>% 
  mutate(code=stringr::str_replace(code,'newrel','new_rel')) %>% 
  separate(code,c('new','var','sexage')) %>% 
  select(c(-new,-iso2,-iso3)) %>% 
  separate(sexage,c('sex','age'),sep=1)

View(airports)
flights2 %>% 
  left_join(airports,c('dest'='faa'))

flights2 %>% 
  left_join(airports,c('origin'='faa'))

View(flights)
top_dest <- flights %>% 
  count(dest,sort = T) %>% 
  head(10)
flights %>% 
  filter(dest  %in%  top_dest$dest)

flights %>% 
  semi_join(top_dest)

flights %>% 
  anti_join(planes,by='tailnum') %>% 
  count(tailnum,sort = T)

airports %>% 
  count(alt,lon) %>% 
  filter(n>1)

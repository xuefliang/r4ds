library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(nycflights13)

today()
now()
ymd('2017-01-31')
mdy('January 31st, 2017')
dmy('31-Jan-2017')
ymd_hms('2017-01-31 20:11:59')

flights %>% 
  select(year,month,day,hour,minute)

make_datetime_100 <- function(year,month,day,time){
  make_datetime(year,month,day,time %/%100,time%%100)
}

flights %>% 
  select(year,month,day,hour,minute) %>% 
  mutate(
    departure=make_datetime(year,month,day,hour,minute)
  )

flights_dt <- flights %>% 
  filter(!is.na(dep_time),!is.na(arr_time)) %>% 
  mutate(
    dep_time=make_datetime_100(year,month,day,dep_time),
    arr_time=make_datetime_100(year,month,day,arr_time),
    sched_dep_time=make_datetime_100(year,month,day,sched_dep_time),
    sched_arr_time=make_datetime_100(year,month,day,sched_arr_time)
  ) %>% 
  select(origin,dest,ends_with('delay'),ends_with('time'))

flights_dt %>% 
  ggplot(aes(dep_time))+
  geom_freqpoly(binwidth=86400)

flights_dt %>% 
  filter(dep_time<ymd('20130102')) %>% 
  ggplot(aes(dep_time))+
  geom_freqpoly(binwidth=600)

flights_dt %>% 
  mutate(wday=wday(dep_time,label = T)) %>% 
  ggplot(aes(x=wday))+
  geom_bar()

flights_dt %>% 
  mutate(minute=minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarize(
    avg_delay=mean(arr_delay,na.rm=T),
    n=n()
  ) %>% 
  ggplot(aes(minute,avg_delay))+
  geom_line()

sched_dep <- flights_dt %>% 
  mutate(minute=minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarize(
    avg_delay=mean(arr_delay,na.rm = T),
    n=n()
  )

ggplot(sched_dep,aes(minute,avg_delay))+
  geom_line()

flights_dt %>% 
  count(week=floor_date(dep_time,'week')) %>% 
  ggplot(aes(week,n))+
  geom_line()

(datetime <- ymd_hms("2016-07-08 12:34:56"))
hour(datetime) <- hour(datetime) + 1
datetime
update(datetime,year=2020,month=2,mday=2,hour=2)
ymd('2015-02-01') %>% 
  update(mday=30)

flights_dt %>% 
  mutate(dep_hour=update(dep_time,yday=1)) %>% 
  ggplot(aes(dep_hour))+
  geom_freqpoly(binwidth=300)

h_age <- today()-ymd(19791014)
as.duration(h_age)
2 * dyears(1)

one_pm <- ymd_hms(
  "2016-03-12 13:00:00",
  tz = "America/New_York"
)
one_pm
one_pm + ddays(1)
one_pm + days(1)

flights_dt %>% 
  filter(arr_time<dep_time)

flights_dt <- flights_dt %>% 
  mutate(
    overnight=arr_time<dep_time,
    arr_time=arr_time+days(overnight*1),
    sched_arr_time=sched_arr_time+days(overnight*1)
  )

flights_dt %>%
  filter(overnight, arr_time < dep_time)

years(1) / days(1)

next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)

Sys.timezone()

x1 <-ymd_hms("2015-06-01 12:00:00", tz = "America/New_York")
x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen")
x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland")
x4 <- c(x1, x2, x3)

x4a <- with_tz(x4,tzone = 'Australia/Lord_Howe')

x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")





























































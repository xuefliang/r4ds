library(tidyverse)
library(stringr)
library(forcats)
library(purrr)
library(lubridate)

x <- sample(20,100,replace = T)
y <- x>10
y
sum(y)
mean(y)

x <- c("one", "two", "three", "four", "five")
x[c(3,2,5)]

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a[1:2]

getS3method('as.Date','default')
x <- factor(c('ab','cd','ab'),levels = c('ab','cd','ef'))
typeof(x)
attributes(x)
class(x)

x <- as.Date('1971-01-01')
typeof(x)
class(x)
unclass(x)
attributes(x)

x <- ymd_hm('1971-01-01 01:00')
class(x)
typeof(x)
unclass(x)

attr(x,'tzone') <- 'US/Eastern'
x

y <- as.POSIXlt(x)
y
typeof(y)
attributes(y)

rvcheck::update_all()

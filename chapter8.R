library(tidyverse)
as.tibble(iris)
as_tibble(iris)

tibble(
  x=1:5,
  y=1,
  z=x^2+y
)

height <- read_csv('data/heights.csv')
print(height,width = Inf)
class(height)

read_csv('a,b,c
         1,2,3
         4,5,6')

parse_integer(c('1','231','.','456'),na='.')

fruit <- c("apple", "banana")
test <- parse_factor(c("apple", "banana", "bananana"), levels = fruit)
challenge <- read_csv(readr_example("challenge.csv"))
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)




library(magrittr)
foo_foo <- little_bunny()
foo_foo_1 <- hop(foo_foo, through = forest)
foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
foo_foo_3 <- bop(foo_foo_2, on = head)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat=price/carat)
pryr::object_size(diamonds)
pryr::object_size(diamonds2)

rnorm(100) %>% 
  matrix(ncol = 2) %>% 
  plot() %>% 
  str()

rnorm(100) %>% 
  matrix(ncol = 2) %T>%
  plot() %>% 
  str()

rnorm(10000) %>% 
  abs() %>% 
  '*' (50) %>% 
  matrix(ncol = 100) %>% 
  rowMeans() %>% 
  round() %>% 
  '%%' (7) %T>% 
  hist() %>%
  sum()

mtcars %$%
  cor(disp,mpg)

cor(mtcars$disp,mtcars$mpg)

mtcars %<>% transform(cyl=cyl*2)

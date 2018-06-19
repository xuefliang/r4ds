library(tidyverse)
library(modelr)
library(lubridate)
library(broom)
library(gapminder)
gapminder %>% 
  ggplot(aes(year,lifeExp,group=country))+
  geom_line(alpha=1/3)

nz <- filter(gapminder,country=='New Zealand')
nz %>% 
  ggplot(aes(year,lifeExp))+
  geom_line()+
  ggtitle("Full data=")

nz_mod <- lm(lifeExp~year,data=nz)
nz %>% 
  add_predictions(nz_mod) %>% 
  ggplot(aes(year,pred))+
  geom_line()+
  ggtitle("Linear trend +")

nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")

by_country <- gapminder %>% 
  group_by(country,continent) %>% 
  nest()

country_model <- function(df){
  lm(lifeExp~year,data=df)
}

models <- map(by_country$data,country_model)

by_country <- by_country %>% 
  mutate(model=map(data,country_model))

by_country <- by_country %>% 
  mutate(
    resids=map2(data,model,add_residuals)
  )

resids <- unnest(by_country,resids)
resids %>% 
  ggplot(aes(year,resid))+
  geom_line(aes(group=country),alpha=1/3)+
  geom_smooth(se=F)

resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) +
  facet_wrap(~continent)

broom::glance(nz_mod)
glimpse(by_country)

glance <- by_country %>% 
  mutate(glance=map(model,broom::glance)) %>% 
  unnest(glance,.drop = T)

glance %>% 
  arrange(r.squared)

glance %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

bad_fit <- filter(glance,r.squared<0.25)
gapminder %>% 
  semi_join(bad_fit,by='country') %>% 
  ggplot(aes(year,lifeExp,color=country))+
  geom_line()

tibble(
  x = list(1:3, 3:5),
  y = c("1, 2", "3, 4, 5")
)
tribble(
  ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
)

df <- tribble(
  ~x1,
  "a,b,c",
  "d,e,f,g"
)

df %>% 
  mutate(x2=stringr::str_split(x1,',')) %>% 
  unnest()

df %>% 
  tidyr::separate_rows()

sim <- tribble(
  ~f,
  ~params,
  "runif", list(min = -1, max = -1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>% 
  mutate(sims=invoke_map(f,params,n=10))

mtcars %>% 
  group_by(cyl) %>% 
  summarize(q=list(quantile(mpg)))

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>%
  group_by(cyl) %>%
  summarize(p = list(probs), q = list(quantile(mpg, probs))) %>%
  unnest()

x <- list(
  a = 1:5,
  b = 3:4,
  c = 5:6
)
df <- enframe(x)

df %>% 
  mutate(
    smry=map2_chr(
      name,
      value,
      ~stringr::str_c(.x,':',.y[1])
    )
  )

df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)

df %>% mutate(
  type=map_chr(x,typeof),
  length=map_int(x,length)
)

df <- tribble(
  ~x,
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)

df %>% mutate(
  a=map_dbl(x,'a'),
  b=map_dbl(x,'b',.null=NA_real_)
)

tibble(x = 1:2, y = list(1:4, 1)) %>% 
  unnest(y)

















































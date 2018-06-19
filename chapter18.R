library(tidyverse)
library(modelr)
options(na.action = na.warn)
ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250,-20,40),
  a2 = runif(250,-5,5)
)

ggplot(sim1,aes(x,y))+
  geom_abline(
    aes(intercept=a1,slope=a2),
    data = models,alpha=1/4
  )+geom_point()

grid <- sim1 %>% 
  data_grid(x)

sim1_mod <- lm(y ~ x, data = sim1)

grid <- grid %>% 
  add_predictions(sim1_mod)

ggplot(sim1,aes(x))+
  geom_point(aes(y=y))+
  geom_line(
    aes(y=pred),
    data=grid,
    color='red',
    size=1
  )

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

ggplot(sim1,aes(resid))+
  geom_freqpoly(binwidth=0.5)

ggplot(sim1,aes(x,resid))+
  geom_ref_line(h=0)+
  geom_point()

df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)

model_matrix(df, y ~ x1)
model_matrix(df, y ~ x1 - 1)
model_matrix(df, y ~ x1 + x2)

ggplot(sim3,aes(x1,y))+
  geom_point(aes(color=x2))

mod1 <- lm(y~x1+x2,data=sim3)
mod2 <- lm(y~x1*x2,data = sim3)

grid <- sim3 %>% 
  data_grid(x1,x2) %>% 
  gather_predictions(mod1,mod2)

ggplot(sim3,aes(x1,y,color=x2))+
  geom_point()+
  geom_line(data=grid,aes(y=pred))+
  facet_wrap(~model)

sim3 <- sim3 %>% 
  gather_residuals(mod1,mod2)

ggplot(sim3,aes(x1,resid,color=x2))+
  geom_point()+
  facet_grid(model~x2)

mod1 <- lm(y~x1+x2,data=sim4)
mod2 <- lm(y~x1*x2,data=sim4)

grid <- sim4 %>% 
  data_grid(x1=seq_range(x1,5),x2=seq_range(x2,5)) %>% 
  gather_predictions(mod1,mod2)

x2 <- c(0,1)
seq_range(x2,n=5)
x1 <- rcauchy(100)
seq_range(x1, n = 5)

ggplot(grid,aes(x1,x2))+
  geom_tile(aes(fill=pred))+
  facet_wrap(~model)

ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)

df <- tribble(
  ~y, ~x,
  1, 1,
  2, 2,
  3, 3
)

model_matrix(df,y~x^2+x)
model_matrix(df,y~I(x^2)+x)
model_matrix(df, y ~ poly(x, 2))

library(splines)
model_matrix(df, y ~ ns(x, 2))

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y~ns(x,1),data=sim5)
mod2 <- lm(y~ns(x,2),data=sim5)
mod3 <- lm(y~ns(x,3),data=sim5)
mod4 <- lm(y~ns(x,4),data=sim5)
mod5 <- lm(y~ns(x,5),data=sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, color = "red") +
  facet_wrap(~ model)

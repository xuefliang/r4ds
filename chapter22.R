ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+
  geom_smooth(se=F)+
  labs(
    title = paste(
      "Fuel efficiency generally decreases with","engine size"
    ),
    subtitle = paste(
      "Two seaters (sports cars) are an exception",
      "because of their light weight"
    ),
    caption = "Data from fueleconomy.gov")
    
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )

df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )

best_in_class <- mpg %>% 
  group_by(class) %>% 
  filter(row_number(desc(hwy))==1)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_text(aes(label = model), data = best_in_class)

ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+
  geom_label(
    aes(label=model),
    data=best_in_class,
    nudge_y = 2,
    alpha=0.5
  )

ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+
  geom_point(size=3,shape=1,data=best_in_class)+
  ggrepel::geom_label_repel(
    aes(label=model),
    data=best_in_class
  )

class_avg <- mpg %>% 
  group_by(class) %>% 
  summarize(
    displ=median(displ),
    hwy=median(hwy)
  )

ggplot(mpg,aes(displ,hwy,color=class))+
  ggrepel::geom_label_repel(aes(label=class),
                            data=class_avg,
                            size=6,
                            label.size = 0,
                            segment.color = NA)+
  geom_point()+
  theme(legend.position = 'none')

label <- mpg %>% 
  summarize(
    displ=max(displ),
    hwy=max(hwy),
    label=paste("Increasing engine size is \nrelated to",
                "decreasing fuel economy.")
  )

ggplot(mpg,aes(displ,hwy))+
  geom_point()+
  geom_text(
    aes(label=label),
    data=label,
    vjust='top',
    hjust='right'
  )

label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = paste(
    "Increasing engine size is \nrelated to",
    "decreasing fuel economy."
  )
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = "right"
  )

"Increasing engine size related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 30) %>%
  writeLines()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
  geom_point()+
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

presidential %>% 
  mutate(id=33+row_number()) %>% 
  ggplot(aes(start,id))+
  geom_point()+
  geom_segment(aes(xend=end,yend=id))+
  scale_x_date(
    NULL,
    breaks=presidential$start,
    date_labels='%Y-%m'
  )

base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))
base+theme(legend.position = 'top')

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 4)
    )
  )

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()
ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
  )

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")
x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))
ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  ggthemes::theme_stata()+
  theme(legend.position = 'right')
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave("my-plot.pdf")






























































#
#   Iraq oil production data divergence example
#

library("tidyverse")
library("cowplot")

df <- read_csv("input-data/iraq-oil-production.csv")

p1 <- df %>%
  filter(date >= as.Date("2018-06-01") %m-% months(120)) %>%
  ggplot(aes(x = date, y = value, group = source)) +
  geom_line(size = 1) +
  geom_hline(yintercept = cutpoints, lty = 3, size = .3) +
  theme_void() +
  theme(plot.background = element_rect(color = "black", fill = "white")) +
  labs(x = "", y = "") +
  annotate("rect", xmin = as.Date("2017-01-01"), xmax = as.Date("2018-06-30"), 
           ymin = 4100, ymax = 4700, fill = NA, color = "gray50", size = 1)

p2 <- df %>% 
  filter(date > "2017-01-01") %>%
  ggplot(aes(x = date, y = value, colour = source)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = cutpoints, lty = 3) +
  theme_minimal() +
  labs(x = "Month", y = "Oil tb/d") +
  scale_colour_discrete("Source") +
  theme(legend.position = "top")

ggdraw() +
  draw_plot(p2, 0, 0, 1, 1) +
  draw_plot(p1, 0.1, 0.13, .32, .2)
ggsave(file = "output/figures/opec-iraq-divergence.png", height = 5, width = 8)


plot.mpg <- ggplot(mpg, aes(x = cty, y = hwy, colour = factor(cyl))) + 
  geom_point(size=2.5) + theme_cowplot()
plot.mpg
plot.diamonds <- ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar() +
  theme_cowplot() + theme(axis.text.x = element_text(angle=70, vjust=0.5, hjust = 0.9))
plot.diamonds

ggdraw() +
  draw_plot(
    plot.diamonds + theme(legend.justification = "bottom"),
    0, 0, 1, 1
  ) +
  draw_plot(
    plot.mpg + scale_color_viridis_d() + 
      theme(legend.justification = "top", plot.background = element_rect(fill = "white")),
    0.5, 0.54, 0.5, 0.4
  ) +
  draw_plot_label(
    c("A", "B"),
    c(0, 0.5),
    c(1, 0.94),
    size = 15
  )

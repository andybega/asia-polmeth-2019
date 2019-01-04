#
#   Iraq oil production data divergence example
#

library("tidyverse")
library("cowplot")
library("lubridate")
library("forecast")

df <- read_csv("input-data/iraq-oil-production.csv")

cutpoints <- c(4280, 4384, 4473, 4576)

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


# 
#   Example of chart and chart + forecast for slides
#   ____________________________

train_df <- df %>%
  filter(date <= as.Date("2018-02-01")) %>%
  filter(date >= as.Date("2018-02-01") %m-% months(60)) %>%
  filter(source=="Platform / tradingeconomics.com")

p1 <- train_df %>%
  tail(12*4) %>%
  ggplot() +
  geom_line(aes(x = date, y = value)) +
  theme_minimal() +
  labs(x = "", y = "") +
  scale_x_date(limits = c(as.Date("2018-02-01") %m-% months(12*4), as.Date("2018-05-01"))) +
  scale_y_continuous(limits = c(2700, 5000))
ggsave(p1, file = "output/figures/opec-iraq-with-chart.png", height = 4, width = 8)

fcast <- forecast(auto.arima(train_df$value), h = 3) %>%
  as.data.frame() %>%
  mutate(date = seq(as.Date("2018-03-01"), length.out = 3, by = "month"))

p1 +
  geom_line(data = fcast, aes(x = date, y = `Point Forecast`), color = "blue") +
  geom_ribbon(data = fcast, aes(x = date, ymin = `Lo 95`, ymax = `Hi 95`), 
              fill = "blue", alpha = .2) 
ggsave(file = "output/figures/opec-iraq-with-forecast.png", height = 4, width = 8)

# Get cutpoint probabilities
mu  <- tail(fcast, 1)$`Point Forecast`
u95 <- tail(fcast, 1)$`Hi 95`
se  <- abs(mu - u95)  / 1.96

cprob <- c(0, pnorm(cutpoints, mean = mu, sd = se), 1)
probs <- diff(cprob)
format(probs, digits = 2)

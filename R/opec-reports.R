#
# Rscript opec-reports.R

library("tidyverse")
library("readxl")
library("hrbrthemes")
library("here")
library("cowplot")

setwd(here::here("data-modules/opec"))

opec <- read_xlsx("data/opec_reports.xlsx") %>%
  mutate(Report = as.Date(Report),
         Date = as.Date(Date))

opec %>% group_by(Country, Date) %>%
  summarize(Crude_oil_production_secondary_sources_tbd = mean(Crude_oil_production_secondary_sources_tbd)) %>%
  ungroup() %>%
  print(n = 100)

ggplot(opec, aes(x = Date, y = Crude_oil_production_secondary_sources_tbd)) +
  facet_wrap(~ Country, scales = "free_y") +
  geom_line(aes(color = factor(Report))) +
  scale_color_discrete(guide = FALSE) +
  theme_ipsum_rc()

ggsave("opec-divergence.png", height = 8, width = 15)

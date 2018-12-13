library("tidyverse")
library("lme4")
library("mgcv")
library("hrbrthemes")
library("MuMIn")
library("betareg")
library("rstan")
library("rstanarm")
library("forcats")
library("broom")

dropbox_path <- "~/Dropbox/Work/asia-polmeth-2019"
in_dir  <- file.path(dropbox_path, "input-data")
out_dir <- file.path(dropbox_path, "output")


user_fcasts <- read_rds(file.path(out_dir, "user_forecasts.rds")) %>%
  mutate(date = as.Date(date),
         # x is just a date index for plotting, i think
         x = as.numeric(date) - min(as.numeric(date)),
         sees_chart = sees_chart==1,
         sees_model = sees_model==1) 

machine_fcasts <- read_rds(file.path(out_dir, "machine_forecasts.rds"))

data_sources <- read_csv(file.path(in_dir, "questions-with-answers.csv")) 

#fcasts$sees_model <- factor(fcasts$sees_model, labels = c("No", "Yes"))
#fcasts$sees_chart <- factor(fcasts$sees_chart, labels = c("No", "Yes"))

df1 <- machine_fcasts %>%
  select(ifp_id, date, has_historic_data, has_arima, needs_data_agg,
         num_options, p_correct, brier) %>%
  mutate(Forecaster = "Machine", condition = "n/a", sub_condition_1 = "n/a",
         sees_chart = NA, sees_model = NA)
df2 <- user_fcasts %>%
  mutate(Forecaster = ifelse(turker, "Turker", "Volunteer")) %>%
  select(ifp_id, user_id, date, has_historic_data, has_arima, needs_data_agg,
         num_options, p_correct, brier, Forecaster, condition, sub_condition_1,
         sees_chart, sees_model)
all_fcasts <- bind_rows(df1, df2) %>%
  left_join(data_sources %>% select(ifp_id, data_source), by = "ifp_id") %>%
  replace_na(list(data_source = "other")) %>%
  # add in summary info for chart/arima status
  mutate(IFP_Group = case_when(
    has_historic_data & has_arima ~ "Chart and model",
    has_historic_data & !has_arima ~ "Chart only",
    !has_historic_data ~ "No TS data",
    TRUE ~ NA_character_
  ), IFP_Group = factor(IFP_Group, levels = c("No TS data", "Chart only", "Chart and model"))) %>%
  mutate(Condition = case_when(
    condition=="a" ~ "A: no chart",
    condition=="b" ~ "B: chart only",
    condition=="c" ~ "C: chart and model",
    condition=="n/a" ~ "Machine",
    TRUE ~ NA_character_
  ))

# Human and machine forecasts during common period
common_period <- all_fcasts %>%
  filter(date >= "2018-05-02" & date <= "2018-08-02")


# Summary statistics ------------------------------------------------------

nrow(common_period)

common_period %>%
  group_by(Forecaster) %>%
  summarize(n = n())

by_ifp <- common_period %>%
  group_by(ifp_id) %>%
  summarize_at(vars(one_of("has_arima", "has_historic_data", "IFP_Group")), unique) %>%
  group_by(has_arima, has_historic_data) %>%
  summarize(IFP_Group = unique(IFP_Group), n = n()) 
write_csv(by_ifp, "output/tables/n-ifps-by-data-status.csv")
sum(by_ifp$n)
by_ifp

# Overall condition table
brier_by_forecaster <- common_period %>%
  group_by(Forecaster) %>%
  summarize(avg_Brier = mean(brier), n = n()) %>%
  write_csv(., "output/tables/brier-by-forecaster.csv")
brier_by_forecaster %>%
  knitr::kable(digits = 3)

brier_by_forecaster_condition <- common_period %>%
  group_by(Forecaster, Condition) %>%
  summarize(avg_Brier = mean(brier)) %>%
  write_csv("output/tables/brier-by-forecaster-condition.csv")
brier_by_forecaster_condition %>%
  spread(Condition, avg_Brier) %>%
  knitr::kable(digits = 3)


brier_by_forecaster_ifp_group_condition <- common_period %>%
  group_by(Forecaster, IFP_Group, Condition) %>%
  summarize(avg_Brier = mean(brier), 
            sd_Brier = sd(brier), 
            n = n()) %>%
  ungroup() %>%
  arrange(Condition, Forecaster, IFP_Group) %>%
  mutate(Group = 1:n()) %>%
  select(Group, Condition, Forecaster, IFP_Group, everything()) %>%
  write_csv("output/tables/brier-by-forecaster-ifp-group-condition.csv")
brier_by_forecaster_ifp_group_condition %>%
  knitr::kable(digits = 2)

caption = paste0(
  create_label('tab:', opts_current$get('label'), latex = (format == 'latex')),
  caption
)

# Design table
brier_by_forecaster_ifp_group_condition %>%
  select(Condition, Forecaster, IFP_Group) %>%
  arrange(Condition, Forecaster, IFP_Group) %>%
  mutate(
    `Sees chart?` = case_when(
      str_detect(Condition, "(B:)|(C:)") & IFP_Group %in% c("Chart only", "Chart and model")  ~ "X",
      TRUE ~ ""
    ),
    `Sees model?` = case_when(
      str_detect(Condition, "C:") & IFP_Group=="Chart and model" ~ "X",
      TRUE ~ ""
    )) %>%
  dplyr::mutate(Group = 1:n()) %>%
  select(Group, everything()) %>%
  knitr::kable()

brier_by_ifp_group <- common_period %>%
  group_by(IFP_Group) %>%
  summarize(avg_Brier = mean(brier), n_ifps = length(unique(ifp_id)), n_fcasts = n())
brier_by_ifp_group


# Turker pairwise comparison ----------------------------------------------

turker_pairs <- common_period %>% 
  filter(condition!="b") %>%
  mutate(Condition = fct_recode(Condition, `C: chart and model` = "Machine"),
         Condition = fct_relabel(Condition, ~ paste0("Condition ", .x))) %>%
  mutate(Forecaster = factor(Forecaster, levels = c("Turker", "Volunteer", "Machine"))) %>%
  mutate(IFP_Group = factor(IFP_Group),
         IFP_Group = fct_relabel(IFP_Group, ~ paste0("IFP Group: ", .x))) %>%
  unite(Group, Condition, IFP_Group, sep = "\n")
group_all <- turker_pairs %>%
  mutate(Group = "All conditions and IFPs") 
turker_pairs <- bind_rows(turker_pairs, group_all) %>%
  group_by(Group) %>%
  nest(Forecaster, brier) %>%
  # now add linear models
  mutate(lm = map(data, function(x) lm(brier ~ Forecaster, data = x)),
         coefs = map(lm, tidy),
         rsq = map_dbl(lm, function(x) glance(x)$adj.r.squared))

turker_pairs %>%
  select(Group, coefs) %>%
  unnest(coefs) %>%
  mutate(term = factor(term) %>% fct_recode(`Baseline: Turker` = "(Intercept)")) %>%
  ggplot(., aes(x = Group, colour = term)) +
  geom_pointrange(aes(y = estimate, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error)) +
  coord_flip() +
  scale_y_reverse() +
  geom_hline(yintercept = 0, linetype = 3) +
  theme_minimal() +
  labs(y = "Average Brier / change in Average Brier") +
  theme(legend.position = "top") +
  scale_color_discrete("Coefficient:")
ggsave("output/figures/pairwise-comparisons-turker.png", height = 4, width = 7)



# Linear models for chart/model effects -----------------------------------
#
#   Maybe add an interaction model here?
#   summary(lm(brier ~ Forecaster + IFP_Group + sees_chart:Forecaster + sees_model:Forecaster, data = common_period_no_machine))
#



common_period_no_machine <- common_period[common_period$Forecaster!="Machine", ] %>%
  mutate(sees_chart = as.integer(sees_chart), 
         sees_model = as.integer(sees_model))

# % who saw a chart
table(common_period_no_machine$sees_chart) / nrow(common_period_no_machine) * 100
# % who saw a model
table(common_period_no_machine$sees_model) / nrow(common_period_no_machine) * 100

mdl1 <- lm(brier ~ Forecaster + IFP_Group + sees_chart + sees_model, 
           data = common_period_no_machine)
mdl2 <- lmer(brier ~ Forecaster + IFP_Group + sees_chart + sees_model + (1|ifp_id),
             data = common_period_no_machine)

sink("output/tables/mdl1-summary.txt")
summary(mdl1) 
sink()

sink("output/tables/mdl2-summary.txt")
summary(mdl2) 
sink()

bind_rows(
  tidy(mdl1) %>% mutate(Model = "Model 1: linear model"),
  tidy(mdl2) %>% mutate(Model = "Model 2: linear model with IFP random\nintercepts") %>%
    filter(group=="fixed")
) %>%
  mutate(term = factor(term) %>% fct_recode(`Baseline\nForecasterTurker, IFP_GroupNo_TS_data` = "(Intercept)")) %>%
  ggplot(aes(x = term, color = Model)) +
  geom_pointrange(aes(y = estimate, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = .5)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 3) +
  theme_minimal() +
  labs(y = "Outcome: Brier score", x = "Coefficient") +
  theme(legend.position = "top") +
  scale_color_discrete("Model:")
ggsave("output/figures/model-chart-and-model-effects.png", height = 4, width = 7)

mdl_summary_stats <- tibble(
  Model = c("Model 1: Linear model", rep("", 2),
            "Model 2: Linear model with IFP random intercepts", rep("", 2)),
  Statistic = c("N", "R^2", "Adj. R^2", "N", "Marginal R^2", "Conditional R^2"),
  Value = c(length(resid(mdl1)), summary(mdl1)$r.squared, summary(mdl1)$adj.r.squared,
            length(resid(mdl2)), MuMIn::r.squaredGLMM(mdl2))
)

write_csv(mdl_summary_stats, "output/tables/model-summary-stats.csv")

mdl_summary_stats 



# Where did models do well? Look by data source ---------------------------


# Based on this, split out ACLED binary questions
View(common_period %>%
       filter(IFP_Group=="Chart and model") %>%
       group_by(data_source, num_options, Forecaster) %>%
       summarize(n_ifps = length(unique(ifp_id)),
                 n_fcasts = n(),
                 avg_Brier = mean(brier), 
                 sd_Brier = sd(brier)))

arima_qs <- common_period %>%
  filter(IFP_Group=="Chart and model") %>%
  mutate(Data_source = case_when(
    data_source=="acled" & num_options==2 ~ "ACLED 2op",
    data_source=="acled" & num_options==5 ~ "ACLED 5op",
    TRUE ~ data_source
  ))


arima_qs %>%
  group_by(num_options, Forecaster) %>%
  summarize(n_ifps = length(unique(ifp_id)),
            n_fcasts = n(),
            avg_Brier = mean(brier), 
            sd_Brier = sd(brier)) 

arima_qs %>%
  group_by(Data_source, num_options) %>%
  summarize(n_ifps = length(unique(ifp_id)),
            avg_Brier = mean(brier))

brier_arima_qs_only_by_data_source_forecaster <- arima_qs %>%
  group_by(Data_source, Forecaster) %>%
  summarize(n_ifps = length(unique(ifp_id)),
            n_fcasts = n(),
            avg_Brier = mean(brier), 
            sd_Brier = sd(brier)) %>%
  write_csv("output/tables/brier-arima-qs-only-by-data-source-forecaster.csv")
brier_arima_qs_only_by_data_source_forecaster %>%
  group_by(Data_source) %>%
  mutate(N_IFPs = max(n_ifps)) %>%
  ungroup() %>%
  select(Data_source, N_IFPs, Forecaster, avg_Brier) %>%
  spread(Forecaster, avg_Brier) %>%
  write_csv("output/tables/brier-arima-qs-only-by-data-source-forecaster-wide.csv") %>%
  knitr::kable(digits = 3)


mdl3 <- lm(brier ~ -1 + Data_source + Data_source:Forecaster, data = arima_qs )

mdl3_coefs <- tidy(mdl3) %>%
  separate(term, sep = ":", into = c("Data source", "Forecaster"), fill = "right") %>%
  replace_na(list(Forecaster = "Machine (reference)")) %>%
  mutate(Forecaster = gsub("Forecaster", "", Forecaster),
         `Data source` = gsub("Data_source", "", `Data source`)) %>%
  mutate(`Data source` = factor(`Data source`))
# Reorder by median estimate; makes plot easier to read
mdl3_coefs$`Data source` = fct_reorder(mdl3_coefs$`Data source`, mdl3_coefs$estimate)

mdl3_coefs %>%
  mutate(facet = ifelse(Forecaster=="Machine (reference)", "Baseline", "Relative difference")) %>%
  ggplot(., aes(x = `Data source`, color = Forecaster)) +
  geom_pointrange(aes(y = estimate, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 0.2), alpha = .8) +
  facet_wrap(~ facet, scales = "free_x") +
  coord_flip() +
  geom_hline(data = data.frame(facet = "Relative difference", y = 0), 
             aes(yintercept = 0), linetype = 3) +
  scale_x_discrete(limits = rev(levels(mdl3_coefs$`Data source`))) +
  theme_minimal() +
  labs(y = "Coefficient estimate", x = "Data source") +
  theme(legend.position = "top") +
  scale_color_discrete("Forecaster:")
ggsave("output/figures/model-machine-by-data-source.png", height = 7, width = 7)

mdl3_coefs %>%
  mutate(facet = ifelse(Forecaster=="Machine (reference)", "Baseline", "Relative difference")) %>%
  ggplot(., aes(x = `Data source`, color = Forecaster)) +
  geom_pointrange(aes(y = estimate, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 0.2), alpha = .8) +
  facet_wrap(~ facet, scales = "free_x") +
  coord_flip() +
  theme_ipsum_rc() +
  geom_hline(yintercept = 0, linetype = 3)


# Condition B deep dive ---------------------------------------------------

# B did better on non-data questions
volunteer_brier_by_condition_ifp_group <- brier_by_forecaster_ifp_group_condition %>% 
  filter(Forecaster=="Volunteer") %>%
  select(-Forecaster) %>%
  mutate(Condition = factor(Condition) %>% fct_relevel(., "B: chart only")) %>%
  arrange(IFP_Group, Condition) %>%
  write_csv("output/tables/volunteer-brier-by-condition-ifp-group.csv")

# Spillover effect?

# Look at forecast quality by number of times someone forecasted
cond_b_volunteers_n_vs_brier <- common_period %>%
  filter(Forecaster=="Volunteer" & Condition=="B: chart only") %>%
  group_by(user_id) %>%
  mutate(n_fcasts = n(), IFP_had_chart = !IFP_Group=="No TS data") %>%
  group_by(user_id, IFP_had_chart) %>%
  summarize(n_fcasts = unique(n_fcasts), avg_Brier = mean(brier))

ggplot(cond_b_volunteers_n_vs_brier, aes(x = n_fcasts, y = avg_Brier, color = IFP_had_chart)) +
  geom_point() + 
  scale_x_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_ipsum_rc() +
  labs(x = "Total number of forecasts by user")
ggsave("output/figures/cond-b-volunteers-n-vs-brier.png", height = 4, width = 8)

# neither slope seems to be significant
summary(lm(avg_Brier ~ n_fcasts*IFP_had_chart, data = cond_b_volunteers_n_vs_brier))

# Do people who only forecasted a small number of times still do better than 
# users in other groups?
common_period %>%
  filter(!Forecaster %in% c("Turker", "Machine")) %>%
  group_by(user_id) %>%
  mutate(n_fcasts = n()) %>%
  filter(n_fcasts < 50) %>%
  group_by(Forecaster, Condition) %>%
  summarize(users = length(unique(user_id)),
            avg_Brier = mean(brier))

# User 1354 was B volunteer with the most forecasts, did he she get better over time?
u1354 <- common_period %>%
  filter(user_id==1354) %>%
  mutate(IFP_had_chart = !IFP_Group=="No TS data")
ggplot(u1354, aes(x = date, y = brier, color = IFP_had_chart)) +
  geom_point() +
  geom_smooth(method = "lm")

# Overall highest forecasts; this person was in C
u911 <- common_period %>%
  filter(user_id==911) %>%
  mutate(IFP_had_chart = !IFP_Group=="No TS data")
ggplot(u911, aes(x = date, y = brier, color = IFP_Group)) +
  geom_point() +
  geom_smooth(method = "lm")

# look at stats when you take out superheavy users
fcasts_by_user <- common_period %>%
  group_by(user_id) %>%
  summarize(n_fcasts = n()) %>%
  ungroup()

# Do small time users who enter the competition later do as well as those who
# were active earlier? I'm trying to get at whether questions became easier
# over time or forecasters got better
common_period %>%
  filter(!Forecaster %in% c("Turker", "Machine")) %>%
  group_by(user_id) %>%
  mutate(n_fcasts = n()) %>%
  ungroup() %>%
  filter(n_fcasts < 5) -> foo
ggplot(foo, aes(x = date, y = brier)) +
  geom_point() +
  geom_smooth(method = "lm")
# Seems that it really is that forecasters got better over time

# 1892 Oil production in Iraq in June 2018
# This is the question we used at the site visit to illustrate bad data. 
# The B volunteers again did very well
# C volunteers did worse than machine, but turker C did more like machine.
f1892 <- filter(common_period, ifp_id==1892) %>%
  mutate(Group = paste0(Forecaster, ", ", condition),
         Group = factor(Group) %>% fct_relevel("Volunteer, b"))
summary(lm(brier ~ Group, data = f1892))
tbl1892 <- f1892 %>%
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n()) %>%
  arrange(avg_Brier)
tbl1892

# 911 ICEWS Palestine
# the ICEWS data were badly aggregated
f911 <- filter(all_fcasts, ifp_id==911) %>%
  mutate(Group = paste0(Forecaster, ", ", condition),
         Group = factor(Group) %>% fct_relevel("Volunteer, b"))
summary(lm(brier ~ Group, data = f911))
tbl911 <- f911 %>%
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n()) %>%
  arrange(avg_Brier)
tbl911

# 1003 Loya Jirga
# System persistently kept showing wrong chart, with ACLED riots/protests
# B volunteers have lower Brier on this, why?
# C turkers as expected had worse score than A turkers
f1003 <- filter(common_period, ifp_id==1003) %>%
  mutate(Group = paste0(Forecaster, ", ", condition),
         Group = factor(Group) %>% fct_relevel("Volunteer, b"))
summary(lm(brier ~ Group, data = f1003))
tbl1003 <- f1003 %>%
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n()) %>%
  arrange(avg_Brier)
tbl1003

# When the machine models did well, did condition C also do better, relative 
# to B?
good_machine <- common_period %>%
  # Get average for each group on each IFP
  group_by(ifp_id, Forecaster, Condition) %>%
  summarize(avg_Brier = mean(brier), n_fcasts = n()) %>%
  group_by(ifp_id) %>%
  # Don't do a weighted avg for Brier. We want an estimate of the performance
  # of a group on an IFP, i.e. group average and the average of that
  mutate(avg_Brier_for_ifp = mean(avg_Brier),
         rel_performance = (avg_Brier - avg_Brier_for_ifp)/avg_Brier_for_ifp,
         min_avg_Brier = min(avg_Brier),
         n_groups = n()) %>%
  # Make sure we had forecasts from all groups
  filter(n_groups==6) %>%
  # Look only at Machine and select if they had the best avg
  filter(Forecaster=="Machine") %>%
  filter(avg_Brier==min_avg_Brier) %>%
  ungroup() %>%
  arrange(rel_performance) 

fcasts_good_machine <- common_period %>%
  filter(ifp_id %in% good_machine$ifp_id) %>%
  # Keep track of unique users for each group
  group_by(Forecaster, Condition) %>%
  mutate(N_users = length(unique(user_id))) %>%
  # Get average for each group on each IFP
  group_by(ifp_id, Forecaster, Condition) %>%
  summarize(avg_Brier = mean(brier), N_fcasts = n(), N_users = unique(N_users)) %>%
  group_by(ifp_id) %>%
  # Don't do a weighted avg for Brier. We want an estimate of the performance
  # of a group on an IFP, i.e. group average and the average of that
  mutate(avg_Brier_for_ifp = mean(avg_Brier),
         rel_performance = (avg_Brier - avg_Brier_for_ifp)/avg_Brier_for_ifp,
         min_avg_Brier = min(avg_Brier),
         N_groups = n())

fcasts_good_machine %>%
  group_by(Forecaster, Condition) %>% 
  summarize(avg_rel_Brier = mean(rel_performance),
            N_users = unique(N_users), N_fcasts = sum(N_fcasts)) %>%
  knitr::kable(digits = 2)

ggplot(fcasts_good_machine, aes(x = interaction(Forecaster, Condition), y = brier)) + 
  geom_boxplot()
# need to check this more, but it seems that when the model was pretty good 
# actually, turkers who saw the model tended to do better than turkers who 
# were in a; while volunteers who saw the model still tended to do worse than
# volunteers who saw only the chart




# Selection effects -------------------------------------------------------



# Look at groupings by data source
by_source <- common_period %>%
  group_by(data_source, Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())
by_source
with(by_source[by_source$Forecaster=="Volunteer" & by_source$condition=="a", ],
     plot(n, avg_Brier))
with(by_source[by_source$Forecaster=="Volunteer" & by_source$condition=="b", ],
     plot(n, avg_Brier))
with(by_source[by_source$Forecaster=="Volunteer" & by_source$condition=="c", ],
     plot(n, avg_Brier))
with(by_source[by_source$Forecaster=="Volunteer", ], plot(n, avg_Brier))
with(by_source[by_source$Forecaster=="Turker", ], plot(n, avg_Brier))
with(by_source[by_source$Forecaster=="ARIMA", ], plot(n, avg_Brier))
with(by_source, plot(n, avg_Brier))

# selection effect, are forecasters in C forecasting on more difficult questions?

# people in condition B who only forecasted once
user_fcasts %>%
  filter(condition=="b") %>%
  group_by(user_id) %>%
  summarize(n = n()) %>% pull(n) -> foo

# a couple of heavyweights are pulling B
user_fcasts %>% group_by(user_id, condition) %>% summarize(brier = mean(brier), n = n()) -> foo
filter(foo, n > 200) %>% arrange(brier)

# do people in B forecast more?
user_fcasts %>% group_by(user_id, condition, turker) %>% summarize(brier = mean(brier), n = n()) -> foo
foo %>% group_by(turker, condition) %>%
  summarize(users = n(),
            med_fcasts = median(n),
            mean_fcasts = mean(n))

# ok, back. let's look at forecasters who only forecasted a few times, and
# spillover does not seem likely
user_fcasts %>%
  group_by(user_id) %>%
  mutate(n_fcasts_by_user = n()) -> foo
foo %>%
  filter(!turker) %>%
  group_by(condition, has_historic_data) %>%
  summarize(mean(brier))
# questions with historic data don't seem to be harder
foo %>%
  filter(condition=="b") -> foo
foo %>%
  group_by(has_historic_data) %>%
  summarize(mean(brier))
foo %>%
  group_by(n_fcasts_by_user==1, has_historic_data) %>%
  summarize(mean(brier))
foo %>%
  group_by(n_fcasts_by_user < 8, has_historic_data) %>%
  summarize(mean(brier))

# Look at it by how well the model did; when the model did well, did C do well, too?
# Look at it when you take out the heavy users. Why would or would not short-term
# users be impacted by platform design choices?




# Specific questions ------------------------------------------------------



# activity by question
fcasts_by_ifp <- all_fcasts %>%
  group_by(ifp_id) %>%
  summarize(avg_Brier = mean(brier), n = n()) %>%
  arrange(desc(n))
fcasts_by_ifp
tail(fcasts_by_ifp)

# 2126 had a lot of forecasts
f2126 <- filter(all_fcasts, ifp_id==2126)
table(f2126$Forecaster, f2126$condition)
f2126 %>% 
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

# 2297
f2297 <- filter(all_fcasts, ifp_id==2297)
table(f2297$Forecaster, f2297$condition)
f2297 %>% 
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

# low activity 
# 1208
f1208 <- filter(all_fcasts, ifp_id==1208)
table(f1208$Forecaster, f1208$condition)
f1208 %>% 
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

# 1235
f1235 <- filter(all_fcasts, ifp_id==1235)
table(f1235$Forecaster, f1235$condition)
f1235 %>% 
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

# 1172
f1172 <- filter(all_fcasts, ifp_id==1172)
f1172 %>% 
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

# 1892
f1892 <- filter(all_fcasts, ifp_id==1892)
f1892 %>% 
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())



# Plots over time ---------------------------------------------------------


# Grand plot of everything
df1 <- user_fcasts %>%
  filter(date > "2018-05-02") %>%
  group_by(date, user_id, ifp_id, turker, condition) %>%
  summarize(MDB = mean(brier), n = n(), has_historic_data = unique(has_historic_data)) %>%
  ungroup() %>%
  mutate(has_historic_data = factor(has_historic_data),
         has_historic_data = fct_recode(has_historic_data, `IFP without data` = "FALSE", `IFP had data` = "TRUE"),
         turker = factor(turker),
         turker = fct_recode(turker, Turker = "TRUE", Volunteer = "FALSE"),
         condition = factor(condition),
         condition = fct_recode(condition, `A: no chart` = "a", `B: chart only` = "b", `C: chart and model` = "c")) %>%
  rename(Forecaster = turker, Question = has_historic_data)
ggplot(df1, aes(x = date, y = MDB, color = condition)) +
  facet_grid(Forecaster ~ Question) +
  geom_point(alpha = .02, aes(size = n)) +
  scale_size(guide = FALSE) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs")) +
  theme_ipsum_rc() +
  scale_y_continuous(limits = c(0, 1.01))

# Table of MMDB for same groupings
mmdb_by_cond_turker_data <- df1 %>%
  group_by(ifp_id, Forecaster, Question, condition) %>%
  summarize(MMDB = mean(MDB)) %>%
  group_by(Forecaster, Question, condition) %>%
  summarize(mMMDB = mean(MMDB)) %>%
  spread(condition, mMMDB)
mmdb_by_cond_turker_data %>%
  knitr::kable(digits = 2)

# Plot average MDB by turker/volunteer over course of RCT
mean_daily_briers <- fcasts %>%
  group_by(date, user_id, ifp_id, turker) %>%
  summarize(MDB = mean(brier),
            n = n()) 
ggplot(mean_daily_briers, aes(x = date, y = MDB, colour = turker)) +
  geom_point(alpha = .02, aes(size = n)) +
  scale_size(guide = FALSE) +
  scale_colour_discrete("Forecaster", labels = c("Volunteer", "Turker")) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs")) +
  theme_ipsum_rc() +
  labs(x = "Date", y = "Mean daily Brier score") 
ggsave("output/figures/mdb-turker-over-time.png", height = 5, width = 8)


# Was this improvment only for chart/model or generaly improvement over time?
fcasts %>%
  group_by(date, user_id, ifp_id, turker, has_arima, sees_chart, sees_model) %>%
  summarize(MDB = mean(brier),
            n = n()) %>%
  ggplot(., aes(x = date, y = MDB, color = turker)) +
  facet_wrap(~ interaction(has_arima, sees_chart, sees_model)) +
  geom_point(alpha = .02) +
  geom_smooth()

# Are questions getting more difficult over time?
fcasts %>%
  group_by(date, ifp_id) %>%
  summarize(MDB = mean(brier), n = n()) %>%
  ggplot(., aes(x = date, y = MDB)) +
  geom_point(alpha = .1) + 
  geom_smooth()

# Even for people who saw now charts/models, better Brier over time
fcasts %>%
  filter(condition=="a") %>%
  group_by(date, ifp_id) %>%
  summarize(MDB = mean(brier), n = n()) %>%
  ggplot(., aes(x = date, y = MDB)) +
  geom_point(alpha = .1) + 
  geom_smooth(se = FALSE)

# Compare conditions
fcasts %>%
  group_by(date, ifp_id, condition) %>%
  summarize(MDB = mean(brier), n = n()) %>%
  ggplot(., aes(x = date, y = MDB)) +
  geom_point(alpha = .1) + 
  geom_smooth(aes(color = condition), se = FALSE)

# Compare conditions B and C for non-chartable/chartable, only volunteers
fcasts %>%
  filter(condition %in% c("b", "c") & !turker) %>%
  filter(!xor(has_arima, has_historic_data)) %>%
  mutate(has_arima = factor(has_arima, labels = c("Question did not have chartable data", "Question had chart and model forecast")),
         condition = factor(condition, labels = c("B: Saw chart only", "C: Saw chart and model"))) %>%
  group_by(date, ifp_id, condition, has_arima) %>%
  summarize(MDB = mean(brier), n = n()) %>%
  ggplot(., aes(x = date, y = MDB)) +
  facet_wrap(~ has_arima) +
  geom_point(alpha = .1) + 
  geom_smooth(aes(color = condition), se = FALSE) +
  scale_colour_discrete("Condition") +
  theme_ipsum_rc() +
  theme(legend.position = "top")
ggsave("output/figures/volunteers-only-accuracy-by-chartability-over-time.png", height = 5, width = 8)



# Beta regression ---------------------------------------------------------

df <- fcasts %>% 
  mutate(brier = brier / 2,
         brier = case_when(
           brier==0 ~ brier + 1e-10,
           brier==1 ~ brier - 1e-10,
           TRUE ~ brier)
  )
fit_beta <- betareg(brier ~ turker + sees_chart + sees_model + has_arima, 
                    data = df)
summary(fit_beta)

fit_beta2 <- stan_betareg(brier ~ turker + sees_chart + sees_model + has_arima,
                          data = df)




# GAM ---------------------------------------------------------------------

fit <- mgcv::gam(brier ~ s(x, k = 5), data = fcasts)

fit <- mgcv::gam(brier ~ turker + has_arima + sees_chart + sees_model + s(x, k = 7), data = fcasts)

fit <- mgcv::gam(brier ~ turker + has_arima + sees_chart + sees_model + 
                   s(x, sees_model, bs = "fs", k = 7), 
                 data = fcasts)

fit <- mgcv::gam(brier ~ turker + has_arima + sees_chart + sees_model + 
                   s(x, by = sees_model, k = 5), 
                 data = fcasts)

fit <- mgcv::gam(brier ~ turker + has_arima + sees_chart + sees_model + 
                   s(x, by = sees_model, k = 5) + s(x, by = sees_chart, k = 5), 
                 data = fcasts)

summary(fit)
plot(fit, residuals = TRUE, pages = 1)
gam.check(fit)

ggplot(fcasts, aes(x = date, y = brier)) +
  geom_point(alpha = .05) +
  hrbrthemes::theme_ipsum_rc() +
  geom_smooth()

# Proportion of forecasts by Turkers
fcasts %>%
  mutate(date = lubridate::floor_date(date, "week", week_start = 1)) %>%
  group_by(date) %>%
  summarize(Volunteer = sum(!turker),
            Turker = sum(turker)) %>%
  gather(Group, value, -date) %>%
  ggplot(., aes(x = date, y = value, fill = Group)) +
  geom_area() +
  theme_ipsum_rc() +
  labs(x = "Date", y = "Number of forecasts per week")


user_clean %>%
  group_by(condition) %>%
  summarize(total_users = length(unique(user_id)),
            turkers     = length(unique(user_id[turker])),
            volunteers  = total_users - turkers,
            forecasts = n())




user <- user_raw %>%
  left_join(., closed_qs, by = "ifp_id")

user%>%filter(turker=="True")%>%pull(date)%>%min()

user %>%
  group_by(condition)%>%summarize(mean(brier))

ggplot(user, aes(x = condition, y = brier)) +
  geom_boxplot()

user %>%
  filter(has_arima==FALSE)%>%group_by(condition)%>%summarize(mean(brier))

user %>%
  filter(has_arima==TRUE)%>%group_by(condition)%>%summarize(mean(brier))

user %>%
  filter(has_arima==FALSE) %>%
  ggplot(., aes(color = condition, x = brier)) +
  geom_density() +
  hrbrthemes::theme_ipsum_rc()


user %>%
  filter(date>="2018-06-15") %>%
  group_by(condition)%>%summarize(mean(brier))

user %>%
  filter(date>="2018-06-15") %>%
  filter(has_arima==FALSE)%>%group_by(condition)%>%summarize(mean(brier))

user %>%
  filter(date>="2018-06-15") %>%
  filter(has_arima==TRUE)%>%group_by(condition)%>%summarize(mean(brier))

user %>%
  group_by(condition, turker, has_arima) %>%
  summarize(mean(brier))


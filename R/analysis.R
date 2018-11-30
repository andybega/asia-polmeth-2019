library("tidyverse")
library("lme4")
library("mgcv")
library("hrbrthemes")
library("MuMIn")
library("betareg")
library("rstan")
library("rstanarm")
library("forcats")

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
  select(ifp_id, date, has_historic_data, has_arima, needs_data_agg,
         num_options, p_correct, brier, Forecaster, condition, sub_condition_1,
         sees_chart, sees_model)
all_fcasts <- bind_rows(df1, df2) %>%
  left_join(data_sources %>% select(ifp_id, data_source), by = "ifp_id") %>%
  replace_na(list(data_source = "other")) %>%
  # add in summary info for chart/arima status
  mutate(Data_status = case_when(
    has_historic_data & has_arima ~ "Chart and model",
    has_historic_data & !has_arima ~ "Chart only",
    !has_historic_data ~ "None",
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
  summarize_at(vars(one_of("has_arima", "has_historic_data", "Data_status")), unique) %>%
  group_by(has_arima, has_historic_data) %>%
  summarize(Data_status = unique(Data_status), n = n()) 
sum(by_ifp$n)
by_ifp

# Overall condition table
cond_tbl <- common_period %>%
  group_by(Forecaster) %>%
  summarize(avg_Brier = mean(brier))
cond_tbl















# Compare human and machine forecasts -------------------------------------

ggplot(all_fcasts, aes(x = date, y = brier)) +
  geom_point(alpha = .1) +
  geom_smooth(aes(color = Forecaster), se = FALSE)

ggplot(all_fcasts, aes(x = date, y = brier)) +
  geom_point(alpha = .1) +
  geom_smooth(aes(color = condition), se = FALSE)

ggplot(all_fcasts, aes(x = date, y = brier)) +
  geom_point(alpha = .1) +
  geom_smooth(aes(color = interaction(condition, Forecaster)), se = FALSE)



common_period %>%
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

table(common_period$Forecaster, common_period$condition)

common_period %>%
  filter(sub_condition_1 %in% c("team", "machine")) %>%
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

# 1892 Oil production in Iraq in June 2018
# This is the question we used at the site visit to illustrate bad data. 
# The B volunteers again did very well
# C volunteers did worse than machine, but turker C did more like machine.
f1892 <- filter(all_fcasts, ifp_id==1892)
table(f1892$Forecaster, f1892$condition)
f1892 %>% 
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

# 911 ICEWS Palestine
# the ICEWS data were badly aggregated
f911 <- filter(all_fcasts, ifp_id==911)
table(f911$Forecaster, f911$condition)
f911 %>% 
  group_by(Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

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

# Look at groupings by question type
common_period %>%
  group_by(num_options, Forecaster, condition) %>%
  summarize(avg_Brier = mean(brier), n = n())

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

# Summary statistics ------------------------------------------------------

nrow(fcasts)

# Unique IFPs
length(unique(fcasts$ifp_id))

# Forcasts by forecaster type
table(fcasts$turker)

# How many IFPs had no chart, no model?
fcasts %>%
  group_by(ifp_id) %>%
  summarize(has_arima = unique(has_arima),
            has_historic_data = unique(has_historic_data)) %>%
  ungroup() %>%
  summarize(ifps = n(),
            has_arima = sum(has_arima),
            has_historic_data = sum(has_historic_data))


# Specific questions ------------------------------------------------------

# 1003 Loya Jirga
# System persistently kept showing wrong chart, with ACLED riots/protests
# B volunteers have lower Brier on this, why?
# C turkers as expected had worse score than A turkers
f1003 <- filter(fcasts, ifp_id==1003)
table(f1003$turker)
table(f1003$condition)
f1003 %>% 
  group_by(turker, condition) %>%
  summarize(avg_brier = mean(brier), n = n())

# 1892 Oil production in Iraq in June 2018
# This is the question we used at the site visit to illustrate bad data. 
# The B volunteers again did very well
# C volunteers did worse than machine, but turker C did more like machine.
f1892 <- filter(fcasts, ifp_id==1892)
table(f1892$turker)
table(f1892$condition)
f1892 %>% 
  group_by(turker, condition) %>%
  summarize(avg_brier = mean(brier), n = n())
filter(machine_fcasts, ifp_id==1892) %>%
  summarize(avg_brier = mean(brier), n = n())

# 911 ICEWS Palestine
# the ICEWS data were badly aggregated
f911 <- filter(fcasts, ifp_id==911)
table(f911$turker)
table(f911$condition)
f911 %>% 
  group_by(turker, condition) %>%
  summarize(avg_brier = mean(brier), n = n())
filter(machine_fcasts, ifp_id==911) %>%
  summarize(avg_brier = mean(brier), n = n())

# 902 ICEWS Palestine
# seems to not be in data


# 866
# the ICEWS data were badly aggregated
f866 <- filter(fcasts, ifp_id==866)
table(f911$turker)
table(f911$condition)
f911 %>% 
  group_by(turker, condition) %>%
  summarize(avg_brier = mean(brier), n = n())
filter(machine_fcasts, ifp_id==911) %>%
  summarize(avg_brier = mean(brier), n = n())

# Conditions --------------------------------------------------------------


user_fcasts %>%
  group_by(condition) %>% 
  summarize(mean_brier = mean(brier))
user_fcasts %>%
  filter(date >= "2018-05-02") %>%
  group_by(condition) %>% 
  summarize(mean_brier = mean(brier))

user_fcasts %>%
  group_by(turker, sees_chart, sees_model) %>%
  summarize(mean_brier = mean(brier))

cond_tbl <- user_fcasts %>%
  group_by(condition, turker, sees_chart, sees_model, has_historic_data) %>%
  summarize(mean_brier = mean(brier),
            n_ifps = length(unique(ifp_id)),
            n_forecasts = n()) %>%
  ungroup() %>%
  select(condition, turker, has_historic_data, sees_chart, sees_model, everything()) %>%
  arrange(condition, turker, has_historic_data, sees_chart, mean_brier)

cond_tbl %>%
  arrange(condition, turker, has_historic_data) %>%
  mutate(condition = fct_recode(condition, A = "a", B = "b", C = "c"),
         turker = fct_recode(as.character(turker), Yes = "TRUE", No = "FALSE"),
         has_historic_data = fct_recode(as.character(has_historic_data), Yes = "TRUE", No = "FALSE")
         ) %>%
  rename(Condition = condition, Turker = turker, `Has data` = has_historic_data, 
         `Saw chart` = sees_chart, `Saw model` = sees_model, 
         `Mean Brier` = mean_brier, Questions = n_ifps, Forecasts = n_forecasts) %>%
  write_csv("output/tables/brier-by-conditions.csv")


# fur turker, diffs are  -0.009683274 -0.074542881  0.013981207 -0.037984003
cond_tbl %>% 
  arrange(condition, has_arima) %>%
  filter(condition!="b") %>%
  pull(mean_brier) %>%
  diff(.) %>% 
  `[`(seq(1, 7, by = 2))
# for has_arima, diffs are 0.17083689 0.10597728 0.13141347 0.14777215 0.09580694
# turkers seem to do better when they have model, little effect for humans
cond_tbl %>% 
  arrange(condition, turker, has_arima) %>%
  pull(mean_brier) %>%
  diff(.) %>% 
  `[`(seq(1, 9, by = 2))
# sees_chart, diffs are 0.13141347 0.14777215 0.09580694
# however, also need to take into account has_arima difficulty
# based on that, chart maybe helps reduce by about .17 - .13 ~ .04
cond_tbl %>% 
  filter(condition!="a") %>%
  arrange(condition, turker, has_arima) %>%
  pull(mean_brier) %>%
  diff(.) %>% 
  `[`(seq(1, 5, by = 2))
# sees_model, diffs are 0.14777215 0.09580694
# helps turkers, but hurts humans? would also be consistent with better
# machine accuracy later in the RCT, since turkers didn't join in first few
# weeks
cond_tbl %>% 
  filter(condition=="c") %>%
  arrange(condition, turker, has_arima) %>%
  pull(mean_brier) %>%
  diff(.) %>% 
  `[`(seq(1, 3, by = 2))


# Overall, turkers actually had better (a) and no worse (c) accuracy compared
# to humans. But this disregards IFP, etc. difference. 
fcasts %>%
  filter(condition!="b") %>%
  group_by(condition, turker) %>%
  summarize(mean_brier = mean(brier)) 
# What if we cut off the initial period?
fcasts %>%
  filter(date >= "2018-05-02") %>%
  filter(condition!="b") %>%
  group_by(condition, turker) %>%
  summarize(mean_brier = mean(brier)) 
# Turkers do about .036, 0.031 worse

# .168?
fit <- lm(brier ~ turker + has_arima + sees_chart + sees_model, data = fcasts)
predict(fit, newdata = data.frame(
  turker = FALSE,
  has_arima = FALSE,
  sees_chart = 0,
  sees_model = 0
))

df <- fcasts
df$turker <- ifelse(df$turker==TRUE, 1, -1)
df$has_arima <- ifelse(df$has_arima==TRUE, 1, -1)
df$sees_chart <- ifelse(df$sees_chart==1, 1, -1)
df$sees_model <- ifelse(df$sees_model==1, 1, -1)
fit <- lm(brier ~ turker + has_arima + sees_chart + sees_model, data = df)
predict(fit, newdata = data.frame(
  turker = -1,
  has_arima = -1,
  sees_chart = -1,
  sees_model = -1
))


fcasts %>%
  group_by(turker, sees_chart, sees_model, has_arima) %>%
  summarize(mean_brier = mean(brier))

ggplot(fcasts, aes(x = factor(condition), y = brier)) +
  geom_boxplot()

ggplot(fcasts, aes(x = interaction(turker, sees_chart, sees_model), y = brier)) +
  geom_boxplot()



fit <- aov(brier ~ factor(turker)*factor(sees_chart)*factor(sees_model), data = fcasts)
model.tables(fit, "means")

summary(lm(brier ~ turker + has_arima + sees_chart + sees_model, data = fcasts))
summary(lm(log(brier + 0.001) ~ turker + has_arima + sees_chart + sees_model, data = fcasts))


# How much variation is in there in skill between users?
user_tbl <- fcasts %>%
  group_by(user_id) %>%
  summarize(mean_brier = mean(brier), 
            forecasts = n())
hist(user_tbl$mean_brier)
mean(user_tbl$mean_brier)
sd(user_tbl$mean_brier)

# How much, relative to how much is possible, are users forecasting?
hist(user_tbl$forecasts)
# add derivation of number of possible forecasts (daily)

# How much variation is there in dificulty between IFPs?
ifp_tbl <- fcasts %>%
  group_by(ifp_id) %>%
  summarize(mean_brier = mean(brier), 
            forecasts = n())
hist(ifp_tbl$mean_brier)
mean(ifp_tbl$mean_brier)
sd(ifp_tbl$mean_brier)

# Are users forecasting more for easy questions?
ggplot(ifp_tbl, aes(x = mean_brier, y = forecasts)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
summary(lm(forecasts ~ mean_brier, data = ifp_tbl))
# This also depends on how long a question was open, take that into account



# Do forecasters in different conditions choose easier questions?
df2 <- fcasts %>% 
  group_by(ifp_id, condition, turker, date) %>%
  summarize(has_arima = unique(has_arima), 
            has_historic_data = unique(has_historic_data),
            MDB = mean(brier),
            start_date = min(date)) %>%
  group_by(ifp_id, condition, turker) %>% 
  summarize(has_arima = unique(has_arima), 
            has_historic_data = unique(has_historic_data), 
            q_cond_n = n(),
            avg_MDB = mean(MDB),
            start_date = min(start_date)) %>% 
  group_by(ifp_id) %>% 
  mutate(q_n = sum(q_cond_n),
         start_date = min(start_date)) %>% 
  ungroup() %>% 
  arrange(desc(q_n))
ggplot(df2, aes(x = factor(ifp_id), y = avg_MDB)) +
  geom_point()


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


# Linear model ------------------------------------------------------------

fit_lm1 <- lm(brier ~ turker + has_arima + sees_chart + sees_model, data = fcasts)
summary(fit_lm1)

# Look only from when Turkers entered
fit_lm2 <- lm(brier ~ turker + has_arima + sees_chart + sees_model, data = fcasts[fcasts$date >= "2018-05-02", ])
summary(fit_lm2)

# do with MDBs
df <- fcasts %>%
  filter(date >= "2018-05-02") %>%
  group_by(ifp_id, date, turker, has_arima, sees_chart, sees_model) %>%
  summarize(MDB = mean(brier))
fit_lm3 <- lm(MDB ~ turker + has_arima + sees_chart + sees_model, data = df)
summary(fit_lm3)

# do with MMDBs, i.e. at IFP level
df <- fcasts %>%
  filter(date >= "2018-05-02") %>%
  group_by(ifp_id, date, turker, has_arima, sees_chart, sees_model) %>%
  summarize(MDB = mean(brier)) %>%
  group_by(ifp_id, turker, has_arima, sees_chart, sees_model) %>%
  summarize(MMDB = mean(MDB))
fit_lm4 <- lm(MMDB ~ turker + has_arima + sees_chart + sees_model, data = df)
summary(fit_lm4)



summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = fcasts[fcasts$date<"2018-06-15",]))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = fcasts[fcasts$date>="2018-06-16",]))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = fcasts[fcasts$date>="2018-07-01",]))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = fcasts[fcasts$date>="2018-07-16",]))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = fcasts[fcasts$date>="2018-08-01",]))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = fcasts[fcasts$date>="2018-08-16",]))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = fcasts[fcasts$date>="2018-09-01",]))



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


# LMER --------------------------------------------------------------------


fit_lmer1 <- lmer(brier ~ turker + has_arima + sees_chart + sees_model + 
                    (1|ifp_id), data = fcasts)
summary(fit_lmer1)
MuMIn::r.squaredGLMM(fit_lmer1)

fit_lmer2 <- lmer(brier ~ x + turker + has_arima + x*sees_chart + x*sees_model + (1|ifp_id), data = fcasts)
summary(fit_lmer2)
MuMIn::r.squaredGLMM(fit_lmer2)

fit_lmer3 <- lmer(brier ~ x + turker + has_arima + x*sees_chart + x*sees_model + 
                    (1|ifp_id) + (1|user_id), data = fcasts)
summary(fit_lmer3)
MuMIn::r.squaredGLMM(fit_lmer3)

fit <- lm(brier ~ turker + has_arima + sees_chart + sees_model + factor(ifp_id), data = fcasts)
summary(fit)



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



summary(lm(brier ~ as.logical(sees_chart)*needs_data_agg, data = user[user$has_arima,]))
summary(lm(brier ~ as.logical(sees_chart)*needs_data_agg + sees_model, data = user[user$has_arima,]))
summary(lm(brier ~ as.logical(sees_chart)*needs_data_agg + sees_model + turker, data = user[user$has_arima & user$date>="2018-06-15",]))
summary(lm(brier ~ -1 + as.logical(sees_chart):needs_data_agg, data = user[user$has_arima,]))
# ME chart:
#   no agg -.1
#   agg -.1 + .12 ~ 0
summary(lm(brier ~ -1+sees_chart*needs_data_agg + turker, data = user))
summary(lm(brier ~ -1+sees_chart*needs_data_agg + turker + has_arima, data = user))
summary(lm(brier ~ as.logical(sees_chart)*needs_data_agg, data = user[user$has_arima,]))

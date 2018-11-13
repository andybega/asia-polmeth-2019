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


fcasts <- read_rds(file.path(out_dir, "user_forecasts_w_features.rds")) %>%
  mutate(date = as.Date(date),
         x = as.numeric(date) - min(as.numeric(date)),
         sees_chart = sees_chart==1,
         sees_model = sees_model==1) 

#fcasts$sees_model <- factor(fcasts$sees_model, labels = c("No", "Yes"))
#fcasts$sees_chart <- factor(fcasts$sees_chart, labels = c("No", "Yes"))

fcasts %>%
  group_by(condition) %>% 
  summarize(mean_brier = mean(brier))

fcasts %>%
  group_by(turker, sees_chart, sees_model) %>%
  summarize(mean_brier = mean(brier))

cond_tbl <- fcasts %>%
  group_by(condition, turker, sees_chart, sees_model, has_arima) %>%
  summarize(mean_brier = mean(brier),
            n_ifps = length(unique(ifp_id)),
            n_forecasts = n()) %>%
  ungroup() %>%
  select(condition, turker, has_arima, sees_chart, sees_model, everything()) %>%
  arrange(condition, turker, has_arima, sees_chart, mean_brier)

cond_tbl %>%
  arrange(condition, has_arima) %>%
  mutate(condition = fct_recode(condition, A = "a", B = "b", C = "c"),
         turker = fct_recode(as.character(turker), Yes = "TRUE", No = "FALSE"),
         has_arima = fct_recode(as.character(has_arima), Yes = "TRUE", No = "FALSE")
         ) %>%
  rename(Condition = condition, Turker = turker, `Has data` = has_arima, 
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
  summarize(mean_brier = mean(brier)) %>%
  select(condition, turker, has_arima, sees_chart, sees_model, mean_brier) %>%
  arrange(condition, turker, has_arima, sees_chart, mean_brier)

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



# Plots over time ---------------------------------------------------------


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

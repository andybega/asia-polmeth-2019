library("tidyverse")

dropbox_path <- "~/Dropbox/Work/asia-polmeth-2019"




user_clean %>%
  group_by(condition) %>%
  summarize(total_users = length(unique(user_id)),
            turkers     = length(unique(user_id[turker])),
            volunteers  = total_users - turkers,
            forecasts = n())



mbrier <- function(f, o, ordered = TRUE) {
  prediction = f
  outcome = o

  stopifnot(length(prediction)==length(outcome))
  stopifnot(all(outcome %in% c(0L, 1L, NA)))
  stopifnot(all.equal(sum(prediction), sum(outcome), 1L) | any(is.na(prediction)))

  if (ordered) {
    pairs <- NULL
    for (i in 1:(length(prediction)-1)) {
      p_i <- sapply(split(prediction, 1:length(prediction) > i), sum)
      o_i <- sapply(split(outcome, 1:length(outcome) > i), max)
      pairs <- c(pairs, sum((p_i - o_i)^2))
    }
    brier <- mean(pairs)
  } else {
    brier <- sum((prediction - outcome)^2)
  }

  return(brier)
}


user_raw <- read_csv("dump_user_forecasts_08_02_2018_21_00_22.csv") %>%
  mutate_at(vars(contains("date")), as.Date) %>%
  arrange(ifp_id, date, user_id) %>%
  group_by(ifp_id, date, user_id) %>%
  mutate(fcast_seq = 1:n()) %>%
  mutate(
    num_options = case_when(
      num_options==1 ~ 2L,
      TRUE ~ num_options
    ),
    option_2 = case_when(
      is.na(option_2) ~ 1 - option_1,
      TRUE ~ option_2
    )) %>%
  tidyr::gather(option, prob, starts_with("option")) %>%
  mutate(prob = prob / 100) %>%
  mutate(option = str_replace(option, "option\\_", "")) %>%
  filter(option <= num_options)


suppressWarnings(
  q_dump <- read_csv("dump_questions_08_02_2018_11_16_40.csv",
             col_types = cols(
               .default = col_character(),
               ifp_id = col_integer(),
               discover_id = col_integer(),
               start_date = col_datetime(format = ""),
               end_date = col_datetime(format = ""),
               resolved_date = col_datetime(format = ""),
               voided_date = col_datetime(format = ""),
               num_options = col_integer(),
               is_ordinal = col_logical(),
               is_resolved = col_logical(),
               is_voided = col_logical(),
               has_chart = col_logical()
             )) %>%
    mutate_at(vars(contains("date")), as.Date) %>%
    mutate_at(vars(starts_with("has"), starts_with("is")), ~.x=="True")
)

closed_qs <- q_dump %>%
  filter(!is.na(resolved_date)) %>%
  mutate(resolution_opt = pmap(
    .,
    function(resolution, option_1, option_2, option_3, option_4, option_5, ...) {
      match(resolution, list(option_1, option_2, option_3, option_4, option_5))
    })  %>% unlist()) %>%
  select(ifp_id, resolution, resolution_opt, start_date, end_date, title, has_arima)

user <- user_raw %>%
  left_join(., closed_qs, by = "ifp_id")

user%>%filter(turker=="True")%>%pull(date)%>%min()

agg_q <- q_dump %>%
  group_by(ifp_id, title) %>%
  summarize() %>%
  filter(str_detect(title, "ACLED|ICEWS|earthquakes|Boko|sea ice")) %>%
  pull(ifp_id)

user <- user %>%
  filter(!is.na(resolution)) %>%
  # drop pre-Turker time
  filter(date>="2018-05-02") %>%
  arrange(ifp_id, date, user_id, fcast_seq) %>%
  mutate(correct = as.integer(option==resolution_opt)) %>%
  group_by(ifp_id, date, user_id, fcast_seq) %>%
  summarize(brier = mbrier(prob, correct, ordered = TRUE),
            turker = unique(turker),
            condition = unique(condition),
            num_options = unique(num_options),
            has_arima = unique(has_arima)) %>%
  ungroup() %>%
  mutate(sees_model = has_arima*(condition=="c"),
         sees_chart = has_arima*(condition %in% c("b", "c")),
         needs_data_agg = ifp_id %in% agg_q)

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

summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = user))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = user[user$date<"2018-06-15",]))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = user[user$date>="2018-06-15",]))
summary(lm(brier ~ turker + sees_chart + sees_model + has_arima, data = user[user$date>="2018-07-01",]))



summary(lm(brier ~ as.logical(sees_chart)*needs_data_agg, data = user[user$has_arima,]))
summary(lm(brier ~ as.logical(sees_chart)*needs_data_agg + sees_model, data = user[user$has_arima,]))
summary(lm(brier ~ as.logical(sees_chart)*needs_data_agg + sees_model + turker, data = user[user$has_arima & user$date>="2018-06-15",]))
summary(lm(brier ~ -1 + as.logical(sees_chart):needs_data_agg, data = user[user$has_arima,]))
# ME chart:
#   no agg -.1
#   agg -.1 + .12 ~ 0
summary(lm(brier ~ -1+sees_chart*needs_data_agg + turker, data = user))
summary(lm(brier ~ -1+sees_chart*needs_data_agg + turker + has_arima, data = user))

library("betareg")
summary(lm(brier ~ as.logical(sees_chart)*needs_data_agg, data = user[user$has_arima,]))
user$brier01 = user$brier/2
summary(betareg(brier01 ~ as.logical(sees_chart)*needs_data_agg, data = user[user$has_arima,]))



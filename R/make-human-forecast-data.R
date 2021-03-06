#
#   Process the raw staff dumps to make the analysis data
#

library("tidyverse")
library("futile.logger")
library("future.apply")

dropbox_path  <- "~/Dropbox/Work/asia-polmeth-2019"
in_dir  <- file.path(dropbox_path, "input-data")
out_dir <- file.path(dropbox_path, "output")
user_dump     <- "dump_user_forecasts_09_07_2018_09_15_12.csv"
answer_key <- "answer_key.rds"


source("R/mbrier.R")

answers <- read_rds(file.path(out_dir, answer_key))

# The warnings are related to missing option_2, option_3, etc.
user_raw <-  file.path(dropbox_path, "input-data", user_dump) %>%
  read_csv(.)

user_clean <- user_raw %>%
  # turker is string true/false, make logical
  mutate(turker = case_when(
    turker=="False" ~ FALSE,
    turker=="True"  ~ TRUE,
    TRUE ~ NA
  )) %>%
  # convert options from percent to probability
  mutate_at(vars(starts_with("option_")), ~ ./100) %>%
  # binary questions show as having only 1 option, make it 2 for "yes"/"no"
  # and fill in 1-p for 2nd option
  mutate(
    num_options = case_when(
      num_options==1 ~ 2L,
      TRUE ~ num_options
    ),
    option_2 = case_when(
      is.na(option_2) ~ 1 - option_1,
      TRUE ~ option_2
    )) 

# Check that options sum to 1
p_range <- user_clean %>% 
  gather(option, p, starts_with("option_")) %>%
  group_by(date, user_id, ifp_id) %>% 
  summarize(p = sum(p, na.rm = TRUE)) %>%
  pull(p) %>%
  range(.)
if (p_range[1]<0.999999 | p_range[2]>1.000001) {
  stop("p is wrong")
}



# Add in answers
user_w_answer <- left_join(user_clean, answers, by = "ifp_id") %>%
  # 801, some weird test IFP or something
  filter(!is.na(correct_option)) %>%
  arrange(ifp_id, user_id, date) %>%
  rename(needs_data_agg = needs_agg) %>%
  select(ifp_id, user_id, date, everything(), 
         # don't need these
         -discover_id, -rationale)
flog.info("%s%% of %s forecasts are dropping out because no resolution is available",
          (sum(is.na(user_w_answer$correct_option))/nrow(user_w_answer)*100) %>% round(1),
          format(nrow(user_w_answer), big.mark = ","))
# Add in Brier and condition info
user_w_brier <- user_w_answer %>%
  filter(!is.na(correct_option)) %>%
  # make long to get Brier calculation
  gather(option, p, starts_with("option_")) %>%
  filter(!is.na(p)) %>%
  mutate(oo = option==correct_option) %>%
  group_by(ifp_id, user_id, date) %>%
  mutate(
    p_correct = p[oo],
    brier = mbrier(p, oo, ordered = unique(is_ordinal)),
    oo = NULL) %>%
  ungroup() %>%
  spread(option, p) 
  
# Add in condition indicators
user_w_features <- user_w_brier %>%
  mutate(sees_model = has_arima*(condition=="c"),
         sees_chart = has_historic_data*(condition %in% c("b", "c")))

flog.info("Clean user forecasts: N = %s",
          format(nrow(user_clean), big.mark = ","))
flog.info("User forecasts with answer: N = %s",
          format(nrow(user_w_features), big.mark = ","))

write_rds(user_w_features, file.path(out_dir, "user_forecasts.rds"))
#write_csv(user_w_features, file.path(out_dir, "user_forecasts.csv"))





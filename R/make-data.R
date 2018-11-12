#
#   Process the raw staff dumps to make the analysis data
#

library("tidyverse")
library("futile.logger")

dropbox_path  <- "~/Dropbox/Work/asia-polmeth-2019"
in_dir  <- file.path(dropbox_path, "input-data")
out_dir <- file.path(dropbox_path, "output")
user_dump     <- "dump_user_forecasts_09_07_2018_09_15_12.csv"
question_dump <- "dump_questions_11_12_2018_08_51_25.csv" 


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

# The latest question dump don't have IFP_id's anymore, need to back-engineer 
# this based on title
ifp_id_mapping <- file.path(in_dir, "dump_questions_09_07_2018_09_30_55.csv") %>%
  read_csv(., col_types = cols(
    .default = col_character(),
    ifp_id = col_integer(),
    discover_id = col_integer(),
    start_date = col_datetime(format = ""),
    end_date = col_datetime(format = ""),
    resolved_date = col_datetime(format = ""),
    voided_date = col_datetime(format = ""),
    num_options = col_integer()
  )) %>%
  select(ifp_id, title)

q_dump <- file.path(in_dir, question_dump) %>%
  read_csv(.,
           col_types = cols(
             .default = col_character(),
             start_date = col_datetime(format = ""),
             end_date = col_datetime(format = ""),
             resolved_date = col_datetime(format = ""),
             voided_date = col_datetime(format = ""),
             num_options = col_integer(),
             is_ordinal = col_logical(),
             is_resolved = col_logical(),
             is_voided = col_logical()
           )) %>%
  mutate_at(vars(contains("date")), as.Date) %>%
  mutate_at(vars(starts_with("has"), starts_with("is")), ~.x=="True") %>%
  # add in IFP ID
  full_join(ifp_id_mapping, by = "title")

# only 2 practice questions
no_q  <- filter(q_dump, !is.na(ifp_id) & is.na(question_id))
# since no unmatched IFP IDs, should be all good
no_id <- filter(q_dump, is.na(ifp_id))

q_dump <- q_dump %>%
  filter(!is.na(ifp_id))

# Questions that require data aggregation
agg_q <- q_dump %>%
  group_by(ifp_id, title) %>%
  summarize() %>%
  filter(str_detect(title, "ACLED|ICEWS|earthquakes|Boko|sea ice")) %>%
  pull(ifp_id)

# Get correct option and has_arima, has_chart for RCT-A (closed) questions
answers <- q_dump %>%
  filter(!is.na(resolution)) %>%
  # wide to long so we can code correct option_#
  tidyr::gather(option, option_label, starts_with("option_")) %>%
  dplyr::mutate(is_correct = option_label==resolution) %>%
  # take out incorrect options now
  filter(is_correct) %>%
  rename(correct_option = option) %>%
  select(ifp_id, has_historic_data, has_arima, correct_option)
  


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
    )) %>%
  # fill in missing option p values
  replace_na(list(option_3 = 0, option_4 = 0, option_5 = 0))

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
  arrange(ifp_id, user_id, date) %>%
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
  mutate(oo = option==correct_option) %>%
  group_by(ifp_id, user_id, date) %>%
  mutate(brier = mbrier(p, oo, ordered = TRUE),
         oo = NULL) %>%
  ungroup() %>%
  spread(option, p) 
  
# Add in condition indicators
user_w_features <- user_w_brier %>%
  mutate(sees_model = has_arima*(condition=="c"),
         sees_chart = has_arima*(condition %in% c("b", "c")),
         needs_data_agg = ifp_id %in% agg_q)

flog.info("Clean user forecasts: N = %s",
          format(nrow(user_clean), big.mark = ","))
flog.info("User forecasts with answer: N = %s",
          format(nrow(user_w_features), big.mark = ","))

write_rds(user_clean, file.path(out_dir, "user_forecasts_clean.rds"))
write_csv(user_clean, file.path(out_dir, "user_forecasts_clean.csv"))

write_rds(user_w_features, file.path(out_dir, "user_forecasts_w_features.rds"))
write_csv(user_w_features, file.path(out_dir, "user_forecasts_w_features.csv"))





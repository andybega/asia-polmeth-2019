

dropbox_path  <- "~/Dropbox/Work/asia-polmeth-2019"
in_dir  <- file.path(dropbox_path, "input-data")
out_dir <- file.path(dropbox_path, "output")
question_dump <- "dump_questions_11_29_2018_20_17_01.csv"


q_dump <- read_csv(
  file.path(in_dir, question_dump),
  col_types = cols(
    .default = col_character(),
    ifp_id = col_integer(),
    discover_id = col_integer(),
    start_date = col_datetime(format = ""),
    end_date = col_datetime(format = ""),
    resolved_date = col_datetime(format = ""),
    voided_date = col_datetime(format = ""),
    num_options = col_integer()
  )
) %>%
  mutate_at(vars(contains("date")), as.Date) %>%
  mutate_at(vars(starts_with("has"), starts_with("is")), ~.x=="True")

# only 2 practice questions
no_id <- filter(q_dump, is.na(ifp_id))

q_dump <- q_dump %>%
  # filter practice questions
  filter(!str_detect(title, "Practice Q"))

# Questions that require data aggregation
agg_q <- q_dump %>%
  group_by(ifp_id, title) %>%
  summarize() %>%
  filter(str_detect(title, "ACLED|ICEWS|earthquakes|Boko|sea ice")) %>%
  pull(ifp_id)

# Get correct option and has_arima, has_chart for RCT-A (closed) questions
answers <- q_dump %>%
  # make sure only RCT-A IFPs
  filter(end_date <= "2018-09-07") %>%
  filter(!is.na(resolution)) %>%
  # wide to long so we can code correct option_#
  tidyr::gather(option, option_label, starts_with("option_")) %>%
  dplyr::mutate(is_correct = option_label==resolution,
                needs_agg = ifp_id %in% agg_q) %>%
  # take out incorrect options now
  filter(is_correct) %>%
  rename(correct_option = option) %>%
  select(ifp_id, has_historic_data, is_ordinal, has_arima, needs_agg, correct_option)

write_rds(answers, file.path(out_dir, "answer_key.rds"))

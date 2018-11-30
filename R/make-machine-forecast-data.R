
dropbox_path  <- "~/Dropbox/Work/asia-polmeth-2019"
in_dir  <- file.path(dropbox_path, "input-data")
out_dir <- file.path(dropbox_path, "output")


answers <- read_rds(file.path(out_dir, "answer_key.rds")) %>%
  rename(needs_data_agg = needs_agg)

# Load fcast dump file and add resolutions
# Use machine forecasts in "aggreagted_..." file instead of "dump_machine..."
# because the latter is apparently inaccurate
# However, this only covers back to April 13th, so take fcast_dump... for rest
agg_dump_file <- dir(in_dir,
                     pattern = "dump\\_aggregated",
                     full.names = TRUE) %>%
  tail(1)
agg_dump <- read_csv(agg_dump_file,
                     col_types = cols(
                       date = col_datetime(format = ""),
                       slot_id = col_character(),
                       method_name = col_character(),
                       method_type = col_character(),
                       ifp_id = col_integer(),
                       discover_id = col_integer(),
                       num_options = col_integer(),
                       option_1 = col_double(),
                       option_2 = col_double(),
                       option_3 = col_double(),
                       option_4 = col_double(),
                       option_5 = col_double()
                     ))
agg_dump <- agg_dump %>%
  filter(method_name=="Machine_only_models") %>%
  mutate(date = as.Date(date)) %>%
  left_join(., answers %>% select(ifp_id, has_historic_data), by = "ifp_id") %>%
  # if no historic_data, take it out
  filter(has_historic_data %in% TRUE) %>%
  # some binary questions only have 1 option, fix this
  mutate(
    num_options = case_when(
      is.na(option_2) ~ 2L,
      TRUE ~ num_options
    ),
    option_2 = case_when(
      is.na(option_2) ~ 1 - option_1,
      TRUE ~ option_2
    ),
    has_historic_data = NULL) %>%
  # if multiple forecasts for same IFP, take last one only
  group_by(ifp_id, date) %>%
  filter(1:n()==n())

min(agg_dump$date)

# Use other dump file to fill in forecasts before April 13th
fcast_dump_file <- dir(file.path(in_dir),
                       pattern = "dump\\_machine\\_forecasts",
                       full.names = TRUE) %>%
  tail(1)
fcast_dump <- suppressWarnings(read_csv(fcast_dump_file,
                                        col_types = cols(
                                          date = col_date(),
                                          days = col_integer(),
                                          qid = col_integer(),
                                          discover_id = col_integer(),
                                          type = col_character(),
                                          num_options = col_integer(),
                                          op1 = col_double(),
                                          op2 = col_double(),
                                          op3 = col_double(),
                                          op4 = col_double(),
                                          op5 = col_double()
                                        ))) 
fcast_dump <- fcast_dump %>%
  filter(date < "2018-04-13") %>%
  filter(machine_model=="arima")

machine_fcasts <- bind_rows(agg_dump, fcast_dump) %>%
  left_join(answers, by = "ifp_id") %>%
  select(ifp_id, date, has_historic_data, has_arima, needs_data_agg, num_options,
         correct_option, starts_with("option_")) %>%
  # some questions appear to have forecasts but has_arima is false; these are 
  # dummy forecasts with all weights set to 0.2 or some such
  filter(has_arima)

# add brier scores
mf_long <- machine_fcasts %>%
  gather(option, p, starts_with("option_")) %>%
  filter(!is.na(p))
mf_long <- mf_long %>%
  mutate(correct = (correct_option==option),
         p_correct = p[correct],
         brier = mbrier(p, as.integer(correct), ordere = TRUE)) %>%
  ungroup()
# make sure only one correct per forecast
mf_long %>% 
  group_by(ifp_id, date) %>% 
  summarize(correct = sum(correct)) %>% 
  group_by(correct) %>% 
  summarize(n=n()) %>%
  ungroup()

machine_fcasts <- mf_long %>%
  mutate(correct = FALSE) %>%
  spread(option, p)

write_rds(machine_fcasts, path = file.path(out_dir, "machine_forecasts.rds"))

ggplot(machine_fcasts, aes(x = date, y = brier)) + 
  geom_point(alpha = .1) + geom_smooth(se = FALSE)

machine_fcasts %>%
  group_by(num_options) %>%
  summarize(avg_brier = mean(brier))


fcast_dump <- read_csv(
  file.path(in_dir, "dump_machine_forecasts_11_29_2018_20_17_42.csv")
) %>%
  filter(machine_model=="arima") %>%
  filter(!num_options==0)

# why discrepany with agg dump? Maybe agg dump I accidentally included some
# similarity forecasts?
check_fd <- fcast_dump %>%
  select(date, ifp_id, num_options, option_1, option_2) %>%
  mutate(mach = 1) %>%
  rename(mach_opt1 = option_1, mach_opt2 = option_2)
check_agg <- machine_fcasts %>%
  select(date, ifp_id, num_options, option_1, option_2) %>%
  mutate(agg = 1) %>%
  rename(agg_opt1 = option_1, agg_opt2 = option_2)
check <- full_join(check_fd, check_agg, by = c("ifp_id", "date", "num_options"))
# Common cases
common <- check %>% 
  filter(!is.na(agg) & !is.na(mach))
table(common$mach_opt1==common$agg_opt1)
table(common$mach_opt2==common$agg_opt2)
View(filter(common, ifp_id==1226) )
# ok just seem to be off by 1 day
# Machine cases not in agg
check1 <- check %>% 
  filter(is.na(agg))
View(filter(check, ifp_id==1433))
View(filter(check, ifp_id==1271))
filter(check1, !ifp_id %in% machine_fcasts$ifp_id)
# Agg cases not in machine
check2 <- check %>% 
  filter(is.na(mach))
filter(check2, !ifp_id %in% fcast_dump$ifp_id)
# both agg and m dump sometimes seem to be missing stuff, weird


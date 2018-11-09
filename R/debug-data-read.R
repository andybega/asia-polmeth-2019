#
#   Why is read_csv not parsing some of the dump_user... lines?
#
#   9 November 2018
#

library("tidyverse")
library("future.apply")

plan(multiprocess, workers = 3L)

dropbox_path <- "~/Dropbox/Work/asia-polmeth-2019"
out_dir <- file.path(dropbox_path, "output")

user_str <- "dump_user_forecasts_09_07_2018_09_15_12.csv" %>%
  file.path(dropbox_path, "input-data", .) %>%
  read_lines() 

str_header <- user_str[1]
str_row    <- as.list(tail(user_str, -1))

# this takes a while
parsed_rows <- future_lapply(str_row, function(x) {
  suppressWarnings(read_csv(paste0(c(str_header, x), collapse = "\n")))
})

write_rds(parsed_rows, file.path(out_dir, "debug_user_dump_read.rds"))


user_raw <- "dump_user_forecasts_09_07_2018_09_15_12.csv" %>%
  file.path(dropbox_path, "input-data", .) %>%
  read_csv(., quote = "\"") 


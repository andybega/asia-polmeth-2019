#
#   Why is read_csv not parsing some of the dump_user... lines?
#
#   9 November 2018
#

library("tidyverse")
library("future.apply")

dropbox_path <- "~/Dropbox/Work/asia-polmeth-2019"
out_dir <- file.path(dropbox_path, "output")

user_str <- "dump_user_forecasts_09_07_2018_09_15_12.csv" %>%
  file.path(dropbox_path, "input-data", .) %>%
  read_lines() 

user_raw <- "dump_user_forecasts_09_07_2018_09_15_12.csv" %>%
  file.path(dropbox_path, "input-data", .) %>%
  read_csv(., quote = "\"") 

# Parsing each row as a CSV, separately; this takes a while
if (FALSE) {
  plan(multiprocess, workers = 3L)
  
  str_header <- user_str[1]
  str_row    <- as.list(tail(user_str, -1))
  
  # this takes a while
  parsed_rows <- future_lapply(str_row, function(x) {
    suppressWarnings(read_csv(paste0(c(str_header, x), collapse = "\n"), 
                              quote = "\"",
                              col_types = cols(
                                .default    = col_character(),
                                date        = col_datetime(),
                                user_id     = col_integer(),
                                ifp_id      = col_integer(),
                                discover_id = col_integer(),
                                num_options = col_integer(),
                                option_1 = col_double(),
                                option_2 = col_double(),
                                option_3 = col_double(),
                                option_4 = col_double(),
                                option_5 = col_double()
                              )))
  })
  
  write_rds(parsed_rows, file.path(out_dir, "debug_user_dump_read.rds"))
} else {
  parsed_rows <- read_rds(file.path(out_dir, "debug_user_dump_read.rds"))
}

row_N <- sapply(parsed_rows, nrow)
table(row_N)

user_parsed_by_row <- bind_rows(parsed_rows)

length0_rows <- c(FALSE, row_N==0)
head(user_str[length0_rows])

# There are empty lines in the CSV.
# This still doesn't work. Has to be parsing each row separately.
user_str <- "dump_user_forecasts_09_07_2018_09_15_12.csv" %>%
  file.path(dropbox_path, "input-data", .) %>%
  read_lines() 
user_str <- user_str[!user_str==""]
user_parsed_from_string <- user_str %>% 
  paste0(., collapse = "\n") %>%
  read_csv(., quote = "\"",
           col_types = cols(
             .default    = col_character(),
             date        = col_datetime(),
             user_id     = col_integer(),
             ifp_id      = col_integer(),
             discover_id = col_integer(),
             num_options = col_integer(),
             option_1 = col_double(),
             option_2 = col_double(),
             option_3 = col_double(),
             option_4 = col_double(),
             option_5 = col_double()
           ))


# Are there NA rows?
na_rows <- sapply(parsed_rows, function(x) {
  var <- "date"
  if (is.null(x[, var]) | nrow(x)==0) {
    return(TRUE)
  } else {
    return(is.na(x[, var]))
  }
})
na_rows2 <- unlist(na_rows)
table(na_rows)
table(rowSums(cbind(row_N==0, na_rows)))


user_dt <- data.table::fread(
  file = file.path(dropbox_path, "input-data", "dump_user_forecasts_09_07_2018_09_15_12.csv"),
  header = TRUE, sep = ",")


user_read.csv <- read.csv(
  file = file.path(dropbox_path, "input-data", "dump_user_forecasts_09_07_2018_09_15_12.csv"),
  header = TRUE, sep = ",")


# Ok, there was actually no problem. The rationale field can contain newlines, 
# which readlines would incorrectly not escape. 
user_read.csv[31, ]
user_raw[31, ]$rationale

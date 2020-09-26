library(tidyverse)

data_path <- "~/github/"

tictoc::tic('loading data')
con <- DBI::dbConnect(RSQLite::SQLite(), glue::glue('{data_path}/master_db'))
data <- tbl(con, "cleaned_pbp") %>%
  filter(!is.na(posteam) & !is.na(epa)) %>%
  collect()
tictoc::toc()

DBI::dbDisconnect(con)


games <- readRDS(url("http://www.habitatring.com/games.rds"))

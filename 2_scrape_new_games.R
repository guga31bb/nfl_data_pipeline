source('helpers.R')

#######################################################
## this will add new games to existing db #############
#######################################################

nflfastR::update_db(
  dbdir = data_path,
  dbname = "master_db",
  tblname = "cleaned_pbp",
  force_rebuild = FALSE
  )

con <- DBI::dbConnect(RSQLite::SQLite(), glue::glue('{data_path}/master_db'))

l5 <- dplyr::tbl(con, "cleaned_pbp") %>%
  dplyr::select("game_id") %>%
  dplyr::distinct() %>%
  dplyr::collect() %>%
  dplyr::pull("game_id") %>%
  tail(5) %>%
  paste(collapse = ", ")

message(glue::glue("{lubridate::today()}: Most recent 5 games: {l5}"))

DBI::dbDisconnect(con)


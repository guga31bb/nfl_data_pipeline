source('helpers.R')

#######################################################
## this will nuke the whole thing and start over ######
#######################################################

message('You have decided to nuke the DB and start over')

if (file.exists(glue::glue('{data_path}/master_db'))) {
  tryCatch({
    file.remove(glue::glue('{data_path}/master_db'))
  },
  warning=function(w) {
    stop("converted from warning: ", conditionMessage(w))
  }
  )
}

message(glue::glue('Connecting to database.'))
con <- DBI::dbConnect(RSQLite::SQLite(), glue::glue('{data_path}/master_db'))

for (x in 1999:2020) {
  message(glue::glue('Downloading {x} games now'))
  pbp_cleaned <- readRDS(
    url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds"))
  )
  
  #no point in saving raw data anymore, only use this
  DBI::dbWriteTable(con, "cleaned_pbp", pbp_cleaned, append = TRUE)
  
}


#some sanity checks

message(glue::glue('Connecting to database.'))

DBI::dbListTables(con)
DBI::dbListFields(con, "cleaned_pbp") %>%
  tail(20)

pbp_db <- dplyr::tbl(con, "cleaned_pbp")
pbp_db %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(n=n()) %>%
  dplyr::collect() %>%
  head(11)

pbp_db %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(n=n()) %>%
  dplyr::collect() %>%
  tail(11)

pbp_db %>%
  dplyr::group_by(posteam) %>%
  dplyr::summarize(n=n()) %>%
  dplyr::collect() %>%
  head(20)

DBI::dbDisconnect(con)





source('helpers.R')
source('https://raw.githubusercontent.com/guga31bb/nflfastR-raw/master/get_current_week.R')

#######################################################
## this will nuke the whole thing and start over ######
#######################################################

message('You have decided to nuke the DB and start over')

s <- get_current_season()

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

for (x in 1999:s) {
  message(glue::glue('Downloading {x} games now'))
  pbp_cleaned <- nflreadr::load_pbp(x)
  
  #no point in saving raw data anymore, only use this
  DBI::dbWriteTable(con, "cleaned_pbp", pbp_cleaned, append = TRUE)
  
}


#some sanity checks

message(glue::glue('Connecting to database.'))

pbp_db <- dplyr::tbl(con, "cleaned_pbp")
pbp_db %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(n=n()) %>%
  dplyr::collect() %>%
  print(n = 30)

pbp_db %>%
  dplyr::group_by(posteam) %>%
  dplyr::summarize(n=n()) %>%
  dplyr::collect() %>%
  print(n = 40)

DBI::dbDisconnect(con)






library(tidyverse)

# get from xhr; copy whole thing


cookie_in = "XXXXXX"

# ################################################################################################
# team grades: week-by-week

# scraping function for one week
pff_scrape_week <- function(s, w) {
  
  url <- glue::glue("https://premium.pff.com/api/v1/teams/overview?league=nfl&season={s}&week={w}")
  
  pff_resp <- httr::GET(
    url,
    httr::add_headers(cookie = cookie_in))
  
  json_resp <- httr::content(pff_resp, as="text")
  
  raw_json <- jsonlite::fromJSON(json_resp)
  
  raw_json %>%
    pluck("team_overview") %>%
    as_tibble() %>%
    mutate(season = s, week = w) %>%
    select(season, week, team_abbr = abbreviation, starts_with("grades_"))
  
}

pff_scrape_week(2021, 1)

# get list of seasons/weeks we want
df <- expand.grid(s = 2006 : 2021, w = c(1 : 18, 28:30, 32)) %>% 
  as_tibble() %>% 
  arrange(s, w) %>%
  # get rid of week 18 for pre-2021
  filter(!(s < 2021 & w == 18))

df

# do the thing
data <- map_df(1 : nrow(df), ~{
  
  row <- df %>% dplyr::slice(.x)
  message(glue::glue("Season {row$s} and week {row$w}"))
  
  pff_scrape_week(row$s, row$w)
  
})

# check and clean up
data %>%
  filter(!is.na(grades_pass_block)) %>%
  mutate_at(c("team_abbr"), nflfastR:::team_name_fn) %>%
  mutate(
    week = case_when(
      # conference playoffs
      season < 2021 & between(week, 28, 30) ~ week - 10,
      # super bowl
      season < 2021 & week == 32 ~ 21,
      season >= 2021 & between(week, 28, 30) ~ week - 9,
      season >= 2021 & week == 32 ~ 22,
      TRUE ~ week
    )
  ) %>%
  saveRDS("pff/data/weekly_grades.rds")



# make sure it worked
  readRDS("pff/data/weekly_grades.rds")
  
# ################################################################################################
# team grades: season-by-season

# scraping function for one season
  pff_scrape_season <- function(s) {
    
    url <- glue::glue("https://premium.pff.com/api/v1/teams/overview?league=nfl&season={s}&week=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18")
    
    pff_resp <- httr::GET(
      url,
      httr::add_headers(cookie = cookie_in))
    
    json_resp <- httr::content(pff_resp, as="text")
    
    raw_json <- jsonlite::fromJSON(json_resp)
    
    raw_json %>%
      pluck("team_overview") %>%
      as_tibble() %>%
      mutate(season = s) %>%
      select(season, team_abbr = abbreviation, starts_with("grades_"), starts_with("points_"))
    
  }
  
  pff_scrape_season(2021)
  
  # get list of seasons we want
  df <- expand.grid(s = 2006 : 2021, w = c(1 : 18, 28:30, 32)) %>% 
    as_tibble() %>% 
    arrange(s, w) %>%
    # get rid of week 18 for pre-2021
    filter(!(s < 2021 & w == 18))
  
  df
  
  # do the thing
  data <- map_df(2006 : 2021, ~{

    pff_scrape_season(.x)
    
  })
  
  data
  
  # check and clean up
  data %>%
    filter(!is.na(grades_pass_block)) %>%
    mutate_at(c("team_abbr"), nflfastR:::team_name_fn) %>%
    saveRDS("pff/data/team_season_grades.rds")
  
  # make sure it worked
  readRDS("pff/data/team_season_grades.rds")
  
# ################################################################################################
# player grades

  # scraping function for one week
  pff_scrape_qb_week <- function(s, w) {
    
    url <- glue::glue("https://premium.pff.com/api/v1/facet/passing/summary?league=nfl&season={s}&week={w}")

    pff_resp <- httr::GET(
      url,
      httr::add_headers(cookie = cookie_in))
    
    json_resp <- httr::content(pff_resp, as="text")
    
    raw_json <- jsonlite::fromJSON(json_resp)
    
    raw_json %>%
      pluck("passing_summary") %>%
      as_tibble() %>%
      mutate(season = s, week = w) %>%
      select(player, season, week, team_abbr = team_name, starts_with("grades_"))
    
  }
  
  
  # get list of seasons/weeks we want
  df <- expand.grid(s = 2006 : 2021, w = c(1 : 18, 28:30, 32)) %>% 
    as_tibble() %>% 
    arrange(s, w) %>%
    # get rid of week 18 for pre-2021
    filter(!(s < 2021 & w == 18))
  
  # for testing
  # df <- df %>% head(3)
  # end test
  
  df
  
  # do the thing
  data <- map_df(1 : nrow(df), ~{
    
    row <- df %>% dplyr::slice(.x)
    message(glue::glue("Season {row$s} and week {row$w}"))
    
    pff_scrape_qb_week(row$s, row$w)
    
  })
  
  # check and clean up
  data %>%
    filter(!is.na(grades_offense)) %>%
    mutate_at(c("team_abbr"), nflfastR:::team_name_fn) %>%
    mutate(
      week = case_when(
        # conference playoffs
        season < 2021 & between(week, 28, 30) ~ week - 10,
        # super bowl
        season < 2021 & week == 32 ~ 21,
        season >= 2021 & between(week, 28, 30) ~ week - 9,
        season >= 2021 & week == 32 ~ 22,
        TRUE ~ week
      )
    ) %>%
    saveRDS("pff/data/weekly_grades_qbs.rds")
  
  # make sure it worked
  readRDS("pff/data/weekly_grades_qbs.rds")
  
# ################################################################################################
# blocking grades: week by week

  
  # scraping function for one week
  pff_scrape_ol_week <- function(s, w) {
    
    url <- glue::glue("https://premium.pff.com/api/v1/facet/offense/blocking?league=nfl&season={s}&week={w}")
    
    pff_resp <- httr::GET(
      url,
      httr::add_headers(cookie = cookie_in))
    
    json_resp <- httr::content(pff_resp, as="text")
    
    raw_json <- jsonlite::fromJSON(json_resp)
    
    raw_json %>%
      pluck("blocking_summary") %>%
      as_tibble() %>%
      mutate(season = s, week = w) %>%
      select(player, position, season, week, team_abbr = team_name, starts_with(c("grades_", "snap_counts_")))
    
  }
  
  min_s <- 2006
  
  # get list of seasons/weeks we want
  df <- expand.grid(s = min_s : 2021, w = c(1 : 18, 28:30, 32)) %>% 
    as_tibble() %>% 
    arrange(s, w) %>%
    # get rid of week 18 for pre-2021
    filter(!(s < 2021 & w == 18))
  
  # for testing
  # df <- df %>% head(3)
  # end test
  
  df
  
  # do the thing
  data <- map_df(1 : nrow(df), ~{
    
    row <- df %>% dplyr::slice(.x)
    message(glue::glue("Season {row$s} and week {row$w}"))
    
    pff_scrape_ol_week(row$s, row$w)
    
  })
  
  # check and clean up
  data %>%
    filter(!is.na(grades_offense)) %>%
    mutate_at(c("team_abbr"), nflfastR:::team_name_fn) %>%
    mutate(
      week = case_when(
        # conference playoffs
        season < 2021 & between(week, 28, 30) ~ week - 10,
        # super bowl
        season < 2021 & week == 32 ~ 21,
        season >= 2021 & between(week, 28, 30) ~ week - 9,
        season >= 2021 & week == 32 ~ 22,
        TRUE ~ week
      )
    ) %>%
    saveRDS("pff/data/weekly_grades_ol.rds")
  
  # make sure it worked
  readRDS("pff/data/weekly_grades_ol.rds")
  
  
  # ################################################################################################
  # player blocking grades: season by season
  
  
  # scraping function for one week
  pff_scrape_ol <- function(s) {
    
    url <- glue::glue("https://premium.pff.com/api/v1/facet/offense/blocking?league=nfl&season={s}&week=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,28,29,30,32")
    
    pff_resp <- httr::GET(
      url,
      httr::add_headers(cookie = cookie_in))
    
    json_resp <- httr::content(pff_resp, as="text")
    
    raw_json <- jsonlite::fromJSON(json_resp)
    
    raw_json %>%
      pluck("blocking_summary") %>%
      as_tibble() %>%
      mutate(season = s) %>%
      select(player, player_id, position, season, team_abbr = team_name, starts_with(c("grades_", "snap_counts_")))
    
  }
  
  min_s <- 2006
  
  # get list of seasons/weeks we want
  df <- expand.grid(s = min_s : 2021) %>% 
    as_tibble() %>% 
    arrange(s)
  
  df
  
  # do the thing
  data <- map_df(1 : nrow(df), ~{
    
    row <- df %>% dplyr::slice(.x)
    message(glue::glue("Season {row$s}"))
    
    pff_scrape_ol(row$s)
    
  })
  
  # check and clean up
  data %>%
    filter(!is.na(grades_pass_block), position %in% c("T", "G", "C")) %>%
    mutate_at(c("team_abbr"), nflfastR:::team_name_fn) %>%
    saveRDS("pff/data/season_grades_ol.rds")
  
  # make sure it worked
  readRDS("pff/data/season_grades_ol.rds")
  
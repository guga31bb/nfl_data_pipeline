
# scrape ourladsfor OL starters

library(tidyverse)
library(rvest)

# get OL starters for a team
get_depth_chart <- function(tm) {
  
  message(glue::glue("Doing {tm}"))
  
  page <- glue::glue("https://www.ourlads.com/nfldepthcharts/depthchart/{tm}") %>%
    rvest::read_html()
  
  fa <- page %>%
    rvest::html_nodes(".lc_red") %>%
    html_text2()
  
  df <- page %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    janitor::clean_names() %>%
    mutate(player_1 = case_when(
      player_1 %in% fa & player_2 %in% fa ~ player_3,
      player_1 %in% fa ~ player_2,
      TRUE ~ player_1
    )) %>%
    filter(pos %in% c("LT", "LG", "C", "RG", "RT")) %>%
    select(position = pos, player = player_1) %>%
    mutate(
      # some stuff and then a slash gets the axe
      player = stringr::str_remove_all(player, "(?<=\\s)([:digit:]*|[:upper:]*)\\/.*"),
      # some capital letters and numbers all together
      player = stringr::str_remove_all(player, "(?<=\\s)[:upper:]+[:digit:]+"),
      player = stringr::str_squish(player),
      player = stringr::str_to_title(player)
    )
  
  unlist(strsplit(df$player, ", ")) %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as_tibble() %>%
    set_names(c("last", "first")) %>%
    bind_cols(df) %>%
    select(first, last, position) %>%
    mutate(team_abbr = tm)
  
}

# test it out
# get_depth_chart("SEA")

# get team list for scraping
teams <- nflreadr::load_teams() %>%
  select(team_abbr, team_name) %>%
  filter(!team_abbr %in% c("LA", "SD", "STL", "OAK")) %>%
  mutate(team_abbr = case_when(
    team_abbr == "ARI" ~ "ARZ",
    TRUE ~ team_abbr
  )) %>%
  pull(team_abbr)

# do the scrape

# don't need to scrape every session
ourlads_df <- map_df(teams, get_depth_chart)

# clean up some names so it plays nice with PFF
ourlads_df <- ourlads_df %>%
  mutate(
    player = paste(first, last),
    # fix some problematic teams
    player = case_when(
      position == "RG" & team_abbr == "IND" ~ "Will Fries",
      position == "RT" & team_abbr == "GB" ~ "Elgton Jenkins",
      position == "LG" & team_abbr == "GB" ~ "Jon Runyan",
      TRUE ~ player
    )) %>%
  select(player, team_abbr, position_ourlads = position) %>%
  mutate(
    player = case_when(
      player == "Dj Humphries" ~ "D.J. Humphries",
      player == "Cameron Erving" ~ "Cam Erving",
      player == "Kaleb Mcgary" ~ "Kaleb McGary",
      player == "Connor Mcgovern" & team_abbr == "DAL" ~ "Connor McGovern DAL",
      player == "Connor Mcgovern" & team_abbr == "NYJ" ~ "Connor McGovern NYJ",
      player == "Cam Fp Robinson" ~ "Cam Robinson",
      player == "Orlando Fp Brown" ~ "Orlando Brown",
      player == "Erik Mccoy" ~ "Erik McCoy",
      player == "Mike Mcglinchey" ~ "Mike McGlinchey",
      player == "Justin Mccray" ~ "Justin McCray",
      player == "Orlando Brown" ~ "Orlando Brown Jr.",
      player == "Brian O'neill" ~ "Brian O'Neill",
      player == "Colton Mckivitz" ~ "Colton McKivitz",
      player == "Lloyd Cushenberry Iii" ~ "Lloyd Cushenberry III",
      TRUE ~ player
    ),
    current_team = 
      case_when(current_team == "ARZ" ~ "ARI",
                current_team == "LAR" ~ "LA",
                TRUE ~ current_team)
    )

ourlads_df %>%
  saveRDS("pff/data/ourlads.rds")

ourlads_df %>%
  write_csv("pff/data/ourlads.csv")


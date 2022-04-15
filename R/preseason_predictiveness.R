
library(tidyverse)
library(rvest)

# preseason win totals
  url1 <- "https://www.sportsoddshistory.com/nfl-regular-season-win-total-results-by-team-2000s/"
  url2 <- "https://www.sportsoddshistory.com/nfl-regular-season-win-total-results-by-team/"
  
  tab <- rvest::read_html(url1) %>%
    html_table() %>%
    pluck(1) %>%
    janitor::clean_names() 
  
  early <- tab %>%
    mutate(
      across(x00 : x09, ~  stringr::str_split_fixed(.x, " ", 2)[, 1])
    ) %>%
    select(team : x09) %>%
    pivot_longer(
      x00:x09
    ) %>%
    group_by(team) %>%
    mutate(season = 1 : n() + 1999) %>%
    ungroup() %>%
    select(team, over = value, season)
  
  tab <- rvest::read_html(url2) %>%
    html_table() %>%
    pluck(1) %>%
    janitor::clean_names() %>%
    filter(team != "Team")
  
  late <- tab %>%
    mutate(
      across(x10 : x19, ~  stringr::str_split_fixed(.x, " ", 2)[, 1])
    ) %>%
    select(team : x19) %>%
    pivot_longer(
      x10:x19
    ) %>%
    group_by(team) %>%
    mutate(season = 1 : n() + 2009) %>%
    ungroup() %>%
    select(team, over = value, season)
  
  late
  
  ps_totals <- early %>%
    bind_rows(late) %>%
    mutate(over = as.numeric(over)) %>%
    # Texans before they existed
    filter(!is.na(over)) %>%
    arrange(team, season) %>%
    group_by(season) %>%
    mutate(season_wins = sum(over)) %>% 
    ungroup() %>%
    dplyr::rename(team_name = team) %>%
    mutate(team_name = ifelse(team_name == "Washington Redskins", "Washington Commanders", team_name)) %>%
    left_join(
      nflreadr::load_teams() %>% select(team_abbr, team_name) %>% filter(team_abbr != "LAR"), by = "team_name"
    ) %>%
    select(-team_name) %>%
    mutate_at(c("team_abbr"), nflfastR:::team_name_fn)
  
  ps_totals

# regular season performance
  sched <- nflreadr::load_schedules() %>%
    filter(week <= 17, between(season, 2000, 2019))
  
  results <- bind_rows(
    sched %>% select(season, week, game_id, team_abbr = home_team, result),
    sched %>% select(season, week, game_id, team_abbr = away_team, result) %>% mutate(result = -result)
  ) %>%
    arrange(game_id) %>%
    group_by(team_abbr, season) %>% 
    summarise(diff = sum(result)) %>%
    ungroup()  %>%
    mutate_at(c("team_abbr"), nflfastR:::team_name_fn)

  results

# performance in preseason games
  
  get_season <- function(y) {
    
    url <- glue::glue("https://www.pro-football-reference.com/years/{y}/preseason.htm")
    df <- bind_rows(
      rvest::read_html(url) %>%
        html_table() %>%
        pluck(1),
      rvest::read_html(url) %>%
        html_table() %>%
        pluck(2)
    ) %>%
      janitor::clean_names() %>%
      select(team_name = tm, ps_point_diff = pt_dif) %>%
      filter(!stringr::str_detect(team_name, "AFC|NFC"))
    
    df %>%
      mutate(team_name = ifelse(team_name == "Washington Redskins", "Washington Commanders", team_name), season = y) %>%
      left_join(
        nflreadr::load_teams() %>% select(team_abbr, team_name) %>% filter(team_abbr != "LAR"), by = "team_name"
      )  %>%
      mutate_at(c("team_abbr"), nflfastR:::team_name_fn)
    
  }
  
  ps_diff <- map_df(2000:2019, ~{get_season(.x)})
  ps_diff
  
# join
  
  df <- ps_totals %>%
    left_join(results, by = c("team_abbr", "season")) %>%
    left_join(ps_diff, by = c("team_abbr", "season")) %>%
    ungroup() %>%
    mutate(ps_point_diff = as.numeric(ps_point_diff))

  df
  
  df %>%
    filter(is.na(diff))

  df %>%
    ggplot(aes(over, diff)) + 
    geom_point() + 
    geom_smooth()
  
  df %>%
    ggplot(aes(ps_point_diff, diff)) + 
    geom_point() + 
    geom_smooth(color = "black", size = 3) +
    ggthemes::theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size=12, face="bold"),
      axis.title.y = element_text(size=12, face="bold"),
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 14, hjust = 0.5)
    ) +
    labs(
      title = "Point differential in preseason and regular season",
      subtitle = "2000 through 2019",
      x = "Preseason point differential",
      y = "Regular season point differential",
      caption = "Data: PFR | Figure: @benbbaldwin"
    ) 

  
  lm(diff ~ over, data = df) %>% summary()
  lm(diff ~ over + ps_point_diff, data = df) %>% summary()
  
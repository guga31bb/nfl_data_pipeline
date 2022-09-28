
library(tidyverse)
library(gt)

# load the data
# if you want to use more seasons, change the range here
pbp <- nflreadr::load_participation(2022, include_pbp = T) %>%
  filter(!is.na(down), !is.na(posteam)) %>%
  select(game_id, old_game_id, season, week, play_id, posteam, defteam, desc, offense_players, defense_players, defenders_in_box, number_of_pass_rushers, down, epa, pass, rush, first_down, success)

  
# the function
# takes a name (player name), o = 1 if player on offense or 0 if on defense, first season, last season
make_table <- function(name, o, first, last) {
  
  stopifnot("2016 is the earliest available season" = first >= 2016)
  
  # get the player ID of player in question
  pid <- nflreadr::load_rosters() %>%
    filter(full_name == name) %>%
    pull(gsis_id)
  
  # fill in for multiple players at once
  joined <- pbp %>%
    filter(between(season, first, last))
  
  # make the split
  joined$split <- 0
  joined <- joined %>%
    mutate(
      split = ifelse(
        stringr::str_detect(offense_players, pid) | stringr::str_detect(defense_players, pid),
        1,
        split),
      success = 100 * success, first_down = 100 * first_down
    )
  
  joined %>%
    filter(split == 1) %>%
    select(game_id, posteam, split, defense_players, desc) 
  
  # get the relevant plays (whether on offense or defense)
  if (o == 1) {
    tm = pull(joined %>% filter(split==1) %>% slice(1), posteam)
    joined <- joined %>% filter(posteam==tm)
  } else {
    tm = pull(joined %>% filter(split==1) %>% slice(1), defteam)
    joined <- joined %>% filter(defteam==tm)
  }

  # inspect the data a little
  tm
  
  joined %>%
    group_by(season, split) %>%
    summarize(n=n())
  
  #do stuff for the team summary table
  all <- joined %>% group_by(split) %>% summarize(
    epa = mean(epa), success=mean(success), p=mean(pass), play=n(), fd=mean(first_down, na.rm=T)) %>%
    mutate(rowname="All plays", type=1)
  
  early <- joined %>% filter(down == 1 | down ==2) %>% group_by(split) %>% summarize(
    epa = mean(epa), success=mean(success), p=mean(pass),play=n(), fd=mean(first_down, na.rm=T))%>%
    mutate(rowname="Early downs (1st & 2nd)", type=4)
  
  earlyr <- joined %>% filter((down == 1 | down ==2) & rush==1) %>% group_by(split) %>% summarize(
    epa = mean(epa), success=mean(success), p=mean(pass),play=n(), fd=mean(first_down, na.rm=T))%>%
    mutate(rowname="Early rush", type=5)
  
  earlyp <- joined %>% filter((down == 1 | down ==2) & pass==1) %>% group_by(split) %>% summarize(
    epa = mean(epa), success=mean(success), p=mean(pass),play=n(), fd=mean(first_down, na.rm=T))%>%
    mutate(rowname="Early pass", type=6)
  
  late <- joined %>% filter(down==3  | down == 4) %>% group_by(split) %>% summarize(
    epa = mean(epa), success=mean(success), p=mean(pass), play=n(), fd=mean(first_down, na.rm=T))%>%
    mutate(rowname="3rd/4th down", type=7)
  
  later <- joined %>% filter((down == 3 | down == 4) & rush==1) %>% group_by(split) %>% summarize(
    epa = mean(epa), success=mean(success), p=mean(pass),play=n(), fd=mean(first_down, na.rm=T))%>%
    mutate(rowname="Late rush", type=8)
  
  latep <- joined %>% filter((down == 3 | down ==4) & pass==1) %>% group_by(split) %>% summarize(
    epa = mean(epa), success=mean(success), p=mean(pass),play=n(), fd=mean(first_down, na.rm=T))%>%
    mutate(rowname="Late pass", type=9)
  
  type <- joined %>% group_by(split, pass) %>% summarize(
    epa = mean(epa), success=mean(success), p=mean(pass), play=n(), fd=mean(first_down, na.rm=T)) %>%
    mutate(rowname=if_else(pass==1,"Pass","Rush"), type=2)
  
  bound <- bind_rows(all,early,earlyr, earlyp,late, later, latep, type) %>%
    mutate(p=round(100*p), epa=round(epa, digits=2), success=round(success,digits=2), fd=round(fd,digits=2)) %>%
    arrange(-split,type) %>% select(-pass, -type)
  
  sub <- ifelse(
    first == last, glue::glue(first), glue::glue("{first} - {last}")
  )
  
  #team summary table
  table <- bound %>%  
    select(split, rowname, epa, success, fd, play) %>% 
    mutate(split = ifelse(split == 0, paste("Off Field"), paste("On Field"))) %>%
    group_by(split) %>% 
    gt() %>%
    cols_label(
      epa = md("**EPA**"), fd=md("**1st %**"), success = md("**Success**"), play = md("**Plays**")) %>%
    cols_align(align = "center") %>%
    tab_source_note(
      md(glue::glue("Table: @benbbaldwin | Data: nflreadr <br> EPA = EPA/play, 1st % = First down rate"))) %>%
    tab_header(
      title = paste(tm, "splits with", paste(name), "on and off field"),
      subtitle = sub
      ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")), locations = cells_group(groups=TRUE)) %>%
    tab_style(
      style = list(
        cell_text(style = "italic", align="center")), 
      locations = cells_stub(rows=c(2,3,5,6, 8, 9, 11, 12, 14, 15,17,18))) %>%
    tab_style(
      style = list(
        cell_text(align="left")), 
      locations = cells_stub(rows=c(1, 4, 7, 10, 13, 16))) %>%
    gtExtras::fmt_symbol_first(column = c(4:5), suffix = "%", decimals = 1) %>%
    gtExtras::gt_theme_nytimes() %>%
    tab_options(
      table.font.size = px(16L),
      data_row.padding = px(3)
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold", align="center")), 
      locations = cells_column_labels(columns = everything()))
  
  table
  
  
}

# try it out: look for humphrey, on plays he's on offense, in the 2022 season
table <- make_table(
  "Lil'Jordan Humphrey", 
  o = 1, 
  first = 2022, 
  last =  2022
  )

table

table %>%
  gtsave("img/onoff.png")




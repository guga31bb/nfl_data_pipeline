library(tidyverse)
library(ggrepel)
library(ggimage)
library(mgcv)
library(scales)
library(gt)
library(espnscrapeR)
library(rvest)

load('../metrics/dakota_model.rda')
add_dakota <- function(pbp) {
  
  pbp$index <- mgcv::predict.gam(dakota_model, pbp)
  return(pbp)
  
}

# *********************************************************************************
# variables to set
# *********************************************************************************

seasons <- 2006:2021

# created in helper file (public data from SIS leaderboard)
  sis_all <- readRDS("data/sis.rds") %>%
    separate(player_name, c("first_name","last_name"), sep= " ") %>%
    mutate(
      name = glue::glue('{substr(first_name, 1, 1)}.{last_name}'),
      name = as.character(name)
    ) %>%
    select(name, sis_id = player_id, season, total_points, total_points_per_play, iqr) %>% 
    filter(season >= 2016) %>%
    mutate(
      name = case_when(
        name == "G.Minshew" ~ "G.Minshew II",
        sis_id == 955 ~ "R.Griffin III",
        TRUE ~ name
      )
    ) %>%
    arrange(sis_id, season) %>%
    group_by(sis_id) %>%
    mutate(
      lag_total_points = dplyr::lag(total_points),
      lag_tpp = dplyr::lag(total_points_per_play)
    ) %>%
    ungroup()
  
  sis_all %>%
    filter(name == "R.Griffin III")

# qbr
  qbr <- map_df(seasons, function(x) {
    espnscrapeR::get_nfl_qbr(x) %>%
      mutate(
        name = glue::glue('{substr(name_first, 1, 1)}.{name_last}'),
        name = as.character(name),
        name = dplyr::case_when(
          name == "D.Haskins Jr." ~ "D.Haskins",
          name == "G.Minshew" ~ "G.Minshew II",
          name == "T. Pryor Sr." ~ "T.Pryor",
          TRUE ~ name
        )
      ) %>%
      filter(qb_plays > 10) %>%
      select(name, espn_plays = qb_plays, espn_id = player_id, qbr_total, season)
  }) %>%
    mutate(name = ifelse(name == "T.Pryor Sr.", "T.Pryor", name)) %>%
    arrange(espn_id, season) %>%
    group_by(espn_id) %>%
    mutate(lag_qbr = dplyr::lag(qbr_total)) %>%
    ungroup()
  
  
# grades: download with PFF elite subscription
  grades_all <- map_df(2006:2021, ~{
    read_csv(glue::glue('{path}/pff_qb_grades_{.x}.csv')) %>%
      separate(player, c("f","l"), sep= " ") %>%
      mutate(f=substr(f, 1, 1),
             name = paste0(f,".",l),
             name = case_when(
               name == "G.Minshew"~  "G.Minshew II", 
               name == "A.Rodgers" ~ "A.Rodgers",
               name == "T.Taylor" ~ "T.Taylor",
               player_id == 7008 ~ "R.Griffin III",
               TRUE ~ name)
      ) %>%
      select(name, pff_id = player_id, grade = grades_offense, grade_passing = grades_pass, team_name) %>%
      mutate(season = .x)
  }) %>%
    group_by(pff_id) %>%
    mutate(
      lag_grade = dplyr::lag(grade),
      lag_grade_passing = dplyr::lag(grade_passing)
      ) %>%
    ungroup()
  
  grades_all

  grades_all %>%
    filter(name == "R.Griffin III")

# war: thank you to Eric Eager
  war_all <- read_csv(glue::glue('{path}/WAR_2_2.csv')) %>%
    separate(player, c("f","l"), sep= " ") %>%
    mutate(f=substr(f, 1, 1),
           name = paste0(f,".",l),
           name = case_when(
             name == "G.Minshew"~  "G.Minshew II", 
             player_id == 7008 ~ "R.Griffin III",
             TRUE ~ name)
    ) %>%
    filter(snaps > 0, !is.na(WAR)) %>%
    select(name, pff_id = player_id, season, war = WAR) %>%
    group_by(pff_id) %>%
    mutate(lag_war = dplyr::lag(war)) %>%
    ungroup() %>%
    # bc joining to grades
    select(-name)

# combine grades & war
  pff <- grades_all %>%
    left_join(war_all, by = c("pff_id", "season")) %>%
    select(name, pff_id, grade, season, lag_grade, war, lag_war, grade_passing, lag_grade_passing)
  
  pff
  
  pff %>%
    filter(name == "R.Griffin III")

# get pbp from nflfastR
  all_data <- nflreadr::load_pbp(seasons) %>%
    filter(season_type == "REG", !is.na(epa), rush == 1 | pass == 1) %>%
    select(posteam, season, name, id, down, pass, rush, cpoe, epa, qb_epa, play_type, incomplete_pass, complete_pass, interception, success, yards_gained, pass_touchdown)

# stuff from pbp
  ya <- all_data %>% 
    filter(play_type=="pass", incomplete_pass == 1 | complete_pass == 1 | interception == 1) %>% 
    group_by(id, season) %>%
    summarize(
      name = dplyr::first(name),
      yards = sum(yards_gained),
      ints = sum(interception),
      tds = sum(pass_touchdown),
      n=n()) %>% 
    mutate(aya = (yards + 20 * tds - 45 * ints) / n, ya=yards/n, tdint = tds/ints,
           tdint = ifelse(ints==0, NA, tdint)) %>%
    ungroup()
  
# *********************************************************************************
# start combining stuff
# *********************************************************************************

qb_min = 320

qbs <- all_data %>% 
  filter(rush == 1 | pass == 1, !is.na(down), !is.na(epa)) %>%
  mutate(
    epa = qb_epa,
    adjusted_epa=if_else(epa < -4.5, -4.5, epa)
  ) %>%
  group_by(id, season) %>%
  summarize(
    posteam = dplyr::first(posteam),
    name = dplyr::first(name),
    n_dropbacks = sum(pass),
    n_plays = n(),
    epa_per_play = mean(epa),
    adj_epa = mean(adjusted_epa),
    success_per_play = mean(success),
    cpoe = mean(cpoe, na.rm = TRUE),
    total_epa = sum(epa)
  ) %>%
  filter(n_dropbacks > 30) %>%
  filter(n_plays>= 320) %>%
  left_join(ya, by=c("id", "name", "season")) %>%
  left_join(pff,by=c("name", "season")) %>%
  left_join(qbr,by=c("name", "season"))  %>%
  left_join(sis_all, by=c("name", "season")) %>%
  add_dakota() %>%
  ungroup() %>%
  mutate(
    # someone told me to do this but it doesn't make a difference
    qbr = qbr_total / 100,
    qbr = log(qbr / (1-qbr)),
         war_per_play = war / n_plays
         )

# make sure all the joins worked correctly
  qbs %>%
    filter(is.na(qbr))
  
  qbs %>%
    filter(is.na(total_points), season > 2016)

  qbs %>%
    select(name, season, posteam, n_plays, espn_plays, epa_per_play, total_points, qbr, lag_qbr, cpoe, grade, lag_grade, war)

# get prior season for stuff we don't have already
lqb <- qbs %>%
  group_by(id) %>% 
  mutate(
    epa = epa_per_play,
    ltot_epa = lag(total_epa, n = 1, order_by = season),
    lwarpp = lag(war_per_play, n = 1, order_by = season),
    
    ltp = lag(total_points, n = 1, order_by = season),
    tpp = total_points_per_play,
    ltdint = lag(tdint, n = 1, order_by = season),
    ltpp = lag(tpp, n = 1, order_by = season),
    lepa = lag(epa_per_play, n = 1, order_by = season),
    ladj_epa = lag(adj_epa, n = 1, order_by = season),
    lcpoe = lag(cpoe, n = 1, order_by = season),

    lindex = lag(index, n = 1, order_by = season),
    lteam = lag(posteam, n = 1, order_by = season),
    laya = lag(aya, n = 1, order_by = season),
    lag_qbr = lag(qbr, n = 1, order_by = season),
    lag_posteam = lag(posteam, n = 1, order_by = season)
  ) %>% 
  ungroup()

# more checks
lqb %>% filter(name=="T.Brady") %>%
  select(name, posteam, lag_posteam, season, total_epa, ltot_epa)

# there's probably a better way to do this but oh well
t <- tibble::tribble(
  ~"Metric", ~"Stability", ~"epa",
  "TD/INT ratio",      cor(lqb$tdint, lqb$ltdint, use = "complete.obs"),    cor(lqb$epa, lqb$ltdint, use = "complete.obs"),
  
  "PFF Offense grade", cor(lqb$grade, lqb$lag_grade, use = "complete.obs"),    cor(lqb$epa, lqb$lag_grade, use = "complete.obs"),
  "PFF Passing grade", cor(lqb$grade_passing, lqb$lag_grade_passing, use = "complete.obs"),    cor(lqb$epa, lqb$lag_grade_passing, use = "complete.obs"),
  
  "PFF WAR",           cor(lqb$war, lqb$lag_war, use = "complete.obs"),        cor(lqb$epa, lqb$lag_war, use = "complete.obs"),
  "PFF WAR per play",  cor(lqb$war_per_play, lqb$lwarpp, use = "complete.obs"),        cor(lqb$epa, lqb$lwarpp, use = "complete.obs"),
  
  "Total Points per play (SIS)", cor(lqb$tpp, lqb$ltpp, use = "complete.obs"), cor(lqb$epa, lqb$ltpp, use = "complete.obs"),
  "Total Points (SIS)", cor(lqb$total_points, lqb$ltp, use = "complete.obs"), cor(lqb$epa, lqb$ltp, use = "complete.obs"),
  
  "QBR (ESPN)",        cor(lqb$qbr, lqb$lag_qbr, use = "complete.obs"),        cor(lqb$epa, lqb$lag_qbr, use = "complete.obs"),
  "CPOE",              cor(lqb$cpoe, lqb$lcpoe, use = "complete.obs"),      cor(lqb$epa, lqb$lcpoe, use = "complete.obs"),
  "CPOE + EPA index",  cor(lqb$index, lqb$lindex, use = "complete.obs"),    cor(lqb$epa, lqb$lindex, use = "complete.obs"),
  
  "EPA per play",      cor(lqb$epa, lqb$lepa, use = "complete.obs"),        cor(lqb$epa, lqb$lepa, use = "complete.obs"),
  "Adj. EPA per play", cor(lqb$adj_epa, lqb$ladj_epa, use = "complete.obs"), cor(lqb$epa, lqb$ladj_epa, use = "complete.obs"),
  "Total EPA",      cor(lqb$total_epa, lqb$ltot_epa, use = "complete.obs"),        cor(lqb$epa, lqb$ltot_epa, use = "complete.obs"),
  
  "AY/A",              cor(lqb$aya, lqb$laya, use = "complete.obs"),        cor(lqb$epa, lqb$laya, use = "complete.obs")
)

t

# drop things for table: volume stats were just for curiosity
# and DVOA isn't comparable
  t <- t %>%
    filter(
      !Metric %in% c(
        "Total EPA", 
        "PFF Passing grade", 
        "PFF WAR",
        "Total Points (SIS)"
        )
    )
  
  t

tab <- t %>%
  arrange(-epa) %>%
  gt() %>%
  cols_label(
    Metric = md("**Measure**"),  
    Stability=md("**Year-to-year<br>stability (correlation)**"), 
    epa = md("**Correlation with<br>next year's EPA/play**"),
  ) %>%
  cols_align(align = "center") %>% 
  tab_source_note(
    source_note = paste("Table: @benbbaldwin | Data: nflfastR, PFF, ESPN, SIS | Min", qb_min, "plays")) %>%
  tab_header(
    title = "Comparing measurements of QB play",
    subtitle = "2006 through 2021"
  ) %>%
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
  data_color(
    columns = vars(epa),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(.37,.47)),
    autocolor_text = FALSE
  ) %>%
  gtExtras::gt_theme_538() %>%
  tab_footnote(
    footnote = "Since 2016",
    locations = cells_body(columns = Metric, rows = c(7))
  ) %>%
  tab_footnote(
    footnote = "Adjustment caps value of negative plays at average value of turnover (-4.5)",
    locations = cells_body(columns = Metric, rows = 3)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "white")
    ),
    locations = cells_body(
      columns = vars(epa),
      rows = c(10))
  ) %>%
  gtExtras::fmt_pad_num(columns = c(2, 3), nsmall = 2)


tab

tab %>%
  gtsave("img/qb_properties.png")


# # # #
# make the figure

# old bad code that somehow works
  a <- lqb %>% 
    filter(season > 2006) %>%
    group_by(season) %>%
    summarize(
      c_epa = cor(epa_per_play, lepa, use = "complete.obs"),
      c_qbr = cor(epa_per_play, lag_qbr, use = "complete.obs"),
      c_index = cor(epa_per_play, lindex, use = "complete.obs"),
      c_cpoe = cor(epa_per_play, lcpoe, use = "complete.obs"),
      c_pff = cor(epa_per_play, lag_grade, use = "complete.obs"),
      c_war = cor(epa_per_play, lwarpp, use = "complete.obs")
    )
  
  b <- lqb %>% filter(season>=2017) %>% group_by(season) %>%
    summarize(
      c_tpp = cor(epa_per_play, ltpp, use = "complete.obs")
    )
  
  a
  
  b

###  over time
ggplot(data=a, aes(x=season,y=c_war)) +
  
  geom_point(data=a,
             aes(x=season,y=c_index),color="orange",size=5, alpha = 0.5) +
  
  geom_point(data=a,
             aes(x=season,y=c_war),color="blue",size=5, alpha = 0.5) +
  
  geom_point(data=a,
             aes(x=season,y=c_qbr),color="red",size=5, alpha = 0.5) +
  
  geom_point(data=b,
             aes(x=season,y=c_tpp),color="black",size=5, alpha = 0.5) +
  
  # note: not enough points to smooth total points
  geom_smooth(data=a,
              aes(x=season,y=c_war),color="blue",size=5, se = F) +
  geom_smooth(data=a,
              aes(x=season,y=c_index),color="orange",size=5, se = F) +
  geom_smooth(data=a,
              aes(x=season,y=c_qbr),color="red",size=5, se = F) +
  
  scale_x_continuous(breaks=c(2006:2021)) +
  ggthemes::theme_fivethirtyeight() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(plot.title = element_text(size=16,face = 2,hjust=.5),
        plot.subtitle = element_text(size=16, hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  labs(title=paste("Prediction of EPA/play over time"),
       subtitle=paste("Correlation between EPA/play in current year and measure in prior year"),
       caption = "Figure: @benbbaldwin | Data: nflfastR, PFF, SIS, ESPN | Min 320 plays") +
  ylab("Correlation") + xlab("Season") +
  ggtext::geom_richtext(aes(x=2013.3, y= .65), label = "**EPA+CPOE index**", color="orange", size = 7, fill = NA, label.color = NA) +
  ggtext::geom_richtext(aes(x=2017, y= .16), label = "**QBR**", color="red", size = 7, fill = NA, label.color = NA) +
  ggtext::geom_richtext(aes(x=2016, y= .44), label = "**WAR per play**", color="blue", size = 7, fill = NA, label.color = NA) +
  ggtext::geom_richtext(aes(x=2016, y= .53), label = "**Total Points per play**", color="black", size = 7, fill = NA, label.color = NA) +
  geom_segment(aes(x=2016,xend=2017.9,y=.53,yend=.38),arrow=arrow(length=unit(.2,"cm")),color="black")


ggsave("img/16_qb_predict_t.png", dpi = 600)




# for QBs switching teams

lqb <- lqb %>%
  filter(posteam != lag_posteam)

t2 <- tibble::tribble(
  ~"Metric", ~"Stability", ~"epa",
  "TD/INT ratio",      cor(lqb$tdint, lqb$ltdint, use = "complete.obs"),    cor(lqb$epa, lqb$ltdint, use = "complete.obs"),
  
  "PFF Offense grade", cor(lqb$grade, lqb$lag_grade, use = "complete.obs"),    cor(lqb$epa, lqb$lag_grade, use = "complete.obs"),

  "PFF WAR per play",  cor(lqb$war_per_play, lqb$lwarpp, use = "complete.obs"),        cor(lqb$epa, lqb$lwarpp, use = "complete.obs"),
  
  "Total Points per play (SIS)", cor(lqb$tpp, lqb$ltpp, use = "complete.obs"), cor(lqb$epa, lqb$ltpp, use = "complete.obs"),

  "QBR (ESPN)",        cor(lqb$qbr, lqb$lag_qbr, use = "complete.obs"),        cor(lqb$epa, lqb$lag_qbr, use = "complete.obs"),
  "CPOE",              cor(lqb$cpoe, lqb$lcpoe, use = "complete.obs"),      cor(lqb$epa, lqb$lcpoe, use = "complete.obs"),
  "CPOE + EPA index",  cor(lqb$index, lqb$lindex, use = "complete.obs"),    cor(lqb$epa, lqb$lindex, use = "complete.obs"),
  
  "EPA per play",      cor(lqb$epa, lqb$lepa, use = "complete.obs"),        cor(lqb$epa, lqb$lepa, use = "complete.obs"),
  "Adj. EPA per play", cor(lqb$adj_epa, lqb$ladj_epa, use = "complete.obs"), cor(lqb$epa, lqb$ladj_epa, use = "complete.obs"),

  "AY/A",              cor(lqb$aya, lqb$laya, use = "complete.obs"),        cor(lqb$epa, lqb$laya, use = "complete.obs")
)

t2

tab <- t2 %>%
  arrange(-epa) %>%
  gt() %>%
  cols_label(
    Metric = md("**Measure**"),  
    Stability=md("**Year-to-year<br>stability (correlation)**"), 
    epa = md("**Correlation with<br>next year's EPA/play**"),
  ) %>%
  cols_align(align = "center") %>% 
  tab_source_note(
    source_note = paste("Table: @benbbaldwin | Data: nflfastR, PFF, ESPN, SIS | Min", qb_min, "plays")) %>%
  tab_header(
    title = "Comparing measurements of QB play: QBs WHO SWITCHED TEAMS",
    subtitle = glue::glue("2006 through 2021: {lqb %>% filter(!is.na(lag_grade)) %>% nrow()} Quarterback-seasons")
  ) %>%
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
  data_color(
    columns = vars(epa),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(0.05,0.29)),
    autocolor_text = FALSE
  ) %>%
  gtExtras::gt_theme_538() %>%
  tab_footnote(
    footnote = "Since 2016",
    locations = cells_body(columns = Metric, rows = c(10))
  ) %>%
  tab_footnote(
    footnote = "Adjustment caps value of negative plays at average value of turnover (-4.5)",
    locations = cells_body(columns = Metric, rows = 3)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "white")
    ),
    locations = cells_body(
      columns = vars(epa),
      rows = c(10))
  ) %>%
  gtExtras::fmt_pad_num(columns = c(2, 3), nsmall = 2)


tab

tab %>%
  gtsave("img/qb_properties_switchers.png")

# see list of recent switchers
lqb %>%
  filter(season >= 2019) %>%
  arrange(season) %>%
  select(name, season, posteam, lag_posteam)



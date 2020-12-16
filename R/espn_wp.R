library(tidyverse)
library(gt)

seasons <- 2015:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
}) %>%
  select(game_id, play_id, vegas_home_wp, qtr, down, ydstogo, desc, posteam)

games <- readRDS(url("http://www.habitatring.com/games_alt.rds")) %>% 
  dplyr::filter(
    
    # this probably isn't necessary but whatever
    dplyr::between(season, 2015, 2019),
    
    # hasn't finished yet
    !is.na(result),
    
    # not a tie
    
    result != 0,
    
    # reg season
    week <= 17
    
  )

espn_ids <- games %>% dplyr::pull(espn)

# get WP info from espn game joinable to cleaned pbp data
# The resulting df includes the espn_id, ply_id (joinable),
get_espn_wp <- function(espn_game_id) {
  espn_wp <- data.frame()
  tryCatch(
    expr = {
      espn_wp <-
        httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={espn_game_id}")) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        purrr::pluck("winprobability") %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          espn_game_id = stringr::str_sub(play_id, end = stringr::str_length(espn_game_id)),
          play_id = stringr::str_sub(play_id, stringr::str_length(espn_game_id) + 1),
          # lag because this represents wp after the play
          home_wp = dplyr::lag(home_win_percentage, 1)
        ) %>%
        dplyr::select(espn_game_id, play_id, home_wp) %>%
        dplyr::slice(-1)
    },
    error = function(e) {
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(espn_wp)
}

espn_wps <- map_df(espn_ids, get_espn_wp)
saveRDS(espn_wps, file = "data/espn_wp_2015_2019.rds")

espn_wps <- readRDS("data/espn_wp_2015_2019.rds")

plays <- espn_wps %>%
  left_join(games, by = c("espn_game_id" = "espn")) %>%
  mutate(label = if_else(result > 0, 1, 0)) %>%
  select(label, game_id, play_id, home_wp) %>%
  dplyr::mutate(play_id = as.numeric(play_id)) %>%
  left_join(pbp, by = c("game_id", "play_id")) %>%
  dplyr::rename(espn_home_wp = home_wp, nflfastr_home_wp = vegas_home_wp) %>%
  filter(!is.na(nflfastr_home_wp), !is.na(espn_home_wp), qtr <= 4, !is.na(down)) %>%
  as_tibble()

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("More times\nthan expected", "Fewer times\nthan expected"),
  qtr = factor("1st Quarter")
)

# for espn plot
plays %>%
  # Create BINS for wp:
  mutate(bin_pred_prob = round(espn_home_wp / 0.01) * .01) %>%
  # Group by both the qtr and bin_pred_prob:
  
  group_by(qtr, bin_pred_prob) %>%

  # Calculate the calibration results:
  summarize(
    n_plays = n(),
    n_wins = length(which(label == 1)),
    bin_actual_prob = n_wins / n_plays
  ) %>%
  ungroup() %>%
  mutate(qtr = fct_recode(factor(qtr),
                          "1st Quarter" = "1", "2nd Quarter" = "2",
                          "3rd Quarter" = "3", "4th Quarter" = "4"
  )) %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of plays",
    title = "ESPN",
    x = "Estimated win probability",
    y = "Observed win probability"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  facet_wrap(~qtr, ncol = 4)


# nflfastR
plays %>%
  # Create BINS for wp:
  mutate(bin_pred_prob = round(nflfastr_home_wp / 0.01) * .01) %>%
  # Group by both the qtr and bin_pred_prob:
  
  group_by(qtr, bin_pred_prob) %>%
  
  # Calculate the calibration results:
  summarize(
    n_plays = n(),
    n_wins = length(which(label == 1)),
    bin_actual_prob = n_wins / n_plays
  ) %>%
  ungroup() %>%
  mutate(qtr = fct_recode(factor(qtr),
                          "1st Quarter" = "1", "2nd Quarter" = "2",
                          "3rd Quarter" = "3", "4th Quarter" = "4"
  )) %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of plays",
    title = "nflfastR",
    x = "Estimated win probability",
    y = "Observed win probability"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  facet_wrap(~qtr, ncol = 4)





# **************************************************************************
# for 2020
# table comparing log loss

seasons <- 2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
}) %>%
  select(game_id, play_id, vegas_home_wp, qtr, down, ydstogo, desc, posteam)

games <- readRDS(url("http://www.habitatring.com/games_alt.rds")) %>% 
  dplyr::filter(
    
    # this probably isn't necessary but whatever
    season == 2020,
    
    # hasn't finished yet
    !is.na(result),
    
    # not a tie
    
    result != 0,
    
    # reg season
    week <= 17
    
  )

espn_ids <- games %>% dplyr::pull(espn)

espn_wps <- map_df(espn_ids, get_espn_wp)

plays <- espn_wps %>%
  left_join(games, by = c("espn_game_id" = "espn")) %>%
  mutate(label = if_else(result > 0, 1, 0)) %>%
  select(label, game_id, play_id, home_wp) %>%
  dplyr::mutate(play_id = as.numeric(play_id)) %>%
  left_join(pbp, by = c("game_id", "play_id")) %>%
  dplyr::rename(espn_home_wp = home_wp, nflfastr_home_wp = vegas_home_wp) %>%
  filter(!is.na(nflfastr_home_wp), !is.na(espn_home_wp), qtr <= 4) %>%
  as_tibble()

library(MLmetrics)

plays1 <- plays %>% group_by(game_id) %>% dplyr::slice(1) %>% ungroup()
message('2020 season, 1st play')

LogLoss(y_pred = plays1$espn_home_wp, y_true = plays1$label)
LogLoss(y_pred = plays1$nflfastr_home_wp, y_true = plays1$label)


# all, 1st play of game, 1st q, 2nd q, 3rd q, 4th q
# nflfastR / espn

# log loss comparisons for all downs
all_e <- LogLoss(y_pred = plays %>% filter(down <= 4) %>% pull(espn_home_wp), y_true = plays %>% filter(down <=4) %>% pull(label))
all_n <- LogLoss(y_pred = plays %>% filter(down <= 4) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down <= 4) %>% pull(label))

q1_e <- LogLoss(y_pred = plays %>% filter(down <= 4 & qtr == 1) %>% pull(espn_home_wp), y_true = plays %>% filter(down <=4 & qtr == 1) %>% pull(label))
q1_n <- LogLoss(y_pred = plays %>% filter(down <= 4 & qtr == 1) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down <= 4 & qtr == 1) %>% pull(label))

q2_e <- LogLoss(y_pred = plays %>% filter(down <= 4 & qtr == 2) %>% pull(espn_home_wp), y_true = plays %>% filter(down <=4 & qtr == 2) %>% pull(label))
q2_n <- LogLoss(y_pred = plays %>% filter(down <= 4 & qtr == 2) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down <= 4 & qtr == 2) %>% pull(label))

q3_e <- LogLoss(y_pred = plays %>% filter(down <= 4 & qtr == 3) %>% pull(espn_home_wp), y_true = plays %>% filter(down <=4 & qtr == 3) %>% pull(label))
q3_n <- LogLoss(y_pred = plays %>% filter(down <= 4 & qtr == 3) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down <= 4 & qtr == 3) %>% pull(label))

q4_e <- LogLoss(y_pred = plays %>% filter(down <= 4 & qtr == 4) %>% pull(espn_home_wp), y_true = plays %>% filter(down <=4 & qtr == 4) %>% pull(label))
q4_n <- LogLoss(y_pred = plays %>% filter(down <= 4 & qtr == 4) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down <= 4 & qtr == 4) %>% pull(label))


# log loss comparison for 1st downs
all_e_1d <- LogLoss(y_pred = plays %>% filter(down == 1) %>% pull(espn_home_wp), y_true = plays %>% filter(down == 1) %>% pull(label))
all_n_1d <- LogLoss(y_pred = plays %>% filter(down == 1) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down == 1) %>% pull(label))

q1_e_1d <- LogLoss(y_pred = plays %>% filter(down == 1 & qtr == 1) %>% pull(espn_home_wp), y_true = plays %>% filter(down == 1 & qtr == 1) %>% pull(label))
q1_n_1d <- LogLoss(y_pred = plays %>% filter(down == 1 & qtr == 1) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down == 1 & qtr == 1) %>% pull(label))

q2_e_1d <- LogLoss(y_pred = plays %>% filter(down == 1 & qtr == 2) %>% pull(espn_home_wp), y_true = plays %>% filter(down == 1 & qtr == 2) %>% pull(label))
q2_n_1d <- LogLoss(y_pred = plays %>% filter(down == 1 & qtr == 2) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down == 1 & qtr == 2) %>% pull(label))

q3_e_1d <- LogLoss(y_pred = plays %>% filter(down == 1 & qtr == 3) %>% pull(espn_home_wp), y_true = plays %>% filter(down == 1 & qtr == 3) %>% pull(label))
q3_n_1d <- LogLoss(y_pred = plays %>% filter(down == 1 & qtr == 3) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down == 1 & qtr == 3) %>% pull(label))

q4_e_1d <- LogLoss(y_pred = plays %>% filter(down == 1 & qtr == 4) %>% pull(espn_home_wp), y_true = plays %>% filter(down == 1 & qtr == 4) %>% pull(label))
q4_n_1d <- LogLoss(y_pred = plays %>% filter(down == 1 & qtr == 4) %>% pull(nflfastr_home_wp), y_true = plays %>% filter(down == 1 & qtr == 4) %>% pull(label))


tab <- tibble::tibble(
  model = c("ESPN", "nflfastR", "ESPN", "nflfastR"),
  type = c("All downs: log loss", "All downs: log loss", "1st downs: log loss", "1st downs: log loss"),
  all = c(all_e, all_n, all_e_1d, all_n_1d),
  q1 = c(q1_e, q1_n, q1_e_1d, q1_n_1d),
  q2 = c(q2_e, q2_n, q2_e_1d, q2_n_1d),
  q3 = c(q3_e, q3_n, q3_e_1d, q3_n_1d),
  q4 = c(q4_e, q4_n, q4_e_1d, q4_n_1d)
) %>% 
  group_by(type) %>%
  gt() %>%
  cols_label(
    model = "",
    all = "All quarters",
    q1 = "Q1",
    q2 = "Q2",
    q3 = "Q3",
    q4 = "Q4"
  ) %>%
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(everything())
    )
  ) %>% 
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "black",
    table.border.top.width = px(1),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2)
  ) %>%
  fmt_number(
    columns = vars(all, q1, q2, q3, q4), decimals = 3
  ) %>% 
  cols_align(
    columns = 2:5,
    align = "center"
  ) %>% 
  tab_header(
    title = md(glue::glue("NFL win probability model performance, 2020"))
  ) %>%
  tab_source_note(md('**Notes**: Downs 1-4. Tie game not included. Lower log loss is better.'))

tab

tab %>%
  gtsave("img/wp_2020_comparison.png")


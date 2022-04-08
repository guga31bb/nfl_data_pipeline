
library(tidyverse)

########## pbwr from espn
get_pbwr <- function(url, node) {
  
  .x <- node
  data <- rvest::html_nodes(rvest::read_html(url),'p') %>%  rvest::html_text()

  strsplit(data[.x], "\n")[[1]] %>% 
      as_tibble() %>%
      separate(value, c("f","l"), sep= "[.]") %>%
      separate(l, c("team", "p"), sep= "[,]") %>%
      mutate(
        team_name = substring(team,2), 
        team_name = ifelse(team_name == "Washington Football Team", "Washington Commanders", team_name),
        team_name = ifelse(team_name == "Washington Redskins", "Washington Commanders", team_name),
        wr = substring(p, 2,3)
      ) %>%
      left_join(nflfastR::teams_colors_logos %>% filter(team_abbr != "LAR"), by = c("team_name")) %>%
      select(posteam = team_abbr, wr)
  
}

# 2019

  url1 <- "https://www.espn.com/nfl/story/_/id/27584726/nfl-pass-blocking-pass-rushing-rankings-2019-pbwr-prwr-leaderboard"
  get_pbwr(url, 9)

# 2020
  url2 <- "https://www.espn.com/nfl/story/_/id/29939464/2020-nfl-pass-rushing-run-stopping-blocking-leaderboard-win-rate-rankings"
  get_pbwr(url, 15)
  
# 2021
  url3 <- "https://www.espn.com/nfl/story/_/id/32176833/2021-nfl-pass-rushing-run-stopping-blocking-leaderboard-win-rate-rankings"
  get_pbwr(url, 15)

espn <- bind_rows(
  get_pbwr(url1, 9) %>% mutate(season = 2019),
  get_pbwr(url2, 15) %>% mutate(season = 2020),
  get_pbwr(url3, 15) %>% mutate(season = 2021)
) %>%
  mutate(wr = as.numeric(wr)) %>%
  group_by(season) %>%
  mutate(
    min = min(wr),
    wr = wr - min,
    max = max(wr),
    wr = 100 * wr / max
    ) %>%
  ungroup() %>%
  select(posteam, season, wr) %>%
  mutate_at(c("posteam"), nflfastR:::team_name_fn)
  
############### pff

pff <- readRDS("pff/data/team_season_grades.rds") %>%
  filter(season >= 2019) %>%
  select(posteam = team_abbr, season, pb_grade = grades_pass_block) %>%
  group_by(season) %>%
  mutate(
    min = min(pb_grade),
    pb_grade = pb_grade - min,
    max = max(pb_grade),
    pb_grade = 100 * pb_grade / max
  ) %>%
  ungroup() %>%
  select(posteam, season, pb_grade)

joined <- espn %>%
  full_join(pff, by = c("season", "posteam")) %>%
  left_join(nflreadr::load_teams(), by = c("posteam" = "team_abbr")) %>%
  mutate(label = paste0(posteam, substr(as.character(season), 3, 4)))

joined %>%
  ggplot(aes(x = pb_grade, y = wr)) +
  geom_hline(yintercept = mean(joined$wr), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(joined$pb_grade), color = "red", linetype = "dashed", alpha=0.5) +
  annotate("segment", x = 0, xend = 100, y = 0, yend = 100,
               colour = "black",lty = "dashed", alpha = 0.5) +
  geom_point(color =  if_else(joined$posteam=="SEA",
                              joined$team_color2,joined$team_color),
             alpha = .6,
             size = 3) +
  ggrepel::geom_text_repel(aes(label=label),
                  size = 2,
                  force = 1, 
                  point.padding=.1,
                  max.overlaps = 20,
                  segment.size=0.1) +
  labs(x = "PFF Pass Block Grade",
       y = "ESPN Pass Block Win Rate",
       title = "PBWR and Pass Block Grade are highly correlated",
       subtitle = "2019 through 2021 | Re-scaled to be 0 to 100 in each season",
       caption = paste("@benbbaldwin | Data: ESPN and PFF | Regular season")
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    # aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("img/espn_pff_passpro.png")

# pb_grade versus pass offense

pbp <- nflreadr::load_pbp(2019:2021) %>%
  filter(down <= 4, pass == 1, season_type == "REG", !is.na(epa)) %>%
  group_by(posteam, season) %>%
  summarise(epa = mean(epa)) %>%
  ungroup()

df <- pbp %>%
  left_join(joined, by = c("posteam", "season"))

df


df %>%
  ggplot(aes(x = pb_grade, y = epa)) +
  geom_hline(yintercept = mean(df$epa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(df$pb_grade), color = "red", linetype = "dashed", alpha=0.5) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  geom_point(color =  if_else(df$posteam=="SEA",
                              df$team_color2,df$team_color),
             alpha = .6,
             size = 3) +
  ggrepel::geom_text_repel(aes(label=label),
                           size = 2,
                           force = 1, 
                           point.padding=.1,
                           max.overlaps = 20,
                           segment.size=0.1) +
  labs(x = "PFF Pass Block Grade",
       y = "EPA per dropback",
       title = "Pass efficiency versus pass protection",
       subtitle = "2019 through 2021 | Grade re-scaled to be 0 to 100 in each season",
       caption = paste("@benbbaldwin | Data: nflfastR and PFF | Regular season")
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    panel.background = element_rect(color = "black", linetype = "solid"),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~season)

ggsave("img/pff_passpro_epa.png")


df %>%
  ggplot(aes(x = wr, y = epa)) +
  geom_hline(yintercept = mean(df$epa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(df$wr), color = "red", linetype = "dashed", alpha=0.5) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  geom_point(color =  if_else(df$posteam=="SEA",
                              df$team_color2,df$team_color),
             alpha = .6,
             size = 3) +
  ggrepel::geom_text_repel(aes(label=label),
                           size = 2,
                           force = 1, 
                           point.padding=.1,
                           max.overlaps = 20,
                           segment.size=0.1) +
  labs(x = "ESPN Pass Block Win Rate",
       y = "EPA per dropback",
       title = "Pass efficiency versus PBWR",
       subtitle = "2019 through 2021 | PBWR re-scaled to be 0 to 100 in each season",
       caption = paste("@benbbaldwin | Data: nflfastR and ESPN | Regular season")
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    panel.background = element_rect(color = "black", linetype = "solid"),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~season)

ggsave("img/espn_passpro_epa.png")

# correlations

cors <- df %>%
  arrange(posteam, season) %>%
  group_by(posteam) %>% 
  mutate(lgrade = lag(pb_grade), lwr = lag(wr)) %>%
  select(posteam, season, epa, wr, lwr, pb_grade, lgrade) %>%
  ungroup() %>%
  filter(season > 2019)

cor(cors$pb_grade, cors$lgrade)
cor(cors$wr, cors$lwr)
cor(cors$epa, cors$wr)
cor(cors$epa, cors$lwr)

# players switching teams

panel <- readRDS("pff/data/season_grades_ol.rds") %>%
  filter(snap_counts_pass_play >= 400, season >= 2007) %>%
  group_by(player_id, season) %>%
  arrange(player_id, season, -snap_counts_pass_play) %>%
  # 1 obs per player-season
  dplyr::slice(1) %>%
  dplyr::rename(pb_grade = grades_pass_block) %>%
  group_by(position, season) %>%
  mutate(
    min = min(pb_grade),
    pb_grade = pb_grade - min,
    max = max(pb_grade),
    pb_grade = 100 * pb_grade / max
  ) %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    lgrade = dplyr::lag(pb_grade),
    lteam = dplyr::lag(team_abbr),
    lseason = dplyr::lag(season)
    ) %>%
  select(season, lseason, player, player_id, position, team_abbr, lteam, pb_grade, lgrade, snaps = snap_counts_pass_play) %>%
  filter(
    !is.na(lteam), 
    !is.na(lgrade)
    # season == lseason + 1
    ) %>%
  mutate(
    type = case_when(
      team_abbr == lteam & position == "T" ~ "T, same team",
      team_abbr == lteam & position %in% c("G", "C") ~ "G/C, same team",
      team_abbr != lteam & position == "T" ~ "T, switched teams",
      team_abbr != lteam & position %in% c("G", "C") ~ "G/C, switched teams"
    )) %>%
  left_join(nflreadr::load_teams() %>% select(team_abbr, team_color, team_color2), by = c("team_abbr"))

panel %>%
  filter(team_abbr == "SEA" | lteam == "SEA", season >= 2020)

panel %>%
  filter(team_abbr != lteam) %$%
  cor(pb_grade, lgrade)

panel %>%
  filter(team_abbr == lteam) %$%
  cor(pb_grade, lgrade)

cors <- plyr::ddply(panel, c("type"), summarise, cor = round(cor(pb_grade, lgrade), 2))

panel %>%
  ggplot(aes(x = lgrade, y = pb_grade)) +
  geom_hline(yintercept = mean(panel$pb_grade), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(panel$lgrade), color = "red", linetype = "dashed", alpha=0.5) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  geom_point(color =  if_else(panel$team_abbr=="SEA",
                              panel$team_color2,panel$team_color),
             alpha = .6,
             size = 3) +
  labs(x = "Prior season grade",
       y = "Current season grade",
       title = "Stability of player pass block grades",
       subtitle = "2007 through 2021 | Grade re-scaled to be 0 to 100 for each position-season",
       caption = paste("@benbbaldwin | Data: nflfastR and PFF | Regular season")
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    panel.background = element_rect(color = "black", linetype = "solid"),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~type, ncol = 2) +
  geom_text(data=cors, aes(label=paste("Correlation: ", cor, sep="")), x=7.5, y=97.5, color = "red")

ggsave("img/passpro_stability.png")



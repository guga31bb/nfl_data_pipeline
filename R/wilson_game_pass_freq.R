library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(ggrepel)

# get data
seasons <- 2012:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>%
  filter(posteam == "SEA")
})

df <- pbp %>%
  # get to normal plays
  filter(!is.na(down), rush == 1 | pass == 1) %>%
  group_by(game_id) %>%
  mutate(
    # does one team have less than 10% chance to win?
    # note that i'm not using vegas_wp here because there are games 
    # where the seahawks are huge favorites and there are basically no plays left
    under_wp = if_else(between(wp, .10, .90), 0, 1),
    
    # game is "over" when we've gone under 10% WP
    over = if_else(cumsum(under_wp) > 0, 1, 0),
    
    # for calculating wilson epa
    wilson_epa = if_else(name == "R.Wilson", qb_epa, NA_real_),
    
    # for help making labels
    home = if_else(home_team == "SEA", 1, 0)
    ) %>%
  # want plays where game isn't "over" and early downs
  filter(over == 0, down <= 2) %>%
  summarise(
    pass = mean(pass),
    season = dplyr::first(season),
    week = dplyr::first(week),
    wilson_epa = mean(wilson_epa, na.rm = T),
    defteam = dplyr::first(defteam),
    home = dplyr::first(home)
  ) %>%
  ungroup() %>%
  mutate(
    home_lbl = if_else(home == 1, "", "@"),
    playoff_lbl = if_else(week > 17, "*", ""),
    label = glue::glue("{home_lbl}{defteam}{substr(game_id, 3, 4)}{playoff_lbl}"),
    # for point  colors
    era = case_when(
      season < 2020 ~ 1,
      # rams games in 2020
      season == 2020 & defteam == "LA" ~ 2,
      # first 8 games of 2020
      season == 2020 & week <= 9 ~ 3,
      # all the other games in 2020
      TRUE ~ 4
    )
  )

# labels on the plot
text_df <- tibble(
  label = c(
    "Games in <span style='color:#F8766D'>**2012-2019**</span>",
    "First 8 games of <span style='color:#00BFC4'>**2020**</span>",
    "Rams games in <span style='color:#7CAE00'>**2020**</span>",
    "Other games in <span style='color:#C77CFF'>**2020**</span>"
  ),
  x = c(.75, .752, .751, .7505),
  y = c(1.95, 1.75, 1.55, 1.35),
  angle = c(0, 0, 0, 0),
  color = c("black", "black", "black", "black")
)

# make the plot
df %>%
  ggplot(aes(pass, wilson_epa, color = factor(era), label = label)) + 
  geom_hline(aes(yintercept = mean(wilson_epa))) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, color = "black") +
  geom_ribbon(stat='smooth', se=TRUE, alpha=0.1, 
              aes(color = NULL)) +
  geom_point(size = 3) +
  geom_text_repel(data = filter(df, 
        pass < .35 | pass > .65 | wilson_epa > .8 | wilson_epa < -.25 | era > 1
        )
        , size = 4) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 22, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  labs(
    x = "Dropback %",
    y = "EPA per play",
    title = "Russell Wilson has been just as efficient at high volume",
    subtitle = "Early-down efficiency, game in contention (until one team has < 10% chance to win)",
    caption = "@benbbaldwin | * = playoff game"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,.1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, .01)) +
  geom_richtext(data = text_df,   
                aes(
                  x, y, label = label, angle = angle
                ), color = "black", fill = NA, label.color = NA, size = 4
  ) +
  annotate("point", x = .706, y = 1.95, colour = "#F8766D", size = 3) +
  annotate("point", x = .706, y = 1.75, colour = "#00BFC4", size = 3) +
  annotate("point", x = .706, y = 1.55, colour = "#7CAE00", size = 3) +
  annotate("point", x = .706, y = 1.35, colour = "#C77CFF", size = 3)

ggsave("img/wilson_pass_freq.png")  





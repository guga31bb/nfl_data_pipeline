library(tidyverse)
games <- readRDS(url("http://www.habitatring.com/games.rds"))

games %>%
  select(season, week, home_team, away_team, spread_line, result) %>%
  mutate(home_excess = result - spread_line) %>%
  filter(season == 2020, !is.na(result))

data <- games %>%
  select(season, week, home_team, away_team, spread_line, result) %>%
  mutate(home_excess = result - spread_line) %>%
  group_by(season, week) %>%
  summarize(home_adv = mean(home_excess))

data %>%
  filter(week <= 17) %>%
  ggplot(aes(week, home_adv, color = factor(season))) +
  geom_jitter(width = .1, size = 2.5) +
  labs(x = "Week",
       y = "Home performance over expected",
       caption = "Figure: Ben Baldwin | Data: Lee Sharpe",
       subtitle = "1999 - 2020",
       title = "Home team performance relative to spread") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10)) +
  annotate("text", x = 5, y = 9, label = "2020", color = "red", size = 8) +
  geom_segment(aes(x = 5, y = 8, xend = 1.1, yend = 1.66, colour = "segment"), size = 2, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = 5, y = 8, xend = 2.1, yend = 0.969, colour = "segment"), size = 2, arrow = arrow(length = unit(0.03, "npc"))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave('C:/Users/bback/Dropbox/nfl/current_season/results/99_hfa.png', dpi=800)



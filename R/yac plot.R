

library(tidyverse)
library(ggrepel)
data_path <- "~/github/"
res <- 800

tictoc::tic('loading data')
con <- DBI::dbConnect(RSQLite::SQLite(), glue::glue('{data_path}/master_db'))
data <- tbl(con, "cleaned_pbp") %>%
  filter(!is.na(posteam) & !is.na(epa), season == 2020, down <= 4) %>%
  select(game_id, qtr, desc, posteam, name,air_epa, qb_epa, epa, xyac_epa, yac_epa, complete_pass, incomplete_pass, fumble_lost) %>%
  collect()
tictoc::toc()

DBI::dbDisconnect(con)

qb_df <- data %>%
  group_by(name, posteam) %>%
  mutate(att = sum(complete_pass + incomplete_pass)) %>%
  ungroup() %>%
  filter(att > 15) %>%
  mutate(non_fumble_yac_epa = qb_epa - air_epa)


unadjusted <- qb_df %>%
  group_by(name, posteam) %>%
  summarize(epa = mean(epa)) %>%
  ungroup()

qbs <- qb_df %>%
  group_by(name, posteam) %>%
  filter(complete_pass == 1, !is.na(xyac_epa)) %>%
  summarize(
    tot_epa = mean(epa),
    tot_air = mean(air_epa, na.rm = T),
    tot_yac = mean(yac_epa),
    tot_nfyac = mean(non_fumble_yac_epa),
    tot_xyac = mean(xyac_epa, na.rm = T)
  ) %>%
  left_join(nflfastR::teams_colors_logos, by = c('posteam' = 'team_abbr')) 



qbs %>%
  ggplot(aes(x = tot_xyac, y = tot_nfyac)) +
  geom_hline(yintercept = mean(qbs$tot_nfyac), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(qbs$tot_xyac), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = qbs$team_color, size = 7, alpha = .6) +
  geom_text_repel(aes(label=name),
                  force=1, point.padding=.1,
                  segment.size=0.1) +
  # stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Expected YAC EPA",
       y = "Actual YAC EPA",
       caption = paste0("Figure: @benbbaldwin | Data: @nflfastR | YAC EPA does not include lost fumbles"),
       subtitle = "Shanny and McVay are dealin'",
       title = "Expected versus actual yards after the catch EPA per completion") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(aspect.ratio=1)


ggsave('C:/Users/bback/Dropbox/nfl/current_season/results/99_xyac.png', dpi=res)



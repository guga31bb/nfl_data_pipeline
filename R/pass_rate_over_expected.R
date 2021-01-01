library(tidyverse)
library(ggrepel)
library(ggimage)
library(ggtext)
library(mgcv)
library(scales)
library(ggforce)
library(nflfastR)

pbp <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds"))

big_data <- pbp %>%
  filter(!is.na(posteam) & !is.na(epa), season == 2020)

data <- big_data %>%
  add_xpass() %>%
  filter(!is.na(pass_oe))

chart <- big_data %>%
  add_xpass() %>%
  filter(
    !is.na(pass_oe),
    down <= 2
  ) %>%
  group_by(posteam) %>%
  summarize(
    pass = mean(pass),
    xpass = mean(xpass),
    pass_oe = mean(pass_oe)
  ) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  arrange(pass_oe) %>%
  mutate(
    x = 1 : n(), xend = 1 : n(),
    y = xpass, yend = pass,
    main_color = if_else(posteam == "SEA", team_color2, team_color),
    secondary_color = if_else(posteam == "SEA", team_color, team_color2)
  )


df <- tibble(
  label = c(
    "<span style='color:red'>**Expected**</span> pass %",
    "<span style='color:red'>**Actual**</span> pass %",
    "<span style='color:red'>Expectation</span> from down, distance,<br>field position, time, score differential,<br>and win probability.<br><br>@benbbaldwin"
  ),
  x = c(33.25, 33.25, 4),
  y = c(
    chart %>% filter(x == 32) %>% pull(y),
    chart %>% filter(x == 32) %>% pull(yend),
    .625
  ),
  color = c("black", "black", "black"),
  hjust = c(.5, .5, 0)
)

my_title <- glue::glue("Early-Down <span style='color:red'>**Pass Rate**</span> Over <span style='color:red'>**Expected**</span>, 2020")
chart %>%
  ggplot() +
  geom_link(
    mapping = aes(x = x, y = y, xend = xend, yend = yend, size = 2, alpha = stat(index), color = main_color)
  ) +
  geom_point(aes(x = x, y = y, color = secondary_color), size = 5) +
  scale_colour_identity() +
  coord_flip() +
  theme_bw() +
  geom_image(aes(x = xend, y = yend, image = team_logo_espn), size = 0.04, asp = 16/9) +
  labs(
    x = "",
    y = "Pass rate",
    title = my_title
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 26),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  # diff
  geom_text(aes(
    x = xend,
    y = if_else(pass_oe >= 0, yend + .0125, yend - .0105),
    label = if_else(
      pass_oe >= 0,
      glue::glue("(+{round(pass_oe, 0)}%)"),
      glue::glue("({round(pass_oe, 0)}%)")
    ),
    color = if_else(pass_oe >= 0, "#328a40", "red")
  ),
  fontface = "bold",
  size = 4
  ) +
  geom_richtext(
    data = df,
    aes(
      x = x, y = y, label = label, color = color, hjust = hjust
    ),
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 2), "pt")
  )

ggsave('img/11_poe.png', dpi=700, height=9*.8, width=16*.8)



# ##################################
# defense
chart <- big_data %>%
  add_xpass() %>%
  filter(
    !is.na(pass_oe),
    down <= 2
  ) %>%
  group_by(defteam) %>%
  summarize(
    pass = mean(pass),
    xpass = mean(xpass),
    pass_oe = mean(pass_oe)
  ) %>%
  left_join(nflfastR::teams_colors_logos, by = c("defteam" = "team_abbr")) %>%
  arrange(pass_oe) %>%
  mutate(
    x = 1 : n(), xend = 1 : n(),
    y = xpass, yend = pass,
    main_color = if_else(defteam == "SEA", team_color2, team_color),
    secondary_color = if_else(defteam == "SEA", team_color, team_color2)
  )


df <- tibble(
  label = c(
    "<span style='color:red'>**Expected**</span> pass %",
    "<span style='color:red'>**Actual**</span> pass %",
    "<span style='color:red'>Expectation</span> from down, distance,<br>field position, time, score differential,<br>and win probability.<br><br>@benbbaldwin"
  ),
  x = c(33.25, 33.25, 3),
  y = c(
    chart %>% filter(x == 32) %>% pull(y),
    chart %>% filter(x == 32) %>% pull(yend),
    .60
  ),
  color = c("black", "black", "black"),
  hjust = c(.5, .5, 0)
)

my_title <- glue::glue("Opposing Early-Down <span style='color:red'>**Pass Rate**</span> Over <span style='color:red'>**Expected**</span>, 2020")
chart %>%
  ggplot() +
  geom_link(
    mapping = aes(x = x, y = y, xend = xend, yend = yend, size = 2, alpha = stat(index), color = main_color)
  ) +
  geom_point(aes(x = x, y = y, color = secondary_color), size = 5) +
  scale_colour_identity() +
  coord_flip() +
  theme_bw() +
  geom_image(aes(x = xend, y = yend, image = team_logo_espn), size = 0.04, asp = 16/9) +
  labs(
    x = "",
    y = "Pass rate",
    title = my_title
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 26),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  # diff
  geom_text(aes(
    x = xend,
    y = if_else(pass_oe >= 0, yend + .0125, yend - .0105),
    label = if_else(
      pass_oe >= 0,
      glue::glue("(+{round(pass_oe, 0)}%)"),
      glue::glue("({round(pass_oe, 0)}%)")
    ),
    color = if_else(pass_oe >= 0, "#328a40", "red")
  ),
  fontface = "bold",
  size = 4
  ) +
  geom_richtext(
    data = df,
    aes(
      x = x, y = y, label = label, color = color, hjust = hjust
    ),
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 2), "pt")
  )

ggsave('img/11b_poe_def.png', dpi=700, height=9*.8, width=16*.8)


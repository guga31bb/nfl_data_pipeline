library(tidyverse)
library(gt)

# get pbp to figure out starts
seasons <- 2012:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>%
    filter(posteam %in% c("SEA", "LA", "SF", "ARI"))
})

# get rosters (for player full name)
roster <- nflfastR::fast_scraper_roster(2012:2020) %>%
  select(full_name, gsis_id) %>%
  group_by(gsis_id) %>%
  dplyr::slice(1) %>%
  ungroup()

# put everything together
data <- pbp %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(game_id, game_date, posteam) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  group_by(posteam, passer_player_name, passer_player_id) %>%
  summarize(
    first = dplyr::first(game_date),
    games = n()
    ) %>%
  ungroup() %>%
  arrange(posteam, first) %>%
  nflfastR::decode_player_ids() %>%
  left_join(roster, by = c("passer_player_id" = "gsis_id")) %>%
  select(full_name, posteam)

# hacky way to get columns for each team
ari <- data %>%
  filter(posteam == "ARI") %>%
  select(full_name) %>%
  rename(ari = full_name)

sf <- data %>%
  filter(posteam == "SF") %>%
  select(full_name) %>%
  rename(sf = full_name)

la <- data %>%
  filter(posteam == "LA") %>%
  select(full_name) %>%
  rename(la = full_name)

# delete this after Wolford appears in the data
la <- la %>%
  bind_rows(
    tibble::tibble(
      la = "John Wolford"
    )
  )
# end modification for Wolford

sea <- data %>%
  filter(posteam == "SEA") %>%
  select(full_name) %>%
  rename(sea = full_name)

# get length of max for padding other columns
# otherwise binding all the teams together doesn't work
max <- max(nrow(ari), nrow(sf), nrow(la), nrow(sea))

# padding step
sea <- sea %>%
  bind_rows(
    tibble::tibble(sea = rep(" ", (max - nrow(sea))))
  )

sf <- sf %>%
  bind_rows(
    tibble::tibble(sf = rep(" ", (max - nrow(sf))))
  )

la <- la %>%
  bind_rows(
    tibble::tibble(
      la = rep(" ", (max - nrow(la)))
      )
  )

# put it all together
tab <- bind_cols(
  ari, la, sf, sea
) %>%
  gt() %>%
  cols_label(
    ari = html(web_image("https://a.espncdn.com/i/teamlogos/nfl/500/ari.png", height = 75)),
    la = html(web_image("https://a.espncdn.com/i/teamlogos/nfl/500/la.png", height = 75)),
    sf = html(web_image("https://a.espncdn.com/i/teamlogos/nfl/500/sf.png", height = 75)),
    sea = html(web_image("https://a.espncdn.com/i/teamlogos/nfl/500/sea.png", height = 75))
  ) %>%
  tab_options(
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = 'black',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'black',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = 'black',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = 'black',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'black',
    table.border.top.color = 'black',
    table.border.bottom.color = 'black',
    row.striping.background_color = '#f0f2f1',
    row.striping.include_table_body = TRUE,
    table.font.size = 16,
    heading.title.font.size = 30
  )  %>%
  cols_align(
    columns = 1:4,
    align = "center"
  ) %>% 
  tab_header(
    title = md(glue::glue("**NFC West Starting QBs Since 2012**"))
  )

tab

tab %>%
  gtsave('img/nfc_west_starting_qbs.png')




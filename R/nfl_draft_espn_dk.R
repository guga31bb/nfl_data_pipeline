library(tidyverse)
library(gt)

# DK
  
  dk <- "https://sportsbook.draftkings.com//sites/US-SB/api/v4/eventgroups/88670561/categories/669/subcategories/6444?format=json" %>%
      jsonlite::fromJSON(url) %>%
      pluck("eventGroup", "offerCategories", 1, "offerSubcategoryDescriptors", 2, "offerSubcategory", "offers") %>%
      bind_rows() %>%
      select(player = label, outcomes) %>%
      unnest_wider(outcomes) %>%
      mutate(
        odds = as.numeric(oddsAmerican),
        under = ifelse(stringr::str_detect(label, "Under"), 1, 0)
      ) %>%
      select(player, pick = label, odds, under) %>%
      mutate(
        player = stringr::str_remove(player, " Draft Position"),
        pick = stringr::str_remove(pick, "(Under )|(Over )"),
        pick = as.numeric(pick),
        pct = ifelse(
          odds > 0,
          100 / (odds + 100),
          abs(odds) / (abs(odds) + 100)
        )
      )

  # remove vig using power method: https://www.researchgate.net/publication/326510904_Adjusting_Bookmaker's_Odds_to_Allow_for_Overround
  
    walk(1:10, ~{
      dk <<- dk %>%
        group_by(player) %>%
        mutate(tot_pct = sum(pct)) %>%
        mutate(
          k = log(2) / log(2 / tot_pct),
          pct = pct ^ k
        ) %>%
        select(-tot_pct, -k) %>%
        ungroup()
    })
    
    dk <- dk %>%
      pivot_wider(values_from = c(odds, pct), names_from = under) %>%
      select(player, pick_dk = pick, odds_under = odds_1, odds_over = odds_0, pct_under = pct_1) %>%
      arrange(pick_dk)
    
    dk %>%
      write_csv("data/dk_draft_2022.csv")
    
# ESPN

  espn <- read.csv(
    'http://espnsportsanalytics.com/draft_viz_player_positions.csv',
    stringsAsFactors = FALSE) %>%
    select(name = 1) %>%
    group_by(name) %>%
    mutate(url_name = URLencode(name)) %>%
    pull(url_name) %>%
    map_df(
      ~{
        read.csv(
          glue::glue('http://espnsportsanalytics.com/dataUpdate.php?player={URLencode(.x)}&type=final&pick=0&pos=ALL'),
          stringsAsFactors = FALSE) %>%
          as_tibble() %>%
          select(player, pos, espn_pick = pick, espn_prob = p)
      })
  
# join together
  
  nrow(dk)
  length(unique(espn$player))
  
  full <- espn %>%
    left_join(dk, by = c("player")) %>%
    group_by(player) %>%
    mutate(
      tot_espn = cumsum(espn_prob),
      across(c(pct_under, tot_espn), ~100 * .)
      ) %>%
    filter(espn_pick == pick_dk - 0.5) %>%
    ungroup() 
  
  t <- full %>%
    select(player, pos, line = pick_dk, before_dk = pct_under, before_espn = tot_espn) %>%
    mutate(diff = (before_espn - before_dk)) %>%
    arrange(-diff)
  
  t
  
# make table
  
  
  pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
               "#d9f0d3", "#7fbf7b", "#1b7837")
  
  hulk_pal <- function(x){
    scales::col_numeric(
      pal_hex,
      domain = range(t$diff)
    )(x)
  }
  
  nrow(t)
  
  tab <- bind_cols(
    t[1:24,],
      bind_rows(
        t[25:47,],
        tibble::tibble(player = " ", pos = " ", line = NA_real_, before_dk = NA_real_, before_espn = NA_real_, diff = NA_real_))
  ) %>%
    
  # tab <- bind_cols(
  #   t[1:18,],
  #   t[19:36,]
  # ) %>%
    gt::gt()  %>% 
    cols_label(player...1 = "Player", player...7 = "Player", pos...2 = "Pos", pos...8 = "Pos",
               line...3 = "Pick", line...9 = "Pick", before_dk...4 = "DK", before_dk...10 = "DK", before_espn...5 = "ESPN", before_espn...11 = "ESPN",
               diff...6 = "Diff", diff...12 = "Diff") %>% 
    gt::data_color(columns = c(diff...6, diff...12), colors = hulk_pal) %>%
    # gtExtras::gt_hulk_col_numeric(diff) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(title = paste("ESPN and Draft Kings project the 2022 NFL draft")) %>%
    gt::tab_source_note(gt::md(glue::glue('**Notes**: @benbbaldwin | Source: ESPN and vig-removed Draft Kings | Odds as of {lubridate::today()}'))) %>%
    tab_options(data_row.padding = px(2)) %>%
    gtExtras::fmt_symbol_first(column = c(4:6, 10:12), suffix = "%") %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = c(1, 6, 7, 12))
    ) %>%
    fmt_number(columns = c(4:6, 10:12), decimals = 1) %>%
    tab_spanner(label = "Before prob.", columns = c(4:5), id = "xx") %>%
    tab_spanner(label = "Before prob.", columns = c(10:11)) %>%
    tab_style(
      style = list(
        cell_borders(
          side = c("left"), 
          color = "black",
          weight = px(2)
        )
      ),
      locations = cells_body(
        columns = c(7)
      )
    ) %>%
    # stuff for missing final row
    fmt_missing(columns = everything(), rows = everything(), missing_text = " ") %>%
    tab_style(
      style = list(
        cell_fill(color = "white")
      ),
      locations = cells_body(
        columns = 12,
        rows = ((nrow(t)+1)/2)
      )
    )
  
  tab
  
  tab %>%
    gtsave("img/draft2022.png")
  
# track bets
# saved a week before draft
  
  full
  
  # full %>% saveRDS("data/odds_2022_04_21.rds")
  
  full <- readRDS("data/odds_2022_04_21.rds")
  
  # unders
  
  u <- full %>%
    filter(pct_under < tot_espn) %>%
    arrange(-tot_espn) %>%
    mutate(
      p = tot_espn/100,
      b = 100 / abs(odds_under),
      risked = 1000 * (p + (p - 1) / b),
      to_win = 100 * risked / abs(odds_under)
    ) %>%
    filter(risked > 0) %>%
    arrange(-risked) %>%
    select(player, pos, pick = pick_dk, price = odds_under, risked, to_win) %>%
    mutate(result = "?")

  # overs
  
  o <- full %>%
    filter(pct_under >= tot_espn) %>%
    arrange(-tot_espn) %>%
    mutate(
      p = 1 - tot_espn/100,
      b = case_when(
        odds_over >   0 ~ abs(odds_over) / 100,
        TRUE ~        100 / abs(odds_over)
      ),
      risked = 1000 * (p + (p - 1) / b),
      to_win = case_when(
        odds_over < 0 ~ 100 * risked / abs(odds_over),
        TRUE ~ risked * odds_over / 100
      )
    ) %>%
    filter(risked > 0) %>%
    arrange(-risked)  %>%
    select(player, pos, pick = pick_dk, price = odds_over, risked, to_win) %>%
    mutate(result = "?")
  
  
  diff <- nrow(o) - nrow(u)
  
  bets <- bind_rows(
        u,
        tibble::tibble(player = rep(" ", diff), pos = rep(" ", diff), pick = rep(NA_real_, diff), price = rep(NA_real_, diff), risked = rep(NA_real_, diff), to_win = rep(NA_real_, diff))
        ) %>%
    bind_cols(o) %>%
    gt::gt()  %>% 
    cols_label(player...1 = "Player", player...8 = "Player", pos...2 = "Pos", pos...9 = "Pos",
               pick...3 = "Pick", pick...10 = "Pick", price...4 = "Price", price...11 = "Price",
               risked...5 = "Risked", risked...12 = "Risked", to_win...6 = "To win", to_win...13 = "To win",
               result...7 = "Result", result...14 = "Result") %>% 
    # gtExtras::gt_hulk_col_numeric(risked...5) %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(title = paste("ESPN bets the 2022 NFL draft")) %>%
    gt::tab_source_note(gt::md(glue::glue('**Notes**: @benbbaldwin | Source: ESPN and Draft Kings | Odds as of {lubridate::today()}'))) %>%
    tab_options(data_row.padding = px(2)) %>%
    # gtExtras::fmt_symbol_first(column = c(5:6, 12:13), suffix = "$", symbol_first = T) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = c(1, 7, 8, 14))
    ) %>%
    fmt_number(columns = c(5:6, 12:13), decimals = 0) %>%
    tab_spanner(label = "Under Bets", columns = c(1:6), id = "xx") %>%
    tab_spanner(label = "Over Bets", columns = c(8:13)) %>%
    tab_style(
      style = list(
        cell_borders(
          side = c("left"), 
          color = "black",
          weight = px(2)
        )
      ),
      locations = cells_body(
        columns = c(8)
      )
    ) %>%
    # stuff for missing final row
    fmt_missing(columns = everything(), rows = everything(), missing_text = " ")

  bets
  
  bets %>%
    gtsave("img/draft2022_bets.png")
  
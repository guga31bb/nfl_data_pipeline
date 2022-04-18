library(tidyverse)
library(gt)

# DK (note: no QBs in here)
  
  dk <- "https://sportsbook.draftkings.com//sites/US-SB/api/v4/eventgroups/88670561/categories/669/subcategories/6444?format=json" %>%
      jsonlite::fromJSON(url) %>%
      pluck(
        "eventGroup", 
        "offerCategories", 
        1, 
        "offerSubcategoryDescriptors", 
        2, 
        "offerSubcategory",
        "offers") %>%
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
      filter(under == 1) %>%
      arrange(pick) %>%
      select(-under, -odds, pick_dk = pick)
    
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
          select(player, pos, pick, espn_prob = p)
      })
  
# join together
  
  t <- espn %>%
    left_join(dk, by = c("player")) %>%
    group_by(player) %>%
    mutate(
      tot_espn = cumsum(espn_prob) - espn_prob,
      across(c(pct, tot_espn), ~100 * .)
      ) %>%
    filter(pick == pick_dk + 0.5) %>%
    ungroup() %>%
    select(player, pos, line = pick_dk, before_dk = pct, before_espn = tot_espn) %>%
    mutate(diff = (before_espn - before_dk)) %>%
    arrange(-diff)
  
# make table
  
  t %>%
    gt::gt()  %>% 
    cols_label(player = "Player", pos = "Pos", line = "Pick", before_dk = "DK", before_espn = "ESPN", diff = "Diff") %>% 
    gtExtras::gt_hulk_col_numeric(diff) %>%
    gtExtras::gt_theme_espn() %>%
    gt::tab_header(title = paste("ESPN and Draft Kings disagree frequently")) %>%
    gt::tab_source_note(gt::md(glue::glue('**Notes**: @benbbaldwin | Source: ESPN and vig-removed Draft Kings<br>Odds as of {lubridate::today()}'))) %>%
    tab_options(data_row.padding = px(2)) %>%
    gtExtras::fmt_symbol_first(column = 4:6, suffix = "%") %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = c(1, 6))
    ) %>%
    fmt_number(columns = 4:6, decimals = 1) %>%
    tab_spanner(label = "Before prob.", columns = 4:5) %>%
    gtsave("img/draft2022.png")
  
  
  
  
  

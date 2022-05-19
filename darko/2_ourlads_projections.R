
library(tidyverse)
library(patchwork)

# projections

full_df <- readRDS("data/pass_pro_projections.rds")

full_df

# 2022 draft picks
# need 2021 for aaron banks
  pfr_picks <- nflreadr::load_draft_picks(2021:2022) %>%
    filter(position %in% c("C", "G", "T", "OL")) %>%
    select(pfr_name, pfr_pick = pick)
  
# imputation schedule
  
  draft_imputation <- readRDS("imputed_draft_pass_pro.rds")
  
# get ourlads
  
  ourlads_df <- readRDS("pff/data/ourlads.rds") %>%
    mutate(
      player = case_when(
        player == "Orlando Brown Jr." ~ "Orlando Brown",
        player == "Mike Onwenu" ~ "Michael Onwenu",
        TRUE ~ player
      ))
  ourlads_df 
  
# get player IDs from pff and add to full_df
    ids <- readRDS("pff/data/season_grades_ol.rds") %>%
      # not necessary, but gets rid of older players
      filter(season >= 2010) %>%
      arrange(player_id, season) %>%
      group_by(player_id) %>%
      dplyr::slice_tail() %>%
      mutate(
        player = ifelse(player == "Orlando Brown Jr.", "Orlando Brown", player),
        player = case_when(
          player == "Connor McGovern" & team_abbr == "DAL" ~ "Connor McGovern DAL",
          player == "Connor McGovern" & team_abbr == "NYJ" ~ "Connor McGovern NYJ",
          # player == "Michael Onwenu" ~ "Mike Onwenu",
          TRUE ~ player
        )
      ) %>%
      ungroup() %>%
      select(player, player_id) %>%
      left_join(full_df, by = "player")
    
    ids %>% filter(is.na(player_id))
    ids %>% filter(is.na(team_abbr))
    ids %>% filter(is.na(value))
    
    
  # get ourlads data to join with PFF data
  # to do stuff with
  
  d <- ourlads_df %>%
    select(player, current_team, position_ourlads) %>%
    full_join(ids, by = c("player")) %>%
    left_join(
      pfr_picks,
      by = c("player" = "pfr_name")
    ) %>%
    select(-draft_pct) %>%
    mutate(
      position = ifelse(is.na(position), position_ourlads, position),
      position = case_when(
        position %in% c("LT", "RT") ~ "T",
        position %in% c("LG", "RG") ~ "G",
        TRUE ~ position
      )
    ) %>%
    left_join(draft_imputation, by = c("pfr_pick" = "pick", "position" = "position")) 
  
  # rookies / haven't played
  d %>% filter(is.na(value))
  
  df <- d %>%
    mutate(
      value = ifelse(is.na(value), draft_pct, value),
      season = ifelse(is.na(season), 2022, season)
      ) %>%
    group_by(player, player_id) %>%
    mutate(
      pos_last = dplyr::last(position_ourlads),
      pos_last = ifelse(is.na(pos_last), position_ourlads, pos_last),
      label = glue::glue("({pos_last}) {player}")
    ) %>%
    ungroup()
  
  df
  
  # the 12 rookies right now + aaron banks right now
    df %>% filter(is.na(player_id)) %>% select(player, current_team, position_ourlads, pfr_pick, draft_pct, value)

    df %>% filter(current_team == "SEA")
    
  # norm to 0 to 100 among players who played since 2021
    table_df <- df %>%
      filter(season >= 2020) %>%
      group_by(player) %>%
      slice_tail() %>%
      arrange(position, -value) %>%
      group_by(position) %>%
      mutate(
        rank = 1 : n(),
        pct_normed = 100 * (1 + max(rank) - rank) / max(rank)
      ) %>%
      filter(!is.na(current_team)) %>%
      ungroup()
    
    
# ##############################################
    # do the model
    # made in 6b predictions
    
    library(gt)
    library(mgcv)
    
    m_T <- readRDS("data/m_T_weekly.rds")
    m_GC <- readRDS("data/m_GC_weekly.rds")

    tab <- table_df %>%
      mutate(
        position = case_when(
          position_ourlads %in% c("LT", "RT") ~ "T",
          TRUE ~ "GC"
        )
      ) %>%
      group_by(current_team, position) %>%
      summarise(pct = mean(pct_normed)) %>%
      ungroup() %>%
      pivot_wider(names_from = position, values_from = pct)
    
    t <- predict.gam(m_T, tab) %>% as_tibble_col(column_name = "value_t")
    gc <- predict.gam(m_GC, tab) %>% as_tibble_col(column_name = "value_gc")

    passing_mean <- 0.06580259
    
    t <- tab %>%
      bind_cols(t) %>%
      bind_cols(gc) %>%
      mutate(
        pred = value_t + value_gc,
        pred = pred - mean(pred) + passing_mean
      ) %>%
      arrange(-pred) %>%
      mutate(rank = 1 : n()) %>%
      select(rank, team_abbr = current_team, T, GC, pred) %>%
      mutate(pred = round(pred, 3))
    
    t  
    
    t1 <-
      t %>%
      dplyr::slice(1:16) 
    t2 <-
      t %>%
      dplyr::slice(17:32) 
    
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                 "#d9f0d3", "#7fbf7b", "#1b7837")
    
    # pal_hex2 <- c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0")
    pal_hex2 <- c("#f4a582", "#f7f7f7", "#92c5de")
    
    hulk_pal <- function(x){
      scales::col_numeric(
        pal_hex,
        domain = range(t$pred)
      )(x)
    }
    
    hulk_pal2 <- function(x){
      scales::col_numeric(
        pal_hex2,
        domain = range(t$T)
      )(x)
    }
    
    hulk_pal3 <- function(x){
      scales::col_numeric(
        pal_hex2,
        domain = range(t$GC)
      )(x)
    }
    
    
    table <- bind_cols(t1, t2) %>%
      gt::gt()  %>% 
      cols_label(
        rank...1 = "", rank...6 = "", team_abbr...2 = "", team_abbr...7 = "", 
        pred...5 = "pEPA", pred...10 = "pEPA",
        T...3 = "T", T...8 = "T", 
        GC...4 = "GC", GC...9= "GC"
      ) %>% 
      text_transform(
        locations = cells_body(c(team_abbr...2, team_abbr...7)),
        fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
      ) %>%
      gt::data_color(columns = c(pred...5, pred...10), colors = hulk_pal) %>%
      gt::data_color(columns = c(T...3, T...8), colors = hulk_pal2) %>%
      gt::data_color(columns = c(GC...4, GC...9), colors = hulk_pal3) %>%
      gtExtras::gt_theme_espn() %>%
      gt::tab_header(title = paste("2022 Pass Protection Expectations")) %>%
      gt::tab_source_note(gt::md('**Notes**: Based on current 5 projected starters via Ourlads | @benbbaldwin <br>Position columns represent average pctile ranking based on past play and draft position.<br>PEPA represents predicted EPA based on pass protection grades at each position.')) %>%
      tab_options(
        #table.font.size = px(14L),
        data_row.padding = px(2)
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = c(1, 5, 6, 10)
        )
      ) %>%
      fmt_number(
        columns = c(3:4, 8:9),
        decimals = 0
      ) %>%
      tab_style(
        style = list(
          cell_borders(
            side = c("left", "right"), 
            color = "black",
            weight = px(2)
          )
        ),
        locations = cells_body(
          columns = c(pred...5, pred...10)
        )
      ) 
    
    table  
    
    table %>%
      gtsave("img/pass_pro_projections.png")
    
  
    
# # same thing but with each player
    
    new_t <- table_df %>%
      select(position_ourlads, current_team, pct = pct_normed) %>%
      pivot_wider(names_from = "position_ourlads", values_from = "pct") %>%
      left_join(
        t %>% select(rank, team_abbr, pred),
        by = c("current_team" = "team_abbr")
      ) %>%
      select(rank, team_abbr = current_team, LT, LG, C, RG, RT, pred) %>%
      arrange(rank)
    
    
    new_t  
    
    t1 <-
      new_t %>%
      dplyr::slice(1:16) 
    t2 <-
      new_t %>%
      dplyr::slice(17:32) 
    
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                 "#d9f0d3", "#7fbf7b", "#1b7837")
    
    pal_hex2 <- c("#f4a582", "#f7f7f7", "#92c5de")
    
    hulk_pal <- function(x){
      scales::col_numeric(
        pal_hex,
        domain = range(t$pred)
      )(x)
    }
    
    pal_LT <- function(x){
      scales::col_numeric(
        pal_hex2,
        domain = range(new_t$LT)
      )(x)
    }
    
    pal_LG <- function(x){
      scales::col_numeric(
        pal_hex2,
        domain = range(new_t$LG)
      )(x)
    }
    
    pal_C <- function(x){
      scales::col_numeric(
        pal_hex2,
        domain = range(new_t$C)
      )(x)
    }
    
    pal_RG <- function(x){
      scales::col_numeric(
        pal_hex2,
        domain = range(new_t$RG)
      )(x)
    }
    
    pal_RT <- function(x){
      scales::col_numeric(
        pal_hex2,
        domain = range(new_t$RT)
      )(x)
    }
    
    table <- bind_cols(t1, t2) %>%
      gt::gt()  %>% 
      cols_label(
        rank...1 = "", rank...9 = "", team_abbr...2 = "", team_abbr...10 = "", pred...8 = "pEPA", pred...16 = "pEPA",
        LT...3 = "LT", LT...11 = "LT", LG...4 = "LG", LG...12= "LG", C...5 = "C", C...13 ="C",
        RG...6 = "RG", RG...14 = "RG", RT...7 = "RT", RT...15 = "RT"
      ) %>% 
      text_transform(
        locations = cells_body(c(team_abbr...2, team_abbr...10)),
        fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
      ) %>%
      gt::data_color(columns = c(pred...8, pred...16), colors = hulk_pal) %>%
      
      gt::data_color(columns = c(LT...3, LT...11), colors = pal_LT) %>%
      gt::data_color(columns = c(LG...4, LG...12), colors = pal_LG) %>%
      gt::data_color(columns = c(C...5, C...13), colors = pal_C) %>%
      gt::data_color(columns = c(RG...6, RG...14), colors = pal_RG) %>%
      gt::data_color(columns = c(RT...7, RT...15), colors = pal_RT) %>%
      gtExtras::gt_theme_espn() %>%
      gt::tab_header(title = paste("2022 Pass Protection Expectations")) %>%
      gt::tab_source_note(gt::md('**Notes**: Based on current 5 projected starters via Ourlads.<br>Position columns represent average pctile ranking based on past play or draft position.<br>PEPA represents predicted EPA based on pass protection grades at each position.')) %>%
      tab_options(
        #table.font.size = px(14L),
        data_row.padding = px(2)
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = c(1, 9, 8, 16)
        )
      ) %>%
      fmt_number(
        columns = c(3:7, 11:15),
        decimals = 0
      ) %>%
      tab_style(
        style = list(
          cell_borders(
            side = c("left", "right"), 
            color = "black",
            weight = px(2)
          )
        ),
        locations = cells_body(
          columns = c(pred...8, pred...16)
        )
      ) 
    
    table  
    
    table %>%
      gtsave("img/pass_pro_projections_allpos.png")
    
    
    
# ##############################################
# worst starters
    
    table <- table_df %>%
      arrange(- pct_normed) %>%
      mutate(rank = 1 : n()) %>%
      filter(rank > 140) %>%
      select(rank, player, team = current_team, position = position_ourlads, pct_normed) %>%
      gt() %>%
      cols_label(player = "", team = "", position = "Pos", pct_normed = "Pctile") %>% 
      text_transform(
        locations = cells_body(c(team)),
        fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
      ) %>%
      gtExtras::gt_theme_espn() %>%
      gtExtras::gt_hulk_col_numeric(pct_normed) %>%
      fmt_number(
        columns = c(5),
        decimals = 0
      ) %>%
      tab_options(
        #table.font.size = px(14L),
        data_row.padding = px(2)
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = c(2)
        )
      ) %>%
      gt::tab_header(
        title = paste("Bottom 20 projected starters"),
        subtitle = "Rank amoung 160 projected starters in expected pass protection grade"
        )
    
    table %>%
      gtsave("img/pass_pro_worst.png")
    
    
    # ##############################################
    # best starters
    
    table <- table_df %>%
      arrange(- pct_normed) %>%
      mutate(rank = 1 : n()) %>%
      filter(rank <= 20) %>%
      select(rank, player, team = current_team, position = position_ourlads, pct_normed) %>%
      gt() %>%
      cols_label(player = "", team = "", position = "Pos", pct_normed = "Pctile") %>% 
      text_transform(
        locations = cells_body(c(team)),
        fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
      ) %>%
      gtExtras::gt_theme_espn() %>%
      gtExtras::gt_hulk_col_numeric(pct_normed) %>%
      fmt_number(
        columns = c(5),
        decimals = 0
      ) %>%
      tab_options(
        #table.font.size = px(14L),
        data_row.padding = px(2)
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = c(2)
        )
      ) %>%
      gt::tab_header(
        title = paste("Top 20 projected starters"),
        subtitle = "Rank amoung 160 projected starters in expected pass protection grade"
      )
    
    table
    
    table %>%
      gtsave("img/pass_pro_best.png")
    
# # #################################################
    # contracts
    
    contracts <- nflreadr::load_contracts() %>%
      filter(is_active, position %in% c("LG", "RG", "C", "RT", "LT")) %>%
      mutate(apy = apy / 1000000) %>% 
      arrange(-apy) %>%
      select(player, team, apy) %>%
      distinct() %>%
      left_join(
        nflreadr::load_teams() %>% filter(!team_abbr %in% c("LAR", "STL", "OAK", "SD")) %>% select(team_abbr, team_nick),
        by = c("team" = "team_nick")
      )
    
    contracts %>%
      group_by(team) %>%
      summarise(cap = sum(apy)) %>%
      ungroup() %>%
      arrange(-cap)  %>%
      select(team_abbr, cap, team_color, team_color2) %>%
      left_join(t, by = "team_abbr") %>%
      filter(!is.na(team_abbr)) %>%
      ggplot(aes(cap, pred)) + 
      geom_smooth(method = "lm", se = F, color = "black") +
      nflplotR::geom_nfl_logos(aes(team_abbr = team_abbr), width = 0.06) +
      ggthemes::theme_fivethirtyeight() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(.01, .01)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      labs(
        title = "You Get What You Pay For",
        subtitle = "Pass protection projection versus APY",
        y = "Projected Pass Protection Metric",
        x = "Total APY of OL Spending (millions of dollars)",
        caption = "Data: PFF and OTC | @benbbaldwin"
      ) +
      theme(
        strip.text = element_text(size = 16, face = "bold"),
        panel.background = element_rect(color = "black", linetype = "solid"),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size=10, face="bold"),
        axis.title.y = element_text(size=10, face="bold")
      )
    
    
    
# # #################################################
# plot
    
    df
    
    q_t <- df %>% 
      filter(position == "T", week > 0) %>%
      pull(value) %>%
      quantile(probs = c(.2, .5, .8)) %>%
      as.numeric()
    
    q_c <- df %>% 
      filter(position == "C", week > 0) %>%
      pull(value) %>%
      quantile(probs = c(.2, .5, .8)) %>%
      as.numeric()
    
    q_g <- df %>% 
      filter(position == "G", week > 0) %>%
      pull(value) %>%
      quantile(probs = c(.2, .5, .8)) %>%
      as.numeric()
    
    make_player <- function(team, pos) {
      
      if (pos %in% c("LT", "RT")) {
        q <- q_t
      } else if (pos %in% c("RG", "LG")) {
        q <- q_g
      } else {
        q <- q_c
      }
      
      ann_text <- data.frame(week = 5, value = (q[3]+2), lab = "Top 20%",
                             season = factor(2019,levels = c("2019","2020","2021")))
      
      ann_text2 <- data.frame(week = 6, value = (q[1]+2), lab = "Bottom 20%",
                             season = factor(2019,levels = c("2019","2020","2021")))
      
      step1 <- df %>%
        filter(position_ourlads == pos, current_team == team, season > 2018) 
      
      decayed <- step1 %>%
        bind_rows(
          tibble::tibble(season = 2019:2021, week = 0, value = NA_real_)
        ) %>%
        filter(season != 2022)
      
      decayed %>%
        ggplot(aes(week, value)) +
        geom_hline(yintercept = c(0, 50, 100)) +
        geom_hline(yintercept = c(q[1], q[3]), linetype = "dashed") +
        geom_line(color = "red", size = 2) +
        # geom_smooth(se = F, color = "red", span = 10) +
        geom_point(aes(week, pct), 
                   color = ifelse(decayed$team_abbr %in% c("SEA"), decayed$team_color2, decayed$team_color),
                   size = 2
        ) + 
        facet_wrap(~season, nrow = 1) +
        ggthemes::theme_fivethirtyeight() +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(.01, .01)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
        labs(
          title = glue::glue("{step1$label[1]}"),
          y = "Percentile of week's pass block grade",
          x = "Week"
        ) +
        theme(
          panel.spacing = unit(0, "lines"),
          strip.text = element_text(size = 16, face = "bold"),
          panel.background = element_rect(color = "black", linetype = "solid"),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        geom_text(data = ann_text,label = "Top 20%") +
        geom_text(data = ann_text2,label = "Bottom 20%")
      
    
    }
    
    # make the plot
    
    make_plot <- function(tm) {
      
      lt <- make_player(tm, "LT")
      lg <- make_player(tm, "LG")
      c <-  make_player(tm, "C")
      rg <- make_player(tm, "RG")
      rt <- make_player(tm, "RT")
      
      
      name <- nflreadr::load_teams() %>% filter(team_abbr == tm) %>% pull(team_name)
      
      (lg + c + rg) /
        (lt + plot_spacer() + rt) +
        plot_annotation(
          title = glue::glue('{name} Pass Protection'),
          subtitle = '5 starters from Ourlads | Vertical axis is percentile of pass block grade',
          caption = 'Data: Ourlads and PFF | @benbbaldwin'
        ) &
        ggthemes::theme_fivethirtyeight() & 
        theme(
          panel.spacing = unit(0, "lines"),
          strip.text = element_text(size = 16, face = "bold"),
          panel.background = element_rect(color = "black", linetype = "solid"),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      
    }
    
    tm <- "DEN"
    
    p <- make_plot(tm)
    p
    
    logo <- magick::image_read(nflreadr::load_teams() %>% filter(team_abbr == tm) %>% pull(team_logo_espn))
    png(glue::glue('img/pbgrades_{tm}.png'), height = 9, width = 16, units = 'in', res = 600)
    p
    grid::grid.raster(logo, x = .5, y = .05, just = c('center', 'bottom'), width = unit(3.25, 'inches'))
    dev.off()
    
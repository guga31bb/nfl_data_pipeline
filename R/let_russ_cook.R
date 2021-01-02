library(tidyverse)
library(ggimage)
library(ggtext)
library(ggpmisc)
library(patchwork)
library(ggpubr)
data_path <- "~/github/"

# heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 6, direction = -1))(6)

ids <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c('LAR', 'OAK', 'SD', 'STL'))

images <- map(ids %>% pull(team_logo_espn), magick::image_read)
names(images) <- ids%>% pull(team_abbr)

qbr <- espnscrapeR::get_nfl_qbr("2020") %>%
  mutate(
    name = glue::glue('{substr(first_name, 1, 1)}.{last_name}'),
    name = as.character(name),
    name = dplyr::case_when(
      name == "D.Haskins Jr." ~ "D.Haskins",
      TRUE ~ name
    )
  ) %>%
  group_by(name) %>%
  mutate(tot_n = sum(qb_plays),
         team = if_else(team == "LAR", "LA", team)) %>%
  group_by(team) %>%
  arrange(-tot_n) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(name, first_name, team, qbr_total, headshot_href)

head <- map(qbr %>% pull(headshot_href),  magick::image_read)
names(head) <- qbr %>% pull(team)

tictoc::tic('loading data')
con <- DBI::dbConnect(RSQLite::SQLite(), glue::glue('{data_path}/master_db'))
data <- tbl(con, "cleaned_pbp") %>%
  filter(down == 1 | down == 2, wp >= .2 & wp <= .8) %>%
  filter(half_seconds_remaining > 120 & !is.na(posteam) & !is.na(epa) & season == 2020) %>%
  select(posteam, defteam, rush, pass, week) %>%
  collect()
tictoc::toc()
DBI::dbDisconnect(con)

get.poly <- function(a,b,r1=0.5,r2=1.0) {
  th.start <- pi*(1-a/100)
  th.end   <- pi*(1-b/100)
  th       <- seq(th.start,th.end,length=100)
  x        <- c(r1*cos(th),rev(r2*cos(th)))
  y        <- c(r1*sin(th),rev(r2*sin(th)))
  return(data.frame(x,y))
}

get.point <- function(a, b = 1) {
  th <- pi*(1-a/100)
  x        <- c(rev(b * cos(th)))
  y        <- c(rev(b * sin(th)))
  return(data.frame(x,y))
}


fires <- get.point(c(85, 97), 1.1) %>%
  as_tibble() %>%
  mutate(
    url = "fire.png"
  )

schotty_head <- tibble::tibble(
  # "image" =  "schotty.png"
  "image" = "img/schotty_chair.jpg"
)

breaks=c(0, 100 * (1/6), 100 * (2/6), 100 * (3/6), 100 * (4/6), 100 * (5/6), 100 * (6/6))


get_figure <- function(data, figure_team = "SEA") {
  
  sea_data <- data %>%
    filter(posteam == figure_team)
  
  sea_weeks <- unique(sea_data$week)
  
  max_week <- max(sea_data$week)
  min_week <- min(sea_data$week)
  
  # only draw schotty for SEA
  # only draw schotty for multiple weeks combined
  if (figure_team != "SEA" | min_week == max_week) {
    schotty_size <- .001
  } else {
    schotty_size <- 0.50
  }
  
  # restrict to weeks SEA played
  if (min_week == max_week) {
    data <- data %>%
      filter(week %in% sea_weeks)
  }

  
  teams <- data %>%
    group_by(posteam) %>%
    summarize(p = mean(pass)) %>%
    ungroup() 
  max <- max(teams$p)
  min <- min(teams$p)
  mean <- mean(teams$p)
  
  teams <- teams %>%
    mutate(
      pct = p - min,
      pct = 100 * pct / max(pct)
    )
  
  pos <- teams %>%
    filter(posteam == figure_team) %>%
    pull(pct)
  
  
  logos <- map_df(
    1 : length(unique(data$posteam)), function(x) {
      tm <- teams %>% dplyr::slice(x) %>% pull(posteam)
      tibble::tibble(
        "team" = teams %>% filter(posteam == tm) %>% pull(posteam),
        "pct" = teams %>% filter(posteam == tm) %>% pull(pct),
        "x" = teams %>% filter(posteam == tm) %>% pull(pct) %>% get.point() %>% pull(x),
        "y" = teams %>% filter(posteam == tm) %>% pull(pct) %>% get.point() %>% pull(y),
        "width" = 0.05,
        "grob" =  list(grid::rasterGrob(images[[tm]]))
      )
    }
  ) %>%
    filter(team != figure_team)
  
  rw_head <- tibble::tibble(
    "x" = teams %>% filter(posteam == figure_team) %>% pull(pct) %>% get.point(1.1) %>% pull(x),
    "y" = teams %>% filter(posteam == figure_team) %>% pull(pct) %>% get.point(1.1) %>% pull(y),
    "image" =  qbr %>% filter(team == figure_team) %>% pull(headshot_href)
  )
  
  name <- dplyr::if_else(
    figure_team == "SEA", "Russ",
    qbr %>% filter(team == figure_team) %>% pull(first_name)
  )
  
  if (max_week == min_week) {
    my_title <- glue::glue("W{min_week}: {dplyr::first(sea_data$defteam)}")
    title_size <- 12
  } else {
    my_title <- glue::glue("Is {name} <span style='color:red'>**cooking**</span>? 2020 season")
    title_size <- 24
  }
  
  p <- ggplot()+ 
    geom_image(data = schotty_head, aes(x = 0, y = .25, image = image),
               size = schotty_size, asp = 16/9) +
    geom_image(data = fires, aes(x = x, y = y, image = url),
               size = 0.10, asp = 16/9) +
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#FFFFCC")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#FFEDA0")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#FED976")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#FEB24C")+
    geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#FD8D3C")+
    geom_polygon(data=get.poly(breaks[6],breaks[7]),aes(x,y),fill="#FC4E2A")+
    geom_polygon(data=get.poly(pos-2,pos+2,0.00), aes(x,y), fill = "red")+
    geom_polygon(data=get.poly(pos-0.25,pos+0.25,0.00), aes(x,y), fill = "black")+
    # geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
    #           aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(round(breaks, 0),"%")))+
    # annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(
      plot.title = element_markdown(size = title_size, hjust = 0.5),
      axis.text=element_blank(),
      axis.title=element_blank(),
      axis.ticks=element_blank(),
      panel.grid=element_blank(),
      panel.border=element_blank()) +
    geom_grob(data = logos,
              aes(x, y, label = grob, vp.width = width)) +
    geom_image(data = rw_head, aes(x = x, y = y, image = image),
               size = 0.10, asp = 16/9) +
    labs(
      # caption = "Early-down pass percentage, win probability 20-80%, weeks 1-2\nBen Baldwin | @benbbaldwin",
         title = my_title
         )  + 
    theme(plot.margin = unit(c(0,0,0,0), "pt"))
  
  return(p)
  
}


build_plot <- function(tm = "SEA") {
  # need to account for bye
  total_weeks <- unique(data %>% filter(posteam == tm) %>% pull(week))
  
  overall <- get_figure(data, tm) + theme(plot.margin = unit(c(5,0,0,0), "pt"))
  
  weekly <- map(total_weeks, function(x) {
    get_figure(data %>% filter(week == x), tm)
  })
  
  cols <- length(total_weeks) %/% 2 + 1
  
  bottom <- ggarrange(
    plotlist = weekly,
    heights = 1, ncol = cols, nrow = 2)

  p <- ggarrange(overall, bottom, heights = c(2, 0.7),
            ncol = 1, nrow = 2)
  
  ggsave(glue::glue('img/lrc_{tm}.png'), plot = p, dpi=800)
  
  return(p)
}

p <- build_plot("SEA")

p





## Load packages ====
library(tidyverse)
library(rvest)

## Set up url ====
tm_url <- "https://www.transfermarkt.com/laliga/torschuetzenliste/wettbewerb/ES1/saison_id/2019/altersklasse/alle/detailpos//plus/1"

## Harvest data and extract 'items' table ====
goalscorers <- read_html(tm_url) %>% 
  html_nodes(css = ".items") %>% 
  html_table(fill = T) %>% 
  magrittr::extract2(1) %>% 
  as_tibble(.name_repair = "unique") 
View(goalscorers)

## Perform cleaning ====
goalscorers_tidy <- goalscorers %>% 
  select_if(~!is.logical(.x)) %>% 
  rename("ranking" = 1, "player_pos" = 2, "player" = 3, "position" = 4, 
         "age" = 5, "games" = 6, "assists" = 7, "penalties" = 8, 
         "minutes" = 9, "minutes_per_goal" = 10, "goals_per_match" = 11, 
         "goals" = 12) %>% 
  select(-player_pos) %>% 
  drop_na(ranking) %>% 
  mutate_at("age", str_sub, end = 2) %>% 
  mutate_at("goals_per_match", parse_double, locale = locale(decimal_mark = ",")) %>% 
  mutate_at(vars(starts_with("minutes")), str_remove_all, "[:punct:]") %>% 
  mutate_at(vars(matches("^a|minutes")), as.numeric)
View(goalscorers_tidy)


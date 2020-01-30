## Load packages ====
library(tidyverse)
library(rvest)

## Webpage with goals and assists numbers ====
tm_url <- "https://www.transfermarkt.co.in/laliga/scorerliste/wettbewerb/ES1/saison_id/2019/altersklasse/alle/plus/1/page/"

## Build url for each page ====
tm_urls <-map_chr(seq(1:11), ~str_c(tm_url, .x))

## Build function to harvest table ====
rvest_tm_scorers <- function(tm_url) {
  read_html(tm_url) %>% 
    html_nodes(css = ".items") %>% 
    html_table(fill = T) %>% 
    magrittr::extract2(1) %>% 
    as_tibble(.name_repair = "unique") %>% 
    select_if(~!is.logical(.x)) %>% 
    select_if(~!any(str_detect(names(table(.x)), "for 2 clubs"))) %>% 
    rename("ranking" = 1, "player_pos" = 2, "player" = 3, "position" = 4, 
           "age" = 5, "games" = 6, "sub_in" = 7, "sub_off" = 8, 
           "goals" = 9, "assists" = 10, "points" = 11) %>% 
    select(-player_pos) %>% 
    drop_na(ranking) %>% 
    return()
}

## Apply function to each page ====
scorers <- map_df(tm_urls, rvest_tm_scorers)

## Get teams data ====
team_names <- c("atletico-madrid", "athletic-bilbao", "ca-osasuna", 
                "cd-leganes", "celta-vigo", "deportivo-alaves", 
                "espanyol-barcelona", "fc-barcelona", "fc-getafe", 
                "fc-granada", "fc-sevilla", "fc-valencia", 
                "fc-villarreal", "rcd-mallorca", "real-betis-sevilla", 
                "real-madrid", "real-sociedad-san-sebastian", 
                "real-valladolid", "sd-eibar", "ud-levante")
names(team_names) <- team_names
team_ids <- c(13, 621, 331, 1244, 940, 1108, 714, 131, 3709, 16795, 368, 
              1049, 1050, 237, 150, 418, 681, 366, 1533, 3368)

rvest_tm_squads <- function(name, id) {
  str_c("https://www.transfermarkt.co.in/", name, "/kader/verein/", id) %>% 
    read_html() %>% 
    html_nodes(css = ".items") %>% 
    html_table(fill = T) %>% 
    magrittr::extract2(1) %>% 
    as_tibble(.name_repair = "unique") %>% 
    select_if(~!is.logical(.x)) %>% 
    rename("number" = 1, "player" = 2, "player_name" = 3, "position" = 4, 
           "birth_date" = 5, "contract_expires" = 6, "market_value" = 7) %>% 
    drop_na(position) %>% 
    mutate_at("player_name", str_extract, ".+[:lower:](?=[:upper:])") %>% 
    mutate_at("number", as.numeric) %>% 
    return()
}
squads <- map2_dfr(team_names, team_ids, rvest_tm_squads, .id = "team")

## Compute proportions by team ====
scorers %>% 
  left_join(select(squads, player_name, team), c("player" = "player_name")) %>% 
  mutate(team = if_else(player == "Karl Toko Ekambi", "fc-villarreal", team)) %>% 
  group_by(team) %>% 
  add_tally(goals, name = "team_goals") %>% 
  add_tally(assists, name = "team_assists") %>% 
  mutate(goal_prop = goals / team_goals, 
         assist_prop = assists / team_assists) %>% 
  drop_na(team) %>% 
  filter(goal_prop > .25 | assist_prop > .15) %>% 
  ggplot() + 
  aes(goal_prop, assist_prop, label = player) + 
  geom_point() + 
  geom_text() + 
  geom_hline(yintercept = .15) + 
  geom_vline(xintercept = .25) + 
  theme_minimal()



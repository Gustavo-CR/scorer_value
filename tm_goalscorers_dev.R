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

## Manual adjustments when needed ====
mutate(team_goals2 = case_when(
  team == "cd-leganes" ~ 18, 
  team == "athletic-bilbao" ~ 23, 
  team == "celta-vigo" ~ 19, 
  team == "fc-barcelona" ~ 55, 
  team == "fc-getafe" ~ 35, 
  team == "fc-granada" ~ 27, 
  team == "fc-sevilla" ~ 29, 
  team == "fc-valencia" ~ 33, 
  team == "fc-villarreal" ~ 40, 
  team == "rcd-mallorca" ~ 22, 
  team == "real-madrid" ~ 44, 
  team == "real-sociedad-san-sebastian" ~ 39, 
  team == "real-valladolid" ~ 19
))

## Plot version 1 ====
ggplot(scorers_prop) + 
  aes(goal_prop, assist_prop, label = player, color = position2) + 
  geom_hline(yintercept = 16, color = lines, alpha = .1, size = 1.5) + 
  geom_vline(xintercept = 22, color = lines, alpha = .1, size = 1.5) + 
  annotate("text", x = 40, y = 31, 
           family = "SegoeUI", fontface = "bold.italic", color = text, 
           label = "Alta participación \nen goles y asistencias") + 
  geom_point(alpha = .5) + 
  geom_text_repel(data = filter(scorers_prop, goal_prop > 24 | assist_prop > 20), 
                  size = 3.8, fontface = "bold.italic", family = "SegoeUI") + 
  scale_color_manual(values = c(defenders, strikers, midfielders, goalkeepers)) + 
  labs(title = title1, subtitle = subtitle1,  
       x = "Porcentaje de goles", y = "Porcentaje de asistencias", 
       caption = "Fuente: TransferMarkt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        plot.background = element_rect(fill = background), 
        text = element_text(family = "SegoeUI", color = text), 
        title = element_text(face = "bold", size = 18), 
        axis.text = element_text(color = text), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12, color = legend), 
        legend.spacing.x = unit(1, "cm"), 
        legend.position = "bottom")



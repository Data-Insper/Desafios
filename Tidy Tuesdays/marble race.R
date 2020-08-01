library(skimr)
library(tidyverse)
library(janitor)
library(ggthemes)
library(scales)

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

marbles <- marbles %>%
  separate(race, c("season", "race"), sep = 2) %>% 
  separate(race, c("stage", "race"), sep = 1) %>% 
  mutate(season = as.numeric(str_remove(season, "S")),
         stage = ifelse(stage == "Q", "quali", "race"),
         race = as.numeric(race))

skim(marbles)

quali_rnk <- marbles %>% 
  filter(!is.na(pole)) %>% 
  mutate(pole = as.numeric(str_replace(pole, "P", ""))) %>% 
  group_by(marble_name) %>% 
  summarise(average = mean(pole),
            std_dev = sd(pole)) %>% 
  arrange(average, std_dev)

victory <- marbles %>% 
  filter(!is.na(pole)) %>% 
  mutate(pole = as.numeric(str_replace(pole, "P", "")),
         pole = ifelse(pole != 1, NA, 1)) %>% 
  filter(!is.na(pole)) %>% 
  group_by(marble_name) %>% 
  summarise(count = sum(pole)) %>% 
  arrange(desc(count))
  
race_rnk <- marbles %>% 
  filter(stage == "race") %>% 
  group_by(race) %>% 
  arrange(time_s) %>% 
  mutate(position = seq_along(time_s)) %>%
  group_by(marble_name) %>% 
  filter(!is.na(position)) %>% 
  summarise(average = mean(position),
            std_dev = sd(position)) %>% 
  arrange(average, std_dev)


pos_gain <- data.frame(quali_rnk %>% arrange(marble_name) %>% select(marble_name),
                       quali_rnk %>% arrange(marble_name) %>% select(average) - race_rnk %>% arrange(marble_name) %>% select(average))
pos_gain <- pos_gain %>% 
              arrange(desc(average))
colnames(pos_gain) <- c("Marble", "Average gain")
pos_gain <- tibble(pos_gain)
pos_gain

### Race Results
# number <- as.character(readline(prompt = "Race: "))
# marbles %>%
#   filter(str_detect(race, "R"),
#          str_detect(race, number))%>%
#   group_by(race) %>%
#   arrange(time_s) %>%
#   mutate(position = seq_along(time_s))

standings <- marbles %>% 
                filter(stage == "race") %>% 
                group_by(marble_name) %>% 
                summarise(points = sum(points)) %>% 
                arrange(desc(points))

standings_until <- function(data, final_race, championship){
  if(championship == "marble"){
    data %>% 
      filter(stage == "race",
             race <= final_race) %>% 
      group_by(marble_name) %>% 
      summarise(points = sum(points)) %>% 
      arrange(desc(points))
  }
  else if (championship == "constructor"){
    data %>% 
      filter(stage == "race",
             race <= final_race) %>% 
      group_by(team_name) %>% 
      summarise(points = sum(points)) %>% 
      arrange(desc(points))
  }
  else {
    cat("Championship Invalid")
  }
  }

standings_until(marbles, 8, "marble")
                
marbles %>% 
  filter(stage == "race") %>% 
  group_by(race) %>% 
  arrange(time_s) %>% 
  mutate(position = seq_along(time_s)) %>% 
  group_by(host) %>% 
  summarise(average = mean(position),
            std_dev = sd(position)) %>% 
  arrange(average, std_dev)
  
points <- marbles %>% 
  filter(stage == "race") %>% 
  group_by(marble_name) %>% 
  select(marble_name, race, points)

marbles %>% 
  filter(stage == "race",
         !is.na(avg_time_lap)) %>% 
  group_by(race) %>% 
  mutate(gen_avg_time = mean(avg_time_lap)) %>% 
  group_by(marble_name) %>% 
  mutate(compared_speed = avg_time_lap / gen_avg_time) %>% 
  ungroup() %>%
  mutate(marble_name = fct_reorder(marble_name, -compared_speed)) %>%
  ggplot(aes(marble_name, compared_speed, fill = marble_name)) +
    geom_boxplot() +
    coord_flip() +
    theme_clean() +
    theme(legend.position = "none") +
    scale_y_reverse(labels = number_format(suffix = "x", accuracy = .01))
    
marbles %>% 
  filter(stage == "race",
         !is.na(avg_time_lap)) %>% 
  group_by(race) %>% 
  mutate(gen_avg_time = mean(avg_time_lap)) %>% 
  group_by(team_name) %>% 
  mutate(compared_speed = avg_time_lap / gen_avg_time) %>% 
  ungroup() %>%
  mutate(team_name = fct_reorder(team_name, compared_speed, .desc = TRUE)) %>%
  ggplot(aes(team_name, compared_speed, fill = team_name)) +
  geom_boxplot() +
  coord_flip() +
  theme_clean() +
  theme(legend.position = "none") +
  scale_y_reverse(labels = number_format(suffix = "x", accuracy = .01)) +
  labs(x = "",
       y = "Compared Speed",
       title = "Average track speed",
       subtitle = "compared with general average")

marbles %>% 
  filter(stage == "race") %>% 
  group_by(race) %>% 
  arrange(time_s) %>% 
  mutate(position = seq_along(time_s)) %>%
  filter(!is.na(position)) %>% 
  group_by(team_name) %>% 
  mutate(med = median(position)) %>% 
  ungroup() %>% 
  mutate(team_name = fct_reorder(team_name, med, .desc = TRUE)) %>% 
  ggplot(aes(team_name, position, fill = team_name)) +
    geom_boxplot() +
    coord_flip() +
    theme_clean() +
    theme(legend.position = "none") +
    scale_y_reverse() +
    labs(x = "", 
         y = "Position",
         title = "Position distribution by team")

marbles %>% 
  filter(!is.na(pole)) %>% 
  group_by(team_name) %>% 
  mutate(pole = as.numeric(str_replace(pole, "P", "")),
         med = median(pole)) %>%
  ungroup() %>% 
  mutate(team_name = fct_reorder(team_name, med, .desc = TRUE)) %>% 
  ggplot(aes(team_name, pole, fill = team_name)) +
    geom_boxplot() +
    coord_flip() +
    theme_clean() +
    theme(legend.position = "none") +
    scale_y_reverse() +
    labs(x = "", 
         y = "Position",
         title = "Qualifying position distribution by team")

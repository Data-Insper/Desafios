library(tidyverse)
library(scales)
library(ggthemes)
library(ggrepel)
library(gganimate)

theme_set(theme_bw())

standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv') %>% 
  mutate(week = as.numeric(week))
attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv') %>% 
  left_join(games %>% 
              select(year, week, home_team_name, home_team_city), by = c("year", "week",
                                                         "team" = "home_team_city"),
            keep = TRUE) %>% 
  mutate(is_home = !is.na(home_team_name)) %>% 
  select(-home_team_name)

### Home victory percent by team

games %>% 
  mutate(home_win = home_team == winner & is.na(tie)) %>% 
  left_join(attendance %>% 
            select(year, week, team_name, weekly_attendance, is_home), 
            by = c("year", "week", 
                   "home_team_name" = "team_name")) %>%
  filter(!is.na(weekly_attendance)) %>% 
  group_by(home_team) %>%
  summarise(avg_home_wins = percent(mean(home_win)),
            std_dev = percent(sd(home_win)),
            avg_attendance = mean(weekly_attendance)) %>% 
  arrange(desc(avg_home_wins), std_dev) %>%
  ggplot(aes(std_dev, avg_home_wins, color = home_team, size = avg_attendance)) +
    geom_point(alpha = 0.5) +
    scale_size_continuous() +
    geom_label_repel(aes(label = home_team), size = 3) +
    guides(color = FALSE) +
    labs(x = "Standard Deviation",
         y = "Wins (%)",
         title = "Home team advantage analysis",
         caption = "Data from 2000 - 2019",
         size = "Average Attendance")

### Away victory percent by team

games %>% 
  mutate(away_win = away_team == winner & is.na(tie)) %>% 
  left_join(attendance %>% 
              select(year, week, team_name, weekly_attendance, is_home), 
            by = c("year", "week", 
                   "home_team_name" = "team_name")) %>%
  filter(!is.na(weekly_attendance)) %>% 
  group_by(away_team) %>%
  summarise(avg_away_wins = percent(mean(away_win)),
            std_dev = percent(sd(away_win))) %>% 
  arrange(desc(avg_away_wins), std_dev) %>% 
  ggplot(aes(std_dev, avg_away_wins, color = away_team)) +
  geom_point(alpha = 0.5) +
  geom_label_repel(aes(label = away_team), size = 3, force = 2, max.iter = 10000) +
  guides(color = FALSE) +
  labs(x = "Standard Deviation",
       y = "Wins (%)",
       title = "Away team wins analysis",
       caption = "Data from 2000 - 2019")

### Effect of attendance on attack and defense rankings

standings %>% 
  left_join(attendance %>% 
              filter(!is.na(weekly_attendance),
                     is_home) %>% 
              group_by(team, year) %>% 
              summarise(weekly_attendance = mean(weekly_attendance)), by = c("team", "year")) %>%
  group_by(team_name) %>% 
  summarize(weekly_attendance = mean(weekly_attendance),
            offensive_ranking = mean(offensive_ranking),
            defensive_ranking = mean(defensive_ranking)) %>% 
  ggplot(aes(offensive_ranking, defensive_ranking, color = team_name, size = weekly_attendance)) +
    geom_point(alpha = 0.5) +
    scale_size_continuous(range = c(1, 8)) +
    geom_label_repel(aes(label = team_name), size = 3, force = 2, max.iter = 10000) +
    guides(color = FALSE) +
    labs(x = "Offensive Ranking",
         y = "Defensive Ranking",
         size = "Average Attendance",
         title = "Impact of attendance in offense and defense",
         caption = "Data from 2000 - 2019")

### Distribution of offense and defense rankings

standings %>% 
  mutate(team_name = fct_reorder(team_name, offensive_ranking)) %>% 
  ggplot(aes(team_name, offensive_ranking, fill = team_name)) +
    geom_boxplot() +
    theme(legend.position = "none") +
    coord_flip() +
    labs(x = "Team name",
         y = "Offensive ranking",
         title = "Offensive ranking distribution",
         subtitle = "from 2000 - 2019")

standings %>% 
  mutate(team_name = fct_reorder(team_name, defensive_ranking)) %>% 
  ggplot(aes(team_name, defensive_ranking, fill = team_name)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Team name",
       y = "Defensive ranking",
       title = "Defensive ranking distribution",
       subtitle = "from 2000 - 2019")

### Trying animations

(standings %>% 
  left_join(attendance %>% 
              filter(!is.na(weekly_attendance),
                     is_home) %>% 
              group_by(team, year) %>% 
              summarise(weekly_attendance = mean(weekly_attendance)), by = c("team", "year")) %>%
  group_by(team_name, year) %>% 
  summarize(weekly_attendance = mean(weekly_attendance),
            offensive_ranking = mean(offensive_ranking),
            defensive_ranking = mean(defensive_ranking)) %>% 
  ggplot(aes(offensive_ranking, defensive_ranking, color = team_name, size = weekly_attendance)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1, 8)) +
  geom_label_repel(aes(label = team_name), size = 3, force = 2, max.iter = 10000) +
  guides(color = FALSE) +
  labs(x = "Offensive Ranking",
       y = "Defensive Ranking",
       size = "Average Attendance",
       title = "Impact of attendance in offense and defense: {round(frame_time)}",
       caption = "Data from 2000 - 2019") +
  transition_time(year)) %>% 
animate(fps = 30, res = 100, end_pause = 5, 
        duration = 60, height = 700, width = 1500)

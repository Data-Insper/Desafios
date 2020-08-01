library(tidyverse)
library(scales)

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')


### Data cleaning & tidying


passwords <- passwords %>% 
  filter(!is.na(password)) %>% 
  select(-c(font_size, rank_alt)) %>% 
  mutate(value_online = ifelse(time_unit == "minutes", value * 60, 
                            ifelse(time_unit == "hours",
                               value * 60 * 60,
                               ifelse(time_unit == "days",
                                      value * 60 * 60 * 24,
                                      ifelse(time_unit == "months",
                                             value * 60 * 60 * 24 * 30,
                                             ifelse(time_unit == "years",
                                                    value * 60 * 60 * 24 * 30 * 12, value))))),
         value_offline = offline_crack_sec) %>% 
  select(-c(time_unit, value, offline_crack_sec)) %>% 
  pivot_longer(value_online:value_offline, names_to = "method",
               names_prefix = "value_", values_to = "time_s")


### Categories of password and their security (separated by online & offline hacking)


passwords %>%
  filter(method == "offline") %>% 
  group_by(category) %>% 
  summarise(mean_time = mean(time_s)) %>% 
  ungroup() %>% 
  mutate(category = fct_reorder(category, mean_time)) %>% 
  ggplot(aes(category, mean_time)) +
    geom_col(fill = "lightgrey",
             color = "black") +
    theme_clean() +
    labs(x = "Category",
         y = "Time (seconds)",
         title = "Offline") +
    theme(legend.position = "none") +
    coord_flip()

passwords %>% 
  filter(method == "online") %>% 
  group_by(category) %>% 
  summarise(mean_time = mean(time_s)) %>% 
  ungroup() %>% 
  mutate(category = fct_reorder(category, mean_time)) %>% 
  ggplot(aes(category, mean_time)) +
    geom_col(fill = "lightgrey",
             color = "black") +
    theme_clean() +
    labs(x = "Category",
         y = "Time (seconds)",
         title = "Online") +
    scale_y_continuous(labels = number_format()) +
    theme(legend.position = "none") +
    coord_flip()


### Separating simple passwords from complex ones and measuring time spent hacking them


passwords %>% 
  filter(method == "offline") %>% 
  mutate(complexity = ifelse(str_detect(password, "^[a-z][a-z]*[a-z]$") | str_detect(password, "^[0-9][0-9]*[0-9]$"), "simple", "complex")) %>%
  group_by(complexity) %>% 
  summarise(mean_time = mean(time_s)) %>% 
  ggplot(aes(complexity, mean_time)) +
    geom_col(fill = "lightgrey", color = "black") +
    theme_clean() +
    labs(x = "Complexity",
         y = "Mean Time (seconds)",
         title = "Offline")

passwords %>% 
  filter(method == "online") %>% 
  mutate(complexity = ifelse(str_detect(password, "^[a-z][a-z]*[a-z]$") | str_detect(password, "^[0-9][0-9]*[0-9]$"), "simple", "complex")) %>%
  group_by(complexity) %>% 
  summarise(mean_time = mean(time_s)) %>% 
  ggplot(aes(complexity, mean_time)) +
  geom_col(fill = "lightgrey", color = "black") +
  theme_clean() +
  labs(x = "Complexity",
       y = "Mean Time (seconds)",
       title = "Online")


### Measuring strength by size of the password


passwords %>% 
  filter(method == "online") %>%
  mutate(length = str_length(password),
         complexity = ifelse(str_detect(password, "^[a-z][a-z]*[a-z]$") | str_detect(password, "^[0-9][0-9]*[0-9]$"), "simple", "complex")) %>%
  ggplot(aes(length, strength)) +
    geom_point(aes(color = complexity)) +
    geom_smooth(se = FALSE, method = "lm", color = "black") +
    theme_clean() +
    labs(x = "Length",
         y = "Strength",
         title = "Regression of strength by length")

passwords %>% 
  filter(method == "online") %>%
  mutate(length = str_length(password),
         complexity = ifelse(str_detect(password, "^[a-z][a-z]*[a-z]$") | str_detect(password, "^[0-9][0-9]*[0-9]$"), "simple", "complex")) %>%
  lm(strength ~ length + complexity, data = .) %>% 
  summary

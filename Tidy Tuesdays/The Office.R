# install.packages("schrute")
# install.packages("tidyverse")
# install.packages("broom")

library(schrute)
library(tidyverse)
library(broom)

office <- schrute::theoffice
theme_set(theme_bw())

# Filtering directors with more than one appearance

directors <- office %>% 
  group_by(season, episode, director) %>% 
  summarise(imdb_rating = mean(imdb_rating)) %>% 
  ungroup() %>% 
  count(director) %>%
  arrange(desc(n)) %>% 
  filter(n > 1) # minimum number of appearances

# Tidying and cleaning the data

tidy_office <- office %>% 
  group_by(season, episode, character, director) %>% 
  count(character) %>% 
  filter(n > 5) %>% # Characters with 5 or more lines in the episode
  left_join(office %>% 
              group_by(season, episode) %>% 
              summarise(imdb_rating = mean(imdb_rating))) %>% 
  semi_join(directors, by = "director") %>% 
  ungroup() %>% 
  mutate(season = as_factor(season)) %>% 
  select(season, episode, director, character, n, imdb_rating) %>% 
  pivot_wider(names_from = character,
              values_from = n)

### Why is an episode good?

## All seasons

tidy_office %>% 
  lm(imdb_rating ~ Dwight + Michael + Jim +
       Pam + Andy + season + director,
     data = .) %>% 
  summary

# Tidy

tidy_office %>% 
  lm(imdb_rating ~ Dwight + Michael + Jim +
       Pam + Andy + season + director,
     data = .) %>% 
  tidy()

## By season

tidy_office %>% 
  group_split(season) %>% 
  map(~ lm(imdb_rating ~ Dwight + Pam + Jim, data = .x)) %>% 
  map(~ summary(.x))

# Tidy

tidy_office %>% 
  group_split(season) %>% 
  map(~ lm(imdb_rating ~ Dwight + Pam + Jim, data = .x)) %>% 
  # map(~ summary(.x)) %>% 
  map_dfr(tidy, .id = "season")

### Visualization

model <- tidy_office %>% 
  lm(imdb_rating ~ Dwight + Michael + Jim +
       Pam + Andy + season + director,
     data = .) %>% 
  tidy()

model %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  arrange(desc(abs(estimate))) %>% 
  # filter(abs(estimate) >= head(abs(estimate), 15)[15]) %>% # only consider the largest effects
  ggplot(aes(term, estimate, color = estimate > 0)) +
    geom_point() +
    geom_errorbar(aes(ymin = (estimate - 2 * std.error),
                      ymax = (estimate + 2 * std.error))) +
    scale_color_manual(values = c("red", "black")) +
    coord_flip() +
    labs(x = "",
         y = "Estimated impact",
         title = "Most important factors in the determinations of The Office's IMDB score",
         subtitle = "using data from all episodes",
         caption = "Error bar represents 95% confidence interval") +
    theme(legend.position = "none")

## Without the intercept

model %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  arrange(desc(abs(estimate))) %>% 
  # filter(abs(estimate) >= head(abs(estimate), 15)[15]) %>% # only consider the largest effects
  filter(term != "(Intercept)") %>% 
  ggplot(aes(term, estimate, color = estimate > 0)) +
  geom_point() +
  geom_errorbar(aes(ymin = (estimate - 2 * std.error),
                    ymax = (estimate + 2 * std.error))) +
  scale_color_manual(values = c("red", "black")) +
  coord_flip() +
  labs(x = "",
       y = "Estimated impact",
       title = "Most important factors in the determinations of The Office's IMDB score",
       subtitle = "using data from all episodes",
       caption = "Error bar represents 95% confidence interval") +
  theme(legend.position = "none")


### That's what she said

office %>% 
  mutate(joke = str_detect(str_to_lower(text), "that's what she said")) %>%
  filter(joke) %>% 
  view

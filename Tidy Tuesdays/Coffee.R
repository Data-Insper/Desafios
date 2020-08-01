library(tidyverse)
library(spData)
library(sf)
library(RColorBrewer)
library(ggthemes)

theme_set(theme_bw())

coffee <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

coffee %>% 
  group_by(species) %>% 
  summarise(avg_aroma = mean(aroma),
            avg_flavor = mean(flavor),
            avg_aftertaste = mean(aftertaste),
            avg_acidity = mean(acidity),
            avg_body = mean(body),
            avg_balance = mean(balance),
            avg_uniformity = mean(uniformity),
            avg_clean_cup = mean(clean_cup),
            avg_sweetness = mean(sweetness),
            avg_moisture = mean(moisture)) %>% 
  pivot_longer(-species, names_to = "measure", values_to = "avg_score", names_prefix = "avg_") %>% 
  ggplot(aes(species, avg_score, fill = measure)) +
    geom_col(color = "black", position = "stack") +
    scale_y_continuous(limits = c(0, 100))

coffee %>% 
  group_by(country_of_origin) %>% 
  summarise(avg_aroma = mean(aroma),
            avg_flavor = mean(flavor),
            avg_aftertaste = mean(aftertaste),
            avg_acidity = mean(acidity),
            avg_body = mean(body),
            avg_balance = mean(balance),
            avg_uniformity = mean(uniformity),
            avg_clean_cup = mean(clean_cup),
            avg_sweetness = mean(sweetness),
            avg_moisture = mean(moisture)) %>% 
  pivot_longer(-country_of_origin, names_to = "measure", 
               values_to = "avg_score", names_prefix = "avg_") %>% 
  filter(!is.na(country_of_origin)) %>% 
  mutate(country_of_origin = fct_reorder(country_of_origin, avg_score, sum)) %>% 
  ggplot(aes(country_of_origin, avg_score, fill = measure)) +
  geom_col(color = "black", position = "stack") +
  scale_y_continuous(limits = c(0, 100)) +
  coord_flip()

coffee %>% 
  group_by(country_of_origin) %>% 
  summarise(avg_aroma = mean(aroma),
            avg_flavor = mean(flavor),
            avg_aftertaste = mean(aftertaste),
            avg_acidity = mean(acidity),
            avg_body = mean(body),
            avg_balance = mean(balance),
            avg_uniformity = mean(uniformity),
            avg_clean_cup = mean(clean_cup),
            avg_sweetness = mean(sweetness),
            avg_moisture = mean(moisture)) %>% 
  pivot_longer(-country_of_origin, names_to = "measure", 
               values_to = "avg_score", names_prefix = "avg_") %>% 
  filter(!is.na(country_of_origin)) %>% 
  mutate(country_of_origin = fct_reorder(country_of_origin, avg_score, sum)) %>% 
  ggplot(aes(country_of_origin, avg_score, fill = measure)) +
  geom_col(color = "black", position = "fill") +
  coord_flip()

coffee %>% 
  group_by(country_of_origin) %>% 
  summarise(avg_flavor = mean(flavor),
            avg_balance = mean(balance)) %>%
  filter(!is.na(country_of_origin)) %>% 
  pivot_longer(-country_of_origin, names_to = "measure",
               values_to = "score", names_prefix = "avg_") %>% 
  mutate(country_of_origin = fct_reorder(country_of_origin, score, sum)) %>% 
  ggplot(aes(country_of_origin, score)) +
  geom_col(aes(fill = measure), color = "black", position = "stack") +
  coord_flip()

coffee %>% 
  filter(!is.na(country_of_origin)) %>% 
  mutate(country_of_origin = fct_reorder(country_of_origin, balance)) %>% 
  semi_join(valid_sample) %>% 
  ggplot(aes(country_of_origin, balance)) +
    geom_boxplot(aes(fill = country_of_origin), show.legend = FALSE) +
    coord_flip() +
    scale_y_continuous(limits = c(6, 9))

valid_sample <- coffee %>% 
  count(country_of_origin) %>%
  filter(n >= 10)

coffee %>% 
  filter(!is.na(country_of_origin)) %>% 
  mutate(country_of_origin = fct_reorder(country_of_origin, flavor)) %>% 
  semi_join(valid_sample) %>% 
  ggplot(aes(country_of_origin, flavor)) +
  geom_boxplot(aes(fill = country_of_origin), show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(limits = c(6, 9))

map <- coffee %>% 
  group_by(country_of_origin) %>% 
  summarise(avg_aroma = mean(aroma),
            avg_flavor = mean(flavor),
            avg_aftertaste = mean(aftertaste),
            avg_acidity = mean(acidity),
            avg_body = mean(body),
            avg_balance = mean(balance),
            avg_uniformity = mean(uniformity),
            avg_clean_cup = mean(clean_cup),
            avg_sweetness = mean(sweetness),
            avg_moisture = mean(moisture)) %>% 
  filter(!is.na(country_of_origin)) %>% 
  semi_join(valid_sample) %>% 
  right_join(x, by = c("country_of_origin" = "name_long"))

map %>%
  filter(is.na(avg_flavor)) %>% 
  ggplot() +
    geom_sf(aes(geometry = geom, fill = avg_flavor), data = map) +
    scale_fill_gradient(low = "red", high = "green", limits = c(7, 8.5)) +
    geom_sf(aes(geometry = geom), fill = "#fff7bc") +
    theme_map() +
    theme(panel.background = element_rect(fill = "lightblue"),
          legend.position = "right") +
    labs(title = "Average coffee flavor score by country",
         fill = "")

map %>%
  filter(is.na(avg_balance)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geom, fill = avg_balance), data = map) +
  scale_fill_gradient(low = "red", high = "green", limits = c(7, 8)) +
  geom_sf(aes(geometry = geom), fill = "#fff7bc") +
  theme_map() +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.position = "right") +
  labs(title = "Average coffee balance score by country",
       fill = "")

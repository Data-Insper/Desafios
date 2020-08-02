library(tidytuesdayR)
library(tidyverse)
library(scales)
library(ggrepel)
library(lubridate)
library(plotly)

theme_set(theme_bw())

tidytuesday <- tt_load("2018-10-23")

movie20 <- tidytuesday$movie_profit %>% 
  mutate(distributor = fct_lump(distributor, 20),
         release_date = mdy(release_date),
         return = worldwide_gross / production_budget)

movie5 <- tidytuesday$movie_profit %>% 
  mutate(distributor = fct_lump(distributor, 5),
         release_date = mdy(release_date),
         return = worldwide_gross / production_budget)

############### Análise exploratória

movie20 %>%
  count(distributor, sort = TRUE)

movie20 %>% 
  count(genre, sort = TRUE)

movie20 %>% 
  count(mpaa_rating, sort = TRUE)

############### Análise do lucro

lucro20 <- movie20 %>% 
  mutate(profit = worldwide_gross - production_budget)

lucro5 <- movie5 %>% 
  mutate(profit = worldwide_gross - production_budget)

### Por distribuidora

# Geral

lucro20 %>% 
  filter(!is.na(distributor)) %>%
  mutate(distributor = fct_reorder(distributor, profit)) %>% 
  ggplot(aes(profit, distributor, fill = distributor)) +
    geom_boxplot(show.legend = FALSE) +
    scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                              suffix = " milhões")) +
    labs(x = "Lucro",
         y = "",
         title = "Lucro por filme",
         subtitle = "por distribuidora")

# Orçamento

lucro20 %>% 
  filter(!is.na(distributor)) %>%
  mutate(distributor = fct_reorder(distributor, production_budget)) %>% 
  ggplot(aes(production_budget, distributor, fill = distributor)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                            suffix = " milhões")) +
  labs(x = "Orçamento",
       y = "",
       title = "Orçamento por filme",
       subtitle = "por distribuidora")

# Filmes

lucro5 %>% 
  group_by(distributor) %>% 
  mutate(rank = min_rank(desc(profit)),
         distributor = fct_reorder(distributor, profit)) %>% 
  filter(!is.na(distributor),
         rank <= 5) %>% 
  ggplot(aes(profit, distributor)) +
    geom_point(aes(color = distributor), show.legend = FALSE) +
    geom_text_repel(aes(label = movie), force = 20) +
    scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                              suffix = " milhões")) +
  labs(x = "Lucro",
       y = "",
       title = "5 filmes com maior lucro",
       subtitle = "em cada distribuidora")

# Linha do tempo

(lucro5 %>% 
  filter(!is.na(distributor)) %>% 
  group_by(distributor, year = year(release_date)) %>% 
  summarise(total_profit = sum(profit)) %>% 
  ggplot(aes(year, total_profit, color = distributor)) +
    geom_smooth(se = FALSE) +
    scale_y_continuous(labels = number_format(scale = 1/10^9, 
                                            suffix = " bilhões",
                                            accuracy = 1)) +
    labs(x = "Ano",
         y = "Lucro Total",
         title = "Lucro total de cada distribuidora",
         subtitle = "ao longo dos anos",
         color = "")) %>% 
  ggplotly(tooltip = c("total_profit", "year"))

# Melhores filmes ao longo dos anos

lucro5 %>% 
  filter(!is.na(distributor)) %>% 
  group_by(distributor, year = year(release_date)) %>% 
  filter(profit == max(profit),
         year(release_date) %% 5 == 0) %>% 
  ggplot(aes(year, profit, label = movie, color = distributor)) +
    geom_point() +
    geom_text_repel(show.legend = FALSE) +
    scale_y_continuous(labels = number_format(scale = 1/10^6, 
                                            suffix = " milhões",
                                            accuracy = 1)) +
    labs(x = "Ano",
         y = "Lucro",
         color = "",
         title = "Filme que mais lucrou por ano",
         subtitle = "por distribuidora")

### Por gênero

# Lucro Geral

lucro20 %>% 
  filter(!is.na(genre)) %>%
  mutate(genre = fct_reorder(genre, profit)) %>% 
  ggplot(aes(profit, genre, fill = genre)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                            suffix = " milhões")) +
  labs(x = "Lucro",
       y = "",
       title = "Lucro por filme",
       subtitle = "por gênero")

# Orçamento

lucro20 %>% 
  filter(!is.na(genre)) %>%
  mutate(genre = fct_reorder(genre, production_budget)) %>% 
  ggplot(aes(production_budget, genre, fill = genre)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                            suffix = " milhões")) +
  labs(x = "Orçamento",
       y = "",
       title = "Orçamento por filme",
       subtitle = "por gênero")

# Retorno

lucro20 %>% 
  filter(!is.na(genre)) %>%
  mutate(genre = fct_reorder(genre, return)) %>% 
  ggplot(aes(return, genre, fill = genre)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Retorno",
       y = "",
       title = "Retorno por filme",
       subtitle = "por gênero")

# Filmes por lucro

lucro20 %>% 
  group_by(genre) %>% 
  mutate(rank = min_rank(desc(profit)),
         genre = fct_reorder(genre, profit)) %>% 
  filter(!is.na(genre),
         rank <= 5) %>% 
  ggplot(aes(profit, genre)) +
  geom_point(aes(color = genre), show.legend = FALSE) +
  geom_text_repel(aes(label = movie), force = 20) +
  scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                            suffix = " milhões")) +
  labs(x = "Lucro",
       y = "",
       title = "5 filmes com maior lucro",
       subtitle = "em cada distribuidora")

# Filmes por retorno

lucro20 %>% 
  group_by(genre) %>% 
  mutate(rank = min_rank(desc(return)),
         genre = fct_reorder(genre, return)) %>% 
  filter(!is.na(genre),
         rank <= 5) %>% 
  ggplot(aes(return, genre)) +
  geom_point(aes(color = genre), show.legend = FALSE) +
  geom_text_repel(aes(label = movie), force = 20) +
  labs(x = "Retorno",
       y = "",
       title = "5 filmes com maior retorno",
       subtitle = "em cada distribuidora")

# Linha do tempo

(lucro20 %>% 
    filter(!is.na(genre)) %>% 
    group_by(genre, year = year(release_date)) %>% 
    summarise(total_revenue = sum(worldwide_gross)) %>% 
    ggplot(aes(year, total_revenue, color = genre)) +
    geom_smooth(se = FALSE) +
    scale_y_continuous(labels = number_format(scale = 1/10^9, 
                                              suffix = " bilhões",
                                              accuracy = 1)) +
    labs(x = "Ano",
         y = "Receita Total",
         title = "Receita total de cada gênero",
         subtitle = "ao longo dos anos",
         color = "")) %>% 
  ggplotly(tooltip = c("total_revenue", "year"))

### Por faixa etária

# Lucro Geral

lucro20 %>% 
  filter(!is.na(mpaa_rating)) %>%
  mutate(mpaa_rating = fct_reorder(mpaa_rating, profit)) %>% 
  ggplot(aes(profit, mpaa_rating, fill = mpaa_rating)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                            suffix = " milhões")) +
  labs(x = "Lucro",
       y = "",
       title = "Lucro por filme",
       subtitle = "por faixa etária")

# Orçamento

lucro20 %>% 
  filter(!is.na(mpaa_rating)) %>%
  mutate(mpaa_rating = fct_reorder(mpaa_rating, production_budget)) %>% 
  ggplot(aes(production_budget, mpaa_rating, fill = mpaa_rating)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                            suffix = " milhões")) +
  labs(x = "Orçamento",
       y = "",
       title = "Orçamento por filme",
       subtitle = "por faixa etária")

# Filmes por lucro

lucro20 %>% 
  group_by(mpaa_rating) %>% 
  mutate(rank = min_rank(desc(profit)),
         genre = fct_reorder(mpaa_rating, profit)) %>% 
  filter(!is.na(mpaa_rating),
         rank <= 5) %>% 
  ggplot(aes(profit, mpaa_rating)) +
  geom_point(aes(color = mpaa_rating), show.legend = FALSE) +
  geom_text_repel(aes(label = movie), force = 20) +
  scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                            suffix = " milhões")) +
  labs(x = "Lucro",
       y = "",
       title = "5 filmes com maior lucro",
       subtitle = "em cada faixa etária")

# Filmes por orçamento

lucro20 %>% 
  group_by(mpaa_rating) %>% 
  mutate(rank = min_rank(desc(production_budget)),
         genre = fct_reorder(mpaa_rating, production_budget)) %>% 
  filter(!is.na(mpaa_rating),
         rank <= 5) %>% 
  ggplot(aes(production_budget, mpaa_rating)) +
  geom_point(aes(color = mpaa_rating), show.legend = FALSE) +
  geom_text_repel(aes(label = movie), force = 10) +
  scale_x_continuous(labels = number_format(scale = 1/1000000, 
                                            suffix = " milhões")) +
  labs(x = "Lucro",
       y = "",
       title = "5 filmes com maior orçamento",
       subtitle = "em cada faixa etária")

# Linha do tempo

(lucro20 %>% 
    filter(!is.na(mpaa_rating)) %>% 
    group_by(mpaa_rating, year = year(release_date)) %>% 
    summarise(total_revenue = sum(worldwide_gross)) %>% 
    ggplot(aes(year, total_revenue, color = mpaa_rating)) +
    geom_smooth(se = FALSE) +
    scale_y_continuous(labels = number_format(scale = 1/10^9, 
                                              suffix = " bilhões",
                                              accuracy = 1)) +
    labs(x = "Ano",
         y = "Receita Total",
         title = "Receita total de cada faixa etária",
         subtitle = "ao longo dos anos",
         color = "")) %>% 
  ggplotly(tooltip = c("total_revenue", "year"))

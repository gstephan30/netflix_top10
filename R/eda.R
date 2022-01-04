source("R/download_current.R")
download_current()

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
theme_set(theme_light())
heute <- gsub("-", "", Sys.Date())


# get recent files
recent_files <- list.files("data_raw", full.names = TRUE) %>% 
  as_tibble() %>% 
  mutate(datum = str_extract(value, "[0-9]{8}"),
         cat = basename(value), 
         cat = str_remove(cat, "[0-9]{8}_"),
         cat = str_remove(cat, ".tsv")) %>% 
  arrange(desc(datum)) %>% 
  slice(1:3)

df <- map(pull(recent_files, value), read_tsv) 
names(df) <- pull(recent_files, cat)


# eda
df$weeks_countries %>% 
  filter(grepl("Squid|Arcane", show_title),
         grepl("Germany", country_name))

df$weeks_countries %>% 
  filter(weekly_rank == 1,
         show_title == "Arcane")

df$weeks_global %>% 
  filter(grepl("Squid|Arcane", show_title))

read_tsv("data_raw/20211117_weeks_global.tsv") %>% 
  filter(grepl("Squid|Arcane", show_title))

top_views_per_week <- df$weeks_global %>% 
  group_by(show_title) %>% 
  mutate(total_hours = sum(weekly_hours_viewed),
         avg_per_week = total_hours/max(cumulative_weeks_in_top_10)) %>% 
  ungroup() %>% 
  arrange(desc(avg_per_week)) %>% 
  select(show_title, avg_per_week) %>% 
  distinct() 


save(top_views_per_week,
     file = paste0("data/", heute, "_top_weeks.rds"))


top_weeks_n <- 5
df$weeks_global %>% 
  filter(show_title %in% pull(slice(top_views_per_week, 1:top_weeks_n), show_title)) %>% 
  filter(!is.na(category)) %>% 
  ggplot() +
  #geom_line(aes(cumulative_weeks_in_top_10, weekly_hours_viewed, group = season_title), color = "black", size = 1.2, alpha = 0.5) +
  geom_line(aes(cumulative_weeks_in_top_10, weekly_hours_viewed, color = season_title, group = season_title), size = 1.2, alpha = 0.9) +
  geom_point(aes(cumulative_weeks_in_top_10, weekly_hours_viewed, color = season_title, group = season_title), size = 1.2, alpha = 0.9) +
  labs(title = paste("Top", top_weeks_n, "Movies/Shows"),
       subtitle = paste("Date:", format(Sys.Date(), "%b %d, %Y")),
       x = "Cumulative weeks in top 10",
       y = "Weekly hours viewed",
       color = "Season Title") +
  facet_wrap(~category, ncol = 1) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  scale_y_continuous(labels = scales::number_format()) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) 

# explore weekly hours viewed
df$weeks_global %>% 
  filter(cumulative_weeks_in_top_10 %in% c(1, 2)) %>%
  pivot_wider(names_from = cumulative_weeks_in_top_10,
              values_from = weekly_hours_viewed,
              values_fill = 0) %>% 
  janitor::clean_names() %>%
  mutate(ind = x1>x2) %>% 
  filter(ind == FALSE, x1 != 0)
  

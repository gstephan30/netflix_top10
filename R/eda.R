source("R/download_current.R")
download_current()

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)


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

# explore weekly hours viewed
df$weeks_global %>% 
  filter(cumulative_weeks_in_top_10 %in% c(1, 2)) %>%
  pivot_wider(names_from = cumulative_weeks_in_top_10,
              values_from = weekly_hours_viewed,
              values_fill = 0) %>% 
  janitor::clean_names() %>%
  mutate(ind = x1>x2) %>% 
  filter(ind == FALSE, x1 != 0)
  

library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(forcats)
library(patchwork)

netflix <- list.files("data", "rds", full.names = TRUE) %>% 
  sort(decreasing = TRUE) %>% 
  .[1] %>% 
  read_rds() %>% 
  group_by(category) %>% 
  arrange(desc(avg_per_week)) %>% 
  slice(1:10) %>% 
  mutate(heute = Sys.Date()) 

episodes <- read_delim("https://datasets.imdbws.com/title.episode.tsv.gz", delim = "\t")
title <- read_delim("https://datasets.imdbws.com/title.basics.tsv.gz", delim = "\t")
ratings <- read_delim("https://datasets.imdbws.com/title.ratings.tsv.gz", delim = "\t")

netflix_imdb <- title %>% 
  filter(primaryTitle %in% unique(netflix$show_title))

netflix_shows <- title %>% 
  filter(primaryTitle %in% (netflix %>% filter(category == "TV") %>% pull(show_title)))
netflix_movies <- title %>% 
  filter(primaryTitle %in% (netflix %>% filter(category == "Movie") %>% pull(show_title)))

movie_ratings <- netflix_movies %>%
  arrange(primaryTitle) %>% 
  filter(titleType == "movie") %>% 
  select(tconst, primaryTitle, startYear, genres) %>% 
  mutate(startYear = as.numeric(startYear)) %>% 
  group_by(primaryTitle) %>% 
  filter(startYear == max(startYear)) %>% 
  left_join(
    ratings
  ) %>% 
  left_join(
    df$weeks_global %>% 
      group_by(show_title) %>% 
      summarise(total_hours = sum(weekly_hours_viewed),
                total_weeks = max(cumulative_weeks_in_top_10)) %>% 
      rename(primaryTitle = show_title)
  )

movie_ratings %>% 
  ggplot(aes(averageRating, total_hours, label = primaryTitle)) +
  geom_point() +
  ggrepel::geom_text_repel() +
  theme_light() +
  scale_y_continuous(labels = scales::number_format()) +
  scale_color_viridis_c(guide = "none") +
  scale_size_continuous(guide = "none") +
  labs(x = "Average Movie Rating (Source @imbd)",
       y = "Total Hours Watched (Source @netflix")


#### shows
show_ratings <- netflix_shows %>% 
  arrange(primaryTitle) %>%
  filter(titleType == "tvSeries" |
           titleType == "tvMiniSeries") %>% 
  select(tconst, primaryTitle, startYear, genres) %>% 
  mutate(startYear = as.numeric(startYear)) %>% 
  group_by(primaryTitle) %>% 
  filter(startYear == max(startYear, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(
    episodes %>% 
      rename(tconst = parentTconst, tconst_epi = tconst)
  ) %>% 
  left_join(
    ratings %>% 
      rename(tconst_epi = tconst)
  ) %>% 
  left_join(
    df$weeks_global %>% 
      group_by(show_title) %>% 
      summarise(total_hours = sum(weekly_hours_viewed),
                total_weeks = max(cumulative_weeks_in_top_10)) %>% 
      rename(primaryTitle = show_title)
  )

show_ratings %>% 
  mutate(seasonNumber = as.numeric(seasonNumber)) %>% 
  filter(!is.na(numVotes)) %>% 
  group_by(primaryTitle) %>% 
  filter(seasonNumber == max(seasonNumber)) %>% 
  ungroup() %>% 
  group_by(primaryTitle, total_hours) %>% 
  summarise(avg_rating = mean(averageRating, na.rm = TRUE),
            episodes = n()) %>% 
  ggplot(aes(avg_rating, total_hours, label = primaryTitle)) +
  geom_point(aes(size = episodes +0.5)) +
  geom_point(aes(size = episodes, color = episodes)) +
  ggrepel::geom_text_repel() +
  theme_light() +
  scale_y_continuous(labels = scales::number_format()) +
  scale_color_viridis_c(guide = "none") +
  scale_size_continuous(guide = "none") +
  labs(x = "Average Show Rating (Source @imdb)",
       y = "Total Hours Watched of current Season (Source @netflix)",
       )

show_ids <- show_ratings %>% 
  distinct(tconst) %>% 
  pull()

create_data <- function(id){
  
  episodes %>%
    filter(parentTconst == id) %>%
    left_join(
      ratings
      
    ) %>%
    type_convert() %>%
    filter(!is.na(averageRating)) %>% 
    left_join(
      title.akas %>% 
        filter(titleId == id) %>% 
        count(titleId, title, sort = TRUE) %>% 
        slice(1) %>% 
        select(parentTconst = titleId, title)
    ) %>% 
    left_join(
      title
    ) %>% 
    mutate(startYear = as.numeric(startYear))
}

create_plot <- function(data) {
  
  min_rate <- data$averageRating %>% min()
  max_rate <- data$averageRating %>% max()
  rating_mid <- max_rate - ((max_rate - min_rate) / 2)
  
  empty_tiles <- tibble(
    episodeNumber = 0:max(data$episodeNumber),
    seasonNumber = 0,
    averageRating = 0:max(data$episodeNumber),
    color = "#000000"
  ) %>%
    bind_rows(
      tibble(
        seasonNumber = 1:max(data$seasonNumber),
        episodeNumber = 0,
        averageRating = 1:max(data$seasonNumber),
        color = "#000000"
      )
    ) %>%
    mutate(label_color = "white")
  
  tile_color <- tibble(
    rating = 0:10,
    color = c(
      "#000000",
      "#a50026",
      "#d73027",
      "#f46d43",
      "#fdae61",
      "#fee08b",
      "#d9ef8b",
      "#a6d96a",
      "#66bd63",
      "#1a9850",
      "#006837"
    )
  )
  
  plot_data <- data %>%
    select(seasonNumber, episodeNumber, averageRating, startYear, title) %>%
    mutate(rating = floor(averageRating)) %>%
    left_join(tile_color) %>%
    mutate(label_color = "black") %>%
    bind_rows(empty_tiles) #%>% 
  # group_by(seasonNumber) %>% 
  # mutate(first_release = min(startYear, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  # mutate(seasonNumber = ifelse(episodeNumber == 0, paste(seasonNumber, " - ", first_release), seasonNumber)) 
  
  g1 <- plot_data %>%
    ggplot(aes(seasonNumber, episodeNumber, fill = color, label = averageRating)) +
    geom_tile(color = "white") +
    scale_fill_identity(guide = "legend",
                        labels = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)) +
    scale_x_continuous(position = "top") +
    scale_y_reverse() +
    geom_text(aes(color = label_color)) +
    scale_color_manual(values = c("black", "white"), guide = "none") +
    coord_fixed(ratio = 1) +
    labs(x = "Season",
         y = "Episode",
         fill = "Average Rating") +
    theme(line = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom")
  
  g2 <- plot_data %>% 
    filter(seasonNumber != 0,
           episodeNumber != 0) %>% 
    group_by(seasonNumber) %>%
    mutate(first_release = min(startYear, na.rm = TRUE),
           season_avg_rating = mean(averageRating, na.rm = TRUE),
           season_avg_rating = round(season_avg_rating, 1)) %>%
    ungroup() %>% 
    mutate(season_label = paste("Season", seasonNumber, "-", first_release, " ( avg Rating: ", season_avg_rating, ")")) %>% 
    ggplot(aes(episodeNumber, averageRating, fill = color)) +
    geom_col() +
    facet_wrap(~season_label, ncol = 1) +
    scale_fill_identity(guide = "legend",
                        labels = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)) +
    theme_light() +
    theme(
      legend.position = "bottom",
      strip.background =element_rect(fill = "#003d80"),
      strip.text = element_text(colour = 'white', face = "bold")) +
    labs(
         x = "Number of Episode",
         y = "Average Rating",
         fill = "") +
    lims(y = c(0, 10))
  
  
  g1 + g2 + 
    plot_annotation(title = paste0("Ratings for: ", unique(plot_data$title)),
                    caption = paste0("Data Source: @imdb, Date: ", format(Sys.Date(), "%b %d, %Y")))
  
}


show_ids[10] %>% 
  create_data() %>% 
  create_plot()


rick_morty_id <- "tt2861424"
outlander_id <- "tt3006802"
this_is_us_id <- "tt5555260"
home_improvement_id <- "tt0101120"
game_of_thrones_id <- "tt0944947"
ateam_id <- "tt0084967"
cobrakai_id <- "tt7221388"
breakingbad_id <- "tt0903747"
st_discovery_id <- "tt5171438"

st_discovery_id |> 
  create_data() |> 
  create_plot()

breakingbad_id |> 
  create_data() |> 
  create_plot()

cobrakai_id |> 
  create_data() |> 
  create_plot()

rick_morty_id %>% 
  create_data() %>% 
  create_plot()

this_is_us_id %>% 
  create_data() %>% 
  create_plot()

home_improvement_id %>% 
  create_data() %>% 
  create_plot()

outlander_id %>% 
  create_data() %>% 
  create_plot()

game_of_thrones_id %>% 
  create_data() %>% 
  create_plot()



ateam_id %>% 
  create_data() %>% 
  create_plot()

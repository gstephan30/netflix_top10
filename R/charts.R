#' ggplot charts for the dashboard, in the style of the original README
#' (`README.Rmd` @ 0686e75): theme_light, Schoolbell font, rendered at build
#' time for this week's global Top 10.

`%>%` <- dplyr::`%>%`

#' Register the Schoolbell font (vendored, no web request) and the theme.
chart_setup <- function(font_path = "fonts/Schoolbell-Regular.ttf") {
  sysfonts::font_add("bell", font_path)
  showtext::showtext_auto()
  ggplot2::theme_set(ggplot2::theme_light(base_size = 20, base_family = "bell"))
}

#' Read the exported Parquet files plus this week's global Top 10.
read_chart_data <- function(data_dir = "data") {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  pq <- \(name) DBI::dbQuoteString(con, file.path(data_dir, paste0(name, ".parquet")))
  q <- \(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))

  global <- q(sprintf("SELECT * FROM read_parquet(%s)", pq("global")))
  titles <- q(sprintf("SELECT * FROM read_parquet(%s)", pq("titles")))
  episodes <- q(sprintf("
    SELECT * FROM read_parquet(%s)
    WHERE tconst IN (SELECT tconst FROM read_parquet(%s) WHERE kind = 'TV')",
    pq("episodes"), pq("titles")))

  week <- max(global$week)
  current <- global %>%
    dplyr::filter(week == !!week) %>%
    dplyr::left_join(
      dplyr::select(titles, title_id, season_number, total_hours, total_views, tconst,
                    imdb_title, imdb_rating, imdb_votes, match_confidence),
      by = "title_id"
    )
  list(week = week, global = global, titles = titles, episodes = episodes, current = current)
}

kind_label <- function(kind) ifelse(kind == "TV", "TV", "Movies")
fmt_week <- function(week) format(week, "%b %d, %Y")
fmt_short <- scales::label_number(scale_cut = scales::cut_short_scale())
int_breaks <- \(lim) unique(floor(pretty(lim)))
# Matches good enough to name a title on a chart (low = uncertain)
confident <- c("high", "medium", "manual")

#' Weekly views per season over cumulative weeks in the Top 10 (`make_graph()`).
#'
#' Picks this week's `n` most-viewed entries of one kind, one per show: its
#' latest season (by `season_number`, not by title text).
make_graph <- function(d, cat, n = 6) {
  picks <- d$current %>%
    dplyr::filter(kind == cat) %>%
    dplyr::group_by(show_title) %>%
    dplyr::slice_max(dplyr::coalesce(season_number, 0L), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(weekly_views, n = n, with_ties = FALSE)

  # Views exist since June 2023 only; earlier weeks of a long run are dropped
  data <- d$global %>%
    dplyr::filter(title_id %in% picks$title_id, !is.na(weekly_views)) %>%
    dplyr::mutate(
      season_title = dplyr::coalesce(season_title, show_title),
      season_title = factor(
        stringr::str_trunc(season_title, 45),
        levels = unique(stringr::str_trunc(
          dplyr::coalesce(picks$season_title, picks$show_title), 45))
      )
    )

  ggplot2::ggplot(data, ggplot2::aes(cumulative_weeks_in_top_10, weekly_views,
                                     color = season_title, group = season_title)) +
    ggplot2::geom_line(linewidth = 1.2, alpha = 0.9) +
    ggplot2::geom_point(size = 1.2, alpha = 0.9) +
    ggplot2::labs(
      title = paste("Top", n, "in", kind_label(cat)),
      subtitle = paste("Week ending", fmt_week(d$week)),
      x = "Cumulative weeks in top 10",
      y = "Weekly views",
      color = NULL
    ) +
    ggplot2::scale_color_brewer(type = "qual", palette = "Paired") +
    ggplot2::scale_y_continuous(labels = fmt_short) +
    ggplot2::scale_x_continuous(breaks = int_breaks) +
    ggplot2::theme(legend.position = "bottom", legend.justification = "left") +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
}

#' Netflix hours viewed vs IMDb rating with direct labels.
#'
#' Films: one point per film. Shows: the charting season, rated by the mean
#' of its IMDb episode ratings, point size = episodes.
rating_scatter <- function(d, cat) {
  data <- d$current %>%
    dplyr::filter(kind == cat, !is.na(imdb_rating)) %>%
    dplyr::distinct(title_id, .keep_all = TRUE) %>%
    dplyr::mutate(label = dplyr::if_else(match_confidence %in% confident, show_title, NA_character_))

  if (cat == "TV") {
    seasons <- d$episodes %>%
      dplyr::group_by(tconst, season) %>%
      dplyr::summarise(season_rating = mean(rating), episodes = dplyr::n(), .groups = "drop")
    one_season <- seasons %>% dplyr::count(tconst) %>% dplyr::filter(n == 1)
    data <- data %>%
      dplyr::mutate(season = dplyr::if_else(is.na(season_number) & tconst %in% one_season$tconst,
                                            1L, season_number)) %>%
      dplyr::left_join(seasons, by = c("tconst", "season")) %>%
      dplyr::mutate(imdb_rating = dplyr::coalesce(season_rating, imdb_rating),
                    label = dplyr::if_else(!is.na(label) & !is.na(season_number) & season_number > 1,
                                           paste0(label, " (S", season_number, ")"), label))
  }

  g <- ggplot2::ggplot(data, ggplot2::aes(imdb_rating, total_hours, label = label))
  g <- if (cat == "TV") {
    g +
      ggplot2::geom_point(ggplot2::aes(size = episodes + 0.5)) +
      ggplot2::geom_point(ggplot2::aes(size = episodes, color = episodes)) +
      ggplot2::scale_color_viridis_c(guide = "none") +
      ggplot2::scale_size_continuous(guide = "none")
  } else {
    g + ggplot2::geom_point(size = 2.5)
  }
  g +
    ggrepel::geom_text_repel(family = "bell", size = 6, max.overlaps = Inf, na.rm = TRUE,
                             min.segment.length = 0.3, seed = 1) +
    ggplot2::scale_y_log10(labels = fmt_short) +
    ggplot2::labs(
      x = if (cat == "TV") "Avg IMDb episode rating of the season" else "IMDb rating",
      y = "Hours viewed on Netflix\n(total in global Top 10)",
      title = paste(kind_label(cat), "– Netflix hours viewed vs IMDb rating"),
      subtitle = paste("Global Top 10, week ending", fmt_week(d$week)),
      caption = "Sources: Netflix Top 10, IMDb. Uncertain IMDb matches are not labelled."
    )
}

#' This week's Top 10 shows with a confident IMDb match and a grid-sized
#' episode list, by weekly views.
episode_shows <- function(d, n = 10, max_episodes = 40, max_seasons = 20) {
  sizes <- d$episodes %>%
    dplyr::group_by(tconst) %>%
    dplyr::summarise(max_ep = max(episode), n_seasons = dplyr::n_distinct(season))
  d$current %>%
    dplyr::filter(kind == "TV", match_confidence %in% confident) %>%
    dplyr::arrange(dplyr::desc(weekly_views)) %>%
    dplyr::distinct(tconst, .keep_all = TRUE) %>%
    dplyr::inner_join(sizes, by = "tconst") %>%
    dplyr::filter(max_ep <= max_episodes, n_seasons <= max_seasons) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, tconst, show_title, imdb_title, n_seasons, max_ep)
}

#' IMDb episode ratings of one show (the export already joins ratings).
create_data <- function(d, id) {
  dplyr::filter(d$episodes, tconst == id)
}

# Red -> green as before, with a darker red end so that low and high ratings
# also differ in lightness (red-green colour blindness).
rating_colours <- c(
  "1" = "#3d0010", "2" = "#67001f", "3" = "#a50026", "4" = "#d73027", "5" = "#f46d43",
  "6" = "#fdae61", "7" = "#fee08b", "8" = "#a6d96a", "9" = "#1a9850", "10" = "#00512a"
)
dark_ratings <- c("1", "2", "3", "4", "9", "10")

#' Season x episode tile grid plus per-season bars (`create_plot()`).
create_plot <- function(data, title, rank) {
  data <- data %>%
    dplyr::mutate(
      bin = factor(pmin(pmax(floor(rating), 1), 10), levels = names(rating_colours)),
      text_colour = dplyr::if_else(bin %in% dark_ratings, "white", "black")
    )
  headers <- dplyr::bind_rows(
    tibble::tibble(season = 0L, episode = 0:max(data$episode)),
    tibble::tibble(season = seq_len(max(data$season)), episode = 0L)
  ) %>%
    dplyr::mutate(label = dplyr::if_else(season == 0L, episode, season))
  headers <- dplyr::filter(headers, label > 0)
  fill <- ggplot2::scale_fill_manual(values = rating_colours, drop = TRUE, name = "Rating",
                                     guide = ggplot2::guide_legend(nrow = 1, reverse = TRUE))

  g1 <- ggplot2::ggplot(data, ggplot2::aes(season, episode)) +
    ggplot2::geom_tile(ggplot2::aes(fill = bin), color = "white") +
    ggplot2::geom_tile(data = headers, fill = "#000000", color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = rating, color = text_colour), family = "bell", size = 5) +
    ggplot2::geom_text(data = headers, ggplot2::aes(label = label), color = "white", family = "bell", size = 5) +
    fill +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(position = "top") +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(x = "Season", y = "Episode") +
    ggplot2::theme(legend.position = "bottom",
                   line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank())

  seasons <- data %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(season_label = sprintf("Season %d - %s (avg rating: %.1f)", season,
                                         suppressWarnings(min(year, na.rm = TRUE)), mean(rating))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(season_label = sub(" - Inf ", " ", season_label))

  g2 <- ggplot2::ggplot(seasons, ggplot2::aes(episode, rating, fill = bin)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~season_label, ncol = 1) +
    fill +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_x_continuous(breaks = int_breaks) +
    ggplot2::scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "#003d80"),
                   strip.text = ggplot2::element_text(colour = "white", face = "bold")) +
    ggplot2::labs(x = "Number of episode", y = "Average rating")

  patchwork::wrap_plots(g1, g2) +
    patchwork::plot_annotation(
      title = paste0("Ratings for: Rank ", rank, " - ", title),
      caption = paste0("Data source: IMDb, ", format(Sys.Date(), "%b %d, %Y"))
    )
}

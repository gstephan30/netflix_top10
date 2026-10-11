source(file.path(root_dir, "R", "charts.R"))

chart_fixture <- function() {
  week <- as.Date("2026-10-04")
  current <- tibble::tibble(
    title_id = c("g1", "g2", "s10", "s2", "f1"),
    kind = c("TV", "TV", "TV", "TV", "Film"),
    show_title = c("Gents", "Gents", "Long", "Long", "Film A"),
    season_title = c("Gents: Season 1", "Gents: Season 2", "Long: Season 10", "Long: Season 2", NA),
    season_number = c(1L, 2L, 10L, 2L, NA),
    weekly_views = c(9e6, 1e6, 2e6, 8e6, 5e6),
    week = week
  )
  global <- dplyr::bind_rows(
    dplyr::mutate(current, cumulative_weeks_in_top_10 = 2L),
    dplyr::mutate(current, cumulative_weeks_in_top_10 = 1L, week = week - 7)
  )
  episodes <- tibble::tibble(
    tconst = "tt1", season = c(1L, 1L, 2L), episode = c(1L, 2L, 1L),
    year = c(2020L, 2020L, 2022L), rating = c(9.4, 7.1, 3.2)
  )
  list(week = week, current = current, global = global, episodes = episodes)
}

test_that("make_graph() keeps each show's latest season by number, not by text", {
  g <- make_graph(chart_fixture(), "TV")
  shown <- levels(g$data$season_title)
  expect_setequal(shown, c("Gents: Season 2", "Long: Season 10"))
})

test_that("create_plot() bins ratings and labels seasons with the episode year", {
  d <- chart_fixture()
  p <- create_plot(create_data(d, "tt1"), "Show", 1)
  tiles <- p[[1]]$data
  expect_equal(as.character(tiles$bin), c("9", "7", "3"))
  expect_equal(tiles$text_colour, c("white", "black", "white"))
  labels <- unique(p[[2]]$data$season_label)
  expect_equal(labels, c("Season 1 - 2020 (avg rating: 8.2)", "Season 2 - 2022 (avg rating: 3.2)"))
})

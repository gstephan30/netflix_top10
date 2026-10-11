root_dir <- normalizePath(testthat::test_path("..", ".."))
for (f in c("utils.R", "netflix.R", "imdb.R", "export.R")) {
  source(file.path(root_dir, "R", f))
}

local_db <- function(env = parent.frame()) {
  con <- connect_db()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

write_tsv_plain <- function(df, path, na = "") {
  utils::write.table(df, path, sep = "\t", quote = FALSE, row.names = FALSE, na = na)
}

write_tsv_gz <- function(df, path) {
  con <- gzfile(path, "w")
  on.exit(close(con))
  utils::write.table(df, con, sep = "\t", quote = FALSE, row.names = FALSE, na = "\\N")
}

#' Small but schema-complete Netflix export: `n_weeks` Sundays, 4 global
#' categories and one country, ranks 1-10 everywhere.
make_netflix_fixture <- function(dir, n_weeks = 2) {
  weeks <- as.Date("2026-09-27") - 7 * (seq_len(n_weeks) - 1)
  cats <- c("Films (English)", "Films (Non-English)", "TV (English)", "TV (Non-English)")
  global <- expand.grid(weekly_rank = 1:10, category = cats, week = weeks, stringsAsFactors = FALSE)
  global$show_title <- paste(global$category, global$weekly_rank)
  global$season_title <- ifelse(grepl("^TV", global$category),
                                paste0(global$show_title, ": Season 1"), "N/A")
  global$weekly_hours_viewed <- 1e6 * (11 - global$weekly_rank)
  global$runtime <- 2
  global$weekly_views <- global$weekly_hours_viewed / 2
  global$cumulative_weeks_in_top_10 <- 1
  global <- global[, c("week", "category", "weekly_rank", "show_title", "season_title",
                       "weekly_hours_viewed", "runtime", "weekly_views",
                       "cumulative_weeks_in_top_10")]

  countries <- expand.grid(weekly_rank = 1:10, category = c("Films", "TV"), week = weeks,
                           stringsAsFactors = FALSE)
  countries$country_name <- "Germany"
  countries$country_iso2 <- "DE"
  countries$show_title <- paste(ifelse(countries$category == "TV", "TV (English)",
                                       "Films (English)"), countries$weekly_rank)
  countries$season_title <- ifelse(countries$category == "TV",
                                   paste0(countries$show_title, ": Season 1"), "N/A")
  countries$cumulative_weeks_in_top_10 <- 1
  countries <- countries[, c("country_name", "country_iso2", "week", "category",
                             "weekly_rank", "show_title", "season_title",
                             "cumulative_weeks_in_top_10")]

  popular <- data.frame(
    category = "Films (English)", rank = 1:10, show_title = paste("Films (English)", 1:10),
    season_title = "N/A", hours_viewed_first_91_days = 1e8, runtime = 2,
    views_first_91_days = 5e7
  )

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write_tsv_plain(global, file.path(dir, "all-weeks-global.tsv"))
  write_tsv_plain(countries, file.path(dir, "all-weeks-countries.tsv"))
  write_tsv_plain(popular, file.path(dir, "most-popular.tsv"))
  invisible(list(global = global, countries = countries, popular = popular))
}

fixture_sources <- function() {
  lapply(netflix_sources, \(src) modifyList(src, list(min_rows = 1, min_bytes = 1)))
}

# Synthetic IMDb extract with the traps seen in the real data: famous
# namesakes, a remake, an anime/live-action pair, an AKA collision and a
# spacing variant. Every case must be resolved by the scoring, not by votes.

make_imdb_fixture <- function(dir) {
  basics <- data.frame(
    tconst = c("tt0000101", "tt0000102", "tt0000201", "tt0000202",
               "tt0000301", "tt0000302", "tt0000401", "tt0000402",
               paste0("tt00009", 10:13)),
    titleType = c("movie", "movie", "tvSeries", "tvSeries",
                  "movie", "movie", "tvSeries", "tvSeries",
                  rep("tvEpisode", 4)),
    primaryTitle = c("Frankenstein", "Frankenstein", "One Piece", "One Piece",
                     "Hidden Figures", "Dream", "Dandadan", "Dan Da Dan Live",
                     paste("Episode", 1:4)),
    originalTitle = c("Frankenstein", "Frankenstein", "Wan pisu", "One Piece",
                      "Hidden Figures", "Deurim", "Dandadan", "Dan Da Dan Live",
                      paste("Episode", 1:4)),
    isAdult = 0,
    startYear = c(1931, 2025, 1999, 2023, 2016, 2023, 2024, 2019, 1999, 2000, 2023, 2026),
    endYear = NA,
    runtimeMinutes = c(70, 150, 24, 60, 127, 125, 24, 30, NA, NA, NA, NA),
    genres = "Drama"
  )
  ratings <- data.frame(
    tconst = basics$tconst[1:8],
    averageRating = c(7.8, 7.5, 9.0, 8.3, 7.8, 6.4, 8.5, 5.0),
    numVotes = c(80000, 50000, 300000, 200000, 300000, 4000, 60000, 300)
  )
  episode <- data.frame(
    tconst = paste0("tt00009", 10:13),
    parentTconst = c("tt0000201", "tt0000201", "tt0000202", "tt0000202"),
    seasonNumber = c(1, 2, 1, 2),
    episodeNumber = 1
  )
  akas <- data.frame(
    titleId = "tt0000301", ordering = 2, title = "Dream", region = "US",
    language = NA, types = NA, attributes = NA, isOriginalTitle = 0
  )
  write_tsv_gz(basics, file.path(dir, imdb_files[["basics"]]))
  write_tsv_gz(ratings, file.path(dir, imdb_files[["ratings"]]))
  write_tsv_gz(episode, file.path(dir, imdb_files[["episode"]]))
  write_tsv_gz(akas, file.path(dir, imdb_files[["akas"]]))
}

make_titles <- function(con) {
  titles <- data.frame(
    kind = c("Film", "TV", "Film", "TV"),
    language = c("English", "English", "Non-English", "Non-English"),
    show_title = c("Frankenstein", "ONE PIECE", "Dream", "DAN DA DAN"),
    season_title = c(NA, "ONE PIECE: Season 2", NA, "DAN DA DAN: Season 1"),
    first_week = as.Date(c("2025-11-09", "2026-03-15", "2023-09-03", "2024-10-06")),
    season_number = c(NA, 2L, NA, 1L),
    runtime = c(2.5, NA, 2.0833, NA),
    total_views = 1e7,
    country_weeks = 10
  )
  DBI::dbWriteTable(con, "titles", titles, overwrite = TRUE)
}

run_fixture_match <- function(overrides) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  make_imdb_fixture(dir)
  ov <- file.path(dir, "overrides.csv")
  utils::write.csv(overrides, ov, row.names = FALSE)
  con <- local_db(parent.frame())
  make_titles(con)
  run_imdb(con, dir, ov, fetch = FALSE, min_titles = 1)
  DBI::dbGetQuery(con, "SELECT show_title, tconst, method, confidence FROM matches")
}

no_overrides <- data.frame(kind = character(), show_title = character(),
                           tconst = character(), note = character())

test_that("scoring resolves namesakes, remakes, AKA traps and spacing", {
  m <- run_fixture_match(no_overrides)
  got <- setNames(m$tconst, m$show_title)

  # runtime + release year beat the more-voted 1931 classic
  expect_equal(got[["Frankenstein"]], "tt0000102")
  # anchor season 2 aired 2026 only for the live-action series
  expect_equal(got[["ONE PIECE"]], "tt0000202")
  # Non-English + own original title beat a popular film's US AKA
  expect_equal(got[["Dream"]], "tt0000302")
  # 'DAN DA DAN' equals 'Dandadan' without spaces
  expect_equal(got[["DAN DA DAN"]], "tt0000401")
})

test_that("manual overrides win and 'none' removes a match", {
  overrides <- data.frame(
    kind = c("Film", "TV"), show_title = c("Frankenstein", "DAN DA DAN"),
    tconst = c("tt0000101", "none"), note = c("test", "test")
  )
  m <- run_fixture_match(overrides)
  frank <- m[m$show_title == "Frankenstein", ]
  expect_equal(frank$tconst, "tt0000101")
  expect_equal(frank$method, "manual")
  expect_true(is.na(m$tconst[m$show_title == "DAN DA DAN"]))
})

test_that("malformed overrides are rejected", {
  bad <- data.frame(kind = "Film", show_title = "Dream", tconst = "12345", note = "")
  expect_error(run_fixture_match(bad), "Invalid tconst")
})

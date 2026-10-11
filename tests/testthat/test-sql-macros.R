test_that("norm_title normalises spelling variants to one key", {
  con <- local_db()
  key <- \(x) DBI::dbGetQuery(con, "SELECT norm_title(?) AS k", params = list(x))$k

  expect_equal(key("Don't Look Up"), "dont look up")
  expect_equal(key("Don’t Look Up"), "dont look up")
  expect_equal(key("The Witcher"), "witcher")
  expect_equal(key("Amélie & Co.: The Movie"), "amelie and co the movie")
  expect_equal(
    key("Dahmer - Monster: The Jeffrey Dahmer Story"),
    key("DAHMER: Monster: The Jeffrey Dahmer Story")
  )
  # non-Latin scripts collapse to '' and are dropped by the callers
  expect_equal(key("오징어 게임"), "")
})

test_that("season_number reads the common Netflix season labels", {
  con <- local_db()
  season <- \(x) DBI::dbGetQuery(con, "SELECT season_number(?) AS s", params = list(x))$s

  expect_equal(season("Bridgerton: Season 3"), 3)
  expect_equal(season("Money Heist: Part 5"), 5)
  expect_equal(season("Lottie Dottie Chicken: 2ª temporada"), 2)
  expect_equal(season("Stranger Things 4"), 4)
  expect_true(is.na(season("Inventing Anna: Limited Series")))
  expect_true(is.na(season("WWE WrestleMania 2024")))
  expect_true(is.na(season("Raw: April 28, 2025")))
})

test_that("core_key drops filler words for typo-level comparison", {
  con <- local_db()
  dist <- \(a, b) DBI::dbGetQuery(con,
    "SELECT levenshtein(core_key(norm_title(?)), core_key(norm_title(?))) AS d",
    params = list(a, b))$d

  expect_equal(dist("Maze Runner: Death Cure", "Maze Runner: The Death Cure"), 0)
  expect_lte(dist("Ante Sundaraniki", "Ante Sundharaniki"), 2)
  expect_gt(dist("Lady Di", "Lady Bird"), 2)
  expect_gt(dist("The First Days", "The First Lady"), 2)
  expect_gt(dist("Battle of the Fists", "Battle of the Sexes"), 2)
})

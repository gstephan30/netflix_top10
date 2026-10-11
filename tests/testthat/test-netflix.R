test_that("a valid export loads, validates and reconciles", {
  dir <- withr::local_tempdir()
  fx <- make_netflix_fixture(dir)
  con <- local_db()

  expect_no_error(load_netflix(con, dir, fixture_sources()))
  build_titles(con)

  n_global <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM global")$n
  expect_equal(n_global, nrow(fx$global))
  totals <- DBI::dbGetQuery(con, "
    SELECT (SELECT sum(weekly_hours_viewed) FROM global) AS g,
           (SELECT sum(total_hours) FROM titles) AS t")
  expect_equal(totals$g, totals$t)

  # films have no season; they must still pick up their country stats
  film <- DBI::dbGetQuery(con, "
    SELECT countries_reached, country_weeks FROM titles
    WHERE show_title = 'Films (English) 1'")
  expect_equal(film$countries_reached, 1)
  expect_equal(film$country_weeks, 2)
})

test_that("metrics are per season, not summed across seasons", {
  dir <- withr::local_tempdir()
  fx <- make_netflix_fixture(dir)
  # two seasons of one show, one week each (the WWE 'Raw' pattern)
  g <- fx$global
  tv <- g$category == "TV (English)" & g$weekly_rank == 1
  g$show_title[tv] <- "Raw"
  g$season_title[tv] <- c("Raw: Season 1", "Raw: Season 2")
  write_tsv_plain(g, file.path(dir, "all-weeks-global.tsv"))

  con <- local_db()
  load_netflix(con, dir, fixture_sources())
  build_titles(con)
  raw <- DBI::dbGetQuery(con, "
    SELECT season_title, weeks_global, avg_hours_per_week, season_number
    FROM titles WHERE show_title = 'Raw' ORDER BY season_number")

  expect_equal(nrow(raw), 2)
  expect_equal(raw$weeks_global, c(1, 1))
  expect_equal(raw$avg_hours_per_week, c(1e7, 1e7))
  expect_equal(raw$season_number, 1:2)
})

test_that("schema and content violations stop the build", {
  con <- local_db()
  bad_case <- function(mutate) {
    dir <- withr::local_tempdir(.local_envir = parent.frame())
    fx <- make_netflix_fixture(dir)
    write_tsv_plain(mutate(fx$global), file.path(dir, "all-weeks-global.tsv"))
    dir
  }

  dir <- bad_case(\(g) { g$weekly_rank[1] <- 11; g })
  expect_error(load_netflix(con, dir, fixture_sources()), "rank outside")

  dir <- bad_case(\(g) g[, names(g) != "weekly_views"])
  expect_error(load_netflix(con, dir, fixture_sources()), "missing column")

  dir <- bad_case(\(g) { g$week <- as.Date(g$week) + 1; g })
  expect_error(load_netflix(con, dir, fixture_sources()), "Sunday")

  dir <- bad_case(\(g) { g$weekly_hours_viewed[3] <- "lots"; g })
  expect_error(load_netflix(con, dir, fixture_sources()), "Conversion|convert|cast", ignore.case = TRUE)

  dir <- bad_case(\(g) g[-1, ])
  expect_error(load_netflix(con, dir, fixture_sources()), "exactly ranks 1-10")

  dir <- bad_case(identity)
  expect_error(load_netflix(con, dir), "truncated")
})

test_that("new upstream columns only warn", {
  dir <- withr::local_tempdir()
  fx <- make_netflix_fixture(dir)
  g <- fx$global
  g$is_staggered_launch <- "false"
  write_tsv_plain(g, file.path(dir, "all-weeks-global.tsv"))
  con <- local_db()
  expect_warning(load_netflix(con, dir, fixture_sources()), "unexpected new column")
})

test_that("downloads reject error pages and never clobber the cached file", {
  dest <- withr::local_tempfile(fileext = ".tsv")
  writeLines("good cached copy", dest)

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "text/html"),
      body = charToRaw(strrep("<html>blocked</html>", 200))
    )
  })
  expect_error(download_checked("https://example.org/x.tsv", dest), "content type")
  expect_equal(readLines(dest), "good cached copy")

  expect_error(download_checked("http://example.org/x.tsv", dest))
})

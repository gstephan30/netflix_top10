#' Netflix Top 10: download, validate, model.
#'
#' Source: https://www.netflix.com/tudum/top10 (public TSV exports, updated
#' weekly on Tuesdays). All three files are cumulative, so only the current
#' copy is needed - no snapshots.

netflix_base_url <- "https://www.netflix.com/tudum/top10/data/"

netflix_sources <- list(
  global = list(
    file = "all-weeks-global.tsv",
    min_bytes = 500e3,
    min_rows = 10000,
    columns = c(
      "week", "category", "weekly_rank", "show_title", "season_title",
      "weekly_hours_viewed", "runtime", "weekly_views", "cumulative_weeks_in_top_10"
    )
  ),
  countries = list(
    file = "all-weeks-countries.tsv",
    min_bytes = 10e6,
    min_rows = 400000,
    columns = c(
      "country_name", "country_iso2", "week", "category", "weekly_rank",
      "show_title", "season_title", "cumulative_weeks_in_top_10"
    )
  ),
  popular = list(
    file = "most-popular.tsv",
    min_bytes = 1000,
    min_rows = 40,
    columns = c(
      "category", "rank", "show_title", "season_title",
      "hours_viewed_first_91_days", "runtime", "views_first_91_days"
    )
  )
)

fetch_netflix <- function(dir) {
  for (src in netflix_sources) {
    download_checked(
      url = paste0(netflix_base_url, src$file),
      dest = file.path(dir, src$file),
      min_bytes = src$min_bytes
    )
  }
  invisible(dir)
}

#' Read the raw TSVs as text, check the schema, then cast to typed tables.
#'
#' Casting uses plain `CAST`, which errors on bad values - a malformed upstream
#' file stops the build instead of silently producing NAs.
load_netflix <- function(con, dir, sources = netflix_sources) {
  for (name in names(sources)) {
    src <- sources[[name]]
    raw <- paste0(name, "_raw")
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS
       SELECT * FROM read_csv(%s, delim = '\t', header = true, quote = '\"',
                              escape = '\"', all_varchar = true, nullstr = '')",
      raw, sql_path(file.path(dir, src$file))
    ))
    check_columns(DBI::dbListFields(con, raw), src$columns, src$file)
    n <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", raw))$n
    if (n < src$min_rows) {
      cli::cli_abort("{src$file}: only {n} rows (expected >= {src$min_rows}); file looks truncated.")
    }
  }

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE global AS
    SELECT
      CAST(week AS DATE)                              AS week,
      category,
      CASE WHEN category LIKE 'TV%' THEN 'TV' ELSE 'Film' END AS kind,
      CASE WHEN category LIKE '%Non-English%' THEN 'Non-English' ELSE 'English' END AS language,
      CAST(weekly_rank AS INTEGER)                    AS weekly_rank,
      show_title,
      NULLIF(season_title, 'N/A')                     AS season_title,
      CAST(weekly_hours_viewed AS BIGINT)             AS weekly_hours_viewed,
      -- runtime 0 = unknown (weekly-airing K-dramas); hours/runtime is undefined there
      NULLIF(CAST(runtime AS DOUBLE), 0)              AS runtime,
      CAST(weekly_views AS BIGINT)                    AS weekly_views,
      CAST(cumulative_weeks_in_top_10 AS INTEGER)     AS cumulative_weeks_in_top_10
    FROM global_raw
  ")

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE countries AS
    SELECT
      country_name,
      country_iso2,
      CAST(week AS DATE)                              AS week,
      CASE WHEN category = 'TV' THEN 'TV' ELSE 'Film' END AS kind,
      CAST(weekly_rank AS INTEGER)                    AS weekly_rank,
      show_title,
      NULLIF(season_title, 'N/A')                     AS season_title,
      CAST(cumulative_weeks_in_top_10 AS INTEGER)     AS cumulative_weeks_in_top_10
    FROM countries_raw
  ")

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE popular AS
    SELECT
      CASE WHEN category LIKE 'TV%' THEN 'TV' ELSE 'Film' END AS kind,
      CASE WHEN category LIKE '%Non-English%' THEN 'Non-English' ELSE 'English' END AS language,
      CAST(rank AS INTEGER)                           AS rank,
      show_title,
      NULLIF(season_title, 'N/A')                     AS season_title,
      CAST(hours_viewed_first_91_days AS BIGINT)      AS hours_viewed_first_91_days,
      NULLIF(CAST(runtime AS DOUBLE), 0)              AS runtime,
      CAST(views_first_91_days AS BIGINT)             AS views_first_91_days
    FROM popular_raw
  ")

  validate_netflix(con)
  invisible(con)
}

validate_netflix <- function(con) {
  assert_no_rows(con, "
    SELECT DISTINCT category FROM global
    WHERE category NOT IN ('Films (English)', 'Films (Non-English)',
                           'TV (English)', 'TV (Non-English)')",
    "global: unknown category")
  assert_no_rows(con, "
    SELECT DISTINCT category FROM countries_raw WHERE category NOT IN ('Films', 'TV')",
    "countries: unknown category")
  today <- DBI::dbQuoteLiteral(con, Sys.Date())
  for (tbl in c("global", "countries")) {
    assert_no_rows(con, sprintf("
      SELECT * FROM %s
      WHERE weekly_rank NOT BETWEEN 1 AND 10
         OR dayofweek(week) <> 0
         OR week > %s
         OR show_title IS NULL OR trim(show_title) = ''
         OR cumulative_weeks_in_top_10 < 1", tbl, today),
      sprintf("%s: rank outside 1-10, week not a Sunday/in the future, or empty title", tbl))
  }
  assert_no_rows(con, "
    SELECT week, category, count(*) AS n, count(DISTINCT weekly_rank) AS ranks
    FROM global GROUP BY ALL HAVING n <> 10 OR ranks <> 10",
    "global: a week/category does not have exactly ranks 1-10")
  assert_no_rows(con, "
    SELECT country_iso2, week, kind, count(*) AS n, count(DISTINCT weekly_rank) AS ranks
    FROM countries GROUP BY ALL HAVING n <> 10 OR ranks <> 10",
    "countries: a country/week/kind does not have exactly ranks 1-10")
  assert_no_rows(con, "
    SELECT * FROM global
    WHERE weekly_hours_viewed <= 0 OR weekly_views < 0 OR runtime < 0",
    "global: non-positive hours, negative views or runtime")

  latest <- DBI::dbGetQuery(con, "SELECT max(week) AS w FROM global")$w
  age <- as.numeric(Sys.Date() - latest)
  if (age > 16) {
    cli::cli_warn("Latest Netflix week is {latest} ({age} days old) - upstream may be stale.")
  }
  invisible(TRUE)
}

#' Season-level title table: one row per (kind, language, show_title, season_title).
#'
#' This is the unit Netflix ranks. The old metric summed hours over *all*
#' seasons of a show and divided by the weeks of one season, which inflated
#' long-running shows (WWE "Raw" became #1). Here every number belongs to
#' exactly one season / film.
#'
#' Keys: films have no season (`NULL`), and `NULL = NULL` never joins, so
#' `season_key` (empty string for films) is used for joining. Language is part
#' of the key because different films share a name across language categories
#' ("Inheritance", "Ballerina"). The countries file has no language; for those
#' few ambiguous names its stats are attached to every candidate and flagged.
build_titles <- function(con) {
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE titles AS
    WITH g AS (
      SELECT
        kind, language, show_title, season_title,
        coalesce(season_title, '')                       AS season_key,
        min(week)                                        AS first_week_global,
        max(week)                                        AS last_week_global,
        count(*)                                         AS weeks_global,
        min(weekly_rank)                                 AS best_rank_global,
        sum(weekly_hours_viewed)                         AS total_hours,
        sum(weekly_views)                                AS total_views,
        count(weekly_views)                              AS weeks_with_views,
        max(weekly_views)                                AS peak_views,
        max(runtime)                                     AS runtime
      FROM global GROUP BY ALL
    ),
    c AS (
      SELECT
        kind, show_title,
        any_value(season_title)                          AS season_title,
        coalesce(season_title, '')                       AS season_key,
        min(week)                                        AS first_week_countries,
        count(DISTINCT country_iso2)                     AS countries_reached,
        count(*)                                         AS country_weeks,
        count(*) FILTER (weekly_rank = 1)                AS country_weeks_at_1
      FROM countries GROUP BY kind, show_title, season_key
    ),
    joined AS (
      SELECT
        coalesce(g.kind, c.kind)                         AS kind,
        g.language,
        coalesce(g.show_title, c.show_title)             AS show_title,
        coalesce(g.season_title, c.season_title)         AS season_title,
        CASE WHEN c.show_title IS NOT NULL
          THEN count(*) OVER (PARTITION BY c.kind, c.show_title, c.season_key) > 1
        END                                              AS country_stats_ambiguous,
        g.* EXCLUDE (kind, language, show_title, season_title, season_key),
        c.* EXCLUDE (kind, show_title, season_title, season_key)
      FROM g FULL OUTER JOIN c
        ON g.kind = c.kind AND g.show_title = c.show_title AND g.season_key = c.season_key
    )
    SELECT
      substr(md5(concat_ws('|', kind, language, show_title, season_title)), 1, 12) AS title_id,
      kind, language, show_title, season_title,
      least(first_week_global, first_week_countries)     AS first_week,
      first_week_global, last_week_global, weeks_global, best_rank_global,
      total_hours, total_views, weeks_with_views, peak_views, runtime,
      total_hours / weeks_global                         AS avg_hours_per_week,
      total_views / nullif(weeks_with_views, 0)          AS avg_views_per_week,
      coalesce(countries_reached, 0)                     AS countries_reached,
      coalesce(country_weeks, 0)                         AS country_weeks,
      coalesce(country_weeks_at_1, 0)                    AS country_weeks_at_1,
      coalesce(country_stats_ambiguous, false)           AS country_stats_ambiguous,
      season_number(season_title)                        AS season_number
    FROM joined
  ")
  assert_no_rows(con, "
    SELECT title_id, count(*) AS n FROM titles GROUP BY ALL HAVING n > 1",
    "titles: title_id is not unique")
  invisible(con)
}

#' Export the dashboard data set (Parquet + JSON) and validate it.
#'
#' Files written to `out_dir`:
#'   titles.parquet    one row per season / film incl. IMDb match
#'   global.parquet    weekly global Top 10
#'   countries.parquet weekly Top 10 per country
#'   popular.parquet   Netflix all-time "most popular" lists
#'   episodes.parquet  IMDb episode ratings of matched shows
#'   matches.csv       match audit trail (one row per show / film)
#'   meta.json         build metadata

export_dashboard_data <- function(con, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out <- \(file) sql_path_new(file.path(out_dir, file))

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE titles_out AS
    SELECT
      t.title_id, t.kind, t.language, t.show_title, t.season_title, t.season_number,
      t.first_week, t.first_week_global, t.last_week_global, t.weeks_global,
      t.best_rank_global, t.total_hours, t.total_views, t.weeks_with_views,
      t.avg_hours_per_week, t.avg_views_per_week, t.peak_views, t.runtime,
      t.countries_reached, t.country_weeks, t.country_weeks_at_1, t.country_stats_ambiguous,
      m.tconst, m.imdb_title, m.imdb_type, m.imdb_year, m.imdb_rating, m.imdb_votes,
      m.imdb_genres, m.method AS match_method, m.confidence AS match_confidence
    FROM titles t
    LEFT JOIN matches m
      ON m.kind = t.kind AND m.show_title = t.show_title
     AND m.language IS NOT DISTINCT FROM t.language
  ")

  # Countries carry no language: map each row to one title_id, preferring the
  # globally charting variant for the handful of ambiguous names.
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE country_title_map AS
    SELECT kind, show_title, coalesce(season_title, '') AS season_key,
           arg_max(title_id, coalesce(weeks_global, 0)) AS title_id
    FROM titles_out GROUP BY ALL
  ")

  copies <- c(
    titles = "SELECT * FROM titles_out ORDER BY kind, show_title, season_number",
    global = "
      SELECT g.week, g.kind, g.language, g.weekly_rank, t.title_id,
             g.show_title, g.season_title, g.weekly_hours_viewed, g.weekly_views,
             g.runtime, g.cumulative_weeks_in_top_10
      FROM global g
      JOIN titles_out t
        ON t.kind = g.kind AND t.language = g.language AND t.show_title = g.show_title
       AND t.season_title IS NOT DISTINCT FROM g.season_title
      ORDER BY g.week, g.kind, g.language, g.weekly_rank",
    countries = "
      SELECT c.country_iso2, c.country_name, c.week, c.kind, c.weekly_rank, m.title_id,
             c.show_title, c.season_title, c.cumulative_weeks_in_top_10
      FROM countries c
      JOIN country_title_map m
        ON m.kind = c.kind AND m.show_title = c.show_title
       AND m.season_key = coalesce(c.season_title, '')
      ORDER BY c.country_iso2, c.week, c.kind, c.weekly_rank",
    popular = "
      SELECT p.*, t.title_id
      FROM popular p
      LEFT JOIN titles_out t
        ON t.kind = p.kind AND t.language = p.language AND t.show_title = p.show_title
       AND t.season_title IS NOT DISTINCT FROM p.season_title
      ORDER BY p.kind, p.language, p.rank",
    episodes = "
      SELECT e.tconst, e.season, e.episode, e.episode_title, e.year, e.rating, e.votes
      FROM imdb_episodes e
      WHERE e.tconst IN (SELECT tconst FROM matches
                         WHERE kind = 'TV' AND imdb_type IN ('tvSeries', 'tvMiniSeries'))
        AND e.season BETWEEN 1 AND 60 AND e.episode BETWEEN 1 AND 400
        AND e.rating IS NOT NULL
      ORDER BY e.tconst, e.season, e.episode"
  )
  for (name in names(copies)) {
    DBI::dbExecute(con, sprintf(
      "COPY (%s) TO %s (FORMAT parquet, COMPRESSION zstd)",
      copies[[name]], out(paste0(name, ".parquet"))
    ))
  }

  DBI::dbExecute(con, sprintf("
    COPY (SELECT kind, language, show_title, tconst, imdb_title, imdb_type, imdb_year,
                 imdb_votes, method, confidence, round(score, 2) AS score, margin, n_candidates
          FROM matches ORDER BY kind, show_title, language)
    TO %s (HEADER, DELIMITER ',')", out("matches.csv")))

  write_meta(con, out_dir)
  validate_export(con, out_dir)
  invisible(out_dir)
}

#' Quote a path for a file that does not exist yet.
sql_path_new <- function(path) {
  path <- file.path(normalizePath(dirname(path), winslash = "/", mustWork = TRUE), basename(path))
  DBI::dbQuoteString(DBI::ANSI(), path)
}

write_meta <- function(con, out_dir) {
  q <- \(sql) DBI::dbGetQuery(con, sql)
  match_stats <- q("
    SELECT kind, coalesce(confidence, 'none') AS confidence, count(*) AS n
    FROM matches WHERE language IS NOT NULL GROUP BY ALL ORDER BY ALL")
  meta <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    latest_week = format(q("SELECT max(week) AS w FROM global")$w),
    first_week = format(q("SELECT min(week) AS w FROM global")$w),
    rows = list(
      global = q("SELECT count(*) AS n FROM global")$n,
      countries = q("SELECT count(*) AS n FROM countries")$n,
      titles = q("SELECT count(*) AS n FROM titles_out")$n
    ),
    match_stats_global_titles = match_stats,
    sources = list(
      netflix = list(url = "https://www.netflix.com/tudum/top10", note = "Netflix Top 10, weekly"),
      imdb = list(
        url = "https://developer.imdb.com/non-commercial-datasets/",
        note = "Information courtesy of IMDb (https://www.imdb.com). Used with permission for non-commercial use."
      )
    )
  )
  jsonlite::write_json(meta, file.path(out_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE)
}

#' Re-read the exported files and check they are complete and consistent.
validate_export <- function(con, out_dir) {
  f <- \(name) sql_path(file.path(out_dir, name))
  n <- \(sql) DBI::dbGetQuery(con, sql)$n

  checks <- list(
    "global.parquet has every global row" = n(sprintf(
      "SELECT count(*) AS n FROM %s", f("global.parquet"))) == n("SELECT count(*) AS n FROM global"),
    "countries.parquet has every country row" = n(sprintf(
      "SELECT count(*) AS n FROM %s", f("countries.parquet"))) == n("SELECT count(*) AS n FROM countries"),
    "title_id unique in titles.parquet" = n(sprintf(
      "SELECT count(*) - count(DISTINCT title_id) AS n FROM %s", f("titles.parquet"))) == 0,
    "every global row has a title" = n(sprintf(
      "SELECT count(*) AS n FROM %s g ANTI JOIN %s t USING (title_id)",
      f("global.parquet"), f("titles.parquet"))) == 0,
    "every country row has a title" = n(sprintf(
      "SELECT count(*) AS n FROM %s c ANTI JOIN %s t USING (title_id)",
      f("countries.parquet"), f("titles.parquet"))) == 0,
    "global hours reconcile with titles" = n(sprintf(
      "SELECT abs((SELECT sum(weekly_hours_viewed) FROM %s) - (SELECT sum(total_hours) FROM %s)) AS n",
      f("global.parquet"), f("titles.parquet"))) == 0,
    "episodes exist" = n(sprintf("SELECT count(*) AS n FROM %s", f("episodes.parquet"))) > 0
  )
  failed <- names(checks)[!unlist(checks)]
  if (length(failed) > 0) cli::cli_abort("Export validation failed: {failed}")
  cli::cli_inform(c("v" = "Export validated ({length(checks)} checks)."))
  invisible(TRUE)
}

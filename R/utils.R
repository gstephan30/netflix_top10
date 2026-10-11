#' Shared helpers for the Netflix Top 10 x IMDb pipeline.

user_agent_string <- "netflix-top10-dashboard/1.0 (+https://github.com/gstephan30/netflix_top10)"

#' Download a file atomically and verify it is what we expect.
#'
#' The file is written to a temporary path first and only moved into place
#' after the HTTP status, content type and size checks pass, so a failed or
#' hijacked download never overwrites a good cached copy.
#'
#' @param url Source URL (https only).
#' @param dest Destination path.
#' @param min_bytes Smallest plausible file size.
#' @param forbidden_types Content types that indicate an error page.
#' @param max_age_hours Re-use an existing `dest` younger than this.
#' @return `dest`, invisibly.
download_checked <- function(url,
                             dest,
                             min_bytes = 1000,
                             forbidden_types = c("text/html", "application/json"),
                             max_age_hours = 0) {
  stopifnot(startsWith(url, "https://"))

  if (file.exists(dest) && max_age_hours > 0) {
    age <- difftime(Sys.time(), file.mtime(dest), units = "hours")
    if (age < max_age_hours) {
      cli::cli_inform("Using cached {.file {basename(dest)}} ({round(age, 1)} h old).")
      return(invisible(dest))
    }
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest, ".part")
  on.exit(unlink(tmp), add = TRUE)

  cli::cli_inform("Downloading {.url {url}}")
  resp <- httr2::request(url) |>
    httr2::req_user_agent(user_agent_string) |>
    httr2::req_retry(max_tries = 4, backoff = \(attempt) 2^attempt) |>
    httr2::req_timeout(900) |>
    httr2::req_perform(path = tmp)

  type <- httr2::resp_content_type(resp)
  if (!is.na(type) && type %in% forbidden_types) {
    cli::cli_abort("{.url {url}} returned content type {.val {type}}, expected a data file.")
  }
  size <- file.size(tmp)
  if (is.na(size) || size < min_bytes) {
    cli::cli_abort("{.url {url}} returned only {size} bytes (expected >= {min_bytes}).")
  }

  if (!file.rename(tmp, dest)) {
    file.copy(tmp, dest, overwrite = TRUE)
  }
  invisible(dest)
}

#' Abort with a readable message when a schema does not match.
#'
#' Missing columns are fatal; extra columns only warn, so an additive change
#' upstream does not break the weekly build but still shows up in the log.
check_columns <- function(actual, required, source) {
  missing <- setdiff(required, actual)
  extra <- setdiff(actual, required)
  if (length(missing) > 0) {
    cli::cli_abort("{source}: missing column{?s} {.field {missing}}.")
  }
  if (length(extra) > 0) {
    cli::cli_warn("{source}: unexpected new column{?s} {.field {extra}} (ignored).")
  }
  invisible(TRUE)
}

#' Run a validation query that must return zero rows.
assert_no_rows <- function(con, sql, message) {
  bad <- DBI::dbGetQuery(con, sql)
  if (nrow(bad) > 0) {
    print(utils::head(bad, 10))
    cli::cli_abort("{message} ({nrow(bad)} offending row{?s}).")
  }
  invisible(TRUE)
}

#' SQL string literal for a file path (DuckDB accepts forward slashes everywhere).
sql_path <- function(path) {
  DBI::dbQuoteString(DBI::ANSI(), normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Register SQL macros used across the pipeline.
#'
#' `norm_title()` is the single definition of title normalisation used on
#' both the Netflix and the IMDb side: accents stripped, lower case,
#' `&` -> `and`, apostrophes dropped, punctuation collapsed to single spaces
#' and a leading article removed. Non-Latin scripts normalise to `''` and are
#' excluded from matching by the callers.
register_macros <- function(con) {
  DBI::dbExecute(con, "
    CREATE OR REPLACE MACRO norm_title(t) AS
      regexp_replace(
        trim(regexp_replace(
          regexp_replace(
            replace(lower(strip_accents(t)), '&', ' and '),
            '[''’`]', '', 'g'),
          '[^a-z0-9]+', ' ', 'g')),
        '^(the|a|an) ', '')
  ")
  # norm_title() key without filler words, for typo-level fuzzy comparison
  DBI::dbExecute(con, "
    CREATE OR REPLACE MACRO core_key(k) AS
      trim(regexp_replace(
        regexp_replace(' ' || k || ' ', ' (the|a|an|and|of|in|on|to) ', ' ', 'g'),
        ' (the|a|an|and|of|in|on|to) ', ' ', 'g'))
  ")
  # "Show: Season 2", "Show: Part 3", "Show: 2ª temporada", "Show 4"; NULL otherwise
  DBI::dbExecute(con, r"---(
    CREATE OR REPLACE MACRO season_number(t) AS
      TRY_CAST(coalesce(
        nullif(regexp_extract(t,
          '(?i)(?:season|part|volume|vol\.|series|book|chapter|collection|temporada|staffel|saison) (\d+)', 1), ''),
        nullif(regexp_extract(t, '(?i)(\d+)\S{0,2} (?:temporada|staffel|saison)', 1), ''),
        nullif(regexp_extract(t, '\s(\d{1,2})$', 1), '')
      ) AS INTEGER)
  )---")
  invisible(con)
}

#' Open an in-memory DuckDB connection.
#'
#' Extension auto-install is disabled so the pipeline never downloads code at
#' runtime; everything used (CSV, Parquet, regex) ships with the duckdb package.
connect_db <- function(temp_dir = tempdir()) {
  drv <- duckdb::duckdb(
    shared_home = FALSE,
    config = list(
      autoinstall_known_extensions = "false",
      autoload_known_extensions = "false",
      temp_directory = temp_dir,
      preserve_insertion_order = "false"
    )
  )
  con <- DBI::dbConnect(drv)
  register_macros(con)
  con
}

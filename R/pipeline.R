#!/usr/bin/env Rscript
#' Build the dashboard data set.
#'
#' Usage:
#'   Rscript R/pipeline.R            # download sources, build, validate
#'   Rscript R/pipeline.R --offline  # re-use files in cache/ (development)

for (f in c("utils.R", "netflix.R", "imdb.R", "export.R")) source(file.path("R", f))

run_pipeline <- function(cache_dir = "cache",
                         out_dir = "dashboard/data",
                         overrides_path = "data/imdb_overrides.csv",
                         offline = FALSE) {
  started <- Sys.time()
  netflix_dir <- file.path(cache_dir, "netflix")
  imdb_dir <- file.path(cache_dir, "imdb")

  con <- connect_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  cli::cli_h1("Netflix Top 10")
  if (!offline) fetch_netflix(netflix_dir)
  load_netflix(con, netflix_dir)
  build_titles(con)

  cli::cli_h1("IMDb")
  run_imdb(con, imdb_dir, overrides_path, fetch = !offline)

  cli::cli_h1("Export")
  export_dashboard_data(con, out_dir)

  elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "secs")))
  cli::cli_alert_success("Done in {elapsed} s.")
  invisible(out_dir)
}

if (sys.nframe() == 0L) {
  run_pipeline(offline = "--offline" %in% commandArgs(trailingOnly = TRUE))
}

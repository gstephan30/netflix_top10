#' IMDb: download, match to Netflix titles, episode ratings.
#'
#' Source: https://datasets.imdbws.com (refreshed daily). Licensed for
#' personal and non-commercial use; the dashboard credits IMDb accordingly.
#'
#' Matching strategy (replaces the old exact `primaryTitle %in%` filter):
#'
#' 1. Candidate generation - normalised exact key match (see `norm_title()`)
#'    of several Netflix spellings (show title, full season title, season title
#'    with the "Season 2" / "Limited Series" suffix removed) against IMDb
#'    `primaryTitle`, `originalTitle` and the English/worldwide AKAs.
#' 2. Fuzzy fallback - Jaro-Winkler on the normalised keys, only for titles
#'    with no exact candidate, blocked on type and year.
#' 3. Scoring - type compatibility, release year vs. first Netflix week
#'    (a title cannot chart before it exists), popularity (log votes) and the
#'    source of the key. The margin to the runner-up gives a confidence.
#' 4. Manual overrides in `data/imdb_overrides.csv` always win.

imdb_base_url <- "https://datasets.imdbws.com/"

imdb_files <- c(
  basics = "title.basics.tsv.gz",
  episode = "title.episode.tsv.gz",
  ratings = "title.ratings.tsv.gz",
  akas = "title.akas.tsv.gz"
)

imdb_types <- list(
  TV = c(tvSeries = 0, tvMiniSeries = 0, tvSpecial = 1.5, tvMovie = 2),
  Film = c(movie = 0, tvMovie = 0.5, tvSpecial = 0.5, video = 1, tvMiniSeries = 2.5, tvSeries = 3)
)

fetch_imdb <- function(dir, max_age_hours = 20) {
  for (f in imdb_files) {
    download_checked(
      url = paste0(imdb_base_url, f),
      dest = file.path(dir, f),
      min_bytes = 1e6,
      max_age_hours = max_age_hours
    )
  }
  invisible(dir)
}

#' Read an IMDb TSV. IMDb uses `\N` for NULL and never quotes fields.
imdb_read_sql <- function(dir, name) {
  sprintf(
    "read_csv(%s, delim = '\t', header = true, quote = '', escape = '',
              nullstr = '\\N', all_varchar = true)",
    sql_path(file.path(dir, imdb_files[[name]]))
  )
}

#' Netflix-side match units and their lookup keys.
#'
#' Unit = (kind, language, show_title): a TV show is matched once (all its
#' seasons share the IMDb series), a film once.
build_match_keys <- function(con) {
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE match_units AS
    SELECT
      substr(md5(concat_ws('|', kind, language, show_title)), 1, 12) AS unit_id,
      kind, language, show_title,
      min(first_week)                                    AS first_week,
      -- earliest season seen on Netflix and the week it first charted: used to
      -- check that the IMDb series has that season and that it aired back then
      min(coalesce(season_number, 1))                    AS anchor_season,
      arg_min(first_week, coalesce(season_number, 1))    AS anchor_first_week,
      max(runtime) FILTER (kind = 'Film')                AS runtime_hours,
      list(DISTINCT season_title) FILTER (season_title IS NOT NULL) AS season_titles,
      sum(coalesce(total_views, 0))                      AS total_views,
      sum(country_weeks)                                 AS country_weeks
    FROM titles
    GROUP BY kind, language, show_title
  ")
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE match_keys AS
    WITH raw AS (
      SELECT unit_id, show_title AS variant, 'show' AS key_source FROM match_units
      UNION ALL
      SELECT unit_id, unnest(season_titles), 'season' FROM match_units
      UNION ALL
      -- season title without the season / part / limited-series suffix
      SELECT unit_id,
             regexp_replace(unnest(season_titles),
               '(?i)[:\\-–]\\s*(limited series|miniseries|season \\d+|part \\d+|volume \\d+|vol\\. \\d+|book \\d+|chapter \\d+|collection \\d+|series \\d+)\\s*$', ''),
             'season_base'
      FROM match_units
      UNION ALL
      -- 'Show: Subtitle' -> 'Subtitle' (e.g. anthology seasons)
      SELECT unit_id, regexp_extract(unnest(season_titles), '^[^:]+:\\s*(.+)$', 1), 'season_sub'
      FROM match_units
      UNION ALL
      -- Netflix packaging: 'Beast (English)', 'Jawan: Extended Cut', '... (Reloaded Version)'
      SELECT unit_id,
             regexp_replace(show_title,
               '(?i)\\s*(\\([^)]*\\)|[:\\-]\\s*(extended cut|extended look|reloaded version))\\s*$', ''),
             'show_base'
      FROM match_units
    ),
    keyed AS (
      SELECT DISTINCT unit_id, norm_title(variant) AS key, key_source
      FROM raw
      WHERE variant IS NOT NULL AND length(norm_title(variant)) >= 2
    )
    SELECT *, replace(key, ' ', '') AS ckey FROM keyed
  ")
  invisible(con)
}

#' Load the IMDb titles that can be candidates at all.
load_imdb <- function(con, dir, min_titles = 1e6) {
  all_types <- unique(unlist(lapply(imdb_types, names)))
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TABLE imdb_titles AS
    SELECT
      b.tconst, b.titleType AS title_type, b.primaryTitle AS primary_title,
      b.originalTitle AS original_title,
      TRY_CAST(b.startYear AS INTEGER) AS start_year,
      TRY_CAST(b.endYear AS INTEGER)   AS end_year,
      TRY_CAST(b.runtimeMinutes AS INTEGER) AS runtime_minutes,
      b.genres,
      TRY_CAST(r.averageRating AS DOUBLE) AS rating,
      coalesce(TRY_CAST(r.numVotes AS INTEGER), 0) AS votes
    FROM %s b
    LEFT JOIN %s r USING (tconst)
    WHERE b.isAdult = '0' AND b.titleType IN (%s)
      AND TRY_CAST(b.startYear AS INTEGER) >= 1920",
    imdb_read_sql(dir, "basics"), imdb_read_sql(dir, "ratings"),
    paste(DBI::dbQuoteString(con, all_types), collapse = ", ")
  ))
  n <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM imdb_titles")$n
  if (n < min_titles) cli::cli_abort("IMDb basics: only {n} candidate titles; file looks truncated.")

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE imdb_keys AS
    WITH k AS (
      SELECT tconst, norm_title(primary_title) AS key, 'primary' AS imdb_source
      FROM imdb_titles
      UNION
      SELECT tconst, norm_title(original_title), 'original'
      FROM imdb_titles WHERE original_title <> primary_title
    )
    SELECT *, replace(key, ' ', '') AS ckey FROM k WHERE key <> ''
  ")
  # AKAs: only rows whose key can match something - semi-join first, it is 50M+ rows
  DBI::dbExecute(con, sprintf("
    INSERT INTO imdb_keys
    SELECT DISTINCT a.titleId, norm_title(a.title), 'aka', replace(norm_title(a.title), ' ', '')
    FROM %s a
    WHERE (a.region IN ('US', 'GB', 'XWW', 'CA', 'AU', 'IN') OR a.language = 'en')
      AND replace(norm_title(a.title), ' ', '') IN (SELECT ckey FROM match_keys)
      AND a.titleId IN (SELECT tconst FROM imdb_titles)",
    imdb_read_sql(dir, "akas")
  ))
  invisible(con)
}

#' Candidate (unit, tconst) pairs: exact key matches plus a fuzzy fallback.
build_candidates <- function(con) {
  DBI::dbWriteTable(con, "type_penalties", overwrite = TRUE, data.frame(
    kind = rep(names(imdb_types), lengths(imdb_types)),
    title_type = unlist(lapply(imdb_types, names), use.names = FALSE),
    type_penalty = unlist(imdb_types, use.names = FALSE)
  ))

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE candidates AS
    SELECT
      k.unit_id, i.tconst,
      min(CASE k.key_source WHEN 'show' THEN 0 WHEN 'season' THEN 0
                            WHEN 'show_base' THEN 0.25 WHEN 'season_base' THEN 0.25 ELSE 0.5 END
        + CASE i.imdb_source WHEN 'primary' THEN 0 WHEN 'original' THEN 0.25 ELSE 1.5 END
        -- equal only without spaces ('Dan Da Dan' / 'Dandadan')
        + CASE WHEN k.key = i.key THEN 0 ELSE 0.5 END
      ) AS key_penalty,
      'exact' AS method
    FROM match_keys k
    JOIN imdb_keys i USING (ckey)
    GROUP BY ALL
  ")

  # Fuzzy fallback: only for units without any type-compatible exact candidate.
  # Blocked on the first two characters and restricted to titles with some
  # traction (votes), which keeps it to well under a million comparisons.
  DBI::dbExecute(con, "
    INSERT INTO candidates
    WITH open_units AS (
      SELECT u.unit_id, u.kind, k.key
      FROM match_units u
      JOIN match_keys k USING (unit_id)
      WHERE k.key_source IN ('show', 'season_base') AND length(k.key) >= 4
        AND u.unit_id NOT IN (
          SELECT c.unit_id FROM candidates c
          JOIN imdb_titles t USING (tconst)
          JOIN match_units mu USING (unit_id)
          JOIN type_penalties tp ON tp.kind = mu.kind AND tp.title_type = t.title_type
        )
    ),
    pool AS (
      SELECT i.tconst, i.key, tp.kind
      FROM imdb_keys i
      JOIN imdb_titles t USING (tconst)
      JOIN type_penalties tp ON tp.title_type = t.title_type
      WHERE i.imdb_source <> 'aka' AND t.votes >= 200 AND length(i.key) >= 4
    )
    SELECT unit_id, tconst,
           min(1 + 20 * (1 - jaro_winkler_similarity(o.key, p.key))) AS key_penalty,
           'fuzzy' AS method
    FROM open_units o
    JOIN pool p ON p.kind = o.kind AND left(p.key, 2) = left(o.key, 2)
    -- similar overall AND at most a typo apart once filler words are ignored:
    -- accepts 'Ante Sundaraniki' ~ 'Ante Sundharaniki', 'Maze Runner: Death Cure'
    -- ~ 'Maze Runner: The Death Cure'; rejects 'Lady Di' ~ 'Lady Bird'
    WHERE jaro_winkler_similarity(o.key, p.key) >= 0.92
      AND levenshtein(core_key(o.key), core_key(p.key)) <= 2
    GROUP BY ALL
  ")
  invisible(con)
}

#' Episodes of every TV candidate (plus ratings and air year).
#'
#' Used twice: season start years feed the match score, and the episodes of
#' the final matches become the episode heatmap in the dashboard.
load_imdb_episodes <- function(con, dir) {
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TABLE imdb_episodes AS
    WITH parents AS (
      SELECT DISTINCT c.tconst
      FROM candidates c JOIN imdb_titles t USING (tconst)
      WHERE t.title_type IN ('tvSeries', 'tvMiniSeries')
    ),
    e AS (
      SELECT tconst AS episode_tconst, parentTconst AS tconst,
             TRY_CAST(seasonNumber AS INTEGER)  AS season,
             TRY_CAST(episodeNumber AS INTEGER) AS episode
      FROM %s
      WHERE parentTconst IN (SELECT tconst FROM parents)
    )
    SELECT e.*, b.primaryTitle AS episode_title,
           TRY_CAST(b.startYear AS INTEGER) AS year,
           TRY_CAST(r.averageRating AS DOUBLE) AS rating,
           TRY_CAST(r.numVotes AS INTEGER) AS votes
    FROM e
    LEFT JOIN (SELECT tconst, primaryTitle, startYear FROM %s
               WHERE titleType = 'tvEpisode') b ON b.tconst = e.episode_tconst
    LEFT JOIN %s r ON r.tconst = e.episode_tconst",
    imdb_read_sql(dir, "episode"), imdb_read_sql(dir, "basics"), imdb_read_sql(dir, "ratings")
  ))
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE imdb_seasons AS
    SELECT tconst, season, min(year) AS first_year, count(*) AS n_episodes
    FROM imdb_episodes WHERE season IS NOT NULL
    GROUP BY ALL
  ")
  invisible(con)
}

#' Score every candidate and keep the best one per unit.
#'
#' score = 2 * log10(min(votes, 100k)) popularity (charting titles are watched)
#'       + language                     Non-English <=> IMDb original title differs
#'       - type_penalty                 e.g. a Netflix film matched to a tvSeries
#'       - key_penalty                  aka / season-derived / fuzzy key
#'       + film: release year vs first chart week, runtime vs Netflix runtime
#'       + TV:   IMDb has the anchor season, and it aired when it charted
#'
#' A title can never chart before it exists: candidates released more than a
#' year after the first Netflix week are dropped. Older films lose a little
#' per year (new Netflix films often share names with famous classics); older
#' TV seasons lose at most 1 point, because licensed catalogue shows (Suits,
#' season 1 from 2011, charting in 2021) are common.
score_candidates <- function(con) {
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE candidate_scores AS
    WITH c AS (
      SELECT
        c.unit_id, c.tconst, c.key_penalty, c.method,
        u.kind, u.anchor_season, u.runtime_hours,
        year(u.first_week)        AS y_first,
        year(u.anchor_first_week) AS y_anchor,
        u.language,
        t.title_type, t.primary_title, t.original_title, t.start_year, t.runtime_minutes,
        t.votes, t.rating,
        tp.type_penalty,
        s.first_year              AS season_year,
        m.max_season
      FROM candidates c
      JOIN match_units u USING (unit_id)
      JOIN imdb_titles t USING (tconst)
      JOIN type_penalties tp ON tp.kind = u.kind AND tp.title_type = t.title_type
      LEFT JOIN imdb_seasons s ON s.tconst = c.tconst AND s.season = u.anchor_season
      LEFT JOIN (SELECT tconst, max(season) AS max_season FROM imdb_seasons GROUP BY ALL) m
        ON m.tconst = c.tconst
      WHERE t.start_year <= year(u.first_week) + 1
    ),
    parts AS (
      SELECT *,
        -- capped: beyond ~100k votes popularity says nothing more about identity
        2 * log10(least(votes, 100000) + 1) AS pop_score,
        -- Netflix 'Non-English' <=> IMDb original title differs from the primary title
        CASE
          WHEN language IS NULL THEN 0
          WHEN (language = 'Non-English') = (original_title <> primary_title) THEN 0.75
          ELSE -0.75
        END AS language_score,
        CASE
          WHEN kind <> 'Film' THEN 0
          WHEN start_year > y_first THEN -2
          ELSE -0.15 * least(y_first - start_year, 20)
        END AS year_score,
        CASE
          WHEN kind <> 'Film' OR runtime_hours IS NULL OR runtime_minutes IS NULL THEN 0
          WHEN abs(runtime_hours * 60 - runtime_minutes) <= 3  THEN 2.5
          WHEN abs(runtime_hours * 60 - runtime_minutes) <= 8  THEN 1.5
          WHEN abs(runtime_hours * 60 - runtime_minutes) <= 15 THEN 0
          ELSE -least(abs(runtime_hours * 60 - runtime_minutes) / 15, 4)
        END AS runtime_score,
        CASE
          WHEN kind <> 'TV' THEN 0
          WHEN season_year IS NOT NULL THEN
            CASE
              WHEN abs(y_anchor - season_year) <= 1 THEN 2
              WHEN season_year > y_anchor THEN -3
              ELSE -least(0.15 * (y_anchor - season_year), 1)
            END
          WHEN max_season IS NOT NULL AND max_season < anchor_season THEN -2
          WHEN start_year > y_anchor THEN -2
          ELSE -least(0.15 * (y_anchor - start_year), 1)
        END AS season_score
      FROM c
    )
    SELECT *,
      pop_score + language_score + year_score + runtime_score + season_score
        - type_penalty - key_penalty AS score
    FROM parts
  ")

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE matches_auto AS
    WITH ranked AS (
      SELECT *,
        row_number() OVER w AS rn,
        nth_value(score, 2) OVER (w ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)
          AS runner_up,
        count(*) OVER (PARTITION BY unit_id) AS n_candidates
      FROM candidate_scores
      WINDOW w AS (PARTITION BY unit_id ORDER BY score DESC, votes DESC, tconst)
    )
    SELECT
      unit_id, tconst, method, score,
      round(least(score - coalesce(runner_up, score - 10), 10), 2) AS margin,
      n_candidates,
      runtime_score, season_score, votes
    FROM ranked
    -- a fuzzy title match needs strong other evidence to count at all
    WHERE rn = 1 AND NOT (method = 'fuzzy' AND score < 6)
  ")
  DBI::dbExecute(con, "
    ALTER TABLE matches_auto ADD COLUMN confidence VARCHAR;
    UPDATE matches_auto SET confidence = CASE
      WHEN votes >= 500 AND margin >= 2.5
           AND (runtime_score >= 1.5 OR season_score >= 2 OR margin >= 5) THEN 'high'
      WHEN votes >= 50 AND margin >= 1 THEN 'medium'
      ELSE 'low'
    END;
    UPDATE matches_auto SET confidence = 'medium'
    WHERE method = 'fuzzy' AND confidence = 'high';
  ")
  invisible(con)
}

#' Apply manual overrides and assemble the final match table.
#'
#' `overrides_path` is a CSV with columns kind, show_title, tconst, note.
#' `tconst = none` removes a wrong automatic match.
finalize_matches <- function(con, overrides_path) {
  overrides <- utils::read.csv(overrides_path, colClasses = "character", encoding = "UTF-8")
  check_columns(names(overrides), c("kind", "show_title", "tconst", "note"), overrides_path)
  bad <- overrides$tconst[!grepl("^(tt\\d{7,9}|none)$", overrides$tconst)]
  if (length(bad) > 0) cli::cli_abort("Invalid tconst in overrides: {.val {bad}}")
  DBI::dbWriteTable(con, "overrides", overrides, overwrite = TRUE)

  unknown <- DBI::dbGetQuery(con, "
    SELECT * FROM overrides
    WHERE tconst <> 'none' AND tconst NOT IN (SELECT tconst FROM imdb_titles)")
  if (nrow(unknown) > 0) {
    cli::cli_warn("Override tconst{?s} not in IMDb title set: {.val {unknown$tconst}}")
  }
  unused <- DBI::dbGetQuery(con, "
    SELECT * FROM overrides o
    WHERE NOT EXISTS (SELECT 1 FROM match_units u
                      WHERE u.kind = o.kind AND u.show_title = o.show_title)")
  if (nrow(unused) > 0) {
    cli::cli_warn("Override{?s} without a Netflix title: {.val {unused$show_title}}")
  }

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE matches AS
    WITH o AS (
      SELECT u.unit_id, o.tconst
      FROM overrides o JOIN match_units u USING (kind, show_title)
    ),
    combined AS (
      SELECT unit_id, tconst, 'manual' AS method, CAST(NULL AS DOUBLE) AS score,
             CAST(NULL AS DOUBLE) AS margin, CAST(NULL AS BIGINT) AS n_candidates,
             'manual' AS confidence
      FROM o WHERE tconst <> 'none'
      UNION ALL
      SELECT unit_id, tconst, method, score, margin, n_candidates, confidence
      FROM matches_auto WHERE unit_id NOT IN (SELECT unit_id FROM o)
    )
    SELECT
      u.unit_id, u.kind, u.language, u.show_title,
      c.tconst, t.primary_title AS imdb_title, t.title_type AS imdb_type,
      t.start_year AS imdb_year, t.rating AS imdb_rating, t.votes AS imdb_votes,
      t.genres AS imdb_genres,
      c.method, c.confidence, c.score, c.margin, c.n_candidates
    FROM match_units u
    LEFT JOIN combined c USING (unit_id)
    LEFT JOIN imdb_titles t USING (tconst)
  ")
  invisible(con)
}

#' Full IMDb stage.
run_imdb <- function(con, dir, overrides_path, fetch = TRUE, min_titles = 1e6) {
  if (fetch) fetch_imdb(dir)
  build_match_keys(con)
  load_imdb(con, dir, min_titles)
  build_candidates(con)
  load_imdb_episodes(con, dir)
  score_candidates(con)
  finalize_matches(con, overrides_path)
  invisible(con)
}

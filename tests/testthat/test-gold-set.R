# Integration check on the real build output against a hand-verified gold set
# (tests/testthat/fixtures/imdb_gold.csv). Runs after `Rscript R/pipeline.R`.

matches_path <- file.path(root_dir, "dashboard", "data", "matches.csv")

test_that("IMDb matches agree with the hand-verified gold set", {
  skip_if_not(file.exists(matches_path), "pipeline output not built")
  matches <- utils::read.csv(matches_path, encoding = "UTF-8")

  gold <- utils::read.csv(test_path("fixtures", "imdb_gold.csv"), encoding = "UTF-8")

  for (i in seq_len(nrow(gold))) {
    g <- gold[i, ]
    got <- matches$tconst[matches$kind == g$kind & matches$show_title == g$show_title]
    if (g$expect == "match") {
      expect(
        length(got) > 0 && g$tconst %in% got,
        sprintf("%s '%s': expected %s, got %s", g$kind, g$show_title, g$tconst,
                paste(got, collapse = ", "))
      )
    } else {
      expect(
        !(g$tconst %in% got),
        sprintf("%s '%s': must not match %s", g$kind, g$show_title, g$tconst)
      )
    }
  }
})

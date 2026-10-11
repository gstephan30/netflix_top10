# Netflix Top 10 × IMDb

Interactive dashboard that combines the weekly [Netflix Top 10](https://www.netflix.com/tudum/top10)
with [IMDb ratings](https://developer.imdb.com/non-commercial-datasets/), rebuilt every week by GitHub Actions.

**Live:** https://gstephan30.github.io/netflix_top10/

- **Weekly Top 10** – any week since July 2021, globally or for 94 countries, with views, hours and IMDb rating
- **This week** – ggplot charts of the current global Top 10: weekly views, Netflix vs IMDb, episode rating grids
- **Title explorer** – weekly views per season, rank heatmap per country, IMDb rating per episode
- **Netflix vs IMDb** – does what people rate predict what they watch?
- **All-time ranking** – per season / film, never summed across seasons

## How it works

```
Netflix TSVs ──┐                                   ┌─ titles / global / countries / episodes .parquet
               ├─ R + DuckDB: validate → model → match → export ─┤
IMDb TSVs  ────┘                                   └─ matches.csv, meta.json
                                                                 │
                         Quarto dashboard (Observable JS + DuckDB-WASM, static) ◄─┘
```

| Step | Code | Notes |
|---|---|---|
| Download | `R/utils.R` `download_checked()` | atomic, retries, rejects HTML error pages |
| Netflix | `R/netflix.R` | schema check (missing column = fail, new column = warning), typed casts that fail on bad values, ranks 1–10 per week, Sundays only, row-count floor |
| Metrics | `build_titles()` | one row per season / film; views = Netflix's metric since June 2023 |
| IMDb matching | `R/imdb.R` | scoring model, see below |
| Export | `R/export.R` | Parquet (zstd) + reconciliation checks |
| Charts | `R/charts.R` | ggplot2 + patchwork, Schoolbell font (`dashboard/fonts/`), rendered at build time |
| Dashboard | `dashboard/index.qmd` | static site, queries run in the browser |

### Matching Netflix titles to IMDb

Netflix publishes titles only. Instead of an exact `primaryTitle` lookup, each show / film gets candidates and a score:

1. **Candidates** – one normalisation (`norm_title()` SQL macro: accents, case, `&`→`and`, apostrophes, punctuation, leading article) for the show title, every season title and its season-less variant (`DAHMER: Monster: The Jeffrey Dahmer Story` → IMDb *Dahmer - Monster: …*), Netflix packaging stripped (`Beast (English)`, `Jawan: Extended Cut`), compared against IMDb primary, original and English/worldwide AKA titles, also without spaces (`DAN DA DAN` = *Dandadan*). A typo-level fuzzy fallback (Jaro-Winkler ≥ 0.92 *and* ≤ 2 edits ignoring filler words) covers the rest.
2. **Evidence** – IMDb type fits; a title cannot be released after it charted; film runtime vs Netflix runtime; for series the charting season must exist on IMDb and have aired around then; Netflix "Non-English" must agree with an IMDb original title in another language; popularity (votes, capped at 100k).
3. **Confidence** – margin to the runner-up → `high` / `medium` / `low`.
4. **Overrides** – `data/imdb_overrides.csv` (`tconst = none` removes a match).

Quality is checked against a hand-verified gold set (`tests/testthat/fixtures/imdb_gold.csv`, including known traps such as AKA collisions) on every build.

## Run locally

Requirements: R ≥ 4.5, [Quarto](https://quarto.org) ≥ 1.9.

```r
renv::restore()                      # exact package versions from renv.lock
```

```sh
Rscript R/pipeline.R                 # download + build (~1 GB IMDb download, ~30 s compute)
Rscript R/pipeline.R --offline       # rebuild from cache/ without downloading
Rscript -e 'testthat::test_dir("tests/testthat")'
quarto render dashboard              # or: quarto preview dashboard
```

## Data & licences

Netflix Top 10 data © Netflix. Information courtesy of IMDb (https://www.imdb.com), used with permission for personal and non-commercial use. This project is non-commercial.

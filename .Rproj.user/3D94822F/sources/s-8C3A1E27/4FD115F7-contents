download_current <- function() {
  heute <- gsub("-", "", Sys.Date())
  download.file("https://top10.netflix.com/data/all-weeks-global.tsv",
                destfile = paste0("data_raw/", heute, "_weeks_global.tsv"),
                mode = "wb")
  
  download.file("https://top10.netflix.com/data/all-weeks-countries.tsv",
                destfile = paste0("data_raw/", heute, "_weeks_countries.tsv"),
                mode = "wb")
  
  download.file("https://top10.netflix.com/data/most-popular.tsv",
                destfile = paste0("data_raw/", heute, "_popular.tsv"),
                mode = "wb")
}

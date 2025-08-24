#!/usr/bin/env Rscript

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE, quiet = TRUE)
  pkgs <- c(
    "tidyverse",
    "lubridate",
    "zoo",
    "yaml",
    "refund",
    "fdapace",
    "roahd",
    "fdaoutlier",
    "future.apply",
    "roxygen2",
    "pkgdown",
    "devtools",
    "quarto"
  )
  install.packages(pkgs, repos = "https://cloud.r-project.org")
  renv::snapshot(prompt = FALSE)
} else {
  renv::restore(prompt = FALSE)
}

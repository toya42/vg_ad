load_config <- function(path = "config.yaml") {
  stopifnot(requireNamespace("yaml", quietly = TRUE))
  cfg <- yaml::read_yaml(path)
  # ensure dirs
  out_dir <- cfg$runtime$output_dir
  log_dir <- cfg$runtime$log_dir
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  cfg
}

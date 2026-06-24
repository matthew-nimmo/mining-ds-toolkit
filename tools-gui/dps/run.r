# Run the DPS pipeline from the root directory using targets-only workflows.
# Usage: Rscript run.r [data_path] [output_catalogue]

args <- commandArgs(trailingOnly = TRUE)
data_path <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "./data"
output_catalogue <- if (length(args) >= 2 && nzchar(args[2])) args[2] else "./output/catalogue"

cat("Running DPS targets-only pipeline with:\n")
cat("  data_path:", data_path, "\n")
cat("  output_catalogue:", output_catalogue, "\n")

if (!dir.exists(data_path)) {
  stop(sprintf("data_path does not exist: %s\nCreate the folder or set a valid path.", data_path))
}

output_dir <- dirname(output_catalogue)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. install.packages('targets')")
}
if (!requireNamespace("RSQLite", quietly = TRUE)) {
  stop("Package 'RSQLite' is required. install.packages('RSQLite')")
}

pipeline_file <- tempfile(fileext = ".R")
writeLines(c(
  "library(targets)",
  "library(RSQLite)",
  "Sys.setenv(`_R_USE_PIPEBIND_` = TRUE)",
  "tar_option_set(packages = c('dplyr','readr','readxl','writexl','univariateML',",
  "                            'RSQLite','reticulate'))",
  "source_files <- list(",
  "  'clope.R','datafile.R','datafile_class.R',",
  "  'datafile_csv.R','datafile_unkwn.R','datafile_xls.R',",
  "  'dataset.R','detect_colnames.R','detect_limit.R',",
  "  'detect_method.R','detect_rownames.R','detect_units.R',",
  "  'dps.R','dps_cluster.R','dps_datafiles.R',",
  "  'dps_export.R','dps_extract.R','dps_metadata.R',",
  "  'dps_profile.R','dps_score.R','encode_format.R',",
  "  'has_unit.R','metadata.R','new_score.R',",
  "  'plot_donut.R','profile_data.R','proj_list.R',",
  "  'score_accessibility.R','score_accuracy.R','score_completeness.R',",
  "  'score_complexity.R','score_consistency.R','score_coverage.R',",
  "  'score_freshness.R','score_metadata.R','score_skewness.R',",
  "  'score_uniqueness.R','score_usability.R','value_class.R',",
  "  'value_format.R'",
  ")",
  "paste0('./r/', source_files) |>",
  "  lapply(source)",
  "list(",
  sprintf("  tar_target(dps_client_files, '%s'),", data_path),
  sprintf("  tar_target(dps_db_file, '%s'),", output_catalogue),
  "  tar_target(dps_pipeline, dps(dps_client_files) |> dps_export(dps_db_file))",
  ")"
), con = pipeline_file)

cat("Using temporary targets script:", pipeline_file, "\n")

# Execute pipeline
tryCatch({
  tar_make(script = pipeline_file)
  cat("dps targets-only pipeline complete.\n")
}, finally = {
  unlink(pipeline_file)
})

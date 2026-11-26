#!/usr/bin/env Rscript

# Run this file from the package root: Rscript run_analysis.R

# Check the packages needed to read the archive and recreate the outputs.
required_packages <- c(
  "dplyr", "readr", "tidyr", "ggplot2", "patchwork", "scales", "tibble"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install the following R packages before running the analysis: ",
    paste(missing_packages, collapse = ", "),
    "."
  )
}

# The scripts are deliberately listed in the order they should be run.
analysis_scripts <- c(
  "analysis/plot_figures.R",
  "analysis/make_tables.R",
  "analysis/plot_cstar_robustness.R"
)

for (analysis_script in analysis_scripts) {
  cat("\nRunning ", analysis_script, "\n", sep = "")
  exit_status <- system2(
    file.path(R.home("bin"), "Rscript"),
    analysis_script
  )
  if (exit_status != 0L) {
    stop(analysis_script, " failed with status ", exit_status, ".")
  }
}

cat("\nAll simulation figures and tables were regenerated.\n")

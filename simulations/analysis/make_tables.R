#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# Run this file from the package root: Rscript analysis/make_tables.R

# Paths and included results ---------------------------------------------------

project_root <- normalizePath(".")
results_dir <- file.path(project_root, "results", "simulation")
table_dir <- file.path(project_root, "results", "tables")

source(file.path(project_root, "analysis", "sim_results_helpers.R"))

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

results <- read_included_results(results_dir)

cat("Loaded ", nrow(results), " rows from ",
    dplyr::n_distinct(results$.source_file), " result files.\n", sep = "")

# Online Appendix Table S4: OGA selection counts -------------------------------

oga_selection <- summarize_oga_selection(results)
write_csv(oga_selection, file.path(table_dir, "oga_selection_by_dgp.csv"))

write_latex_table(
  data = oga_selection,
  file = file.path(table_dir, "oga_selection_by_dgp.tex"),
  caption = "Median number of selected covariates",
  label = "tab:app_sim_numselect",
  alignment = "lrrr",
  notes = paste(
    "Entries report numbers of lagged covariates selected by the proposed method.",
    "Shock Eq. reports the median number selected in the common shock equation.",
    "Outcome Eq. and Selected Union report the median across all replications and horizons, with the corresponding minimum--maximum range in parentheses.",
    "Selected Union is the union of shock- and outcome-selected lagged covariates.",
    "Always-included controls and the intercept are not counted."
  )
)

if (nrow(oga_selection) > 0L) {
  cat("\nOGA selection table:\n")
  print(oga_selection, n = Inf)
} else {
  cat("\nSkipped OGA selection table: OGA rows or selection-count columns were not found.\n")
}

# Online Appendix Table S3: estimator runtime ---------------------------------

runtime_table <- summarize_runtime(results)
write_csv(runtime_table, file.path(table_dir, "runtime_by_method.csv"))

runtime_latex <- runtime_table %>%
  dplyr::transmute(DGP, Method, `Median sec.`, `IQR sec.`, N = `n_rep`)

write_latex_table(
  data = runtime_latex,
  file = file.path(table_dir, "runtime_by_method.tex"),
  caption = "Estimator runtime",
  label = "tab:app_sim_runtime",
  alignment = "llrrr",
  notes = paste(
    "Entries are wall-clock seconds per Monte Carlo replication, computed from the estimator-level elapsed time recorded in the simulation output.",
    "The IQR column reports the 25th--75th percentile range.",
    "Runtime comparisons should be read with the bootstrap and tuning settings used for each estimator."
  )
)

if (nrow(runtime_table) > 0L) {
  cat("\nRuntime table:\n")
  print(runtime_table, n = Inf)
} else {
  cat("\nSkipped runtime table: elapsed_seconds was not available.\n")
}

cat("\nWrote tables to ", table_dir, "\n", sep = "")

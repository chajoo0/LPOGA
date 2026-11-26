# Shared data definitions and summary functions for the simulation results.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# -----------------------------------------------------------------------------
# Methods and simulation designs
# -----------------------------------------------------------------------------

method_information <- tibble::tribble(
  ~method,   ~method_label, ~method_group,        ~group_label,                        ~method_order,
  "ols",     "OLS",         "baseline",           "Low-dimensional methods",            1L,
  "lalp",    "LALP",        "baseline",           "Low-dimensional methods",            2L,
  "aic_bc",  "AIC-BC",      "baseline",           "Low-dimensional methods",            3L,
  "falp",    "FALP",        "factor_aggregation", "Factor-based / aggregation methods", 4L,
  "favar",   "FAVAR",       "factor_aggregation", "Factor-based / aggregation methods", 5L,
  "rslp",    "RSLP",        "factor_aggregation", "Factor-based / aggregation methods", 6L,
  "lava",    "LAVA",        "variable_selection", "Coordinate/shrinkage methods",       7L,
  "dlasso",  "dLASSO",      "variable_selection", "Coordinate/shrinkage methods",       8L,
  "oga",     "OGA",         "variable_selection", "Coordinate/shrinkage methods",       9L
)

design_information <- tibble::tribble(
  ~dgp_key,  ~design,                                       ~dgp_label,
  "shock",   "shock_contamination_full_panel_polynomial",   "Shock contamination",
  "outcome", "outcome_nuisance_ar1",                        "Outcome nuisance",
  "factor",  "factor_contamination_restricted_null",        "Factor contamination"
)

method_group_order <- c(
  "Low-dimensional methods",
  "Factor-based / aggregation methods",
  "Coordinate/shrinkage methods"
)

method_display_order <- method_information$method_label

method_colors <- c(
  "OLS" = "#EF476F",
  "LALP" = "#B5179E",
  "AIC-BC" = "#F94144",
  "FALP" = "#43AA8B",
  "FAVAR" = "#A7C957",
  "RSLP" = "#F9C74F",
  "LAVA" = "#F9844A",
  "dLASSO" = "#90CAF9",
  "OGA" = "#4361EE"
)

method_shapes <- c(
  "OLS" = 16,
  "LALP" = 17,
  "AIC-BC" = 15,
  "FALP" = 18,
  "FAVAR" = 19,
  "RSLP" = 8,
  "LAVA" = 16,
  "dLASSO" = 18,
  "OGA" = 17
)

method_linetypes <- c(
  "OLS" = "solid",
  "LALP" = "dashed",
  "AIC-BC" = "dotdash",
  "FALP" = "solid",
  "FAVAR" = "dashed",
  "RSLP" = "dotdash",
  "LAVA" = "solid",
  "dLASSO" = "dashed",
  "OGA" = "dotdash"
)

# These are the nine included file families used in the three main figures.
# Each family contains files job001 through job050.
included_file_stems <- c(
  "dfm_shock_contamination_poly_unionvhat_methods-ols-aic_bc-lalp-falp-oga-dlasso",
  "dfm_shock_contamination_poly_methods-favar-rslp",
  "dfm_shock_contamination_poly_poly150_lava_point_methods-lava",
  "dfm_outcome_nuisance_kz20_methods-ols-aic_bc-lalp-falp-oga-dlasso",
  "dfm_outcome_nuisance_kz20_methods-favar-rslp",
  "dfm_outcome_nuisance_kz20_lava_point_methods-lava",
  "dfm_factor_contamination_unionvhat_methods-ols-aic_bc-lalp-falp-oga-dlasso",
  "dfm_factor_contamination_methods-favar-rslp",
  "dfm_factor_contamination_factor_lava_point_methods-lava"
)

# -----------------------------------------------------------------------------
# Read the included Monte Carlo results
# -----------------------------------------------------------------------------

read_included_results <- function(results_dir) {
  files_by_family <- lapply(included_file_stems, function(file_stem) {
    pattern <- paste0("^", file_stem, "_job[0-9]{3}[.]csv$")
    files <- sort(list.files(results_dir, pattern = pattern, full.names = TRUE))

    if (length(files) != 50L) {
      stop(
        "Expected 50 files for ", file_stem, ", but found ", length(files), "."
      )
    }
    files
  })

  included_files <- unlist(files_by_family, use.names = FALSE)
  cat("Reading ", length(included_files), " included result files.\n", sep = "")

  results <- bind_rows(lapply(included_files, function(file) {
    read_csv(file, show_col_types = FALSE) %>%
      mutate(.source_file = basename(file))
  }))

  required_columns <- c(
    "design", "job_id", "rep_id", "method", "horizon",
    "est", "true_irf", "ci_lower", "ci_upper"
  )
  missing_columns <- setdiff(required_columns, names(results))
  if (length(missing_columns)) {
    stop("The result files are missing: ", paste(missing_columns, collapse = ", "))
  }

  results <- results %>%
    mutate(
      method = as.character(method),
      design = as.character(design),
      across(
        any_of(c(
          "job_id", "rep_id", "horizon", "est", "se", "ci_lower", "ci_upper",
          "true_irf", "elapsed_seconds", "replication_elapsed_seconds",
          "n_shock", "n_outcome", "n_union", "n_always", "n_total"
        )),
        as.numeric
      )
    ) %>%
    left_join(method_information, by = "method") %>%
    left_join(design_information, by = "design")

  if (any(is.na(results$method_label))) {
    stop("At least one method in the included files is not documented.")
  }
  if (any(is.na(results$dgp_key))) {
    stop("At least one simulation design in the included files is not documented.")
  }

  duplicate_rows <- results %>%
    count(design, job_id, rep_id, method, horizon) %>%
    filter(n > 1L)
  if (nrow(duplicate_rows)) {
    stop("The included files contain duplicate design-job-replication-method-horizon rows.")
  }

  expected_rows <- 324000L
  if (nrow(results) != expected_rows) {
    stop("Expected ", expected_rows, " included rows, but found ", nrow(results), ".")
  }

  results %>%
    arrange(
      match(dgp_key, design_information$dgp_key), method_order,
      job_id, rep_id, horizon
    )
}

# -----------------------------------------------------------------------------
# Simulation performance statistics
# -----------------------------------------------------------------------------

summarize_simulation_metrics <- function(results) {
  results %>%
    mutate(
      estimation_error = est - true_irf,
      has_confidence_interval = is.finite(ci_lower) & is.finite(ci_upper),
      covered = ifelse(
        has_confidence_interval,
        ci_lower <= true_irf & true_irf <= ci_upper,
        NA
      ),
      confidence_interval_width = ifelse(
        has_confidence_interval,
        ci_upper - ci_lower,
        NA_real_
      )
    ) %>%
    group_by(
      dgp_key, dgp_label, method, method_label, method_group,
      group_label, method_order, horizon
    ) %>%
    summarise(
      `Mean bias` = mean(estimation_error, na.rm = TRUE),
      `Absolute mean bias` = abs(mean(estimation_error, na.rm = TRUE)),
      `Median absolute error` = median(abs(estimation_error), na.rm = TRUE),
      RMSE = sqrt(mean(estimation_error^2, na.rm = TRUE)),
      `Coverage (%)` = ifelse(
        any(has_confidence_interval),
        100 * mean(covered, na.rm = TRUE),
        NA_real_
      ),
      `Median CI width` = ifelse(
        any(has_confidence_interval),
        median(confidence_interval_width, na.rm = TRUE),
        NA_real_
      ),
      n_rep = n(),
      n_ci = sum(has_confidence_interval),
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# OGA selection and runtime tables
# -----------------------------------------------------------------------------

format_count <- function(value) {
  ifelse(
    is.na(value),
    "--",
    ifelse(
      abs(value - round(value)) < 1e-8,
      as.character(round(value)),
      sprintf("%.1f", value)
    )
  )
}

format_count_range <- function(median_value, minimum_value, maximum_value) {
  paste0(
    format_count(median_value), " (",
    format_count(minimum_value), "--",
    format_count(maximum_value), ")"
  )
}

summarize_oga_selection <- function(results) {
  results %>%
    filter(method == "oga") %>%
    group_by(dgp_key, dgp_label) %>%
    summarise(
      shock_median = median(n_shock, na.rm = TRUE),
      outcome_median = median(n_outcome, na.rm = TRUE),
      outcome_minimum = min(n_outcome, na.rm = TRUE),
      outcome_maximum = max(n_outcome, na.rm = TRUE),
      union_median = median(n_union, na.rm = TRUE),
      union_minimum = min(n_union, na.rm = TRUE),
      union_maximum = max(n_union, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      DGP = dgp_label,
      `Shock Eq.` = format_count(shock_median),
      `Outcome Eq.` = format_count_range(
        outcome_median, outcome_minimum, outcome_maximum
      ),
      `Selected Union` = format_count_range(
        union_median, union_minimum, union_maximum
      )
    ) %>%
    arrange(match(dgp_key, design_information$dgp_key)) %>%
    select(DGP, `Shock Eq.`, `Outcome Eq.`, `Selected Union`)
}

summarize_runtime <- function(results) {
  results %>%
    filter(is.finite(elapsed_seconds)) %>%
    distinct(
      dgp_key, dgp_label, job_id, rep_id,
      method, method_label, method_order, elapsed_seconds
    ) %>%
    group_by(dgp_key, dgp_label, method, method_label, method_order) %>%
    summarise(
      median_seconds = median(elapsed_seconds),
      p25_seconds = quantile(elapsed_seconds, 0.25, names = FALSE),
      p75_seconds = quantile(elapsed_seconds, 0.75, names = FALSE),
      n_rep = n(),
      .groups = "drop"
    ) %>%
    mutate(
      DGP = dgp_label,
      Method = method_label,
      `Median sec.` = sprintf("%.2f", median_seconds),
      `IQR sec.` = paste0(
        sprintf("%.2f", p25_seconds), "--", sprintf("%.2f", p75_seconds)
      )
    ) %>%
    arrange(match(dgp_key, design_information$dgp_key), method_order) %>%
    select(DGP, Method, `Median sec.`, `IQR sec.`, n_rep)
}

# -----------------------------------------------------------------------------
# LaTeX table output
# -----------------------------------------------------------------------------

escape_latex <- function(value) {
  value <- as.character(value)
  value <- gsub("\\\\", "\\\\textbackslash{}", value)
  value <- gsub("&", "\\\\&", value)
  value <- gsub("%", "\\\\%", value)
  value <- gsub("_", "\\\\_", value)
  gsub("#", "\\\\#", value)
}

write_latex_table <- function(data, file, caption, label, alignment, notes) {
  header <- paste(escape_latex(names(data)), collapse = " & ")
  body <- apply(data, 1, function(row) {
    paste0(paste(escape_latex(row), collapse = " & "), " \\\\")
  })

  table_lines <- c(
    "\\begin{table}[htb!]",
    "\\begin{center}",
    paste0("\\caption{", caption, "}\\label{", label, "}"),
    paste0("\\begin{tabular}{", alignment, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{center}",
    paste0("{\\footnotesize \\textit{Notes.} ", notes, "}"),
    "\\end{table}"
  )

  writeLines(table_lines, file)
}

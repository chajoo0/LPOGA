#!/usr/bin/env Rscript

# Run from the package root and pass the array-task number as the first value:
# Rscript cluster/job_shock_contamination.R 1

# Setup ------------------------------------------------------------------------

project_root <- normalizePath(".")

source(file.path(project_root, "R", "simulation_runner.R"))
load_replication_code(project_root)

job_arguments <- commandArgs(trailingOnly = TRUE)
job_id <- suppressWarnings(as.integer(job_arguments[1]))
if (length(job_arguments) != 1L || is.na(job_id)) {
  stop("Pass one task number from 1 through 50.")
}
if (job_id < 1L || job_id > 50L) {
  stop("The task number must be from 1 through 50.")
}

# Simulation design ------------------------------------------------------------

config <- simulation_config()
dfm <- read_dfm_calibration(project_root)
restricted_indices <- as.integer(
  dfm$settings[, , 1]$specifications[, , 1]$var.select[1, ]
)

cat(sprintf(
  paste0(
    "Shock-contamination job %d: %d replications, %d horizons\n",
    "Polynomial power: %.3f; contamination strength: %.3f\n",
    "Methods: %s\n"
  ),
  job_id, config$reps_per_job, config$horizons,
  config$shock_polynomial_power, config$shock_contamination_strength,
  paste(config$methods, collapse = ", ")
))

# Monte Carlo replications -----------------------------------------------------

replication_results <- vector("list", config$reps_per_job)
job_start <- proc.time()[["elapsed"]]
for (rep_id in seq_len(config$reps_per_job)) {
  replication_start <- proc.time()[["elapsed"]]
  draw <- generate_dfm_draw(
    dfm,
    config,
    seed = simulation_seed(job_id, rep_id)
  )
  shock_design <- make_polynomial_shock_contamination(
    draw,
    power = config$shock_polynomial_power,
    strength = config$shock_contamination_strength
  )

  yt <- draw$observables[, restricted_indices[1]]
  xt <- shock_design$shock
  wt <- draw$observables

  estimates <- run_selected_estimators(
    yt, xt, wt,
    restricted_indices = restricted_indices,
    config = config,
    estimator_seed = 2000000L + simulation_seed(job_id, rep_id)
  )
  replication_results[[rep_id]] <- results_to_data_frame(
    estimates,
    job_id = job_id,
    rep_id = rep_id,
    design = "shock_contamination_full_panel_polynomial",
    config = config
  ) %>%
    dplyr::mutate(
      contamination_sd = stats::sd(shock_design$contamination),
      contamination_shock_correlation = stats::cor(
        shock_design$contamination,
        draw$structural_shock
      ),
      replication_elapsed_seconds =
        proc.time()[["elapsed"]] - replication_start
    )

  elapsed <- proc.time()[["elapsed"]] - job_start
  remaining <- elapsed / rep_id * (config$reps_per_job - rep_id)
  method_times <- vapply(
    estimates, function(x) x$elapsed_seconds, numeric(1)
  )
  cat(sprintf(
    "  completed %d/%d (%.1f%%); replication %.1fs; ETA %.1f min\n",
    rep_id, config$reps_per_job,
    100 * rep_id / config$reps_per_job,
    proc.time()[["elapsed"]] - replication_start,
    remaining / 60
  ))
  cat("    method times: ", paste(
    sprintf("%s=%.2fs", names(method_times), method_times),
    collapse = ", "
  ), "\n", sep = "")
  flush.console()
}

# Save the task-level result file ----------------------------------------------

output <- dplyr::bind_rows(replication_results) %>%
  dplyr::mutate(
    true_irf = as.numeric(dfm$DF.model[, , 1]$target.irf[-1, 1])[horizon],
    polynomial_power = config$shock_polynomial_power,
    contamination_strength = config$shock_contamination_strength,
    contamination_panel = "all_dfm_observables",
    contamination_order = "native_dfm_column_order",
    contamination_lags = 1L,
    structural_shock_source = "original_dfm_innovations",
    num_factors = 6L,
    lp_lags = config$lags,
    run_label = config$run_label,
    method_set = paste(config$methods, collapse = ",")
  )

output_file <- file.path(
  project_root,
  "results", "simulation",
  sprintf(
    "dfm_shock_contamination_poly%s%s_job%03d.csv",
    run_label_suffix(config$run_label),
    method_output_suffix(config$methods),
    job_id
  )
)
readr::write_csv(output, output_file)
cat("Saved:", output_file, "\n")

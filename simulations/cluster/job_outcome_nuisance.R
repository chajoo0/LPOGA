#!/usr/bin/env Rscript

# Run from the package root and pass the array-task number as the first value:
# Rscript cluster/job_outcome_nuisance.R 1

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
    "Outcome-nuisance job %d: %d replications, %d horizons\n",
    "K_z: %d; rho: %.3f; polynomial decay: %.3f; strength: %.3f\n",
    "Methods: %s\n"
  ),
  job_id, config$reps_per_job, config$horizons,
  config$outcome_kz, config$outcome_rho, config$outcome_decay,
  config$outcome_strength,
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
  outcome_design <- make_outcome_nuisance(
    draw,
    rho = config$outcome_rho,
    strength = config$outcome_strength,
    seed = outcome_nuisance_seed(job_id, rep_id),
    k_z = config$outcome_kz,
    decay = config$outcome_decay
  )

  yt <- outcome_design$contaminated_outcome
  xt <- outcome_design$shock
  wt <- cbind(
    draw$observables,
    outcome_design$nuisance_controls
  )

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
    design = "outcome_nuisance_ar1",
    config = config
  ) %>%
    dplyr::mutate(
      nuisance_shock_correlation = outcome_design$sample_correlation,
      nuisance_max_control_shock_correlation =
        outcome_design$max_control_shock_correlation,
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
    outcome_rho = config$outcome_rho,
    outcome_strength = config$outcome_strength,
    outcome_kz = config$outcome_kz,
    outcome_decay = config$outcome_decay,
    structural_shock_source = "original_dfm_innovations",
    nuisance_observed = TRUE,
    nuisance_controls_observed = config$outcome_kz,
    num_factors = 6L,
    lp_lags = config$lags,
    run_label = config$run_label,
    method_set = paste(config$methods, collapse = ",")
  )

output_file <- file.path(
  project_root,
  "results", "simulation",
  sprintf(
    "dfm_outcome_nuisance%s%s_job%03d.csv",
    run_label_suffix(config$run_label),
    method_output_suffix(config$methods),
    job_id
  )
)
readr::write_csv(output, output_file)
cat("Saved:", output_file, "\n")

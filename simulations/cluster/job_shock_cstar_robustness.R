#!/usr/bin/env Rscript

# Run from the package root and pass the array-task number as the first value:
# Rscript cluster/job_shock_cstar_robustness.R 1

# Setup ------------------------------------------------------------------------

project_root <- normalizePath(".")

source(file.path(project_root, "R", "simulation_runner.R"))
load_replication_code(project_root)

parse_cstar_grid <- function(value) {
  grid <- suppressWarnings(as.numeric(trimws(strsplit(value, "[,:]")[[1]])))
  if (!length(grid) || anyNA(grid) || any(!is.finite(grid)) || any(grid <= 0)) {
    stop("CSTAR_VALUES must contain positive numbers separated by commas or colons.")
  }
  unique(grid)
}

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
cstar_values <- parse_cstar_grid(
  Sys.getenv("CSTAR_VALUES", "1.6,1.8,2.0,2.2,2.4")
)
dfm <- read_dfm_calibration(project_root)
restricted_indices <- as.integer(
  dfm$settings[, , 1]$specifications[, , 1]$var.select[1, ]
)

cat(sprintf(
  paste0(
    "Shock C-star robustness job %d: %d replications, %d horizons\n",
    "C-star grid: %s\n",
    "Polynomial power: %.3f; contamination strength: %.3f\n"
  ),
  job_id, config$reps_per_job, config$horizons,
  paste(cstar_values, collapse = ", "),
  config$shock_polynomial_power, config$shock_contamination_strength
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

  cstar_results <- lapply(cstar_values, function(cstar) {
    estimator_start <- proc.time()[["elapsed"]]
    result <- safe_estimator(
      lp_oga(
        yt, xt, wt,
        numhor = config$horizons,
        lags = config$lags,
        HDIC_Type = "HDAIC",
        cstar = cstar,
        hac_lags = config$hac_lags
      ),
      config$horizons
    )
    result$elapsed_seconds <- proc.time()[["elapsed"]] - estimator_start

    results_to_data_frame(
      list(oga = result),
      job_id = job_id,
      rep_id = rep_id,
      design = "shock_contamination_full_panel_polynomial_cstar",
      config = config
    ) %>%
      dplyr::mutate(cstar = cstar)
  })

  replication_elapsed <- proc.time()[["elapsed"]] - replication_start
  replication_results[[rep_id]] <- dplyr::bind_rows(cstar_results) %>%
    dplyr::mutate(
      contamination_sd = stats::sd(shock_design$contamination),
      contamination_shock_correlation = stats::cor(
        shock_design$contamination,
        draw$structural_shock
      ),
      replication_elapsed_seconds = replication_elapsed
    )

  elapsed <- proc.time()[["elapsed"]] - job_start
  remaining <- elapsed / rep_id * (config$reps_per_job - rep_id)
  cstar_times <- vapply(
    cstar_results,
    function(x) unique(x$elapsed_seconds),
    numeric(1)
  )
  cat(sprintf(
    "  completed %d/%d (%.1f%%); replication %.1fs; ETA %.1f min\n",
    rep_id, config$reps_per_job,
    100 * rep_id / config$reps_per_job,
    replication_elapsed,
    remaining / 60
  ))
  cat(
    "    OGA times: ",
    paste(
      sprintf("C*=%.1f: %.2fs", cstar_values, cstar_times),
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
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
    run_label = "cstar_robustness",
    method_set = "oga",
    cstar_grid = paste(cstar_values, collapse = ",")
  )

output_file <- file.path(
  project_root,
  "results", "simulation",
  sprintf("dfm_shock_contamination_poly_cstar_job%03d.csv", job_id)
)
readr::write_csv(output, output_file)
cat("Saved:", output_file, "\n")

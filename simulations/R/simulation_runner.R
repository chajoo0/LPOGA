# Simulation dispatch and result assembly.

# Load the exact estimator implementations used in the simulations ------------

load_replication_code <- function(project_root) {
  suppressPackageStartupMessages(library(dplyr))
  source(file.path(project_root, "R", "configuration.R"))
  source(file.path(project_root, "R", "ohit_1.0.0.R"))
  source(file.path(project_root, "R", "lavash_1.0.R"))
  source(file.path(project_root, "R", "dgp.R"))
  source(file.path(project_root, "R", "estimators.R"))
}

# Run one estimator without losing the rest of an array task after an error ----

safe_estimator <- function(expr, numhor) {
  tryCatch(expr, error = function(error) {
    warning(conditionMessage(error), call. = FALSE)
    list(
      est = rep(NA_real_, numhor),
      se = rep(NA_real_, numhor),
      error = conditionMessage(error)
    )
  })
}

# Run the requested methods ----------------------------------------------------

run_selected_estimators <- function(
    yt, xt, wt, restricted_indices, config, estimator_seed
) {
  restricted_controls <- wt[, restricted_indices, drop = FALSE]
  favar_panel <- wt
  favar_panel[, restricted_indices[1]] <- yt

  # Keep the Monte Carlo random-number streams fixed if the method list changes.
  method_offsets <- c(
    ols = 1L,
    lalp = 2L,
    aic_bc = 3L,
    falp = 4L,
    favar = 5L,
    lava = 6L,
    oga = 8L,
    dlasso = 9L,
    rslp = 10L
  )

  run_and_time <- function(method_name, estimator_call) {
    set.seed(estimator_seed + 1000L * method_offsets[[method_name]])
    start_time <- proc.time()[["elapsed"]]
    result <- safe_estimator(estimator_call, config$horizons)
    result$elapsed_seconds <- proc.time()[["elapsed"]] - start_time

    if (is.null(result$error) && !is.null(result$method_id) &&
        !identical(result$method_id, method_name)) {
      stop("Estimator name mismatch: requested ", method_name,
           " but function returned ", result$method_id)
    }
    result
  }

  estimates <- list()

  if ("ols" %in% config$methods) {
    estimates$ols <- run_and_time("ols", lp_ols(
      yt, xt, wt = restricted_controls,
      numhor = config$horizons, lags = config$lags
    ))
  }

  if ("lalp" %in% config$methods) {
    estimates$lalp <- run_and_time("lalp", lp_lalp(
      yt, xt, wt = restricted_controls,
      numhor = config$horizons, lags = config$lags
    ))
  }

  if ("aic_bc" %in% config$methods) {
    estimates$aic_bc <- run_and_time("aic_bc", lp_aic_bc(
      yt, xt, wt = restricted_controls,
      numhor = config$horizons, lag_max = config$aic_max_lags
    ))
  }

  if ("falp" %in% config$methods) {
    estimates$falp <- run_and_time("falp", lp_falp(
      yt, xt, wt,
      numhor = config$horizons, num_factors = 6, lags = config$lags
    ))
  }

  if ("favar" %in% config$methods) {
    estimates$favar <- run_and_time("favar", var_favar_noisyshock(
      yt_idx = restricted_indices[1], xt = xt, wt = favar_panel,
      numhor = config$horizons, num_factors = 6,
      lags = config$lags, n_boot = config$favar_bootstraps
    ))
  }

  if ("lava" %in% config$methods) {
    estimates$lava <- run_and_time("lava", lp_lava_boot(
      yt, xt, wt,
      numhor = config$horizons, lags = config$lags,
      n_boot = config$lava_bootstraps
    ))
  }

  if ("oga" %in% config$methods) {
    estimates$oga <- run_and_time("oga", lp_oga(
      yt, xt, wt,
      numhor = config$horizons, lags = config$lags,
      cstar = config$cstar, hac_lags = config$hac_lags
    ))
  }

  if ("dlasso" %in% config$methods) {
    estimates$dlasso <- run_and_time("dlasso", lp_dlasso(
      yt, xt, wt,
      numhor = config$horizons, lags = config$lags, parallel = FALSE
    ))
  }

  if ("rslp" %in% config$methods) {
    rslp_seed <- estimator_seed + 1000L * method_offsets[["rslp"]]
    estimates$rslp <- run_and_time("rslp", lp_rslp(
      yt, xt, wt,
      numhor = config$horizons, lags = config$lags,
      essential_indices = restricted_indices,
      M = config$rslp_draws, k = config$rslp_dimension,
      n_boot = config$rslp_bootstraps, seed = rslp_seed
    ))
  }

  estimates[config$methods]
}

# Store one row per method and horizon -----------------------------------------

selected_ids_at <- function(values, horizon) {
  if (is.null(values)) return(NA_character_)
  selected <- if (is.list(values)) values[[horizon]] else values
  if (!length(selected)) return("")
  paste(selected, collapse = ";")
}

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x

results_to_data_frame <- function(
    estimates, job_id, rep_id, design, config
) {
  dplyr::bind_rows(lapply(names(estimates), function(method_name) {
    result <- estimates[[method_name]]
    data.frame(
      job_id = job_id,
      rep_id = rep_id,
      design = design,
      method = method_name,
      horizon = seq_len(config$horizons),
      est = as.numeric(result$est),
      se = as.numeric(result$se),
      ci_lower = if (!is.null(result$lower)) {
        as.numeric(result$lower)
      } else as.numeric(result$est - 1.96 * result$se),
      ci_upper = if (!is.null(result$upper)) {
        as.numeric(result$upper)
      } else as.numeric(result$est + 1.96 * result$se),
      selected_lags = result$selected_lags %||% NA_integer_,
      hac_lags = config$hac_lags,
      elapsed_seconds = result$elapsed_seconds %||% NA_real_,
      favar_bootstraps = config$favar_bootstraps,
      lava_bootstraps = config$lava_bootstraps,
      rslp_draws = config$rslp_draws,
      rslp_dimension = config$rslp_dimension,
      rslp_bootstraps = config$rslp_bootstraps,
      n_shock = result$n_gamma %||% NA_real_,
      n_outcome = result$n_beta %||% NA_real_,
      n_union = result$n_union %||% NA_real_,
      n_always = result$n_always %||% NA_real_,
      n_total = result$n_total %||% NA_real_,
      shock_ids = vapply(seq_len(config$horizons), function(h) {
        selected_ids_at(result$shock_ids, h)
      }, character(1)),
      outcome_ids = vapply(seq_len(config$horizons), function(h) {
        selected_ids_at(result$outcome_ids, h)
      }, character(1)),
      union_ids = vapply(seq_len(config$horizons), function(h) {
        selected_ids_at(result$union_ids, h)
      }, character(1)),
      always_ids = vapply(seq_len(config$horizons), function(h) {
        selected_ids_at(result$always_ids, h)
      }, character(1)),
      error = result$error %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

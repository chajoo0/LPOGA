# Reported settings and optional overrides used by the cluster wrapper.

# -----------------------------------------------------------------------------
# Available estimators
# -----------------------------------------------------------------------------

available_simulation_methods <- function() {
  c("ols", "lalp", "aic_bc", "falp", "favar", "lava",
    "oga", "dlasso", "rslp")
}

parse_method_list <- function(value) {
  requested <- trimws(strsplit(value, "[,:]")[[1]])
  requested <- requested[nzchar(requested)]
  unknown <- setdiff(requested, available_simulation_methods())
  if (length(unknown)) stop("Unknown method(s): ", paste(unknown, collapse = ", "))
  unique(requested)
}

# -----------------------------------------------------------------------------
# Simulation settings
# -----------------------------------------------------------------------------

simulation_config <- function(environment_values = Sys.getenv()) {
  read_setting <- function(name, default) {
    value <- unname(environment_values[name])
    if (!length(value) || is.na(value) || !nzchar(value)) default else value
  }

  all_methods <- paste(available_simulation_methods(), collapse = ",")

  # These are the common reported values. The cluster wrapper divides the
  # nine reported methods into batches and sets LAVA to point estimation.
  # REPS_PER_JOB and NUM_HORIZONS may be reduced for a short implementation
  # check without editing this file.
  config <- list(
    T = 200L,
    burn = 100L,
    horizons = as.integer(read_setting("NUM_HORIZONS", "12")),
    lags = 4L,
    hac_lags = as.integer(read_setting("HAC_LAGS", "4")),
    reps_per_job = as.integer(read_setting("REPS_PER_JOB", "20")),
    methods = parse_method_list(read_setting("METHODS", all_methods)),
    factor_strength = as.numeric(read_setting("FACTOR_STRENGTH", "2")),
    outcome_rho = as.numeric(read_setting("OUTCOME_RHO", "0.85")),
    outcome_strength = as.numeric(read_setting("OUTCOME_STRENGTH", "1")),
    outcome_kz = as.integer(read_setting("OUTCOME_KZ", "20")),
    outcome_decay = as.numeric(read_setting("OUTCOME_DECAY", "1.5")),
    shock_polynomial_power = as.numeric(
      read_setting("SHOCK_POLYNOMIAL_POWER", "1.5")
    ),
    shock_contamination_strength = as.numeric(
      read_setting("SHOCK_CONTAMINATION_STRENGTH", "1")
    ),
    aic_max_lags = as.integer(read_setting("AIC_MAX_LAGS", "20")),
    favar_bootstraps = as.integer(read_setting("FAVAR_BOOTSTRAPS", "100")),
    lava_bootstraps = as.integer(read_setting("LAVA_BOOTSTRAPS", "100")),
    rslp_draws = as.integer(read_setting("RSLP_DRAWS", "1000")),
    rslp_dimension = as.integer(read_setting("RSLP_DIMENSION", "50")),
    rslp_bootstraps = as.integer(read_setting("RSLP_BOOTSTRAPS", "100")),
    cstar = as.numeric(read_setting("CSTAR", "2")),
    run_label = read_setting("RUN_LABEL", "")
  )
  validate_simulation_config(config)
}

validate_simulation_config <- function(config) {
  positive_integers <- c(
    "T", "burn", "horizons", "lags", "reps_per_job", "outcome_kz",
    "aic_max_lags", "favar_bootstraps", "rslp_draws", "rslp_dimension",
    "rslp_bootstraps"
  )
  for (name in positive_integers) {
    value <- config[[name]]
    if (length(value) != 1L || is.na(value) || value < 1L) {
      stop(name, " must be a positive integer.")
    }
  }
  if (is.na(config$hac_lags) || config$hac_lags < 0L) {
    stop("hac_lags must be a nonnegative integer.")
  }
  finite_values <- c(
    "factor_strength", "outcome_rho", "outcome_strength", "outcome_decay",
    "shock_polynomial_power", "shock_contamination_strength", "cstar"
  )
  for (name in finite_values) {
    if (length(config[[name]]) != 1L || !is.finite(config[[name]])) {
      stop(name, " must be finite.")
    }
  }
  config
}

read_dfm_calibration <- function(project_root) {
  path <- file.path(project_root, "dfm_calibration.mat")
  if (!file.exists(path)) stop("Calibration file not found: ", path)
  dfm <- R.matlab::readMat(path)
  required <- c("DF.model", "settings")
  if (!all(required %in% names(dfm))) stop("Calibration object is incomplete.")
  model <- dfm$DF.model[, , 1]
  if (model$n.y != 207 || model$n.fac != 6) {
    stop("Calibration must contain 207 observables and six factors.")
  }
  dfm
}

method_output_suffix <- function(methods) {
  if (identical(methods, available_simulation_methods())) return("")
  paste0("_methods-", paste(methods, collapse = "-"))
}

run_label_suffix <- function(label) {
  if (!nzchar(label)) return("")
  paste0("_", gsub("[^A-Za-z0-9_-]+", "-", label))
}

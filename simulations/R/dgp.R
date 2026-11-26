# Reproducible seeds -----------------------------------------------------------

simulation_seed <- function(job_id, rep_id) {
  10000L + 1000L * as.integer(job_id) + as.integer(rep_id)
}

outcome_nuisance_seed <- function(job_id, rep_id) {
  900000L + 1000L * as.integer(job_id) + as.integer(rep_id)
}

# Draws from the calibrated dynamic factor model -------------------------------

generate_dfm_draw <- function(dfm, config, seed) {
  set.seed(seed)
  model <- dfm$DF.model[, , 1]
  df_settings <- dfm$settings[, , 1]
  state_transition <- model$ABCD[, , 1]$A
  innovation_loading <- model$ABCD[, , 1]$B
  measurement_loading <- model$ABCD[, , 1]$C
  contemporaneous_loading <- model$ABCD[, , 1]$D
  shock_weight <- as.numeric(df_settings$est[, , 1]$shock.weight)

  total_T <- config$T + config$burn
  innovations <- matrix(
    stats::rnorm(total_T * ncol(innovation_loading)),
    nrow = total_T,
    ncol = ncol(innovation_loading)
  )
  states <- matrix(
    NA_real_, nrow = total_T, ncol = nrow(innovation_loading)
  )
  state <- rep(0, nrow(innovation_loading))
  for (tt in seq_len(total_T)) {
    state <- as.numeric(
      state_transition %*% state +
        innovation_loading %*% innovations[tt, ]
    )
    states[tt, ] <- state
  }

  state_rows <- config$burn:(config$burn + config$T - 1L)
  innovation_rows <- (config$burn + 1L):(config$burn + config$T)
  observables <-
    states[state_rows, , drop = FALSE] %*% t(measurement_loading) +
    innovations[innovation_rows, , drop = FALSE] %*%
      t(contemporaneous_loading)
  list(
    observables = observables,
    structural_shock = as.numeric(
      innovations[innovation_rows, , drop = FALSE] %*% shock_weight
    ),
    lagged_factors = states[state_rows, seq_len(6), drop = FALSE]
  )
}

# Contamination designs --------------------------------------------------------

restricted_factor_null_direction <- function(dfm, restricted_indices) {
  loadings <- dfm$DF.model[, , 1]$Lambda[restricted_indices, , drop = FALSE]
  direction <- svd(loadings, nv = ncol(loadings))$v[, ncol(loadings)]
  anchor <- which.max(abs(direction))
  if (direction[anchor] < 0) direction <- -direction
  as.numeric(direction / sqrt(sum(direction^2)))
}

make_factor_contamination <- function(draw, factor_direction, strength) {
  component <- as.numeric(draw$lagged_factors %*% factor_direction)
  draw$structural_shock + strength * as.numeric(scale(component))
}

make_polynomial_shock_contamination <- function(
    draw, power = 1.5, strength = 1
) {
  panel <- scale(draw$observables)
  weights <- seq_len(ncol(panel))^(-power)
  contemporaneous_index <- as.numeric(panel %*% weights)
  lagged_index <- c(0, contemporaneous_index[-length(contemporaneous_index)])
  list(
    shock = draw$structural_shock + strength * lagged_index,
    contamination = strength * lagged_index,
    weights = weights,
    power = power,
    strength = strength
  )
}

stationary_ar1_matrix <- function(n, k, rho, seed) {
  if (k < 1L) stop("k must be positive.")
  set.seed(seed)
  z <- matrix(NA_real_, nrow = n, ncol = k)
  z[1L, ] <- stats::rnorm(k)
  innovation_sd <- sqrt(1 - rho^2)
  for (tt in 2:n) {
    z[tt, ] <- rho * z[tt - 1L, ] + stats::rnorm(k, sd = innovation_sd)
  }
  z
}

make_outcome_nuisance <- function(
    draw, rho, strength, seed, k_z = 20L, decay = 1.5
) {
  structural_shock <- as.numeric(draw$structural_shock)
  k_z <- as.integer(k_z)
  controls <- stationary_ar1_matrix(
    nrow(draw$observables), k_z, rho, seed
  )
  shock_centered <- structural_shock - mean(structural_shock)
  controls <- scale(controls, center = TRUE, scale = FALSE)
  shock_coefficients <- as.numeric(
    crossprod(shock_centered, controls) / sum(shock_centered^2)
  )
  controls <- controls - outer(shock_centered, shock_coefficients)
  controls <- scale(controls)
  colnames(controls) <- sprintf("outcome_nuisance_%02d", seq_len(k_z))

  weights <- seq_len(k_z)^(-decay)
  weights <- weights / sqrt(sum(weights^2))
  nuisance <- as.numeric(scale(as.numeric(controls %*% weights)))
  list(
    nuisance = nuisance,
    nuisance_controls = controls,
    nuisance_weights = weights,
    k_z = k_z,
    decay = decay,
    contaminated_outcome = draw$observables[, 121] + strength * nuisance,
    shock = structural_shock,
    sample_correlation = stats::cor(nuisance, structural_shock),
    max_control_shock_correlation = max(abs(stats::cor(
      controls, structural_shock
    )))
  )
}

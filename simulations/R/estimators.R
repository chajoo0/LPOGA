# Estimators used in the DFM simulations.

# Shared local-projection data preparation ------------------------------------

create_lags <- function(x, lags) {
  x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  result <- matrix(NA_real_, n, p * lags)
  for (lag in seq_len(lags)) {
    result[(lag + 1):n, ((lag - 1) * p + 1):(lag * p)] <-
      x[1:(n - lag), , drop = FALSE]
  }
  result
}

gen_data_lp <- function(yt, xt, wt, horizon, numlag) {
  lag_series <- function(x, lag) {
    x <- as.matrix(x)
    if (lag == 0L) return(x)
    rbind(matrix(NA_real_, lag, ncol(x)),
          x[seq_len(nrow(x) - lag), , drop = FALSE])
  }
  shock <- lag_series(matrix(xt, ncol = 1), horizon)[, 1]
  controls_at_shock <- lag_series(wt, horizon)
  controls <- do.call(cbind, lapply(seq_len(numlag), function(lag) {
    lag_series(controls_at_shock, lag)
  }))
  rows <- (horizon + numlag + 1L):length(yt)
  list(yt[rows], shock[rows], controls[rows, , drop = FALSE])
}

# Low-dimensional local projections ------------------------------------------

lp_ols <- function(yt, xt, wt = NULL, numhor = 12, lags = 4) {
  n <- length(yt)
  est <- se <- rep(NA_real_, numhor)
  lagged_controls <- create_lags(cbind(yt, xt, wt), lags)
  for (h in seq_len(numhor)) {
    rows <- (lags + 1L):(n - h)
    y_reg <- yt[rows + h]
    contemporaneous_controls <- if (is.null(wt)) NULL else {
      wt[rows, , drop = FALSE]
    }
    X <- cbind(
      xt[rows], contemporaneous_controls,
      lagged_controls[rows, , drop = FALSE]
    )
    keep <- stats::complete.cases(y_reg, X)
    if (sum(keep) < ncol(X) + 1L) next
    fit <- stats::lm(y_reg[keep] ~ X[keep, ])
    covariance <- sandwich::NeweyWest(fit)
    est[h] <- stats::coef(fit)[2]
    se[h] <- sqrt(covariance[2, 2])
  }
  list(method_id = "ols", est = est, se = se)
}

# Debiased lasso local projections --------------------------------------------

lp_dlasso <- function(
    yt, xt, wt, numhor = 12, lags, ...,
    r = NULL, q = wt, penalize_x = FALSE,
    y_predetermined = FALSE, OLS = FALSE
) {
  if (!requireNamespace("desla", quietly = TRUE)) {
    stop("Package 'desla' is required for lp_dlasso().")
  }
  fit <- desla::HDLP(
    x = xt, y = yt, r = r, q = q,
    y_predetermined = y_predetermined,
    hmax = numhor, lags = lags,
    penalize_x = penalize_x, OLS = OLS,
    progress_bar = FALSE, ...
  )
  intervals <- fit$intervals[, , 1]
  lower <- as.numeric(intervals[-1, 1])
  est <- as.numeric(intervals[-1, 2])
  upper <- as.numeric(intervals[-1, 3])
  list(
    method_id = "dlasso", est = est,
    se = (upper - lower) / (2 * 1.96),
    lower = lower, upper = upper,
    n_union = colSums(fit$betahats != 0)[-1]
  )
}

# HAC covariance calculation used by OGA --------------------------------------

manual_hac <- function(z, lags = 4L, kernel = c("bartlett", "parzen")) {
  kernel <- match.arg(kernel)
  z <- as.numeric(z)
  n <- length(z)
  z <- z - mean(z)
  lags <- as.integer(lags)
  if (length(lags) != 1L || is.na(lags) || lags < 0L || lags >= n) {
    stop("lags must be one integer between zero and length(z) - 1.")
  }
  weight <- function(x) {
    if (kernel == "bartlett") return(1 - x)
    if (x <= 0.5) return(1 - 6 * x^2 + 6 * x^3)
    if (x <= 1) return(2 * (1 - x)^3)
    0
  }
  value <- sum(z^2) / n
  if (lags > 0L) {
    for (lag in seq_len(lags)) {
      autocovariance <- sum(z[(lag + 1):n] * z[1:(n - lag)]) / n
      value <- value + 2 * weight(lag / (lags + 1)) * autocovariance
    }
  }
  value
}

# Lag-augmented and bias-corrected local projections ---------------------------

.same_series <- function(x, y, tolerance = 1e-12) {
  isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tolerance))
}

.clean_control_matrix <- function(x, protected = list()) {
  if (is.null(x)) return(NULL)
  x <- as.matrix(x)
  if (ncol(x) == 0L) return(NULL)

  keep <- logical(ncol(x))
  kept <- integer(0)
  for (j in seq_len(ncol(x))) {
    duplicates_protected <- any(vapply(
      protected, function(z) .same_series(x[, j], z), logical(1)
    ))
    duplicates_kept <- any(vapply(
      kept, function(k) .same_series(x[, j], x[, k]), logical(1)
    ))
    keep[j] <- !duplicates_protected && !duplicates_kept
    if (keep[j]) kept <- c(kept, j)
  }
  if (!any(keep)) return(NULL)

  out <- x[, keep, drop = FALSE]
  if (is.null(colnames(out))) colnames(out) <- paste0("control_", which(keep))
  colnames(out) <- make.unique(colnames(out))
  out
}

.estimation_system <- function(yt, xt, wt = NULL) {
  controls <- .clean_control_matrix(wt, protected = list(yt, xt))
  out <- cbind(shock = as.numeric(xt), response = as.numeric(yt), controls)
  colnames(out) <- make.unique(colnames(out))
  out
}

.lp_bias_correction <- function(irs, controls) {
  horizon <- length(irs) - 1L
  controls <- scale(as.matrix(controls), center = TRUE, scale = FALSE)
  Tn <- nrow(controls)
  sigma0 <- stats::cov(controls)
  acf_correction <- numeric(horizon)

  for (j in seq_len(horizon)) {
    sigma_j <- crossprod(
      controls[seq_len(Tn - j), , drop = FALSE],
      controls[(j + 1L):Tn, , drop = FALSE]
    ) / (Tn - j - 1L)
    acf_correction[j] <- 1 + sum(diag(solve(sigma0, sigma_j)))
  }

  corrected <- as.numeric(irs)
  for (h in seq_len(horizon)) {
    corrected[h + 1L] <- irs[h + 1L] +
      sum(acf_correction[seq_len(h)] * rev(corrected[seq_len(h)])) /
      (Tn - h)
  }
  corrected
}

.primer_aic_lag_selection <- function(system, lag_max) {
  system <- as.matrix(system)
  Tn <- nrow(system)
  n_variables <- ncol(system)
  denominator <- Tn - lag_max
  criteria <- rep(Inf, lag_max)

  for (p in seq_len(lag_max)) {
    lagged_system <- create_lags(system, p)
    tt <- (p + 1L):Tn
    X <- cbind(intercept = 1, lagged_system[tt, , drop = FALSE])
    fit <- stats::lm.fit(X, system[tt, , drop = FALSE])
    residuals <- fit$residuals
    residual_df <- nrow(X) - ncol(X)
    if (residual_df <= 0L || anyNA(residuals)) next

    sigma <- crossprod(residuals) / residual_df
    log_determinant <- determinant(sigma, logarithm = TRUE)
    if (log_determinant$sign <= 0L) next

    criteria[p] <- as.numeric(log_determinant$modulus) +
      2 * (n_variables^2 * p + n_variables) / denominator
  }

  if (!any(is.finite(criteria))) stop("AIC VAR lag selection failed.")
  list(selected_lags = which.min(criteria), criteria = criteria)
}

# Montiel Olea and Plagborg-Moller lag-augmented LP. The supplied VAR order p
# corresponds to p lag blocks, not p + 1 lag blocks.
lp_lalp <- function(yt, xt, wt = NULL, numhor = 12, lags = 4) {
  system <- .estimation_system(yt, xt, wt)
  lagged_system <- create_lags(system, lags)
  Tn <- length(yt)
  est <- se <- rep(NA_real_, numhor)

  for (h in seq_len(numhor)) {
    tt <- (lags + 1L):(Tn - h)
    y_reg <- yt[tt + h]
    X <- cbind(shock = xt[tt], lagged_system[tt, , drop = FALSE])
    keep <- stats::complete.cases(y_reg, X)
    if (sum(keep) <= ncol(X)) next

    fit <- stats::lm(y_reg[keep] ~ X[keep, ])
    covariance <- sandwich::vcovHC(fit, type = "HC1")
    est[h] <- stats::coef(fit)[2]
    se[h] <- sqrt(covariance[2, 2])
  }

  list(method_id = "lalp", est = est, se = se, selected_lags = lags)
}

# AIC-selected, Herbst-Johannsen bias-corrected LP following the public
# Montiel Olea-Plagborg-Moller-Qian-Wolf implementation.
lp_aic_bc <- function(
    yt,
    xt,
    wt = NULL,
    numhor = 12,
    lag_max = 20
) {
  system <- .estimation_system(yt, xt, wt)
  Tn <- nrow(system)
  feasible_max <- floor((Tn - 2L) / (ncol(system) + 1L))
  lag_max <- min(as.integer(lag_max), feasible_max)
  if (lag_max < 1L) stop("Sample is too short for AIC VAR lag selection.")

  selection <- .primer_aic_lag_selection(system, lag_max)
  selected_lags <- selection$selected_lags
  lagged_system <- create_lags(system, selected_lags)

  irs <- ses <- rep(NA_real_, numhor + 1L)
  for (h in 0:numhor) {
    tt <- (selected_lags + 1L):(Tn - h)
    y_reg <- yt[tt + h]
    X <- cbind(shock = xt[tt], lagged_system[tt, , drop = FALSE])
    keep <- stats::complete.cases(y_reg, X)
    if (sum(keep) <= ncol(X)) next

    fit <- stats::lm(y_reg[keep] ~ X[keep, ])
    covariance <- sandwich::vcovHC(fit, type = "HC1")
    irs[h + 1L] <- stats::coef(fit)[2]
    ses[h + 1L] <- sqrt(covariance[2, 2])
  }

  correction_controls <- lagged_system[
    (selected_lags + 1L):Tn, , drop = FALSE
  ]
  corrected <- .lp_bias_correction(irs, correction_controls)

  list(
    method_id = "aic_bc",
    est = corrected[-1L],
    se = ses[-1L],
    selected_lags = selected_lags,
    lag_search_max = lag_max,
    lag_selection_criterion = selection$criteria
  )
}

# Factor and random-subspace methods ------------------------------------------

# Dynamic factor-augmented LP with p lags of y, x, and the estimated factors.
lp_falp <- function(
    yt,
    xt,
    wt,
    numhor = 12,
    num_factors = 6,
    lags = 4
) {
  wt <- as.matrix(wt)
  usable <- apply(wt, 2, stats::sd, na.rm = TRUE) > sqrt(.Machine$double.eps)
  pca <- stats::prcomp(wt[, usable, drop = FALSE], scale. = TRUE)
  factors <- pca$x[, seq_len(min(num_factors, ncol(pca$x))), drop = FALSE]
  lagged_controls <- create_lags(cbind(yt, xt, factors), lags)

  Tn <- length(yt)
  est <- se <- rep(NA_real_, numhor)
  for (h in seq_len(numhor)) {
    tt <- (lags + 1L):(Tn - h)
    y_reg <- yt[tt + h]
    X <- cbind(shock = xt[tt], lagged_controls[tt, , drop = FALSE])
    keep <- stats::complete.cases(y_reg, X)
    if (sum(keep) <= ncol(X)) next

    fit <- stats::lm(y_reg[keep] ~ X[keep, ])
    covariance <- sandwich::NeweyWest(fit)
    est[h] <- stats::coef(fit)[2]
    se[h] <- sqrt(covariance[2, 2])
  }
  list(method_id = "falp", est = est, se = se)
}

.normalized_favar_irf <- function(var_fit, numhor) {
  response_names <- colnames(var_fit$y)
  irf <- vars::irf(
    var_fit,
    n.ahead = numhor,
    impulse = "shock",
    response = response_names,
    ortho = TRUE,
    boot = FALSE
  )$irf$shock
  irf / irf[1, "shock"]
}

# Two-step PCA FAVAR with the observed shock ordered first. Recursive
# identification therefore permits an impact response of all estimated factors.
var_favar_noisyshock <- function(
    yt_idx,
    xt,
    wt,
    numhor = 12,
    num_factors = 6,
    lags = 4,
    n_boot = 100,
    alpha = 0.05
) {
  if (!requireNamespace("vars", quietly = TRUE) ||
      !requireNamespace("tsDyn", quietly = TRUE)) {
    stop("Packages 'vars' and 'tsDyn' are required for FAVAR.")
  }

  X <- as.matrix(wt)
  usable <- apply(X, 2, stats::sd, na.rm = TRUE) > sqrt(.Machine$double.eps)
  pca <- stats::prcomp(X[, usable, drop = FALSE], scale. = TRUE)
  factors <- pca$x[, seq_len(min(num_factors, ncol(pca$x))), drop = FALSE]
  colnames(factors) <- paste0("factor_", seq_len(ncol(factors)))

  centered_X <- scale(X, center = TRUE, scale = FALSE)
  loadings <- qr.coef(qr(factors), centered_X)
  data_var <- data.frame(shock = as.numeric(xt), factors, check.names = FALSE)
  fit <- vars::VAR(data_var, p = lags, type = "const")
  factor_irf <- .normalized_favar_irf(fit, numhor)[
    , colnames(factors), drop = FALSE
  ]
  observable_irf <- factor_irf %*% loadings
  est <- as.numeric(observable_irf[-1L, yt_idx])

  boot_target <- matrix(NA_real_, nrow = numhor + 1L, ncol = n_boot)
  if (n_boot > 0L) {
    bootstrap_var <- tsDyn::lineVar(data_var, lag = lags, include = "const")
    for (b in seq_len(n_boot)) {
      boot_data <- tsDyn::VAR.boot(bootstrap_var, boot.scheme = "resample")
      boot_fit <- vars::VAR(boot_data, p = lags, type = "const")
      boot_factor_irf <- .normalized_favar_irf(boot_fit, numhor)[
        , colnames(factors), drop = FALSE
      ]
      boot_target[, b] <- as.numeric((boot_factor_irf %*% loadings)[, yt_idx])
    }
    se <- apply(boot_target[-1L, , drop = FALSE], 1, stats::sd, na.rm = TRUE)
    lower <- apply(
      boot_target[-1L, , drop = FALSE], 1, stats::quantile,
      probs = alpha / 2, na.rm = TRUE
    )
    upper <- apply(
      boot_target[-1L, , drop = FALSE], 1, stats::quantile,
      probs = 1 - alpha / 2, na.rm = TRUE
    )
  } else {
    se <- lower <- upper <- rep(NA_real_, numhor)
  }

  list(
    method_id = "favar",
    est = est,
    se = se,
    lower = lower,
    upper = upper,
    identification = "shock_first_recursive"
  )
}

.circular_block_indices <- function(n, block_length, draws) {
  if (draws == 0L) return(matrix(integer(0), nrow = n, ncol = 0L))
  out <- matrix(NA_integer_, nrow = n, ncol = draws)
  blocks_needed <- ceiling(n / block_length)
  for (b in seq_len(draws)) {
    starts <- sample.int(n, blocks_needed, replace = TRUE)
    indices <- unlist(lapply(starts, function(start) {
      ((start - 1L + seq_len(block_length) - 1L) %% n) + 1L
    }))
    out[, b] <- indices[seq_len(n)]
  }
  out
}

# Random-subspace LP for a directly observed shock. Essential variables enter
# with all p lags; each random draw adds k standardized first-lag predictors.
lp_rslp <- function(
    yt,
    xt,
    wt,
    essential_indices = integer(0),
    numhor = 12,
    lags = 4,
    M = 1000,
    k = 50,
    n_boot = 100,
    seed = 1
) {
  wt <- as.matrix(wt)
  Tn <- length(yt)
  essential_indices <- intersect(as.integer(essential_indices), seq_len(ncol(wt)))
  essential <- wt[, essential_indices, drop = FALSE]
  base_series <- .clean_control_matrix(
    cbind(yt = yt, xt = xt, essential), protected = list()
  )
  base_lags <- create_lags(base_series, lags)

  potential_indices <- setdiff(seq_len(ncol(wt)), essential_indices)
  potential <- wt[, potential_indices, drop = FALSE]
  usable <- apply(potential, 2, stats::sd, na.rm = TRUE) >
    sqrt(.Machine$double.eps)
  potential <- scale(potential[, usable, drop = FALSE])
  potential_indices <- potential_indices[usable]
  if (ncol(potential) == 0L) stop("RSLP has no potential controls.")

  k <- min(as.integer(k), ncol(potential))
  M <- as.integer(M)
  set.seed(seed)
  subspaces <- replicate(M, sample.int(ncol(potential), k), simplify = FALSE)

  est <- se <- lower <- upper <- rep(NA_real_, numhor)
  for (h in seq_len(numhor)) {
    tt <- (lags + 1L):(Tn - h)
    y_reg <- yt[tt + h]
    bootstrap_indices <- .circular_block_indices(
      length(tt), max(1L, h), n_boot
    )
    beta_subspace <- rep(NA_real_, M)
    bootstrap_sum <- numeric(n_boot)
    valid <- 0L

    for (m in seq_len(M)) {
      selected <- subspaces[[m]]
      X <- cbind(
        intercept = 1,
        shock = xt[tt],
        base_lags[tt, , drop = FALSE],
        potential[tt - 1L, selected, drop = FALSE]
      )
      keep <- stats::complete.cases(y_reg, X)
      if (!all(keep) || nrow(X) <= ncol(X)) next

      fit <- stats::lm.fit(X, y_reg)
      if (anyNA(fit$coefficients)) next
      valid <- valid + 1L
      beta_subspace[m] <- fit$coefficients["shock"]

      if (n_boot > 0L) {
        xtx_inverse <- tryCatch(
          chol2inv(chol(crossprod(X))),
          error = function(e) NULL
        )
        if (is.null(xtx_inverse)) next
        shock_weights <- xtx_inverse[2L, , drop = FALSE] %*% t(X)
        residuals <- fit$residuals - mean(fit$residuals)
        boot_residuals <- matrix(
          residuals[bootstrap_indices],
          nrow = length(residuals), ncol = n_boot
        )
        bootstrap_sum <- bootstrap_sum + fit$coefficients["shock"] +
          as.numeric(shock_weights %*% boot_residuals)
      }
    }

    if (valid == 0L) next
    est[h] <- mean(beta_subspace, na.rm = TRUE)
    if (n_boot > 0L) {
      bootstrap_mean <- bootstrap_sum / valid
      se[h] <- stats::sd(bootstrap_mean)
      lower[h] <- stats::quantile(bootstrap_mean, 0.025)
      upper[h] <- stats::quantile(bootstrap_mean, 0.975)
    }
  }

  list(
    method_id = "rslp",
    est = est,
    se = se,
    lower = lower,
    upper = upper,
    subspace_draws = M,
    subspace_dimension = k,
    potential_ids = potential_indices
  )
}

# LAVA local projections -------------------------------------------------------

lp_lava <- function(
    yt,
    xt,
    wt,
    numhor = 12,
    lags = 4,
    Lambda1 = NULL,
    Lambda2 = NULL,
    method = "profile",
    Kfold = 2,
    Maxiter = 50
) {
  wt <- as.matrix(wt)
  Tn <- length(yt)
  lagged_controls <- create_lags(cbind(yt = yt, xt = xt, wt), lags)
  est <- se <- rep(NA_real_, numhor)

  if (is.null(Lambda2)) Lambda2 <- c(0.01, 0.07, 0.2, 0.7, 3)
  if (is.null(Lambda1)) Lambda1 <- seq(0.01, 6, 6 / 50)

  for (h in seq_len(numhor)) {
    t_idx <- (lags + 1L):(Tn - h)
    y_lead <- yt[t_idx + h]
    Xmat <- cbind(
      shock = xt[t_idx],
      lagged_controls[t_idx, , drop = FALSE]
    )
    keep <- stats::complete.cases(y_lead, Xmat)
    y_lead <- y_lead[keep]
    Xmat <- Xmat[keep, , drop = FALSE]
    n_obs <- length(y_lead)
    if (n_obs <= 2L || ncol(Xmat) < 2L) next

    n_adj <- if (n_obs %% Kfold == 0L) {
      n_obs
    } else {
      n_obs - (n_obs %% Kfold)
    }
    if (n_adj <= Kfold) next

    lava_fit <- Lavash(
      X = as.matrix(Xmat[seq_len(n_adj), , drop = FALSE]),
      Y = matrix(y_lead[seq_len(n_adj)], ncol = 1),
      K = Kfold,
      Lambda1 = Lambda1,
      Lambda2 = Lambda2,
      method = method,
      Maxiter = Maxiter
    )
    est[h] <- as.numeric(lava_fit$post_lava)[1L]
  }

  list(method_id = "lava", est = est, se = se, selected_lags = lags)
}

lp_lava_boot <- function(
    yt,
    xt,
    wt,
    numhor = 12,
    lags = 4,
    n_boot = 100,
    block_len = 10,
    seed = 1,
    ...
) {
  Tn <- length(yt)
  fit0 <- lp_lava(
    yt = yt, xt = xt, wt = wt,
    numhor = numhor, lags = lags, ...
  )
  est <- fit0$est
  se <- rep(NA_real_, numhor)

  n_boot <- as.integer(n_boot)
  if (n_boot > 0L) {
    set.seed(seed)
    est_boot <- matrix(NA_real_, nrow = numhor, ncol = n_boot)
    bootstrap_indices <- .circular_block_indices(Tn, block_len, n_boot)

    for (b in seq_len(n_boot)) {
      idx <- bootstrap_indices[, b]
      fit_b <- lp_lava(
        yt = yt[idx],
        xt = xt[idx],
        wt = wt[idx, , drop = FALSE],
        numhor = numhor,
        lags = lags,
        ...
      )
      est_boot[, b] <- fit_b$est
    }
    se <- apply(est_boot, 1L, stats::sd, na.rm = TRUE)
  }

  list(method_id = "lava", est = est, se = se, selected_lags = lags)
}

# OGA double-selection local projections --------------------------------------

.partial_out <- function(y, controls) {
  if (is.null(controls) || ncol(controls) == 0L) return(y)
  qr.resid(qr(controls), y)
}

.lagged_control_labels <- function(wt, lags) {
  variable_names <- colnames(wt)
  if (is.null(variable_names)) {
    variable_names <- sprintf("V%03d", seq_len(ncol(wt)))
  } else {
    missing_names <- !nzchar(variable_names)
    variable_names[missing_names] <- sprintf(
      "V%03d", which(missing_names)
    )
    variable_names <- make.unique(variable_names)
  }
  unlist(lapply(seq_len(lags), function(lag) {
    paste0("L", lag, ":", variable_names)
  }))
}

# Double-selection OGA+HDAIC with Parzen HAC inference and selection records.
lp_oga <- function(
    yt,
    xt,
    wt,
    numhor = 12,
    lags = 4,
    HDIC_Type = "HDAIC",
    cstar = 2,
    hac_lags = 4L
) {
  wt <- as.matrix(wt)
  labels <- .lagged_control_labels(wt, lags)

  base <- gen_data_lp(yt, xt, wt = wt, horizon = 1, numlag = lags)
  x_base <- as.numeric(base[[2]])
  controls_base <- base[[3]]

  shock_selection <- Ohit(
    controls_base,
    x_base,
    HDIC_Type = HDIC_Type,
    c2 = cstar,
    intercept = FALSE
  )
  shock_ids_selected <- shock_selection$J_HDIC

  est <- se <- n_beta <- n_union <- rep(NA_real_, numhor)
  n_gamma <- rep(length(shock_ids_selected), numhor)
  n_always <- rep(0L, numhor)
  outcome_ids <- union_ids <- vector("list", numhor)

  for (h in seq_len(numhor)) {
    data_h <- gen_data_lp(yt, xt, wt = wt, horizon = h, numlag = lags)
    y_h <- as.numeric(data_h[[1]])
    x_h <- as.numeric(data_h[[2]])
    controls_h <- data_h[[3]]
    outcome_selection <- Ohit(
      controls_h,
      y_h,
      HDIC_Type = HDIC_Type,
      c2 = cstar,
      intercept = FALSE
    )
    outcome_ids_selected <- outcome_selection$J_HDIC
    union_ids_selected <- union(shock_ids_selected, outcome_ids_selected)
    union_controls_h <- controls_h[, union_ids_selected, drop = FALSE]
    shock_residual <- as.numeric(.partial_out(x_h, union_controls_h))

    n_beta[h] <- length(outcome_ids_selected)
    n_union[h] <- length(union_ids_selected)
    outcome_ids[[h]] <- labels[outcome_ids_selected]
    union_ids[[h]] <- labels[union_ids_selected]

    X <- cbind(
      shock = x_h,
      union_controls_h
    )
    coefficients <- solve(crossprod(X), crossprod(X, y_h))
    est[h] <- coefficients[1]
    residual <- as.numeric(y_h - X %*% coefficients)
    long_run_variance <- manual_hac(
      shock_residual * residual,
      lags = hac_lags,
      kernel = "parzen"
    )
    tau_squared <- mean(shock_residual^2)
    se[h] <- sqrt(long_run_variance / length(y_h)) / tau_squared
  }

  shock_ids <- rep(list(labels[shock_ids_selected]), numhor)
  always_ids <- rep(list(character(0)), numhor)
  list(
    method_id = "oga",
    est = est,
    se = se,
    n_gamma = n_gamma,
    n_beta = n_beta,
    n_union = n_union,
    n_always = n_always,
    n_total = n_union + n_always,
    shock_ids = shock_ids,
    outcome_ids = outcome_ids,
    union_ids = union_ids,
    always_ids = always_ids,
    hac_kernel = "parzen",
    hac_lags = hac_lags
  )
}

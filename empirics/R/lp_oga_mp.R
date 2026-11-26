# Double-selection OGA for the recursively identified monetary-policy LP.
# Current slow variables are always included; fast variables enter with lags.

lp_oga_mp <- function(
    outcomes,
    shock,
    slow,
    fast,
    numhor = 49,
    lags = 13,
    cumulate = NULL,
    HDIC_Type = "HDAIC",
    hac_kernel = "parzen",
    progress = TRUE) {

  if (!is.list(outcomes) || length(outcomes) == 0L || is.null(names(outcomes))) {
    stop("outcomes must be a named, non-empty list")
  }

  shock <- as.numeric(shock)
  slow <- as.matrix(slow)
  fast <- as.matrix(fast)
  T_obs <- length(shock)

  if (nrow(slow) != T_obs || nrow(fast) != T_obs ||
      any(vapply(outcomes, length, integer(1)) != T_obs)) {
    stop("shock, outcomes, slow, and fast must have the same number of observations")
  }
  if (numhor < 1L || lags < 1L || numhor + lags >= T_obs) {
    stop("numhor and lags imply an empty estimation sample")
  }
  hac_kernel <- match.arg(hac_kernel, c("bartlett", "parzen"))

  if (is.null(colnames(slow))) {
    colnames(slow) <- paste0("slow", seq_len(ncol(slow)))
  }
  if (is.null(colnames(fast))) {
    colnames(fast) <- paste0("fast", seq_len(ncol(fast)))
  }
  if (anyDuplicated(c(colnames(slow), "FFR", colnames(fast)))) {
    stop("slow, shock, and fast variable names must be unique")
  }

  outcomes <- lapply(outcomes, as.numeric)
  outcome_names <- names(outcomes)

  if (is.null(cumulate)) {
    cumulate <- setNames(rep(FALSE, length(outcomes)), outcome_names)
  }
  if (is.null(names(cumulate))) {
    if (length(cumulate) != length(outcomes)) {
      stop("unnamed cumulate must have one entry per outcome")
    }
    names(cumulate) <- outcome_names
  }
  if (!all(outcome_names %in% names(cumulate))) {
    stop("cumulate must contain an entry for every outcome")
  }
  cumulate <- setNames(as.logical(cumulate[outcome_names]), outcome_names)

  lag_information <- cbind(slow, FFR = shock, fast)
  lag_pool_names <- unlist(
    lapply(seq_len(lags), function(k) {
      paste0(colnames(lag_information), "_lag", k)
    }),
    use.names = FALSE
  )
  baseline_names <- c("(Intercept)", paste0(colnames(slow), "_t"))

  partial_out <- function(Y, D_qr) qr.resid(D_qr, Y)

  build_lag_pool <- function(idx) {
    W <- do.call(
      cbind,
      lapply(seq_len(lags), function(k) {
        lag_information[idx - k, , drop = FALSE]
      })
    )
    colnames(W) <- lag_pool_names
    W
  }

  base_idx <- (lags + 1L):(T_obs - 1L)
  D_base <- cbind(`(Intercept)` = 1, slow[base_idx, , drop = FALSE])
  colnames(D_base) <- baseline_names
  D_base_qr <- qr(D_base, tol = 1e-10)
  if (D_base_qr$rank != ncol(D_base)) {
    stop("always-included controls are rank deficient in the shock equation")
  }

  W_base <- build_lag_pool(base_idx)
  x_base_po <- as.numeric(partial_out(shock[base_idx], D_base_qr))
  W_base_po <- partial_out(W_base, D_base_qr)
  gamma_res <- Ohit(
    W_base_po,
    x_base_po,
    HDIC_Type = HDIC_Type,
    intercept = FALSE
  )
  J_gamma <- as.integer(gamma_res$J_HDIC)
  shock_selection <- data.frame(
    id = J_gamma,
    covariate = lag_pool_names[J_gamma],
    row.names = NULL
  )

  empty_result <- function() {
    list(
      est = rep(NA_real_, numhor),
      se = rep(NA_real_, numhor),
      J_shock_id = J_gamma,
      J_shock = lag_pool_names[J_gamma],
      shock_selection = shock_selection,
      n_shock = length(J_gamma),
      J_y_id = vector("list", numhor),
      J_y = vector("list", numhor),
      J_all_id = vector("list", numhor),
      J_all = vector("list", numhor),
      n_y = rep(NA_integer_, numhor),
      n_all = rep(NA_integer_, numhor),
      tau = rep(NA_real_, numhor)
    )
  }
  results <- setNames(lapply(outcome_names, function(x) empty_result()), outcome_names)

  for (h in seq_len(numhor)) {
    idx <- (lags + 1L):(T_obs - h)
    Th <- length(idx)

    D_h <- cbind(`(Intercept)` = 1, slow[idx, , drop = FALSE])
    colnames(D_h) <- baseline_names
    D_qr <- qr(D_h, tol = 1e-10)
    if (D_qr$rank != ncol(D_h)) {
      stop("always-included controls are rank deficient at horizon ", h)
    }

    W_h <- build_lag_pool(idx)
    x_h <- shock[idx]
    x_po <- as.numeric(partial_out(x_h, D_qr))
    W_po <- partial_out(W_h, D_qr)

    for (outcome_name in outcome_names) {
      y_series <- outcomes[[outcome_name]]
      if (cumulate[[outcome_name]]) {
        y_h <- vapply(
          idx,
          function(t) sum(y_series[t:(t + h)]),
          numeric(1)
        )
      } else {
        y_h <- y_series[idx + h]
      }
      y_po <- as.numeric(partial_out(y_h, D_qr))

      beta_res <- Ohit(
        W_po,
        y_po,
        HDIC_Type = HDIC_Type,
        intercept = FALSE
      )
      J_beta <- as.integer(beta_res$J_HDIC)
      J_all <- union(J_beta, J_gamma)

      if (length(J_all) > 0L) {
        W_union <- W_po[, J_all, drop = FALSE]
        v_h <- as.numeric(qr.resid(qr(W_union, tol = 1e-10), x_po))
      } else {
        W_union <- W_po[, integer(0), drop = FALSE]
        v_h <- x_po
      }
      tau_h <- mean(v_h * x_po)
      if (!is.finite(tau_h) || abs(tau_h) < .Machine$double.eps^0.5) {
        stop(
          "degenerate union-residualized shock score at horizon ", h,
          " for ", outcome_name
        )
      }

      X_final <- cbind(shock = x_po, W_union)
      bhat <- qr.solve(X_final, y_po, tol = 1e-10)
      u_hat <- as.numeric(y_po - X_final %*% bhat)
      Omega_h <- manual_hac(v_h * u_hat, kernel = hac_kernel)

      results[[outcome_name]]$est[h] <- unname(bhat[1])
      results[[outcome_name]]$se[h] <- sqrt(Omega_h / Th) / abs(tau_h)
      results[[outcome_name]]$J_y_id[[h]] <- J_beta
      results[[outcome_name]]$J_y[[h]] <- lag_pool_names[J_beta]
      results[[outcome_name]]$J_all_id[[h]] <- J_all
      results[[outcome_name]]$J_all[[h]] <- lag_pool_names[J_all]
      results[[outcome_name]]$n_y[h] <- length(J_beta)
      results[[outcome_name]]$n_all[h] <- length(J_all)
      results[[outcome_name]]$tau[h] <- tau_h
    }

    if (isTRUE(progress) && (h == 1L || h %% 5L == 0L || h == numhor)) {
      message("Monetary-policy OGA: horizon ", h, "/", numhor)
    }
  }

  specification <- list(
    identification = paste0(
      "recursive: current slow variables are always included; ",
      "fast variables enter only with lags"
    ),
    baseline_controls = baseline_names,
    n_baseline_including_intercept = length(baseline_names),
    lag_information = colnames(lag_information),
    n_lag_series = ncol(lag_information),
    lags = lags,
    n_structural_regressors_before_selection =
      1L + ncol(slow) + lags * ncol(lag_information),
    shock_selection_sample_horizon = 1L,
    shock_selection_sample_size = length(base_idx),
    HDIC_Type = HDIC_Type,
    hac_kernel = hac_kernel,
    cumulate = cumulate
  )

  lapply(results, function(result) {
    result$baseline_controls <- baseline_names
    result$n_baseline <- length(baseline_names)
    result$specification <- specification
    result
  })
}

# Factor-augmented local projection for the monetary-policy application.

lp_falp <- function(
    yt,
    xt,
    wt,
    slow_controls,
    numhor = 49,
    num_factors = 6,
    lags = 13,
    cumulate_y = FALSE,
    hac_kernel = "parzen") {

  yt <- as.numeric(yt)
  xt <- as.numeric(xt)
  wt <- as.matrix(wt)
  slow_controls <- as.matrix(slow_controls)
  T_obs <- length(yt)

  if (length(xt) != T_obs || nrow(wt) != T_obs ||
      nrow(slow_controls) != T_obs) {
    stop("yt, xt, wt, and slow_controls must have the same number of observations")
  }
  if (numhor < 1L || lags < 1L || numhor + lags >= T_obs) {
    stop("numhor and lags imply an empty estimation sample")
  }
  hac_kernel <- match.arg(hac_kernel, c("bartlett", "parzen"))

  usable <- apply(wt, 2, stats::sd, na.rm = TRUE) > sqrt(.Machine$double.eps)
  if (!any(usable)) stop("wt contains no usable columns for PCA")
  if (anyNA(wt[, usable, drop = FALSE])) stop("PCA input contains missing values")

  pca <- stats::prcomp(wt[, usable, drop = FALSE], center = TRUE, scale. = TRUE)
  n_factors <- min(as.integer(num_factors), ncol(pca$x))
  factors <- pca$x[, seq_len(n_factors), drop = FALSE]
  colnames(factors) <- paste0("factor", seq_len(n_factors))

  same_y_x <- isTRUE(all.equal(yt, xt, tolerance = sqrt(.Machine$double.eps)))
  if (same_y_x) {
    dynamic_series <- cbind(FFR = xt, factors)
  } else {
    dynamic_series <- cbind(outcome = yt, FFR = xt, factors)
  }

  create_lags <- function(x, p) {
    x <- as.matrix(x)
    out <- matrix(NA_real_, nrow = nrow(x), ncol = p * ncol(x))
    out_names <- character(ncol(out))
    for (k in seq_len(p)) {
      cols <- ((k - 1L) * ncol(x) + 1L):(k * ncol(x))
      out[(k + 1L):nrow(x), cols] <- x[seq_len(nrow(x) - k), , drop = FALSE]
      out_names[cols] <- paste0(colnames(x), "_lag", k)
    }
    colnames(out) <- out_names
    out
  }

  lagged_controls <- create_lags(dynamic_series, lags)
  if (is.null(colnames(slow_controls))) {
    colnames(slow_controls) <- paste0("slow", seq_len(ncol(slow_controls)))
  }

  est <- se <- tau <- rep(NA_real_, numhor)
  n_obs <- rep(NA_integer_, numhor)

  for (h in seq_len(numhor)) {
    tt <- (lags + 1L):(T_obs - h)
    if (isTRUE(cumulate_y)) {
      y_reg <- vapply(tt, function(t) sum(yt[t:(t + h)]), numeric(1))
    } else {
      y_reg <- yt[tt + h]
    }

    controls <- cbind(
      slow_controls[tt, , drop = FALSE],
      lagged_controls[tt, , drop = FALSE]
    )
    X <- cbind(`(Intercept)` = 1, shock = xt[tt], controls)
    keep <- stats::complete.cases(y_reg, X)
    X <- X[keep, , drop = FALSE]
    y_reg <- y_reg[keep]
    n_obs[h] <- nrow(X)

    if (nrow(X) <= ncol(X)) stop("insufficient FALP observations at horizon ", h)
    X_qr <- qr(X, tol = 1e-10)
    if (X_qr$rank != ncol(X)) stop("FALP design is rank deficient at horizon ", h)

    bhat <- qr.coef(X_qr, y_reg)
    u_hat <- as.numeric(y_reg - X %*% bhat)
    nuisance <- X[, -2L, drop = FALSE]
    nuisance_qr <- qr(nuisance, tol = 1e-10)
    v_hat <- as.numeric(qr.resid(nuisance_qr, X[, "shock"]))
    tau[h] <- mean(v_hat * X[, "shock"])
    if (!is.finite(tau[h]) || abs(tau[h]) < .Machine$double.eps^0.5) {
      stop("degenerate FALP shock residual at horizon ", h)
    }

    Omega_h <- manual_hac(v_hat * u_hat, kernel = hac_kernel)
    est[h] <- unname(bhat["shock"])
    se[h] <- sqrt(Omega_h / nrow(X)) / abs(tau[h])
  }

  variance_share <- pca$sdev^2 / sum(pca$sdev^2)
  list(
    method_id = "falp",
    est = est,
    se = se,
    tau = tau,
    n_obs = n_obs,
    specification = list(
      identification = paste0(
        "recursive: current slow variables always included; ",
        "factors and other dynamic variables lagged"
      ),
      n_always_included_including_intercept = 1L + ncol(slow_controls),
      n_factors = n_factors,
      factor_variance_share = variance_share[seq_len(n_factors)],
      cumulative_factor_variance_share = sum(variance_share[seq_len(n_factors)]),
      factor_panel = colnames(wt)[usable],
      dynamic_series = colnames(dynamic_series),
      lags = lags,
      cumulate_y = isTRUE(cumulate_y),
      hac_kernel = hac_kernel
    )
  )
}

# Orthogonal greedy selection and HAC inference used by the OGA/FALP methods.

Ohit <- function(X, y, Kn = NULL, c1 = 5, HDIC_Type = "HDBIC",
                  c2 = 2, c3 = 2.01, intercept = TRUE) {
  if (!is.vector(y)) stop("y should be a vector")
  if (!is.matrix(X)) stop("X should be a matrix")

  n <- nrow(X)
  p <- ncol(X)
  if (n != length(y)) stop("y and X must have the same number of observations")
  if (n == 1) stop("the sample size should be greater than 1")

  if (is.null(Kn)) {
    K <- max(1, min(floor(c1 * sqrt(n / log(p))), p))
  } else {
    if (Kn < 1 || Kn > p || Kn != floor(Kn)) {
      stop("Kn should be an integer between 1 and p")
    }
    K <- Kn
  }

  dy <- y - mean(y)
  dX <- apply(X, 2, function(x) x - mean(x))
  Jhat <- sigma2hat <- rep(0, K)
  XJhat <- matrix(0, n, K)
  u <- as.matrix(dy)
  xnorms <- sqrt(colSums(dX^2))

  aSSE <- abs(t(u) %*% dX) / xnorms
  Jhat[1] <- which.max(aSSE)
  XJhat[, 1] <- dX[, Jhat[1]] / sqrt(sum(dX[, Jhat[1]]^2))
  u <- u - XJhat[, 1] %*% t(XJhat[, 1]) %*% u
  sigma2hat[1] <- mean(u^2)

  if (K > 1) {
    for (k in 2:K) {
      aSSE <- abs(t(u) %*% dX) / xnorms
      aSSE[Jhat[1:(k - 1)]] <- 0
      Jhat[k] <- which.max(aSSE)
      rq <- dX[, Jhat[k]] -
        XJhat[, 1:(k - 1), drop = FALSE] %*%
        t(XJhat[, 1:(k - 1), drop = FALSE]) %*% dX[, Jhat[k]]
      XJhat[, k] <- rq / sqrt(sum(rq^2))
      u <- u - XJhat[, k] %*% t(XJhat[, k]) %*% u
      sigma2hat[k] <- mean(u^2)
    }
  }

  if (!HDIC_Type %in% c("HDAIC", "HDBIC", "HDHQ")) {
    stop("HDIC_Type must be HDAIC, HDBIC, or HDHQ")
  }
  if (HDIC_Type == "HDAIC") omega_n <- c2
  if (HDIC_Type == "HDBIC") omega_n <- log(n)
  if (HDIC_Type == "HDHQ") omega_n <- c3 * log(log(n))

  hdic <- n * log(sigma2hat) + seq_len(K) * omega_n * log(p)
  kn_hat <- which.min(hdic)
  benchmark <- hdic[kn_hat]
  J_HDIC <- sort(Jhat[seq_len(kn_hat)])

  J_Trim <- Jhat[seq_len(kn_hat)]
  trim_pos <- rep(0, kn_hat)
  if (kn_hat > 1) {
    for (l in seq_len(kn_hat - 1)) {
      JDrop1 <- J_Trim[-l]
      fit <- lm(dy ~ . - 1, data = data.frame(dX[, JDrop1, drop = FALSE]))
      uDrop1 <- fit$residuals
      HDICDrop1 <- n * log(mean(uDrop1^2)) +
        (kn_hat - 1) * omega_n * log(p)
      if (HDICDrop1 > benchmark) trim_pos[l] <- 1
    }
    trim_pos[kn_hat] <- 1
    J_Trim <- J_Trim[trim_pos == 1]
  }
  J_Trim <- sort(J_Trim)

  X_HDIC <- as.data.frame(as.matrix(X[, J_HDIC, drop = FALSE]))
  X_Trim <- as.data.frame(as.matrix(X[, J_Trim, drop = FALSE]))
  X_frame <- data.frame(X)
  colnames(X_HDIC) <- names(X_frame)[J_HDIC]
  colnames(X_Trim) <- names(X_frame)[J_Trim]

  if (intercept) {
    fit_HDIC <- lm(y ~ ., data = X_HDIC)
    fit_Trim <- lm(y ~ ., data = X_Trim)
  } else {
    fit_HDIC <- lm(y ~ . - 1, data = X_HDIC)
    fit_Trim <- lm(y ~ . - 1, data = X_Trim)
  }

  list(
    n = n,
    p = p,
    Kn = K,
    J_OGA = Jhat,
    HDIC = hdic,
    J_HDIC = J_HDIC,
    J_Trim = J_Trim,
    betahat_HDIC = summary(fit_HDIC),
    betahat_Trim = summary(fit_Trim)
  )
}

manual_hac <- function(z, K = NULL, kernel = c("bartlett", "parzen")) {
  kernel <- match.arg(kernel)
  z <- as.numeric(z)
  T_obs <- length(z)
  z <- z - mean(z)

  if (is.null(K)) K <- floor(4 * (T_obs / 100)^(2 / 9))

  weight <- function(x) {
    if (kernel == "bartlett") return(1 - x)
    if (x <= 0.5) return(1 - 6 * x^2 + 6 * x^3)
    if (x <= 1) return(2 * (1 - x)^3)
    0
  }

  Omega <- 0
  for (k in 0:K) {
    if (k == 0) {
      gamma <- sum(z * z) / T_obs
      Omega <- Omega + gamma
    } else {
      gamma <- sum(z[(k + 1):T_obs] * z[1:(T_obs - k)]) / T_obs
      Omega <- Omega + 2 * weight(k / (K + 1)) * gamma
    }
  }
  Omega
}

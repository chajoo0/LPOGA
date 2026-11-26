# Lavash 1.0
# Authors: Victor Chernozhukov, Christian Hansen, and Yuan Liao
# Contributor and maintainer: Jaeheon Jung
# Source: CRAN Archive, https://cran.r-project.org/src/contrib/Archive/Lavash/
# License: GPL-2
# Paper: Chernozhukov, Hansen, and Liao (2017),
# "A Lava Attack on the Recovery of Sums of Dense and Sparse Signals."
# Reformatted without numerical changes; used for the post-LAVA benchmark.
Lavash <- function (X, Y, K, Lambda1, Lambda2, method = c("profile", "iteration"), Maxiter = 50) 
{
    if (is.matrix(X) < 1) {
        stop("x should be a matrix with 2 or more columns")
    }
    dimX = dim(X)
    if (is.null(dimX) | (dimX[2] <= 1)) {
        stop("X should be a matrix style with 2 or more columns")
    }
    if (is.matrix(Y) < 1) {
        stop("Y should be a n by 1 matrix style")
    }
    dimY = dim(Y)
    if (dimX[1] != dimY[1]) {
        stop("number of observations in Y is not equal to the number of rows of X")
    }
    if ((dimY[1]%%K) != 0 | K < 1) {
        stop("K should be a natural number which divides the number of observations in Y without leaving a remainder")
    }
    if (is.vector(Lambda1) < 1) {
        stop("Lambda1 should be a vector style")
    }
    if (is.vector(Lambda2) < 1) {
        stop("Lambda1 should be a vector style")
    }
    if (method != "profile" & method != "iteration") {
        stop("method should be either profile or iteration")
    }
    if ((Maxiter%%1) != 0 | Maxiter < 1) {
        stop("Maxiter should be a natural number")
    }
    if (method == "iteration") {
        n <- length(Y)
        p <- length(X[1, ])
        Mitera <- Maxiter
        S = t(X) %*% X/n
        temp_svd <- svd(X, nu = n, nv = p)
        U <- temp_svd$u
        M <- diag(temp_svd$d, n, p)
        V <- temp_svd$v
        elava_array <- array(dim = c(K, length(Lambda1), length(Lambda2)))
        elavapost <- array(dim = c(K, length(Lambda1), length(Lambda2)))
        ridge <- matrix(0, p, 1)
        for (k in 1:K) {
            Y1 <- as.matrix(Y[-c(((k - 1) * n/K + 1):(k * n/K)), 1])
            X1 <- X[-c(((k - 1) * n/K + 1):(k * n/K)), ]
            S1 <- t(X1) %*% X1/(n - n/K)
            X1_svd <- svd(X1, nu = nrow(X1), nv = ncol(X1))
            U1 <- X1_svd$u
            M1 <- diag(X1_svd$d, nrow(X1), ncol(X1))
            V1 <- X1_svd$v
            vY <- as.matrix(Y[((k - 1) * n/K + 1):((k - 1) * n/K + n/K), 1])
            vX <- X[((k - 1) * n/K + 1):((k - 1) * n/K + n/K), ]
            for (l in 1:length(Lambda2)) {
                ridge_temp <- glmnet::glmnet(X1, Y1, lambda = Lambda2[l], alpha = 0)
                ridge <- as.matrix(ridge_temp$beta)
                BETAlava <- matrix(0, p, length(Lambda1))
                THETA <- matrix(0, p, length(Lambda1))
                DELTA <- matrix(0, p, length(Lambda1))
                respost <- matrix(0, nrow(vX), length(Lambda1))
                for (mm in 1:length(Lambda1)) {
                  iterar <- 1
                  BETAlava[, mm] <- ridge
                  THETA[, mm] <- pracma::ones(p, 1)
                  tole <- 100
                  while (iterar < Mitera && tole > 1e-07) {
                    THETAold <- THETA[, mm]
                    Z1 <- Y1 - X1 %*% BETAlava[, mm]
                    DELTA_temp <- glmnet::glmnet(X1, Z1, lambda = Lambda1[mm])
                    DELTA[, mm] <- as.matrix(DELTA_temp$beta[, ncol(DELTA_temp$beta):1])
                    BETAlave_temp <- glmnet::glmnet(X1, (Y1 - X1 %*% DELTA[, mm]), lambda = Lambda2[l], 
                      alpha = 0)
                    BETAlava[, mm] <- as.matrix(BETAlave_temp$beta)
                    THETA[, mm] <- DELTA[, mm] + BETAlava[, mm]
                    tole <- norm(as.matrix(THETA[, mm] - THETAold), "E")/sqrt(p)
                    iterar <- iterar + 1
                  }
                  use <- abs(DELTA[, mm]) > 1e-07
                  if (sum(use) > 0) {
                    XJ1 <- X1[, use]
                    ww <- Y1 - X1 %*% BETAlava[, mm]
                    respost[, mm] <- vX[, use] %*% (pracma::mldivide((t(XJ1) %*% XJ1), t(XJ1)) %*% ww) + 
                      vX %*% BETAlava[, mm] - vY
                    rm(use, XJ1, ww)
                  }
                  else {
                    respost[, mm] <- vX %*% BETAlava[, mm] - vY
                  }
                }
                elavapost[k, , l] <- colSums((respost^2))
                residual <- vX %*% THETA - vY %*% matrix(1, 1, length(Lambda1))
                elava_array[k, , l] <- colSums((residual^2))
            }
        }
        CVRlava <- matrix(0, length(Lambda2), ncol(THETA))
        for (l in 1:length(Lambda2)) {
            CVRlava[l, ] <- colMeans(elava_array[, , l])
        }
        a <- matrix(0, 1, ncol(CVRlava))
        b <- matrix(0, 1, ncol(CVRlava))
        for (i in 1:ncol(CVRlava)) {
            a[1, i] <- min(CVRlava[, i])
            b[1, i] <- which.min(CVRlava[, i])
        }
        c <- min(a)
        d <- which.min(a)
        lambda2 <- Lambda2[b[1, d]]
        lambda1 <- Lambda1[d]
        lambda1lava <- lambda1
        lambda2lava <- lambda2
        iterar <- 1
        dense <- ridge
        lavatheta <- pracma::ones(p, 1)
        tole <- 100
        while (iterar < Mitera && tole > 1e-07) {
            THETAold <- lavatheta
            Z <- Y - X %*% dense
            deltahat_temp <- glmnet::glmnet(X, Z, lambda = lambda1)
            deltahat <- as.matrix(deltahat_temp$beta[, ncol(deltahat_temp$beta):1])
            dense_temp <- glmnet::glmnet(X, (Y - X %*% deltahat), lambda = lambda2, alpha = 0)
            dense <- as.matrix(dense_temp$beta)
            sparse <- deltahat
            lavatheta <- sparse + dense
            tole <- norm((lavatheta - THETAold), "E")/sqrt(p)
            iterar <- iterar + 1
        }
        CVRpostlava <- matrix(0, length(Lambda2), ncol(THETA))
        for (l in 1:length(Lambda2)) {
            CVRpostlava[l, ] <- colMeans(elavapost[, , l])
        }
        a <- matrix(0, 1, ncol(CVRpostlava))
        b <- matrix(0, 1, ncol(CVRpostlava))
        for (i in 1:ncol(CVRpostlava)) {
            a[1, i] <- min(CVRpostlava[, i])
            b[1, i] <- which.min(CVRpostlava[, i])
        }
        c <- min(a)
        d <- which.min(a)
        lambda2 <- Lambda2[b[1, d]]
        lambda1 <- Lambda1[d]
        lambda1post <- lambda1
        lambda2post <- lambda2
        iterar <- 1
        post_dense <- dense
        post_lavatheta <- lavatheta
        tole <- 100
        while (iterar < Mitera && tole > 1e-07) {
            THETAold <- post_lavatheta
            Z <- Y - X %*% post_dense
            post_sparse_temp <- glmnet::glmnet(X, Z, lambda = lambda1)
            post_sparse <- as.matrix(post_sparse_temp$beta[, ncol(post_sparse_temp$beta):1])
            post_dense_temp <- glmnet::glmnet(X, (Y - X %*% post_sparse), lambda = lambda2, alpha = 0)
            post_dense <- as.matrix(post_dense_temp$beta)
            post_lavatheta <- post_sparse + post_dense
            tole <- norm((post_lavatheta - THETAold), "E")/sqrt(p)
            iterar <- iterar + 1
        }
        use <- abs(post_sparse) > 1e-07
        post_sparse <- matrix(0, p, 1)
        if (sum(use) > 0) {
            XJ <- X[, use]
            post_sparse[use] <- pracma::pinv(t(XJ) %*% XJ) %*% t(XJ) %*% (Y - X %*% post_dense)
        }
        post_lavatheta <- post_dense + post_sparse
        rownames(dense) <- NULL
        colnames(dense) <- NULL
        rownames(deltahat) <- NULL
        colnames(deltahat) <- NULL
        rownames(lavatheta) <- NULL
        colnames(lavatheta) <- NULL
        rownames(post_dense) <- NULL
        colnames(post_dense) <- NULL
        rownames(post_sparse) <- NULL
        colnames(post_sparse) <- NULL
        rownames(post_lavatheta) <- NULL
        colnames(post_lavatheta) <- NULL
        LAMBDA <- c(lambda1lava, lambda2lava, lambda1post, lambda2post)
        output_list <- list(lava_dense = dense, lava_sparse = deltahat, lava_estimate = lavatheta, postlava_dense = post_dense, 
            postlava_sparse = post_sparse, post_lava = post_lavatheta, LAMBDA = LAMBDA, Iteration = iterar)
    }
    else if (method == "profile") {
        n <- length(Y)
        p <- length(X[1, ])
        S = t(X) %*% X/n
        temp_svd <- svd(X, nu = n, nv = p)
        U <- temp_svd$u
        M <- diag(temp_svd$d, n, p)
        V <- temp_svd$v
        elava_array <- array(dim = c(K, length(Lambda1), length(Lambda2)))
        elavapost <- array(dim = c(K, length(Lambda1), length(Lambda2)))
        for (k in 1:K) {
            Y1 <- as.matrix(Y[-c(((k - 1) * n/K + 1):(k * n/K)), 1])
            X1 <- X[-c(((k - 1) * n/K + 1):(k * n/K)), ]
            S1 <- t(X1) %*% X1/(n - n/K)
            X1_svd <- svd(X1, nu = nrow(X1), nv = ncol(X1))
            U1 <- X1_svd$u
            M1 <- diag(X1_svd$d, nrow(X1), ncol(X1))
            V1 <- X1_svd$v
            vY <- as.matrix(Y[((k - 1) * n/K + 1):((k - 1) * n/K + n/K), 1])
            vX <- X[((k - 1) * n/K + 1):((k - 1) * n/K + n/K), ]
            for (l in 1:length(Lambda2)) {
                H1 <- M1 %*% MASS::ginv(t(M1) %*% M1 + (n - n/K) * Lambda2[l] * diag(p)) %*% t(M1)
                Khalf1 <- U1 %*% pracma::sqrtm(diag(n - n/K) - H1)$B %*% t(U1)
                tY1 = Khalf1 %*% Y1
                tX1 = Khalf1 %*% X1
                DELTA_temp <- glmnet::glmnet(tX1, tY1, lambda = Lambda1)
                DELTA <- as.matrix(DELTA_temp$beta[, ncol(DELTA_temp$beta):1])
                BETAlava <- pracma::mldivide((S1 + Lambda2[l] * diag(p)), t(X1)) %*% (Y1 %*% matrix(1, 
                  1, length(Lambda1)) - X1 %*% DELTA)/(n - n/K)
                THETA <- DELTA + BETAlava
                residual <- vX %*% THETA - vY %*% matrix(1, 1, length(Lambda1))
                elava_array[k, , l] <- colSums((residual^2))
                rm(residual)
                respost <- matrix(0, nrow(vX), length(Lambda1))
                for (j in 1:length(Lambda1)) {
                  use <- abs(DELTA[, j]) > 1e-07
                  if (sum(use) > 0) {
                    XJ1 <- X1[, use]
                    ww <- Y1 - X1 %*% BETAlava[, j]
                    respost[, j] <- vX[, use] %*% (pracma::mldivide((t(XJ1) %*% XJ1), t(XJ1)) %*% ww) + 
                      vX %*% BETAlava[, j] - vY
                    rm(use, XJ1, ww)
                  }
                  else {
                    respost[, j] <- vX %*% BETAlava[, j] - vY
                  }
                }
                elavapost[k, , l] <- colSums((respost^2))
                rm(respost)
            }
        }
        CVRlava <- matrix(0, length(Lambda2), ncol(THETA))
        for (l in 1:length(Lambda2)) {
            CVRlava[l, ] <- colMeans(elava_array[, , l])
        }
        a <- matrix(0, 1, ncol(CVRlava))
        b <- matrix(0, 1, ncol(CVRlava))
        for (i in 1:ncol(CVRlava)) {
            a[1, i] <- min(CVRlava[, i])
            b[1, i] <- which.min(CVRlava[, i])
        }
        c <- min(a)
        d <- which.min(a)
        lambda2 <- Lambda2[b[1, d]]
        lambda1 <- Lambda1[d]
        lambda1lava <- lambda1
        lambda2lava <- lambda2
        P <- pracma::mrdivide(X, (S + lambda2 * diag(p))) %*% t(X)/n
        Kmatrix <- diag(n) - P
        H <- M %*% MASS::ginv(t(M) %*% M + n * lambda2 * diag(p)) %*% t(M)
        Khalf <- U %*% pracma::sqrtm(diag(n) - H)$B %*% t(U)
        tY <- Khalf %*% Y
        tX <- Khalf %*% X
        deltahat_temp <- glmnet::glmnet(tX, tY, lambda = lambda1)
        deltahat <- as.matrix(deltahat_temp$beta[, ncol(deltahat_temp$beta):1])
        dense <- pracma::mldivide((S + lambda2 * diag(p)), t(X)) %*% (Y - X %*% deltahat)/n
        lavatheta <- deltahat + dense
        CVRpostlava <- matrix(0, length(Lambda2), ncol(THETA))
        for (l in 1:length(Lambda2)) {
            CVRpostlava[l, ] <- colMeans(elavapost[, , l])
        }
        a <- matrix(0, 1, ncol(CVRpostlava))
        b <- matrix(0, 1, ncol(CVRpostlava))
        for (i in 1:ncol(CVRpostlava)) {
            a[1, i] <- min(CVRpostlava[, i])
            b[1, i] <- which.min(CVRpostlava[, i])
        }
        c <- min(a)
        d <- which.min(a)
        lambda2 <- Lambda2[b[1, d]]
        lambda1 <- Lambda1[d]
        lambda1post <- lambda1
        lambda2post <- lambda2
        P <- pracma::mrdivide(X, (S + lambda2 * diag(p))) %*% t(X)/n
        H <- M %*% MASS::ginv(t(M) %*% M + n * lambda2 * diag(p)) %*% t(M)
        Khalf <- U %*% pracma::sqrtm(diag(n) - H)$B %*% t(U)
        tY <- Khalf %*% Y
        tX <- Khalf %*% X
        deltahat_post_temp <- glmnet::glmnet(tX, tY, lambda = lambda1)
        deltahat_post <- as.matrix(deltahat_post_temp$beta[, ncol(deltahat_post_temp$beta):1])
        post_dense <- pracma::mldivide((S + lambda2 * diag(p)), t(X)) %*% (Y - X %*% deltahat_post)/n
        use <- abs(deltahat_post) > 1e-07
        post_sparse <- matrix(0, p, 1)
        if (sum(use) > 0) {
            XJ <- X[, use]
            post_sparse[use] <- pracma::pinv(t(XJ) %*% XJ) %*% t(XJ) %*% (Y - X %*% post_dense)
        }
        post_lavatheta <- post_dense + post_sparse
        rownames(dense) <- NULL
        colnames(dense) <- NULL
        rownames(deltahat) <- NULL
        colnames(deltahat) <- NULL
        rownames(lavatheta) <- NULL
        colnames(lavatheta) <- NULL
        rownames(post_dense) <- NULL
        colnames(post_dense) <- NULL
        rownames(post_sparse) <- NULL
        colnames(post_sparse) <- NULL
        rownames(post_lavatheta) <- NULL
        colnames(post_lavatheta) <- NULL
        LAMBDA <- c(lambda1lava, lambda2lava, lambda1post, lambda2post)
        output_list <- list(lava_dense = dense, lava_sparse = deltahat, lava_estimate = lavatheta, postlava_dense = post_dense, 
            postlava_sparse = post_sparse, post_lava = post_lavatheta, LAMBDA = LAMBDA)
        rm(list = setdiff(ls(), "output_list"))
    }
    return(output_list)
}

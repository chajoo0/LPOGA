sanitize_hdlp <- function(
    result,
    response,
    data_sha256,
    archived_from_sha256 = NULL) {
  expected_dim <- c(50L, 3L, 1L)
  if (is.null(result$intervals) ||
      !identical(dim(result$intervals), expected_dim)) {
    stop("HDLP intervals must have dimensions 50 x 3 x 1", call. = FALSE)
  }

  intervals <- result$intervals[, , 1]
  if (any(!is.finite(intervals))) {
    stop("HDLP intervals contain non-finite values", call. = FALSE)
  }

  list(
    method = "debiased Lasso",
    response = response,
    horizon = 0:49,
    estimate = as.numeric(intervals[, 2]),
    lower = as.numeric(intervals[, 1]),
    upper = as.numeric(intervals[, 3]),
    specification = list(hmax = 49L, lags = 13L, PI_constant = 0.4),
    provenance = list(
      data_file_sha256 = data_sha256,
      source_method = "desla::HDLP",
      archived_from_sha256 = archived_from_sha256
    )
  )
}

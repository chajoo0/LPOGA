# Export the archived HDLP intervals to portable, human-readable CSV files.
# The empirical analysis continues to read the compact RDS files.

input_directory <- file.path("results", "archived")
input_files <- Sys.glob(file.path(input_directory, "HDLP_*.rds"))
options(digits = 17)

if (length(input_files) == 0L) {
  stop("Run this script from the empirics directory.")
}

export_one_file <- function(input_file) {
  result <- readRDS(input_file)
  row_count <- length(result$horizon)

  required_vectors <- c("estimate", "lower", "upper")
  vector_lengths <- vapply(result[required_vectors], length, integer(1))
  if (any(vector_lengths != row_count)) {
    stop("Inconsistent result lengths in ", input_file, ".")
  }

  output <- data.frame(
    method = rep(result$method, row_count),
    response = rep(result$response, row_count),
    horizon = result$horizon,
    estimate = result$estimate,
    lower = result$lower,
    upper = result$upper,
    hmax = rep(result$specification$hmax, row_count),
    lags = rep(result$specification$lags, row_count),
    PI_constant = rep(result$specification$PI_constant, row_count),
    data_file_sha256 = rep(result$provenance$data_file_sha256, row_count),
    source_method = rep(result$provenance$source_method, row_count),
    stringsAsFactors = FALSE
  )
  if (!is.null(result$provenance$archived_from_sha256)) {
    output$archived_from_sha256 <- rep(
      result$provenance$archived_from_sha256,
      row_count
    )
  }

  output_file <- sub("\\.rds$", ".csv", input_file)
  output_for_csv <- output
  numeric_columns <- vapply(output_for_csv, is.numeric, logical(1))
  output_for_csv[numeric_columns] <- lapply(
    output_for_csv[numeric_columns],
    function(x) sprintf("%.17g", x)
  )
  utils::write.csv(output_for_csv, output_file, row.names = FALSE, na = "")

  csv_copy <- utils::read.csv(output_file, stringsAsFactors = FALSE, check.names = FALSE)
  comparison <- all.equal(output, csv_copy, tolerance = 1e-15, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop("CSV round-trip check failed for ", basename(input_file), ": ", comparison)
  }

  message("Wrote ", output_file, " (", nrow(output), " horizons).")
}

invisible(lapply(input_files, export_one_file))

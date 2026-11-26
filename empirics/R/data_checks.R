EXPECTED_DATA_SHA256 <- "a721ee8e6d07f59b8d163d1892846844875bae444165f07abd0d49a5c9e15411"
EXPECTED_DATA_MD5 <- "90694a3aef6673b32784b6fc52b2a82c"

check_data_file <- function(path) {
  if (!file.exists(path)) {
    stop(
      paste0(
        "The included copy of data/CDbmedium.RData is missing. ",
        "Restore it from the replication-package archive before running the analysis."
      ),
      call. = FALSE
    )
  }

  md5 <- unname(tools::md5sum(path))
  sha256 <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  if (!identical(md5, EXPECTED_DATA_MD5) ||
      !identical(sha256, EXPECTED_DATA_SHA256)) {
    stop(
      paste0(
        "The data file checksum does not match the required CDbmedium.RData. ",
        "Expected SHA-256: ", EXPECTED_DATA_SHA256,
        "; observed: ", sha256, "."
      ),
      call. = FALSE
    )
  }

  invisible(normalizePath(path, mustWork = TRUE))
}

load_monetary_data <- function(path) {
  CDbmedium <- readRDS(check_data_file(path))

  required_components <- c("slow_data", "fast_data", "FFR", "data_all")
  missing_components <- setdiff(required_components, names(CDbmedium))
  if (length(missing_components) > 0L) {
    stop(
      "CDbmedium.RData is missing components: ",
      paste(missing_components, collapse = ", "),
      call. = FALSE
    )
  }

  required_series <- c("INDPRO", "CPIAUCSL")
  missing_series <- setdiff(required_series, colnames(CDbmedium$data_all))
  if (length(missing_series) > 0L) {
    stop(
      "CDbmedium$data_all is missing series: ",
      paste(missing_series, collapse = ", "),
      call. = FALSE
    )
  }

  CDbmedium
}

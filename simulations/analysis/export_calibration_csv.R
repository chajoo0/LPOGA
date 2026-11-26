# Export the fixed MATLAB calibration to portable, human-readable CSV files.
# The simulation code continues to read dfm_calibration.mat.

if (!requireNamespace("R.matlab", quietly = TRUE)) {
  stop("Package 'R.matlab' is required to read dfm_calibration.mat.")
}

input_file <- "dfm_calibration.mat"
values_file <- "dfm_calibration.csv"
manifest_file <- "dfm_calibration_manifest.csv"

if (!file.exists(input_file)) {
  stop("Run this script from the simulations directory.")
}

calibration <- R.matlab::readMat(input_file)

field_names <- function(x) {
  dimensions <- dim(x)
  labels <- dimnames(x)

  if (!is.null(dimensions) && length(dimensions) == 3L &&
      dimensions[2] == 1L && dimensions[3] == 1L &&
      !is.null(labels) && !is.null(labels[[1]]) &&
      length(labels[[1]]) == length(x)) {
    return(labels[[1]])
  }

  names_x <- names(x)
  if (!is.null(names_x) && all(nzchar(names_x))) {
    return(names_x)
  }

  paste0("element_", seq_along(x))
}

format_value <- function(x) {
  if (is.numeric(x)) {
    return(format(x, digits = 17, scientific = TRUE, trim = TRUE))
  }

  if (is.logical(x)) {
    return(ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE")))
  }

  as.character(x)
}

leaf_rows <- list()
manifest_rows <- list()

record_leaf <- function(x, path) {
  dimensions <- dim(x)
  if (is.null(dimensions)) {
    dimensions <- length(x)
  }

  index_matrix <- arrayInd(seq_along(x), .dim = dimensions)
  index_frame <- as.data.frame(index_matrix, stringsAsFactors = FALSE)
  names(index_frame) <- paste0("index_", seq_len(ncol(index_frame)))

  values <- data.frame(
    path = rep(path, length(x)),
    storage_type = rep(typeof(x), length(x)),
    linear_index = seq_along(x),
    value = format_value(as.vector(x)),
    stringsAsFactors = FALSE
  )
  values <- cbind(values[1:3], index_frame, values[4])
  leaf_rows[[length(leaf_rows) + 1L]] <<- values

  manifest_rows[[length(manifest_rows) + 1L]] <<- data.frame(
    path = path,
    storage_type = typeof(x),
    dimensions = paste(dimensions, collapse = " x "),
    n_values = length(x),
    csv_file = values_file,
    stringsAsFactors = FALSE
  )
}

walk_object <- function(x, path) {
  if (is.list(x)) {
    labels <- field_names(x)
    for (i in seq_along(x)) {
      walk_object(x[[i]], paste(path, labels[[i]], sep = "."))
    }
    return(invisible(NULL))
  }

  if (is.atomic(x)) {
    record_leaf(x, path)
    return(invisible(NULL))
  }

  stop("Unsupported object at ", path, ": ", paste(class(x), collapse = ", "))
}

top_level_names <- names(calibration)
for (i in seq_along(calibration)) {
  walk_object(calibration[[i]], top_level_names[[i]])
}

all_index_columns <- sort(unique(unlist(lapply(leaf_rows, names))))
all_index_columns <- all_index_columns[grepl("^index_", all_index_columns)]

standardize_columns <- function(x) {
  missing_columns <- setdiff(all_index_columns, names(x))
  for (column in missing_columns) {
    x[[column]] <- NA_integer_
  }

  x[c("path", "storage_type", "linear_index", all_index_columns, "value")]
}

calibration_values <- do.call(rbind, lapply(leaf_rows, standardize_columns))
calibration_manifest <- do.call(rbind, manifest_rows)

utils::write.csv(calibration_values, values_file, row.names = FALSE, na = "")
utils::write.csv(calibration_manifest, manifest_file, row.names = FALSE, na = "")

values_check <- utils::read.csv(values_file, colClasses = "character", check.names = FALSE)
manifest_check <- utils::read.csv(manifest_file, colClasses = "character", check.names = FALSE)

expected_values <- data.frame(
  lapply(calibration_values, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
expected_manifest <- data.frame(
  lapply(calibration_manifest, as.character),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

if (!identical(values_check, expected_values)) {
  stop("The values CSV did not pass the round-trip check.")
}
if (!identical(manifest_check, expected_manifest)) {
  stop("The manifest CSV did not pass the round-trip check.")
}

message(
  "Wrote ", values_file, " (", nrow(calibration_values), " values) and ",
  manifest_file, " (", nrow(calibration_manifest), " objects)."
)

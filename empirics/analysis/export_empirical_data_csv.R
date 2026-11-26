# Create and verify the open-format copy of the empirical input data.

if (!file.exists(file.path("data", "CDbmedium.RData"))) {
  stop("Run this script from the empirics directory.", call. = FALSE)
}

rdata_path <- file.path("data", "CDbmedium.RData")
csv_path <- file.path("data", "CDbmedium.csv")
codebook_path <- file.path("data", "CDbmedium_series.csv")

CDbmedium <- readRDS(rdata_path)
if (!is.list(CDbmedium) || !is.data.frame(CDbmedium$data_all)) {
  stop("CDbmedium.RData does not contain the expected data_all table.", call. = FALSE)
}

empirical_data <- CDbmedium$data_all
expected_rows <- 586L
expected_source_series <- 122L
expected_columns <- expected_source_series + 2L

if (nrow(empirical_data) != expected_rows ||
    ncol(empirical_data) != expected_columns) {
  stop(
    "Unexpected data dimensions: expected 586 rows and 124 columns.",
    call. = FALSE
  )
}

if (!identical(names(empirical_data)[c(1L, ncol(empirical_data))],
               c("dates", "lag_FFR"))) {
  stop("The date and derived-lag columns are not in the expected positions.",
       call. = FALSE)
}

write.csv(empirical_data, csv_path, row.names = FALSE, na = "")

# Verify that the written CSV preserves the complete rectangular dataset.
csv_data <- read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE)
if (!identical(dim(csv_data), dim(empirical_data)) ||
    !identical(names(csv_data), names(empirical_data))) {
  stop("The CSV dimensions or column names differ from data_all.", call. = FALSE)
}
if (!identical(csv_data$dates, empirical_data$dates)) {
  stop("The observation dates changed during CSV export.", call. = FALSE)
}

numeric_columns <- setdiff(names(empirical_data), "dates")
for (column_name in numeric_columns) {
  values_match <- isTRUE(all.equal(
    csv_data[[column_name]],
    empirical_data[[column_name]],
    tolerance = 1e-14,
    check.attributes = FALSE
  ))
  if (!values_match) {
    stop("CSV values differ in column: ", column_name, call. = FALSE)
  }
}

# Verify that the codebook covers every CSV column in the same order.
if (!file.exists(codebook_path)) {
  stop("Missing data/CDbmedium_series.csv.", call. = FALSE)
}
codebook <- read.csv(codebook_path, check.names = FALSE, stringsAsFactors = FALSE)
if (!identical(codebook$column_order, seq_len(ncol(csv_data))) ||
    !identical(codebook$column_name, names(csv_data))) {
  stop("The series codebook does not match the CSV columns.", call. = FALSE)
}
if (sum(codebook$column_type == "FRED-MD series") != expected_source_series) {
  stop("The series codebook must describe exactly 122 FRED-MD series.",
       call. = FALSE)
}

# Rows 2 onward verify the stored one-month lag. The first lag uses the
# December 1959 observation, which precedes the exported estimation sample.
if (!isTRUE(all.equal(
  empirical_data$lag_FFR[-1L],
  head(empirical_data$FEDFUNDS, -1L),
  tolerance = 1e-14,
  check.attributes = FALSE
))) {
  stop("lag_FFR is not the expected one-month lag of FEDFUNDS.", call. = FALSE)
}

cat(
  "Wrote", nrow(csv_data), "monthly observations and",
  expected_source_series, "FRED-MD series to", csv_path, "\n"
)
cat("Verified data/CDbmedium_series.csv against the exported columns.\n")

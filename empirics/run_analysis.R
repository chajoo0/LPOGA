# Reproduce the monetary-policy empirical application.

rm(list = ls(all.names = TRUE))
gc()

arguments <- commandArgs(trailingOnly = TRUE)
RUN_HDLP <- "--run-hdlp" %in% arguments

requested_threads <- suppressWarnings(
  as.integer(Sys.getenv("N_THREADS", unset = NA_character_))
)

if (!is.na(requested_threads) && requested_threads >= 1L) {
  N_THREADS <- requested_threads
} else {
  detected_cores <- parallel::detectCores()
  N_THREADS <- if (is.na(detected_cores)) {
    1L
  } else {
    max(1L, detected_cores - 2L)
  }
}

if (!file.exists("run_analysis.R") ||
    !dir.exists("analysis") ||
    !dir.exists("R")) {
  stop("Run this script from the empirics directory.", call. = FALSE)
}

source(file.path("R", "data_checks.R"))
data_path <- check_data_file(file.path("data", "CDbmedium.RData"))

required_packages <- c(
  "digest", "ggplot2", "ggpattern", "magick", "patchwork", "HDLPrepro"
)
if (RUN_HDLP) required_packages <- c(required_packages, "desla")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Install the required R packages before running the analysis: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

for (directory in c(
  file.path("results", "estimates"),
  file.path("results", "figures"),
  file.path("results", "tables"),
  "logs"
)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

source(file.path("R", "oga_helpers.R"))
source(file.path("R", "lp_oga_mp.R"))
source(file.path("R", "lp_falp_mp.R"))
source(file.path("R", "result_helpers.R"))

CDbmedium <- load_monetary_data(data_path)
set.seed(1)
start_time <- proc.time()[["elapsed"]]

source(file.path("analysis", "01_run_oga.R"))
source(file.path("analysis", "02_run_falp.R"))
source(file.path("analysis", "03_run_favar.R"))
source(file.path("analysis", "04_run_hdlp.R"))
source(file.path("analysis", "05_make_irf_figure.R"))
source(file.path("analysis", "06_make_selection_diagnostics.R"))

elapsed_seconds <- proc.time()[["elapsed"]] - start_time
writeLines(
  c(
    paste("Elapsed seconds:", round(elapsed_seconds, 1)),
    paste("RUN_HDLP:", RUN_HDLP),
    paste("N_THREADS:", N_THREADS)
  ),
  file.path("logs", "runtime.txt")
)
capture.output(sessionInfo(), file = file.path("logs", "session_info.txt"))

cat("Monetary-policy replication complete.\n")

# Load archived HDLP estimates or rerun the long HDLP estimation.

if (!RUN_HDLP) {
  HDLP_RESULTS_DIR <- file.path("results", "archived")
} else {
  other_slow <- slow[, !colnames(slow) %in% c("INDPRO", "CPIAUCSL"), drop = FALSE]

  set.seed(1)
  HDLP_FFR_full <- desla::HDLP(
    r = cbind(other_slow, IP, CPI), x = FFR, y = FFR, q = fast,
    y_predetermined = FALSE, cumulate_y = FALSE,
    hmax = 49L, lags = 13L, threads = N_THREADS, PI_constant = 0.4
  )
  HDLP_FFR <- sanitize_hdlp(HDLP_FFR_full, "FFR", EXPECTED_DATA_SHA256)
  saveRDS(HDLP_FFR, file.path("results", "estimates", "HDLP_FFR.rds"))
  rm(HDLP_FFR_full)
  gc()

  HDLP_IP_full <- desla::HDLP(
    r = cbind(other_slow, CPI), x = FFR, y = IP, q = fast,
    y_predetermined = TRUE, cumulate_y = TRUE,
    hmax = 49L, lags = 13L, threads = N_THREADS, PI_constant = 0.4
  )
  HDLP_IP <- sanitize_hdlp(HDLP_IP_full, "IP", EXPECTED_DATA_SHA256)
  saveRDS(HDLP_IP, file.path("results", "estimates", "HDLP_IP.rds"))
  rm(HDLP_IP_full)
  gc()

  HDLP_CPI_full <- desla::HDLP(
    r = cbind(other_slow, IP), x = FFR, y = CPI, q = fast,
    y_predetermined = TRUE, cumulate_y = TRUE,
    hmax = 49L, lags = 13L, threads = N_THREADS, PI_constant = 0.4
  )
  HDLP_CPI <- sanitize_hdlp(HDLP_CPI_full, "CPI", EXPECTED_DATA_SHA256)
  saveRDS(HDLP_CPI, file.path("results", "estimates", "HDLP_CPI.rds"))
  rm(HDLP_CPI_full)
  gc()

  HDLP_RESULTS_DIR <- file.path("results", "estimates")
}

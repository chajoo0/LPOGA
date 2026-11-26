# Estimate factor-augmented local projections.

factor_panel <- cbind(slow, fast)
common_arguments <- list(
  xt = FFR,
  wt = factor_panel,
  slow_controls = slow,
  numhor = 49L,
  num_factors = 6L,
  lags = 13L,
  hac_kernel = "parzen"
)

FALP_FFR <- do.call(
  lp_falp,
  c(list(yt = FFR, cumulate_y = FALSE), common_arguments)
)
FALP_IP <- do.call(
  lp_falp,
  c(list(yt = IP, cumulate_y = TRUE), common_arguments)
)
FALP_CPI <- do.call(
  lp_falp,
  c(list(yt = CPI, cumulate_y = TRUE), common_arguments)
)

saveRDS(FALP_FFR, file.path("results", "estimates", "FALP_FFR.rds"))
saveRDS(FALP_IP, file.path("results", "estimates", "FALP_IP.rds"))
saveRDS(FALP_CPI, file.path("results", "estimates", "FALP_CPI.rds"))

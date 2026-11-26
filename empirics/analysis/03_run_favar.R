# Estimate and bootstrap the FAVAR through HDLPrepro.

set.seed(1)
FAVAR_irfs <- HDLPrepro::make_irf_factors_VAR(
  CDbmedium,
  n_lags = 13L,
  n_ahead = 49L,
  B = 499L
)

required_components <- c(
  "plot_data_FFR",
  "plot_data_cumulative_IP",
  "plot_data_cumulative_CPI"
)
if (!all(required_components %in% names(FAVAR_irfs))) {
  stop("FAVAR output is missing required plot-data components")
}
if (!all(vapply(FAVAR_irfs[required_components], nrow, integer(1)) == 50L)) {
  stop("FAVAR plot-data components must contain 50 horizons")
}

saveRDS(FAVAR_irfs, file.path("results", "estimates", "FAVAR_irfs.rds"))

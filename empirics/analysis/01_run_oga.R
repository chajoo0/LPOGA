# Estimate corrected double-OGA impulse responses.

slow <- as.matrix(CDbmedium$slow_data)
fast <- as.matrix(CDbmedium$fast_data)
FFR <- as.numeric(CDbmedium$FFR)
IP <- as.numeric(CDbmedium$data_all[, "INDPRO"])
CPI <- as.numeric(CDbmedium$data_all[, "CPIAUCSL"])

OGA_results <- lp_oga_mp(
  outcomes = list(FFR = FFR, IP = IP, CPI = CPI),
  shock = FFR,
  slow = slow,
  fast = fast,
  numhor = 49L,
  lags = 13L,
  cumulate = c(FFR = FALSE, IP = TRUE, CPI = TRUE),
  HDIC_Type = "HDAIC",
  hac_kernel = "parzen",
  progress = TRUE
)

for (response in names(OGA_results)) {
  saveRDS(
    OGA_results[[response]],
    file.path("results", "estimates", paste0("OGA_", response, ".rds"))
  )
}

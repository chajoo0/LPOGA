# Plot the saved monetary-policy impulse responses.

if (!requireNamespace("magick", quietly = TRUE)) {
  stop(
    "Install the R package 'magick' before drawing the patterned ribbons.",
    call. = FALSE
  )
}

library(ggplot2)
library(ggpattern)
library(patchwork)
library(magick)

if (!dir.exists(file.path("results", "estimates"))) {
  stop("Run this script from the empirics directory.", call. = FALSE)
}

if (!exists("HDLP_RESULTS_DIR", inherits = TRUE)) {
  HDLP_RESULTS_DIR <- file.path("results", "archived")
}

required_estimate_files <- file.path(
  "results",
  "estimates",
  c(
    "FAVAR_irfs.rds",
    "FALP_FFR.rds", "FALP_IP.rds", "FALP_CPI.rds",
    "OGA_FFR.rds", "OGA_IP.rds", "OGA_CPI.rds"
  )
)
required_hdlp_files <- file.path(
  HDLP_RESULTS_DIR,
  c("HDLP_FFR.rds", "HDLP_IP.rds", "HDLP_CPI.rds")
)
required_result_files <- c(required_estimate_files, required_hdlp_files)
missing_result_files <- required_result_files[!file.exists(required_result_files)]
if (length(missing_result_files) > 0L) {
  stop(
    "The following saved estimates are missing: ",
    paste(missing_result_files, collapse = ", "),
    call. = FALSE
  )
}

FAVAR_irfs <- readRDS(file.path("results", "estimates", "FAVAR_irfs.rds"))
plot_font <- Sys.getenv("PLOT_FONT", unset = "sans")
dir.create(file.path("results", "figures"), recursive = TRUE, showWarnings = FALSE)

responses <- list(
  FFR = list(
    favar_component = "plot_data_FFR",
    favar_estimate = "FFR",
    title = "Federal Funds Rate",
    y_label = "percentage points",
    impact = 1
  ),
  IP = list(
    favar_component = "plot_data_cumulative_IP",
    favar_estimate = "cumIP",
    title = "Industrial Production",
    y_label = "percent",
    impact = 0
  ),
  CPI = list(
    favar_component = "plot_data_cumulative_CPI",
    favar_estimate = "cumCPI",
    title = "Consumer Price Index",
    y_label = "percent",
    impact = 0
  )
)

model_levels <- c("FAVAR", "FALP", "dLASSO", "double OGA")
model_colors <- c(
  "FAVAR" = "#7BAE35",
  "FALP" = "#8E5A9B",
  "dLASSO" = "#58A5E1",
  "double OGA" = "#2457D6"
)
model_linetypes <- c(
  "FAVAR" = "solid",
  "FALP" = "dotdash",
  "dLASSO" = "longdash",
  "double OGA" = "solid"
)
model_shapes <- c(
  "FAVAR" = 18,
  "FALP" = 17,
  "dLASSO" = 15,
  "double OGA" = 16
)
model_patterns <- c(
  "FAVAR" = "right45",
  "FALP" = "left45",
  "dLASSO" = "hexagon",
  "double OGA" = "bricks"
)

make_lp_frame <- function(result, model, impact) {
  rbind(
    data.frame(
      horizon = 0L,
      estimate = impact,
      lower = impact,
      upper = impact,
      model = model
    ),
    data.frame(
      horizon = seq_along(result$est),
      estimate = result$est,
      lower = result$est - 1.96 * result$se,
      upper = result$est + 1.96 * result$se,
      model = model
    )
  )
}

make_plot_data <- function(response, specification) {
  favar <- FAVAR_irfs[[specification$favar_component]]
  hdlp <- readRDS(file.path(HDLP_RESULTS_DIR, paste0("HDLP_", response, ".rds")))
  falp <- readRDS(file.path("results", "estimates", paste0("FALP_", response, ".rds")))
  oga <- readRDS(file.path("results", "estimates", paste0("OGA_", response, ".rds")))
  
  plot_data <- rbind(
    data.frame(
      horizon = 0:49,
      estimate = favar[[specification$favar_estimate]],
      lower = favar$lower,
      upper = favar$upper,
      model = "FAVAR"
    ),
    make_lp_frame(falp, "FALP", specification$impact),
    data.frame(
      horizon = hdlp$horizon,
      estimate = hdlp$estimate,
      lower = hdlp$lower,
      upper = hdlp$upper,
      model = "dLASSO"
    ),
    make_lp_frame(oga, "double OGA", specification$impact)
  )
  plot_data$model <- factor(plot_data$model, levels = model_levels)
  plot_data
}

plot_irf <- function(plot_data, specification) {
  points <- plot_data[
    plot_data$horizon %% 4L == 0L | plot_data$horizon == 49L,
    ,
    drop = FALSE
  ]
  
  ggplot(plot_data, aes(x = horizon)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey25") +
    geom_ribbon_pattern(
      aes(
        ymin = lower,
        ymax = upper,
        fill = model,
        pattern_fill = model,
        pattern_type = model
      ),
      pattern = "magick",
      alpha = 0.30,
      color = NA,
      pattern_alpha = 0.6,
      pattern_density = 1
    ) +
    geom_line(
      aes(y = estimate, color = model, linetype = model),
      linewidth = 0.7,
      show.legend = TRUE
    ) +
    geom_point(
      data = points,
      aes(y = estimate, color = model, shape = model),
      size = 1.4,
      stroke = 0.5,
      show.legend = TRUE
    ) +
    scale_color_manual(
      values = model_colors,
      breaks = model_levels,
      limits = model_levels
    ) +
    scale_fill_manual(
      values = model_colors,
      breaks = model_levels,
      limits = model_levels
    ) +
    scale_linetype_manual(
      values = model_linetypes,
      breaks = model_levels,
      limits = model_levels
    ) +
    scale_shape_manual(
      values = model_shapes,
      breaks = model_levels,
      limits = model_levels
    ) +
    scale_pattern_fill_manual(
      values = model_colors,
      breaks = model_levels,
      limits = model_levels
    ) +
    scale_pattern_type_manual(
      values = model_patterns,
      breaks = model_levels,
      limits = model_levels
    ) +
    scale_x_continuous(breaks = seq(0, 48, by = 12), limits = c(0, 49)) +
    labs(
      title = specification$title,
      x = "Months",
      y = specification$y_label,
      color = "Method",
      fill = "Method",
      linetype = "Method",
      shape = "Method",
      pattern_fill = "Method",
      pattern_type = "Method"
    ) +
    guides(
      fill = guide_legend(
        order = 1,
        override.aes = list(
          pattern_fill = unname(model_colors[model_levels]),
          pattern_type = unname(model_patterns[model_levels]),
          pattern_scale = 18,
          pattern_alpha = 1,
          shape = unname(model_shapes[model_levels]),
          size = 3
        )
      ),
      color = guide_legend(order = 1),
      pattern_type = "none",
      pattern_fill = "none",
      shape = "none",
      linetype = "none"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.3),
      text = element_text(family = plot_font),
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11)
    )
}

irf_plots <- lapply(names(responses), function(response) {
  specification <- responses[[response]]
  plot_irf(make_plot_data(response, specification), specification)
})

combined_irf <- wrap_plots(irf_plots, nrow = 1, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.key.size = grid::unit(1.3, "cm"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  )

ggsave(
  file.path("results", "figures", "monetary_policy_irfs.pdf"),
  combined_irf,
  width = 15,
  height = 5.2
)

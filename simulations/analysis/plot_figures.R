#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

# Run this file from the package root: Rscript analysis/plot_figures.R

# Paths and included results ---------------------------------------------------

project_root <- normalizePath(".")
results_dir <- file.path(project_root, "results", "simulation")
figure_dir <- file.path(project_root, "results", "figures")

source(file.path(project_root, "analysis", "sim_results_helpers.R"))

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

results <- read_included_results(results_dir)

cat("Loaded ", nrow(results), " rows from ",
    dplyr::n_distinct(results$.source_file), " result files.\n", sep = "")

metric_summary <- summarize_simulation_metrics(results)

display_metric_levels <- c(
  "Median absolute error",
  "RMSE",
  "Coverage (%)",
  "Median CI width"
)

# Plot data --------------------------------------------------------------------

display_metric_labels <- c(
  "Median absolute error" = "Median Abs Error",
  "RMSE" = "RMSE",
  "Coverage (%)" = "Coverage",
  "Median CI width" = "Median CI Width"
)

metric_summary_plot <- metric_summary %>%
  pivot_longer(
    cols = all_of(display_metric_levels),
    names_to = "metric",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    metric_plot = factor(
      unname(display_metric_labels[metric]),
      levels = unname(display_metric_labels[display_metric_levels])
    ),
    group_plot = factor(group_label, levels = method_group_order),
    method_label = factor(method_label, levels = method_display_order)
  )

if (nrow(metric_summary_plot) == 0L) {
  stop("No plottable metric rows remain after filtering.")
}

coverage_reference <- tibble::tibble(
  metric_plot = factor(
    "Coverage",
    levels = unname(display_metric_labels[display_metric_levels])
  ),
  reference = 95
)

plot_keys <- metric_summary_plot %>%
  distinct(dgp_key, dgp_label) %>%
  arrange(match(dgp_key, design_information$dgp_key), dgp_label)

# One figure for each simulation design ---------------------------------------

for (design_number in seq_len(nrow(plot_keys))) {
  design_key <- plot_keys$dgp_key[design_number]
  design_label <- plot_keys$dgp_label[design_number]
  plot_data <- metric_summary_plot %>% filter(dgp_key == design_key)

  present_methods <- method_display_order[
    method_display_order %in% unique(as.character(plot_data$method_label))
  ]

  paper_plot <- ggplot(
    plot_data,
    aes(
      x = horizon,
      y = value,
      color = method_label,
      shape = method_label,
      linetype = method_label,
      group = method_label
    )
  ) +
    geom_hline(
      data = coverage_reference,
      aes(yintercept = reference),
      color = "grey45",
      linewidth = 0.45,
      linetype = "dashed"
    ) +
    geom_line(linewidth = 0.45, na.rm = TRUE) +
    geom_point(size = 1.1, na.rm = TRUE) +
    facet_grid(
      rows = vars(metric_plot),
      cols = vars(group_plot),
      scales = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(
      breaks = seq(3, max(plot_data$horizon, na.rm = TRUE), by = 3)
    ) +
    scale_color_manual(
      values = method_colors,
      breaks = present_methods,
      drop = TRUE,
      name = "Method"
    ) +
    scale_shape_manual(
      values = method_shapes,
      breaks = present_methods,
      drop = TRUE,
      name = "Method"
    ) +
    scale_linetype_manual(
      values = method_linetypes,
      breaks = present_methods,
      drop = TRUE,
      name = "Method"
    ) +
    labs(x = "Horizon", y = NULL, color = "Method") +
    guides(
      color = guide_legend(nrow = 1, byrow = TRUE),
      shape = guide_legend(nrow = 1, byrow = TRUE),
      linetype = guide_legend(nrow = 1, byrow = TRUE)
    ) +
    scale_y_continuous(position = "right") +
    theme_classic(base_size = 9) +
    theme(
      text = element_text(family = "serif"),
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 0.25, color = "black"),
      axis.ticks = element_line(linewidth = 0.2, color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.22, color = "grey88"),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.x = element_text(face = "bold", size = 9),
      strip.text.y.left = element_text(
        angle = 90,
        face = "bold",
        size = 9,
        margin = margin(r = 4)
      ),
      axis.title.x = element_text(size = 9, margin = margin(t = 4)),
      axis.text.x = element_text(size = 8),
      axis.text.y.right = element_text(size = 8),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.key.width = grid::unit(1.15, "lines"),
      legend.key.height = grid::unit(0.75, "lines"),
      plot.margin = margin(4, 6, 4, 6)
    )

  stem <- file.path(figure_dir, paste0("simul_", design_key))
  ggsave(paste0(stem, ".pdf"), paper_plot, width = 6.8, height = 5.2, units = "in")
  ggsave(paste0(stem, ".png"), paper_plot, width = 6.8, height = 5.2, units = "in", dpi = 300)

  cat("Saved plots for ", design_label, ": ", stem, ".pdf/.png\n", sep = "")
}

cat("\nDone. Generated", nrow(plot_keys), "paper-style figure set(s) in", figure_dir, "\n")

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(tidyr)
})

# Run this file from the package root:
# Rscript analysis/plot_cstar_robustness.R

# Paths and included results ---------------------------------------------------

project_root <- normalizePath(".")
results_dir <- file.path(project_root, "results", "simulation")
figure_dir <- file.path(project_root, "results", "figures")

result_pattern <- "^dfm_shock_contamination_poly_cstar_job[0-9]{3}[.]csv$"
output_stem <- file.path(figure_dir, "simul_Cstar_robustness_shock_poly150")

files <- list.files(
  results_dir,
  pattern = result_pattern,
  full.names = TRUE
)
if (length(files) != 50L) {
  stop("Expected 50 C-star result files, but found ", length(files), ".")
}

cat("Reading", length(files), "C-star result files.\n")
results <- bind_rows(lapply(files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(.source_file = basename(file))
}))

required_columns <- c(
  "job_id", "rep_id", "method", "horizon", "cstar",
  "est", "true_irf", "ci_lower", "ci_upper"
)
missing_columns <- setdiff(required_columns, names(results))
if (length(missing_columns)) {
  stop(
    "The result files are missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Including cstar in this key is essential: every replication-horizon appears
# once for each value of C-star in the robustness job.
results <- results %>%
  filter(method == "oga", is.finite(cstar)) %>%
  distinct(job_id, rep_id, method, horizon, cstar, .keep_all = TRUE)

if (!nrow(results)) stop("No OGA rows were found in the matched files.")

cstar_values <- sort(unique(results$cstar))
replication_counts <- results %>%
  group_by(cstar, horizon) %>%
  summarise(
    replications = n(),
    usable_estimates = sum(is.finite(est) & is.finite(true_irf)),
    usable_intervals = sum(is.finite(ci_lower) & is.finite(ci_upper)),
    .groups = "drop"
  )
if (any(replication_counts$replications != 1000L)) {
  warning("At least one C-star/horizon cell does not contain 1,000 replications.")
}
cat(
  "Loaded ", nrow(results), " rows for C* = ",
  paste(cstar_values, collapse = ", "), ".\n", sep = ""
)

# Statistics shown in the figure ----------------------------------------------

metric_levels <- c(
  "Median Abs. Error", "RMSE", "Coverage", "Median CI width"
)

metric_summary <- results %>%
  mutate(
    error = est - true_irf,
    covered = ci_lower <= true_irf & true_irf <= ci_upper,
    interval_width = ci_upper - ci_lower
  ) %>%
  group_by(cstar, horizon) %>%
  summarise(
    `Median Abs. Error` = median(abs(error), na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    Coverage = mean(covered, na.rm = TRUE),
    `Median CI width` = median(interval_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(metric_levels),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = factor(metric, levels = metric_levels))

if (any(!is.finite(metric_summary$value))) {
  warning("Some plotted statistics are non-finite; inspect the counts above.")
}

# These common anchors make a statistic directly comparable across C-star
# panels. Coverage is shown on its natural zero-to-one scale.
axis_anchors <- metric_summary %>%
  filter(is.finite(value)) %>%
  group_by(metric) %>%
  summarise(
    horizon = min(metric_summary$horizon),
    y_min = 0,
    y_max = if (first(metric) == "Coverage") 1 else max(value),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(y_min, y_max),
    names_to = "anchor",
    values_to = "value"
  )

reference_line <- tibble::tibble(
  metric = factor("Coverage", levels = metric_levels),
  reference = 0.95
)

# Plot one panel column for each value of C-star -------------------------------

make_cstar_plot <- function(cstar_value, show_y = TRUE) {
  plot_data <- metric_summary %>% filter(cstar == cstar_value)
  title_expression <- substitute(C^"*" == VALUE, list(VALUE = cstar_value))

  plot <- ggplot(plot_data, aes(x = horizon, y = value, group = 1)) +
    geom_blank(
      data = axis_anchors,
      aes(x = horizon, y = value),
      inherit.aes = FALSE
    ) +
    geom_hline(
      data = reference_line,
      aes(yintercept = reference),
      color = "grey45",
      linewidth = 0.45,
      linetype = "dashed"
    ) +
    geom_line(color = "#3346B5", linewidth = 0.7, na.rm = TRUE) +
    geom_point(
      color = "#3346B5", fill = "#3346B5",
      shape = 17, size = 1.8, na.rm = TRUE
    ) +
    facet_grid(
      rows = vars(metric),
      scales = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(
      breaks = seq(3, max(plot_data$horizon, na.rm = TRUE), by = 3)
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 4),
      expand = expansion(mult = c(0.03, 0.10)),
      position = "right"
    ) +
    labs(title = title_expression, x = "Horizon", y = NULL) +
    theme_classic(base_size = 9, base_family = "serif") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, margin = margin(b = 5)),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 90, size = 9),
      panel.spacing.y = unit(0.7, "lines"),
      axis.title.x = element_text(size = 9),
      axis.text = element_text(size = 7),
      axis.text.y.right = element_text(size = 7),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.line = element_line(linewidth = 0.4),
      axis.ticks = element_line(linewidth = 0.35),
      plot.margin = margin(4, 4, 4, 4)
    )

  if (!show_y) {
    plot <- plot + theme(
      strip.text.y.left = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  } else {
    plot <- plot + theme(plot.margin = margin(4, 4, 4, 14))
  }
  plot
}

plots <- lapply(seq_along(cstar_values), function(i) {
  # With five values, labels appear on the left plot of each figure row.
  show_y <- i == 1L || (length(cstar_values) == 5L && i == 4L)
  make_cstar_plot(cstar_values[i], show_y = show_y)
})

# Assemble and save the five-panel-column figure -------------------------------

if (length(plots) == 5L) {
  page_layout <- c(
    patchwork::area(t = 1, l = 1,  b = 1, r = 6),
    patchwork::area(t = 1, l = 7,  b = 1, r = 12),
    patchwork::area(t = 1, l = 13, b = 1, r = 18),
    patchwork::area(t = 2, l = 4,  b = 2, r = 9),
    patchwork::area(t = 2, l = 10, b = 2, r = 15)
  )
  figure <- patchwork::wrap_plots(
    plots,
    design = page_layout
  )
} else {
  figure <- patchwork::wrap_plots(plots, ncol = min(3L, length(plots)))
}

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(
  paste0(output_stem, ".pdf"), figure,
  width = 10, height = 9.5, units = "in"
)
ggsave(
  paste0(output_stem, ".png"), figure,
  width = 10, height = 9.5, units = "in", dpi = 300
)

cat("Saved:", paste0(output_stem, ".pdf"), "\n")
cat("Saved:", paste0(output_stem, ".png"), "\n")

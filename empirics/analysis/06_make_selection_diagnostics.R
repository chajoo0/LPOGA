# Export OGA selection counts and plot the selection heatmaps.

responses <- c("FFR", "IP", "CPI")
response_titles <- c(
  FFR = "Federal Funds Rate",
  IP = "Industrial Production",
  CPI = "Consumer Price Index"
)

oga_results <- setNames(lapply(responses, function(response) {
  readRDS(file.path("results", "estimates", paste0("OGA_", response, ".rds")))
}), responses)

count_rows <- lapply(responses, function(response) {
  result <- oga_results[[response]]
  data.frame(
    response = response,
    horizon = seq_along(result$n_y),
    n_shock = rep(result$n_shock, length(result$n_y)),
    n_outcome = result$n_y,
    n_union = result$n_all,
    n_always_included_covariates = rep(result$n_baseline - 1L, length(result$n_y)),
    n_always_included_with_intercept = rep(result$n_baseline, length(result$n_y))
  )
})
selection_counts <- do.call(rbind, count_rows)

write.csv(
  selection_counts,
  file.path("results", "tables", "selection_counts_by_horizon.csv"),
  row.names = FALSE
)

shock_selection <- do.call(rbind, lapply(responses, function(response) {
  transform(oga_results[[response]]$shock_selection, response = response)[
    , c("response", "id", "covariate")
  ]
}))
write.csv(
  shock_selection,
  file.path("results", "tables", "shock_selection.csv"),
  row.names = FALSE
)

outcome_selection <- do.call(rbind, lapply(responses, function(response) {
  result <- oga_results[[response]]
  do.call(rbind, lapply(seq_along(result$J_y), function(horizon) {
    data.frame(
      response = response,
      horizon = horizon,
      id = result$J_y_id[[horizon]],
      covariate = result$J_y[[horizon]],
      row.names = NULL
    )
  }))
}))
write.csv(
  outcome_selection,
  file.path("results", "tables", "outcome_selection_by_horizon.csv"),
  row.names = FALSE
)

selection_summary <- do.call(rbind, lapply(responses, function(response) {
  result <- oga_results[[response]]
  data.frame(
    response = response,
    shock_selected = result$n_shock,
    outcome_median = median(result$n_y),
    outcome_min = min(result$n_y),
    outcome_max = max(result$n_y),
    union_median = median(result$n_all),
    union_min = min(result$n_all),
    union_max = max(result$n_all),
    always_included_covariates = result$n_baseline - 1L,
    always_included_with_intercept = result$n_baseline,
    row.names = NULL
  )
}))
write.csv(
  selection_summary,
  file.path("results", "tables", "selection_count_summary.csv"),
  row.names = FALSE
)

format_median_range <- function(median_value, minimum, maximum) {
  paste0(median_value, " (", minimum, "--", maximum, ")")
}

table_rows <- vapply(seq_len(nrow(selection_summary)), function(row) {
  values <- selection_summary[row, ]
  paste0(
    values$response, " & ",
    values$shock_selected, " & ",
    format_median_range(values$outcome_median, values$outcome_min, values$outcome_max), " & ",
    format_median_range(values$union_median, values$union_min, values$union_max), " & ",
    values$always_included_covariates, " \\\\"
  )
}, character(1))

writeLines(
  c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Number of OGA-Selected Covariates Across Horizons in the Monetary Policy Application}",
    "\\label{tab:app_empiric_monetary_numselect}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Response & Shock Eq. & Outcome Eq. & Selected Union & Always Included \\\\ ",
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.94\\textwidth}",
    paste0(
      "\\footnotesize \\textit{Notes.} Shock Eq. reports the number of lagged ",
      "covariates selected once in the common shock equation. Outcome Eq. and ",
      "Selected Union report the median across horizons, with the range in ",
      "parentheses. Selected Union is the union of shock- and outcome-selected ",
      "lagged covariates. The 67 contemporaneous slow variables are always ",
      "included and are not subject to OGA selection; the intercept is ",
      "additionally included but not counted as a covariate."
    ),
    "\\end{minipage}",
    "\\end{table}"
  ),
  file.path("results", "tables", "selection_count_table.tex")
)

plot_selection_heatmap <- function(result, title, frequency_cutoff = 0.10) {
  horizons <- seq_along(result$J_y)
  variables <- sort(unique(c(unlist(result$J_y), result$J_shock)))
  heatmap_data <- expand.grid(
    variable = variables,
    horizon = horizons,
    stringsAsFactors = FALSE
  )
  heatmap_data$selection <- "Not selected"

  for (horizon in horizons) {
    at_horizon <- heatmap_data$horizon == horizon
    selected_outcome <- heatmap_data$variable %in% result$J_y[[horizon]]
    selected_shock <- heatmap_data$variable %in% result$J_shock
    heatmap_data$selection[at_horizon & selected_outcome] <- "Outcome only"
    heatmap_data$selection[at_horizon & selected_shock] <- "Shock only"
    heatmap_data$selection[
      at_horizon & selected_outcome & selected_shock
    ] <- "Both"
  }

  frequency <- aggregate(
    as.integer(heatmap_data$selection != "Not selected"),
    by = list(variable = heatmap_data$variable),
    FUN = mean
  )
  names(frequency)[2] <- "frequency"
  keep <- frequency$variable[frequency$frequency > frequency_cutoff]
  heatmap_data <- heatmap_data[
    heatmap_data$variable %in% keep,
    ,
    drop = FALSE
  ]
  ordering <- frequency[frequency$variable %in% keep, , drop = FALSE]
  ordering <- ordering[order(ordering$frequency, ordering$variable), ]
  heatmap_data$variable <- factor(heatmap_data$variable, levels = ordering$variable)
  heatmap_data$selection <- factor(
    heatmap_data$selection,
    levels = c("Not selected", "Outcome only", "Shock only", "Both")
  )

  ggplot(heatmap_data, aes(x = horizon, y = variable, fill = selection)) +
    geom_tile() +
    scale_fill_manual(
      values = c(
        "Not selected" = "#FFFFFF",
        "Outcome only" = "#B1D3B9",
        "Shock only" = "#88BDA4",
        "Both" = "#659287"
      ),
      breaks = c("Outcome only", "Shock only", "Both"),
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(1, seq(5, length(horizons), by = 5)),
      expand = c(0, 0)
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = title, x = "Horizon", y = NULL, fill = "Selection") +
    theme_classic(base_size = 11, base_family = "sans") +
    theme(
      axis.text.y = element_text(size = 7.5),
      axis.text.x = element_text(size = 9),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid = element_blank()
    )
}

selection_plots <- lapply(responses, function(response) {
  plot_selection_heatmap(oga_results[[response]], response_titles[[response]])
})
names(selection_plots) <- responses

plot_heights <- c(FFR = 6.2, IP = 6.8, CPI = 9.5)
for (response in responses) {
  output_stem <- file.path(
    "results", "figures", paste0("oga_selection_heatmap_", response)
  )
  ggsave(
    paste0(output_stem, ".pdf"),
    selection_plots[[response]],
    width = 10,
    height = plot_heights[[response]]
  )
  ggsave(
    paste0(output_stem, ".png"),
    selection_plots[[response]],
    width = 10,
    height = plot_heights[[response]],
    dpi = 300,
    bg = "white"
  )
}

combined_selection <- wrap_plots(selection_plots, ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")
ggsave(
  file.path("results", "figures", "oga_selection_heatmaps_combined.pdf"),
  combined_selection,
  width = 11,
  height = 22,
  limitsize = FALSE
)
ggsave(
  file.path("results", "figures", "oga_selection_heatmaps_combined.png"),
  combined_selection,
  width = 11,
  height = 22,
  dpi = 240,
  bg = "white",
  limitsize = FALSE
)

################################################################################
# figure4_refined_panelB_quintiles.R
#
# Purpose:
#   Experimental/refined Figure 4 version:
#     A. Adjusted model odds ratios
#     B. Observed MFP frequency across coastal exposure quintiles
#
# Panel A refinements:
#   - odds ratios shown on a linear x-axis
#   - x-axis labeled simply as "Odds ratio"
#
# Panel B refinements:
#   - no connecting line between points
#   - capped vertical binomial uncertainty intervals
#   - point size aligned with Panel A
#   - extra spacing between the two Panel B facets
#
# Expected inputs:
#   ./data/analysis_bundle_50km_augmented.rds
#   ./data/events_model_main_50km_augmented.rds
################################################################################

################################################################################
# 0. SETUP
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(tidyverse)
  library(ggplot2)
  library(mgcv)
  library(cowplot)
  library(scales)
  library(grid)
})

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd("..")
}

options(scipen = 999)

out_dir <- "./results"
fig4_dir <- file.path(out_dir, "figure4_refined")

invisible(dir.create(out_dir, showWarnings = FALSE, recursive = TRUE))
invisible(dir.create(fig4_dir, showWarnings = FALSE, recursive = TRUE))

################################################################################
# 1. LOAD ANALYSIS OBJECTS
################################################################################

analysis_bundle_path <- "./data/analysis_bundle_50km_augmented.rds"
events_model_path <- "./data/events_model_main_50km_augmented.rds"

if (!file.exists(analysis_bundle_path)) {
  stop(
    "Cannot find analysis bundle: ", analysis_bundle_path,
    "\nRun ICAM_big_script_new.R first."
  )
}

if (!file.exists(events_model_path)) {
  stop(
    "Cannot find main model: ", events_model_path,
    "\nRun ICAM_big_script_new.R first."
  )
}

analysis_bundle <- readRDS(analysis_bundle_path)
events_model_main <- readRDS(events_model_path)

list2env(analysis_bundle, envir = .GlobalEnv)

################################################################################
# 2. HELPERS
################################################################################

save_figure <- function(plot, filename, width = 12.5, height = 5.2,
                        dpi = 500, bg = "white") {
  png_path <- file.path(fig4_dir, paste0(filename, ".png"))
  pdf_path <- file.path(fig4_dir, paste0(filename, ".pdf"))
  
  ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = bg
  )
  
  ggsave(
    filename = pdf_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = "pdf",
    bg = bg
  )
  
  invisible(plot)
}

binom_ci <- function(events, n, conf.level = 0.95) {
  if (is.na(events) || is.na(n) || n <= 0) {
    return(c(NA_real_, NA_real_))
  }
  
  ci <- stats::prop.test(
    x = events,
    n = n,
    correct = FALSE,
    conf.level = conf.level
  )$conf.int
  
  as.numeric(ci)
}

coastal_numeric <- function(x) {
  dplyr::case_when(
    is.numeric(x) ~ as.numeric(x),
    is.factor(x) ~ as.numeric(as.character(x)),
    is.character(x) ~ as.numeric(x),
    TRUE ~ NA_real_
  )
}

################################################################################
# 3. PREPARE DATA
################################################################################

event_var <- if ("icam_event_augmented" %in% names(cases_with_all)) {
  "icam_event_augmented"
} else {
  "icam_event"
}

if (!("max_chla_10" %in% names(cases_with_all))) {
  cases_with_all <- cases_with_all %>%
    dplyr::mutate(max_chla_10 = max_chla / 10)
}

if (!("sea_surface_temp" %in% names(cases_with_all))) {
  stop("Expected sea_surface_temp in cases_with_all.")
}

if (!("sea_surface_temp_centered" %in% names(cases_with_all))) {
  cases_with_all <- cases_with_all %>%
    dplyr::mutate(
      sea_surface_temp_centered =
        sea_surface_temp - median(sea_surface_temp, na.rm = TRUE)
    )
}

cases_model <- cases_with_all %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    coastal_num = coastal_numeric(coastal),
    event = as.numeric(.data[[event_var]])
  )

coastal_data <- cases_model %>%
  dplyr::filter(
    coastal_num == 1,
    is.finite(max_chla),
    is.finite(sea_surface_temp),
    !is.na(event)
  )

if (nrow(coastal_data) < 10) {
  stop("Too few finite coastal observations for Figure 4B.")
}

cat("\nCoastal clinic-months used for Panel B: ", nrow(coastal_data), "\n", sep = "")
cat(
  "Coastal MFP event clinic-months used for Panel B: ",
  sum(coastal_data$event, na.rm = TRUE),
  "\n",
  sep = ""
)

################################################################################
# 4. PANEL A: PRIMARY MODEL ODDS RATIOS
################################################################################

coef_table <- summary(events_model_main)$p.table %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  dplyr::rename(
    estimate = Estimate,
    se = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`
  ) %>%
  dplyr::mutate(
    or = exp(estimate),
    or_low = exp(estimate - 1.96 * se),
    or_high = exp(estimate + 1.96 * se),
    label = dplyr::case_when(
      term == "coastal" ~ "Coastal",
      grepl("max_chla_10", term, fixed = TRUE) ~
        "Coastal x chlorophyll-a\n(per 10 mg/m3)",
      grepl("sea_surface_temp_centered", term, fixed = TRUE) ~
        "Coastal x SST",
      term == "temperature_2m" ~ "2 m temperature",
      term %in% c("wealth_index", "wealth_index_5") ~ "Wealth index",
      term == "population_10k" ~ "Population\n(per 10,000)",
      term == "pop_density_1000" ~ "Population density\n(per 1,000/km2)",
      term == "fs_typeCSB2" ~ "CSB2 facility",
      TRUE ~ term
    )
  )

coef_terms_to_plot <- c(
  "Coastal",
  "Coastal x chlorophyll-a\n(per 10 mg/m3)",
  "Coastal x SST",
  "2 m temperature",
  "CSB2 facility",
  "Wealth index",
  "Population\n(per 10,000)",
  "Population density\n(per 1,000/km2)"
)

coef_plot_df <- coef_table %>%
  dplyr::filter(label %in% coef_terms_to_plot) %>%
  dplyr::mutate(
    label = factor(label, levels = rev(coef_terms_to_plot))
  )

p_coef <- ggplot(coef_plot_df, aes(x = or, y = label)) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_errorbarh(
    aes(xmin = or_low, xmax = or_high),
    height = 0.18,
    linewidth = 0.45
  ) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "Adjusted model terms",
    x = "Odds ratio",
    y = NULL
  )

################################################################################
# 5. PANEL B: OBSERVED EVENT FREQUENCY BY COASTAL EXPOSURE QUINTILE
################################################################################

make_quintile_rates <- function(data, exposure_var, exposure_label) {
  data %>%
    dplyr::filter(is.finite(.data[[exposure_var]])) %>%
    dplyr::mutate(
      exposure_quintile = dplyr::ntile(.data[[exposure_var]], 5)
    ) %>%
    dplyr::group_by(exposure_quintile) %>%
    dplyr::summarise(
      clinic_months = dplyr::n(),
      events = sum(event == 1, na.rm = TRUE),
      exposure_min = min(.data[[exposure_var]], na.rm = TRUE),
      exposure_max = max(.data[[exposure_var]], na.rm = TRUE),
      exposure_median = median(.data[[exposure_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci = list(binom_ci(events, clinic_months)),
      rate_per_1000 = 1000 * events / clinic_months,
      lower_per_1000 = 1000 * ci[[1]],
      upper_per_1000 = 1000 * ci[[2]],
      exposure = exposure_label
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ci)
}

quintile_rates <- dplyr::bind_rows(
  make_quintile_rates(coastal_data, "max_chla", "Chlorophyll-a"),
  make_quintile_rates(coastal_data, "sea_surface_temp", "Sea surface temperature")
) %>%
  dplyr::mutate(
    exposure_quintile = factor(
      paste0("Q", exposure_quintile),
      levels = paste0("Q", 1:5)
    ),
    exposure = factor(
      exposure,
      levels = c("Chlorophyll-a", "Sea surface temperature")
    )
  )

readr::write_csv(
  quintile_rates,
  file.path(fig4_dir, "panelB_observed_rates_by_exposure_quintile.csv")
)

p_b_quintiles <- ggplot(
  quintile_rates,
  aes(x = exposure_quintile, y = rate_per_1000)
) +
  geom_errorbar(
    aes(ymin = lower_per_1000, ymax = upper_per_1000),
    width = 0.16,
    linewidth = 0.45
  ) +
  geom_point(size = 2) +
  facet_wrap(~ exposure, nrow = 1) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(0.8, "cm"),
    plot.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "Observed MFP frequency across coastal exposure quintiles",
    x = NULL,
    y = "MFP events per 1,000 coastal clinic-months"
  )

################################################################################
# 6. COMPOSITE FIGURE 4
################################################################################

figure4_refined <- cowplot::plot_grid(
  p_coef,
  p_b_quintiles,
  labels = c("A", "B"),
  label_size = 12,
  label_fontface = "bold",
  ncol = 2,
  rel_widths = c(1.00, 1.15),
  align = "h",
  axis = "tb"
)

save_figure(
  figure4_refined,
  "figure4_refined_panelB_quintiles",
  width = 12.5,
  height = 5.2
)

cat("\n==============================\n")
cat("REFINED FIGURE 4 WRITTEN\n")
cat("==============================\n")
cat("Output folder: ", normalizePath(fig4_dir), "\n", sep = "")
cat("Files:\n")
cat("  figure4_refined_panelB_quintiles.png\n")
cat("  figure4_refined_panelB_quintiles.pdf\n")
cat("  panelB_observed_rates_by_exposure_quintile.csv\n")

################################################################################
# END
################################################################################

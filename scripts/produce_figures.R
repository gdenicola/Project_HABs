################################################################################
# produce_figures_new.R
#
# Run after:
#   1) ICAM_preprocessing_new.R
#   2) ICAM_big_script_new.R
#
# Purpose:
#   - Load the augmented analysis bundle and main GAM.
#   - Produce standalone maps/plots for manuscript panels.
#   - Produce composite Figures 2, 3, and 4 for the manuscript.
#   - Produce selected auxiliary/supplementary outputs.
#
# Expected inputs created by ICAM_big_script_new.R:
#   ./data/analysis_bundle_50km_augmented.rds
#   ./data/events_model_main_50km_augmented.rds
#
# Main outputs:
#   ./results/standalone/*.png and *.pdf
#   ./results/figures/figure2_national_mfp_surveillance_record.png/pdf
#   ./results/figures/figure3_environmental_contextual_covariates.png/pdf
#   ./results/figures/figure4_model_results.png/pdf
################################################################################

################################################################################
# 0. SETUP
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(tidyverse)
  library(ggplot2)
  library(mgcv)
  library(viridis)
  library(cowplot)
  library(scales)
  library(grid)
})

# Set working directory to project root, assuming this script lives in ./scripts.
# This matches the convention used in ICAM_preprocessing_new.R and
# ICAM_big_script_new.R.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd("..")
}

options(scipen = 999)

# Output folders
out_dir <- "./results"
standalone_dir <- file.path(out_dir, "standalone")
figures_dir <- file.path(out_dir, "figures")
tables_dir <- file.path(out_dir, "tables")
supplementary_dir <- file.path(out_dir, "supplementary")

invisible(dir.create(out_dir, showWarnings = FALSE, recursive = TRUE))
invisible(dir.create(standalone_dir, showWarnings = FALSE, recursive = TRUE))
invisible(dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE))
invisible(dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE))
invisible(dir.create(supplementary_dir, showWarnings = FALSE, recursive = TRUE))

# Keep the standalone folder lean: remove old standalone PNG/PDF outputs from
# previous runs before writing the current set. Composite figures are left alone
# except when overwritten by save_figure().
old_standalone_files <- list.files(
  standalone_dir,
  pattern = "\\.(png|pdf)$",
  full.names = TRUE
)
if (length(old_standalone_files) > 0) {
  unlink(old_standalone_files)
}

################################################################################
# 1. LOAD ANALYSIS OBJECTS
################################################################################

analysis_bundle_path <- "./data/analysis_bundle_50km_augmented.rds"
events_model_path <- "./data/events_model_main_50km_augmented.rds"

if (!file.exists(analysis_bundle_path)) {
  stop("Cannot find analysis bundle: ", analysis_bundle_path,
       "\nRun ICAM_big_script_new.R first.")
}

if (!file.exists(events_model_path)) {
  stop("Cannot find main model: ", events_model_path,
       "\nRun ICAM_big_script_new.R first.")
}

analysis_bundle <- readRDS(analysis_bundle_path)
events_model_main <- readRDS(events_model_path)

list2env(analysis_bundle, envir = .GlobalEnv)

cat("Loaded analysis bundle objects:\n")
print(names(analysis_bundle))

# Standardize coastline CRS to the main healthshed CRS, if needed.
coastline <- st_transform(coastline, st_crs(combined_data_allyears))

################################################################################
# 2. HELPERS
################################################################################

# Save a standalone panel as PNG only. Composite manuscript figures are saved
# as both PNG and PDF by save_figure().
save_panel <- function(plot, filename, width = 5.5, height = 6.5,
                       dpi = 500, bg = "white") {
  png_path <- file.path(standalone_dir, paste0(filename, ".png"))
  
  ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = bg
  )
  
  invisible(plot)
}

save_figure <- function(plot, filename, width = 10, height = 6.5,
                        dpi = 500, bg = "white") {
  png_path <- file.path(figures_dir, paste0(filename, ".png"))
  pdf_path <- file.path(figures_dir, paste0(filename, ".pdf"))
  
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

save_supplementary_figure <- function(plot, filename, width = 10, height = 5,
                                      dpi = 500, bg = "white") {
  png_path <- file.path(supplementary_dir, paste0(filename, ".png"))
  pdf_path <- file.path(supplementary_dir, paste0(filename, ".pdf"))
  
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

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Main manuscript map style. This deliberately keeps the visual grammar of the
# older full_island_2024_real.R maps: soft healthshed boundaries, minimal theme,
# white background, and the same color palettes.
map_theme <- theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.position = "right"
  )

# Composite panel theme. Titles are kept, axes removed.
composite_map_theme <- map_theme +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.28, "cm"),
    legend.key.width = unit(1.0, "cm"),
    plot.margin = margin(4, 4, 4, 4)
  )

# Tight Madagascar bounding box for consistent maps.
combined_ll <- st_transform(combined_data_allyears, 4326)
bbox_mada_ll <- st_bbox(combined_ll)

x_buffer <- 0.25
y_buffer <- 0.25

bbox_tight_ll <- c(
  xmin = bbox_mada_ll["xmin"] - x_buffer,
  xmax = bbox_mada_ll["xmax"] + x_buffer,
  ymin = bbox_mada_ll["ymin"] - y_buffer,
  ymax = bbox_mada_ll["ymax"] + y_buffer
)

coord_mada <- coord_sf(
  xlim = c(bbox_tight_ll["xmin"], bbox_tight_ll["xmax"]),
  ylim = c(bbox_tight_ll["ymin"], bbox_tight_ll["ymax"]),
  expand = FALSE,
  datum = NA
)

# Fill a numeric variable by nearest non-missing healthshed. This is mostly for
# presentation maps when a few healthsheds have missing covariate values.
fill_nearest <- function(sf_obj, var_name, new_name) {
  sf_obj[[new_name]] <- sf_obj[[var_name]]
  missing_idx <- which(is.na(sf_obj[[new_name]]))
  nonmissing_idx <- which(!is.na(sf_obj[[new_name]]))
  
  if (length(missing_idx) > 0 && length(nonmissing_idx) > 0) {
    nearest_nonmissing <- st_nearest_feature(
      sf_obj[missing_idx, ],
      sf_obj[nonmissing_idx, ]
    )
    sf_obj[[new_name]][missing_idx] <-
      sf_obj[[new_name]][nonmissing_idx][nearest_nonmissing]
  }
  
  sf_obj
}


# Extract internal holes from the healthshed geometry union. These holes are not
# rows with missing data; they are actual voids in the source geometry. For maps
# only, we convert them to pseudo-polygons and assign values from nearby
# healthsheds so that maps do not show distracting gaps.
extract_internal_holes <- function(poly_sf, min_area_km2 = 0.001) {
  poly_sf <- poly_sf %>%
    sf::st_make_valid()
  
  crs_in <- sf::st_crs(poly_sf)
  
  union_geom <- poly_sf %>%
    sf::st_geometry() %>%
    sf::st_union() %>%
    sf::st_make_valid()
  
  union_polys <- suppressWarnings(sf::st_cast(union_geom, "POLYGON"))
  
  hole_list <- list()
  counter <- 1
  
  for (i in seq_along(union_polys)) {
    coords <- sf::st_coordinates(union_polys[i])
    
    if (!("L1" %in% colnames(coords))) next
    
    ring_ids <- sort(unique(coords[, "L1"]))
    internal_rings <- ring_ids[ring_ids > 1]
    
    if (length(internal_rings) == 0) next
    
    for (ring_id in internal_rings) {
      ring_coords <- coords[coords[, "L1"] == ring_id, c("X", "Y"), drop = FALSE]
      ring_coords <- as.matrix(ring_coords)
      
      if (nrow(ring_coords) < 4) next
      
      if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
        ring_coords <- rbind(ring_coords, ring_coords[1, ])
      }
      
      hole_list[[counter]] <- sf::st_polygon(list(ring_coords))
      counter <- counter + 1
    }
  }
  
  if (length(hole_list) == 0) {
    return(
      sf::st_sf(
        hole_id = integer(),
        hole_area_km2 = numeric(),
        geometry = sf::st_sfc(crs = crs_in)
      )
    )
  }
  
  holes_sf <- sf::st_sf(
    hole_id = seq_along(hole_list),
    geometry = sf::st_sfc(hole_list, crs = crs_in)
  ) %>%
    sf::st_make_valid() %>%
    mutate(
      hole_area_km2 = as.numeric(sf::st_area(.)) / 1e6
    ) %>%
    filter(hole_area_km2 >= min_area_km2)
  
  holes_sf
}

append_holes_with_nearest_value <- function(map_sf, holes_sf, value_col,
                                            hole_value = NULL,
                                            nearest_requires_value = FALSE) {
  if (nrow(holes_sf) == 0) {
    return(map_sf)
  }
  
  if (!(value_col %in% names(map_sf))) {
    stop("Column not found in map_sf: ", value_col)
  }
  
  map_sf <- map_sf %>%
    sf::st_make_valid()
  
  holes_sf <- holes_sf %>%
    sf::st_transform(sf::st_crs(map_sf))
  
  real_values <- map_sf[[value_col]]
  
  if (is.null(hole_value)) {
    if (nearest_requires_value) {
      if (is.numeric(real_values)) {
        candidate_idx <- which(!is.na(real_values) & is.finite(real_values))
      } else {
        candidate_idx <- which(!is.na(real_values))
      }
    } else {
      candidate_idx <- seq_len(nrow(map_sf))
    }
    
    if (length(candidate_idx) == 0) {
      stop("No candidate polygons found for hole filling using column: ", value_col)
    }
    
    nearest_idx <- sf::st_nearest_feature(
      sf::st_point_on_surface(holes_sf),
      sf::st_point_on_surface(map_sf[candidate_idx, ])
    )
    
    hole_values <- real_values[candidate_idx][nearest_idx]
  } else {
    hole_values <- rep(hole_value, nrow(holes_sf))
  }
  
  hole_rows <- map_sf[rep(1, nrow(holes_sf)), ]
  sf_col <- attr(hole_rows, "sf_column")
  
  for (nm in names(hole_rows)) {
    if (nm != sf_col) {
      hole_rows[[nm]] <- NA
    }
  }
  
  hole_rows[[value_col]] <- hole_values
  sf::st_geometry(hole_rows) <- sf::st_geometry(holes_sf)
  
  dplyr::bind_rows(map_sf, hole_rows)
}

################################################################################
# 3. HEALTHSHED-LEVEL SUMMARIES FOR MAPS
################################################################################

# Geometry for all healthsheds in the analytic sample.
clinic_geom <- combined_data_allyears %>%
  dplyr::select(clinic_ID, geometry) %>%
  distinct(clinic_ID, .keep_all = TRUE)

# Time-averaged covariates/exposures at clinic-healthshed level.
clinic_avg <- cases_with_all %>%
  st_drop_geometry() %>%
  group_by(clinic_ID) %>%
  summarise(
    coastal = first(coastal),
    avg_chlorophyll = mean(max_chla, na.rm = TRUE),
    avg_sst = mean(sea_surface_temp, na.rm = TRUE),
    avg_temperature_2m = mean(temperature_2m, na.rm = TRUE),
    avg_precipitation = mean(precipitation, na.rm = TRUE),
    avg_population = mean(population, na.rm = TRUE),
    avg_pop_density = mean(pop_density, na.rm = TRUE),
    wealth_index = first(wealth_index[!is.na(wealth_index)]),
    .groups = "drop"
  ) %>%
  mutate(
    avg_chlorophyll = ifelse(coastal == 0, NA_real_, avg_chlorophyll),
    avg_sst = ifelse(coastal == 0, NA_real_, avg_sst),
    log_avg_pop_density = log1p(avg_pop_density),
    log_wealth_index = log1p(wealth_index)
  )

# If all values for a variable were missing in a group, first(...) above can create
# NULL issues in older dplyr versions. This guard keeps the script robust.
if (!("wealth_index" %in% names(clinic_avg))) {
  clinic_avg <- cases_with_all %>%
    st_drop_geometry() %>%
    group_by(clinic_ID) %>%
    summarise(
      coastal = first(coastal),
      avg_chlorophyll = mean(max_chla, na.rm = TRUE),
      avg_sst = mean(sea_surface_temp, na.rm = TRUE),
      avg_temperature_2m = mean(temperature_2m, na.rm = TRUE),
      avg_precipitation = mean(precipitation, na.rm = TRUE),
      avg_population = mean(population, na.rm = TRUE),
      avg_pop_density = mean(pop_density, na.rm = TRUE),
      wealth_index = mean(wealth_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      wealth_index = ifelse(is.nan(wealth_index), NA_real_, wealth_index),
      avg_chlorophyll = ifelse(coastal == 0, NA_real_, avg_chlorophyll),
      avg_sst = ifelse(coastal == 0, NA_real_, avg_sst),
      log_avg_pop_density = log1p(avg_pop_density),
      log_wealth_index = log1p(wealth_index)
    )
}

clinic_avg <- clinic_avg %>%
  mutate(
    across(
      c(avg_chlorophyll, avg_sst, avg_temperature_2m, avg_precipitation,
        avg_population, avg_pop_density, log_avg_pop_density,
        wealth_index, log_wealth_index),
      ~ ifelse(is.nan(.x), NA_real_, .x)
    )
  )

healthshed_avg <- clinic_geom %>%
  left_join(clinic_avg, by = "clinic_ID") %>%
  st_as_sf()

# Fill small missing holes for the presentation maps where appropriate.
healthshed_avg <- healthshed_avg %>%
  fill_nearest("log_avg_pop_density", "log_avg_pop_density_filled") %>%
  fill_nearest("log_wealth_index", "log_wealth_index_filled") %>%
  fill_nearest("avg_temperature_2m", "avg_temperature_2m_filled") %>%
  fill_nearest("avg_precipitation", "avg_precipitation_filled")

# Detect internal geometry holes once and use them as a cartographic layer for
# all healthshed-level maps. These pseudo-polygons are for plotting only and are
# not used in any analysis.
healthshed_geometry_for_holes <- clinic_geom %>%
  sf::st_make_valid()

healthshed_holes_sf <- extract_internal_holes(
  healthshed_geometry_for_holes,
  min_area_km2 = 0.001
)

cat("\nDetected internal geometry holes for plotting:", nrow(healthshed_holes_sf), "\n")
if (nrow(healthshed_holes_sf) > 0) {
  print(healthshed_holes_sf %>% sf::st_drop_geometry())
}

# Outcome map data are already summarized in combined_data_allyears by the big script.
# For event counts, holes are assigned zero recorded events for display. This avoids
# inventing events while also avoiding visible geometry gaps.
outcome_map <- combined_data_allyears %>%
  append_holes_with_nearest_value(
    holes_sf = healthshed_holes_sf,
    value_col = "icam_event_sum",
    hole_value = 0
  ) %>%
  mutate(
    icam_event_sum_plot = ifelse(icam_event_sum == 0, NA_real_, icam_event_sum),
    icam_total_sum_plot = ifelse(icam_total_sum == 0, NA_real_, icam_total_sum)
  )

# Cartographic hole-filled versions of map datasets. For continuous covariates,
# holes receive the value of the nearest real healthshed. For marine exposures,
# the nearest real healthshed is used even if the value is NA, so inland holes
# remain blank on coastal-only maps rather than receiving artificial marine values.
healthshed_avg_chla_plot <- healthshed_avg %>%
  append_holes_with_nearest_value(healthshed_holes_sf, "avg_chlorophyll")

healthshed_avg_sst_plot <- healthshed_avg %>%
  append_holes_with_nearest_value(healthshed_holes_sf, "avg_sst")

healthshed_avg_temp_plot <- healthshed_avg %>%
  append_holes_with_nearest_value(healthshed_holes_sf, "avg_temperature_2m_filled")

healthshed_avg_precip_plot <- healthshed_avg %>%
  append_holes_with_nearest_value(healthshed_holes_sf, "avg_precipitation_filled")

healthshed_avg_wealth_plot <- healthshed_avg %>%
  append_holes_with_nearest_value(healthshed_holes_sf, "log_wealth_index_filled")

healthshed_avg_pop_plot <- healthshed_avg %>%
  append_holes_with_nearest_value(healthshed_holes_sf, "log_avg_pop_density_filled")

################################################################################
# 4. FIGURE 2 STANDALONE PANELS
#    A: national monthly MFP event clinic-months, stacked by event size
#    B: total MFP events by healthshed
################################################################################

# This panel uses the augmented event variables explicitly. Event counts are
# clinic-months with any recorded MFP diagnosis, not individual patients.
# The stacked categories are mutually exclusive:
#   - small event clinic-months: at least one MFP diagnosis but fewer than 5 cases
#   - large event clinic-months: 5 or more cases
mfp_timeseries_df <- cases_with_all %>%
  sf::st_drop_geometry() %>%
  mutate(
    month_int = as.integer(as.character(month)),
    year_int = as.integer(as.character(year)),
    date = as.Date(sprintf("%04d-%02d-01", year_int, month_int))
  ) %>%
  group_by(date, year_int, month_int) %>%
  summarise(
    small_event_clinic_months = sum(
      icam_event_augmented == 1 & large_icam_event_augmented == 0,
      na.rm = TRUE
    ),
    large_event_clinic_months = sum(
      large_icam_event_augmented == 1,
      na.rm = TRUE
    ),
    mfp_event_clinic_months = sum(icam_event_augmented, na.rm = TRUE),
    mfp_cases_augmented = sum(icam_total_augmented, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

# Long format for stacked bars.
# Use explicit namespaces here because some spatial/statistical packages define
# their own select() generics, which can mask dplyr::select().
mfp_timeseries_long <- mfp_timeseries_df %>%
  dplyr::select(
    date,
    small_event_clinic_months,
    large_event_clinic_months
  ) %>%
  tidyr::pivot_longer(
    cols = c(small_event_clinic_months, large_event_clinic_months),
    names_to = "event_size",
    values_to = "event_clinic_months"
  ) %>%
  dplyr::mutate(
    event_size = dplyr::recode(
      event_size,
      small_event_clinic_months = "Small event (<5 cases)",
      large_event_clinic_months = "Large event (≥5 cases)"
    ),
    event_size = factor(
      event_size,
      levels = c("Small event (<5 cases)", "Large event (≥5 cases)")
    )
  )

p_mfp_timeseries <- ggplot(
  mfp_timeseries_long,
  aes(x = date, y = event_clinic_months, fill = event_size)
) +
  geom_col(width = 25) +
  scale_fill_manual(
    values = c(
      "Small event (<5 cases)" = "#A6CEE3",
      "Large event (≥5 cases)" = "#1F78B4"
    ),
    name = "Event size"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  ) +
  labs(
    title = "Monthly recorded MFP events",
    subtitle = "National clinic surveillance, 2016-2024",
    x = NULL,
    y = "MFP event clinic-months"
  )

p_mfp_events <- ggplot() +
  geom_sf(
    data = outcome_map,
    aes(fill = icam_event_sum_plot),
    color = "grey95",
    linewidth = 0.05
  ) +
  geom_sf(
    data = coastline,
    fill = NA,
    color = "grey85",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    na.value = "white",
    name = "MFP events"
  ) +
  coord_mada +
  map_theme +
  labs(
    title = "Total MFP events by healthshed",
    subtitle = "2016-2024"
  )

save_panel(
  p_mfp_timeseries,
  "fig2a_monthly_mfp_events_by_size",
  width = 7.0,
  height = 3.8
)

save_panel(
  p_mfp_events,
  "fig2b_mfp_events_by_healthshed"
)

################################################################################
# 5. FIGURE 3 STANDALONE MAPS
#    A: coastal chlorophyll-a
#    B: precipitation
#    C: population density
#    D: coastal sea surface temperature
#    E: 2 m air temperature
#    F: wealth index
################################################################################

p_chla_hs <- ggplot() +
  geom_sf(
    data = healthshed_avg_chla_plot,
    aes(fill = avg_chlorophyll),
    color = NA
  ) +
  geom_sf(
    data = coastline,
    fill = NA,
    color = "grey85",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = 1,
    na.value = "white",
    name = "Avg chlorophyll-a\n(mg/m3)"
  ) +
  coord_mada +
  map_theme +
  labs(
    title = "Average chlorophyll-a by healthshed",
    subtitle = "Coastal-linked healthsheds only"
  )

p_precip <- ggplot() +
  geom_sf(
    data = healthshed_avg_precip_plot,
    aes(fill = avg_precipitation_filled),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    na.value = "white",
    limits = c(0.78, 9.99),
    oob = scales::squish,
    name = "Avg precip.\n(mm/day)"
  ) +
  coord_mada +
  map_theme +
  labs(
    title = "Average precipitation by healthshed",
    subtitle = "2016-2024"
  )

p_pop_density <- ggplot() +
  geom_sf(
    data = healthshed_avg_pop_plot,
    aes(fill = log_avg_pop_density_filled),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    na.value = "white",
    name = "Log population\ndensity"
  ) +
  coord_mada +
  map_theme +
  labs(
    title = "Population density by healthshed",
    subtitle = "Log-scale"
  )

p_sst_hs <- ggplot() +
  geom_sf(
    data = healthshed_avg_sst_plot,
    aes(fill = avg_sst),
    color = NA
  ) +
  geom_sf(
    data = coastline,
    fill = NA,
    color = "grey85",
    linewidth = 0.25
  ) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1,
    na.value = "white",
    name = "Avg SST\n(C)"
  ) +
  coord_mada +
  map_theme +
  labs(
    title = "Average sea surface temperature by healthshed",
    subtitle = "Coastal-linked healthsheds only"
  )

p_temp_2m <- ggplot() +
  geom_sf(
    data = healthshed_avg_temp_plot,
    aes(fill = avg_temperature_2m_filled),
    color = NA
  ) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1,
    na.value = "white",
    name = "Avg 2 m temp\n(C)"
  ) +
  coord_mada +
  map_theme +
  labs(
    title = "Average 2 m temperature by healthshed",
    subtitle = "2016-2024"
  )

p_wealth <- ggplot() +
  geom_sf(
    data = healthshed_avg_wealth_plot,
    aes(fill = log_wealth_index_filled),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    na.value = "grey90",
    name = "Log wealth\nindex"
  ) +
  coord_mada +
  map_theme +
  labs(
    title = "Wealth index by healthshed",
    subtitle = "Log-scale"
  )

save_panel(p_chla_hs, "fig3a_chlorophyll_by_healthshed")
save_panel(p_precip, "fig3b_precipitation_by_healthshed")
save_panel(p_pop_density, "fig3c_population_density_by_healthshed")
save_panel(p_sst_hs, "fig3d_sst_by_healthshed")
save_panel(p_temp_2m, "fig3e_temperature_2m_by_healthshed")
save_panel(p_wealth, "fig3f_wealth_index_by_healthshed")

# Backward-compatible standalone filename used by the previous script.
save_panel(p_pop_density, "supp_population_density_by_healthshed")

# Artist-friendly transparent population-density map with no legend/title.
p_pop_density_artist <- ggplot() +
  geom_sf(
    data = healthshed_avg_pop_plot,
    aes(fill = log_avg_pop_density_filled),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    na.value = "transparent"
  ) +
  coord_mada +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  filename = file.path(standalone_dir, "artist_population_density_transparent_no_legend.png"),
  plot = p_pop_density_artist,
  width = 3.5,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "transparent"
)

# Artist-friendly transparent wealth-index map with no legend/title.
# This is intended for the schematic's socioeconomic-data layer.
p_wealth_artist <- ggplot() +
  geom_sf(
    data = healthshed_avg_wealth_plot,
    aes(fill = log_wealth_index_filled),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    na.value = "transparent"
  ) +
  coord_mada +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  filename = file.path(standalone_dir, "artist_wealth_index_transparent_no_legend.png"),
  plot = p_wealth_artist,
  width = 3.5,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "transparent"
)

################################################################################
# 6. COMPOSITE FIGURE 2
################################################################################

p_mfp_timeseries_comp <- p_mfp_timeseries +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_blank(),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.28, "cm"),
    legend.key.width = unit(0.7, "cm"),
    plot.margin = margin(4, 4, 4, 4)
  ) +
  labs(title = "Monthly recorded MFP events")

p_mfp_events_comp <- p_mfp_events +
  composite_map_theme +
  labs(title = "Recorded MFP events by healthshed")

figure2 <- plot_grid(
  p_mfp_timeseries_comp,
  p_mfp_events_comp,
  labels = c("A", "B"),
  label_size = 12,
  label_fontface = "bold",
  ncol = 2,
  rel_widths = c(1.1, 0.9),
  align = "h",
  axis = "tb"
)

save_figure(
  figure2,
  "figure2_national_mfp_surveillance_record",
  width = 12,
  height = 5.2
)
################################################################################
# 7. COMPOSITE FIGURE 3
################################################################################

p_chla_hs_comp <- p_chla_hs +
  composite_map_theme +
  labs(title = "Coastal chlorophyll-a")

p_precip_comp <- p_precip +
  composite_map_theme +
  labs(title = "Precipitation")

p_pop_density_comp <- p_pop_density +
  composite_map_theme +
  labs(title = "Population density")

p_sst_hs_comp <- p_sst_hs +
  composite_map_theme +
  labs(title = "Coastal sea surface temperature")

p_temp_2m_comp <- p_temp_2m +
  composite_map_theme +
  labs(title = "2 m temperature")

p_wealth_comp <- p_wealth +
  composite_map_theme +
  labs(title = "Wealth index")

figure3 <- plot_grid(
  p_chla_hs_comp,
  p_precip_comp,
  p_pop_density_comp,
  p_sst_hs_comp,
  p_temp_2m_comp,
  p_wealth_comp,
  labels = c("A", "B", "C", "D", "E", "F"),
  label_size = 12,
  label_fontface = "bold",
  ncol = 3,
  align = "hv",
  axis = "tblr"
)

save_figure(
  figure3,
  "figure3_environmental_contextual_covariates",
  width = 12,
  height = 8.6
)
################################################################################
# 8. FIGURE 4 MODEL RESULTS
################################################################################

# Figure 4 now has two panels:
#   A: estimated odds ratios from the main model
#   B: observed MFP event frequency across coastal chlorophyll-a and SST quintiles
#
# The previous predicted-probability chlorophyll panel and temporal/precipitation
# smooths are no longer part of the main figure. The smooth terms are saved as a
# two-panel supplementary figure in ./results/supplementary/.

# Important:
# Do NOT use model.frame(events_model_main) here. Because the model formula uses
# terms like I(coastal * max_chla_10), model.frame() may keep the evaluated
# interaction but drop the original variable max_chla. We use the original
# analysis data where raw exposure values are needed.

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

coastal_numeric <- function(x) {
  dplyr::case_when(
    is.numeric(x) ~ as.numeric(x),
    is.factor(x) ~ as.numeric(as.character(x)),
    is.character(x) ~ as.numeric(x),
    TRUE ~ NA_real_
  )
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

model_terms <- terms(events_model_main)
model_vars <- unique(all.vars(formula(events_model_main)))

model_df <- cases_with_all %>%
  sf::st_drop_geometry() %>%
  dplyr::select(dplyr::any_of(c(model_vars, "max_chla", "max_chla_10")))

model_df <- model_df[stats::complete.cases(model_df), , drop = FALSE]

cat("\n--- Model data check for Figure 4 ---\n")
cat("Rows in model_df:", nrow(model_df), "\n")
cat("Variables in model_df:\n")
print(names(model_df))
cat("Coastal distribution:\n")
print(table(model_df$coastal, useNA = "ifany"))
cat("max_chla summary:\n")
print(summary(model_df$max_chla))

# -------------------------------
# 8.1 Panel A: estimated model odds ratios
# -------------------------------

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
    title = "Estimated associations with recorded MFP events",
    x = "Odds ratio",
    y = NULL
  )

save_panel(
  p_coef,
  "fig4a_estimated_associations",
  width = 6.0,
  height = 4.5
)

# -------------------------------
# 8.2 Panel B: observed coastal event frequency by exposure quintile
# -------------------------------

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

cat("\nCoastal clinic-months used for Figure 4B: ", nrow(coastal_data), "\n", sep = "")
cat(
  "Coastal MFP event clinic-months used for Figure 4B: ",
  sum(coastal_data$event, na.rm = TRUE),
  "\n",
  sep = ""
)

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
  file.path(tables_dir, "figure4b_observed_rates_by_exposure_quintile.csv")
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

save_panel(
  p_b_quintiles,
  "fig4b_observed_rates_by_exposure_quintile",
  width = 7.0,
  height = 4.5
)

# -------------------------------
# 8.3 Composite Figure 4
# -------------------------------

figure4 <- plot_grid(
  p_coef,
  p_b_quintiles,
  labels = c("A", "B"),
  label_size = 12,
  label_fontface = "bold",
  ncol = 2,
  rel_widths = c(1.10, 0.90),
  align = "h",
  axis = "tb"
)

save_figure(
  figure4,
  "figure4_model_results",
  width = 12.5,
  height = 5.2
)

################################################################################
# 9. SUPPLEMENTARY SMOOTH TERMS
################################################################################

mode_value_local <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

set_coastal_one <- function(x) {
  if (is.factor(x)) {
    return(factor("1", levels = levels(x)))
  }
  if (is.character(x)) {
    return("1")
  }
  return(1)
}

make_reference_row <- function(data) {
  if (nrow(data) == 0) {
    stop("model_df has zero rows after complete-case filtering.")
  }
  
  ref <- data[1, , drop = FALSE]
  
  numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
  factor_vars <- names(data)[vapply(data, is.factor, logical(1))]
  character_vars <- names(data)[vapply(data, is.character, logical(1))]
  
  for (v in numeric_vars) {
    ref[[v]] <- median(data[[v]], na.rm = TRUE)
  }
  
  for (v in factor_vars) {
    mv <- mode_value_local(data[[v]])
    ref[[v]] <- factor(mv, levels = levels(data[[v]]))
  }
  
  for (v in character_vars) {
    ref[[v]] <- as.character(mode_value_local(data[[v]]))
  }
  
  ref
}

newdata_base <- make_reference_row(model_df)

if ("coastal" %in% names(newdata_base)) {
  newdata_base$coastal <- set_coastal_one(newdata_base$coastal)
}

if ("sea_surface_temp_centered" %in% names(newdata_base)) {
  newdata_base$sea_surface_temp_centered <- 0
}

make_smooth_plot <- function(model, base_row, data_for_limits, var_name, term_name,
                             x_label, title, x_limits = NULL,
                             y_limits = NULL,
                             rug_values = NULL) {
  
  if (!(var_name %in% names(data_for_limits))) {
    stop("Variable ", var_name, " not found in data_for_limits.")
  }
  
  if (is.null(x_limits)) {
    x_limits <- quantile(data_for_limits[[var_name]], c(0.01, 0.99), na.rm = TRUE)
    x_limits <- as.numeric(x_limits)
  }
  
  if (!all(is.finite(x_limits)) || x_limits[1] >= x_limits[2]) {
    x_limits <- range(data_for_limits[[var_name]], na.rm = TRUE)
  }
  
  x_seq <- seq(x_limits[1], x_limits[2], length.out = 200)
  
  nd <- base_row[rep(1, length(x_seq)), , drop = FALSE]
  nd[[var_name]] <- x_seq
  
  terms_pred <- predict(model, newdata = nd, type = "terms", se.fit = TRUE)
  term_cols <- colnames(terms_pred$fit)
  
  matched_term <- term_cols[term_cols == term_name]
  
  if (length(matched_term) == 0) {
    matched_term <- term_cols[grepl(term_name, term_cols, fixed = TRUE)]
  }
  
  if (length(matched_term) == 0) {
    stop(
      "Could not find smooth term: ", term_name,
      "\nAvailable terms are: ", paste(term_cols, collapse = ", ")
    )
  }
  
  matched_term <- matched_term[1]
  
  df <- tibble::tibble(
    x = x_seq,
    fit = as.numeric(terms_pred$fit[, matched_term]),
    se = as.numeric(terms_pred$se.fit[, matched_term])
  ) %>%
    dplyr::mutate(
      lower = fit - 1.96 * se,
      upper = fit + 1.96 * se
    )
  
  p <- ggplot(df, aes(x = x, y = fit)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(linewidth = 0.9) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = title,
      x = x_label,
      y = "Estimated smooth effect"
    )
  
  if (!is.null(rug_values)) {
    rug_values <- rug_values[is.finite(rug_values)]
    
    p <- p +
      geom_rug(
        data = tibble::tibble(x = rug_values),
        aes(x = x),
        inherit.aes = FALSE,
        sides = "b",
        alpha = 0.15
      )
  }
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  p
}

p_time_smooth <- make_smooth_plot(
  model = events_model_main,
  base_row = newdata_base,
  data_for_limits = model_df,
  var_name = "time",
  term_name = "s(time)",
  x_label = "Month since start of study period",
  title = "Smooth temporal trend",
  rug_values = model_df$time
)

p_precip_smooth <- make_smooth_plot(
  model = events_model_main,
  base_row = newdata_base,
  data_for_limits = model_df,
  var_name = "precipitation",
  term_name = "s(precipitation)",
  x_label = "Precipitation (mm/day)",
  title = "Smooth precipitation effect",
  x_limits = c(0, 30),
  y_limits = c(-1.5, 1.5),
  rug_values = model_df$precipitation
)

supp_smooth_terms <- plot_grid(
  p_time_smooth,
  p_precip_smooth,
  labels = c("A", "B"),
  label_size = 12,
  label_fontface = "bold",
  ncol = 2,
  align = "h",
  axis = "tb"
)

save_supplementary_figure(
  supp_smooth_terms,
  "supplementary_smooth_time_precipitation",
  width = 10.5,
  height = 4.6
)

################################################################################
# 10. SELECTED SUPPLEMENTARY OUTPUTS
################################################################################

# The lean standalone folder contains one PNG for each manuscript panel,
# transparent socioeconomic maps for the artist, and the supplementary
# news-reported event map below.

# -------------------------------
# 10.1 Supplementary all news-reported event map, if available
# -------------------------------

# This map uses the full event-match table exported by ICAM_preprocessing_new.R.
# It shows all externally reported events, colored only by whether any clinic
# match was found. It does not distinguish strong/plausible/weak clinic matches.

news_match_csv <- "./data/news_clinic_event_match_table_madagascar_only.csv"

if (file.exists(news_match_csv)) {
  news_events <- readr::read_csv(news_match_csv, show_col_types = FALSE) %>%
    mutate(
      match_group = if_else(
        match_status == "No clinic match found",
        "No clinic match found",
        "Clinic match found"
      ),
      people_impacted = suppressWarnings(as.numeric(no_impacted)),
      people_impacted_size = if_else(
        is.na(people_impacted),
        20,
        pmin(people_impacted, 60)
      )
    ) %>%
    filter(
      is.finite(lon),
      is.finite(lat)
    )
  
  news_points <- news_events %>%
    st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    )
  
  # Data-frame version for event ID labels.
  news_label_df <- news_points %>%
    st_transform(4326) %>%
    mutate(
      label_x = st_coordinates(.)[, 1],
      label_y = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry()
  
  matched_cols <- c(
    "Clinic match found" = "#E76F00",
    "No clinic match found" = "#009E73"
  )
  
  p_news_events <- ggplot() +
    geom_sf(
      data = st_transform(combined_data_allyears, 4326),
      fill = "grey98",
      color = "grey85",
      linewidth = 0.05
    ) +
    geom_sf(
      data = st_transform(coastline, 4326),
      fill = NA,
      color = "grey70",
      linewidth = 0.25
    ) +
    geom_sf(
      data = news_points,
      aes(fill = match_group, size = people_impacted_size),
      shape = 21,
      color = "black",
      stroke = 0.35,
      alpha = 0.95
    ) +
    scale_fill_manual(
      name = "Clinic data status",
      values = matched_cols,
      breaks = c("Clinic match found", "No clinic match found")
    ) +
    scale_size_area(
      name = "People impacted",
      breaks = c(20, 40, 60),
      limits = c(0, 60),
      max_size = 8,
      oob = scales::squish
    ) +
    coord_mada +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(
      title = "Externally reported MFP events",
      subtitle = "News and scientific reports, Madagascar, 2016-2024"
    )
  
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p_news_events <- p_news_events +
      ggrepel::geom_label_repel(
        data = news_label_df,
        aes(x = label_x, y = label_y, label = event_id),
        inherit.aes = FALSE,
        size = 3.0,
        min.segment.length = 0,
        label.size = 0.2,
        label.padding = unit(0.12, "lines"),
        fill = "white",
        color = "black",
        seed = 123,
        max.overlaps = Inf
      )
  } else {
    p_news_events <- p_news_events +
      geom_label(
        data = news_label_df,
        aes(x = label_x, y = label_y, label = event_id),
        inherit.aes = FALSE,
        size = 3.0,
        label.size = 0.2,
        label.padding = unit(0.12, "lines"),
        fill = "white",
        color = "black"
      )
  }
  
  save_panel(
    p_news_events,
    "supp_news_reported_events_match_status",
    width = 6.2,
    height = 7.0,
    bg = "transparent"
  )
  
} else {
  message("News-clinic match CSV not found: ", news_match_csv,
          "; skipping supplementary news event map.")
}

################################################################################
# 11. TABLES AND CONSOLE OUTPUTS
################################################################################

core_sample_description <- cases_with_all %>%
  st_drop_geometry() %>%
  summarise(
    clinic_month_observations = n(),
    unique_clinics = n_distinct(clinic_ID),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    distinct_months = n_distinct(month_year),
    total_mfp_cases_capped_for_model = sum(icam_total, na.rm = TRUE),
    total_mfp_events = sum(icam_event, na.rm = TRUE),
    total_large_mfp_events = sum(large_icam_event, na.rm = TRUE),
    share_clinic_months_with_event = mean(icam_event, na.rm = TRUE),
    coastal_clinic_months = sum(coastal == 1, na.rm = TRUE),
    inland_clinic_months = sum(coastal == 0, na.rm = TRUE),
    coastal_events = sum(icam_event[coastal == 1], na.rm = TRUE),
    inland_events = sum(icam_event[coastal == 0], na.rm = TRUE)
  )

coastal_summary <- cases_with_all %>%
  st_drop_geometry() %>%
  group_by(coastal) %>%
  summarise(
    obs = n(),
    units = n_distinct(clinic_ID),
    total_cases_capped_for_model = sum(icam_total, na.rm = TRUE),
    total_events = sum(icam_event, na.rm = TRUE),
    event_rate = mean(icam_event, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(coastal = ifelse(coastal == 1, "Coastal", "Inland"))

top_regions <- cases_with_all %>%
  st_drop_geometry() %>%
  group_by(reg_name) %>%
  summarise(
    total_events = sum(icam_event, na.rm = TRUE),
    total_cases_capped_for_model = sum(icam_total, na.rm = TRUE),
    units = n_distinct(clinic_ID),
    .groups = "drop"
  ) %>%
  arrange(desc(total_events))

smooth_table <- summary(events_model_main)$s.table %>%
  as.data.frame() %>%
  tibble::rownames_to_column("smooth_term")

write_csv(core_sample_description, file.path(tables_dir, "core_sample_description.csv"))
write_csv(coastal_summary, file.path(tables_dir, "coastal_summary.csv"))
write_csv(top_regions, file.path(tables_dir, "top_regions.csv"))
write_csv(coef_table, file.path(tables_dir, "main_model_parametric_terms.csv"))
write_csv(smooth_table, file.path(tables_dir, "main_model_smooth_terms.csv"))
write_csv(mfp_timeseries_df, file.path(tables_dir, "monthly_mfp_event_timeseries.csv"))

cat("\n==============================\n")
cat("CORE SAMPLE DESCRIPTION\n")
cat("==============================\n")
print(core_sample_description, width = 1000)

cat("\n==============================\n")
cat("COASTAL SUMMARY\n")
cat("==============================\n")
print(coastal_summary, width = 1000)

cat("\n==============================\n")
cat("TOP REGIONS\n")
cat("==============================\n")
print(top_regions, n = 10, width = 1000)

cat("\n==============================\n")
cat("MONTHLY MFP EVENT TIME SERIES\n")
cat("==============================\n")
print(mfp_timeseries_df, n = 12, width = 1000)

cat("\n==============================\n")
cat("MAIN MODEL PARAMETRIC TERMS\n")
cat("==============================\n")
print(coef_table, width = 1000)

cat("\n==============================\n")
cat("MAIN MODEL SMOOTH TERMS\n")
cat("==============================\n")
print(smooth_table, width = 1000)

cat("\nAll outputs saved to: ", normalizePath(out_dir), "\n", sep = "")

################################################################################
# END
################################################################################

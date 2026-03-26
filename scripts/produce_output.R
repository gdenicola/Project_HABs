#Models and Plots

library(sf)
library(tidyverse)
library(mgcv)
library(ggplot2)

rm(list=ls())

analysis_bundle <- readRDS("./data/analysis_bundle_50km.rds")
events_model_main <- readRDS("./data/events_model_main_50km.rds")

list2env(analysis_bundle, envir = .GlobalEnv)

cat("Loaded objects:\n")
print(names(analysis_bundle))



chla_seasonality <- cases_with_all %>%
  st_drop_geometry() %>%
  filter(coastal == 1) %>%
  mutate(
    month = factor(month, levels = sprintf("%02d", 1:12)),
    month_name = factor(
      month.abb[as.integer(month)],
      levels = month.abb
    )
  ) %>%
  group_by(month_name) %>%
  summarise(
    avg_max_chla = mean(max_chla, na.rm = TRUE),
    median_max_chla = median(max_chla, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

chla_seasonality


ggplot(chla_seasonality, aes(x = month_name, y = avg_max_chla, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Average chlorophyll seasonality in Madagascar",
    subtitle = "Coastal healthsheds only",
    x = "Month",
    y = "Average max chlorophyll-a (mg/m³)"
  )


# Seasonality of marine food poisoning events - coastal healthsheds only

mfp_event_seasonality <- cases_with_all %>%
  st_drop_geometry() %>%
  filter(coastal == 1) %>%
  mutate(
    month = factor(month, levels = sprintf("%02d", 1:12)),
    month_name = factor(month.abb[as.integer(month)], levels = month.abb)
  ) %>%
  group_by(month, month_name) %>%
  summarise(
    event_rate = mean(icam_event, na.rm = TRUE),
    total_events = sum(icam_event, na.rm = TRUE),
    clinic_months = n(),
    .groups = "drop"
  ) %>%
  arrange(month)

print(mfp_event_seasonality)

ggplot(mfp_event_seasonality, aes(x = month_name, y = event_rate, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Seasonality of marine food poisoning events",
    subtitle = "Coastal healthsheds only",
    x = "Month",
    y = "Proportion of clinic-months with an ICAM event"
  )


ggplot(combined_data_allyears) +
  geom_sf(
    aes(fill = ifelse(icam_event_sum == 0, NA, icam_event_sum)),
    color = "grey95",
    size = 0.05
  ) +
  geom_sf(
    data = coastline,
    fill = NA,
    color = "grey85",
    linewidth = 0.3
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Total marine food poisoning events by healthshed",
    subtitle = "2016-2024",
    fill = "MFP events"
  )




chla_pixel_avg <- chla_coastal_long %>%
  group_by(lon, lat) %>%
  summarise(
    avg_chla = mean(chla, na.rm = TRUE),
    .groups = "drop"
  )


# Pixel-level average chlorophyll map
# Remove only pixels that are BOTH:
#   (1) on land
#   (2) more than 5 km from the coastline

# 1) Average chlorophyll over time for each pixel
chla_pixel_avg <- chla_coastal_long %>%
  st_drop_geometry() %>%
  group_by(lon, lat) %>%
  summarise(
    avg_chla = mean(chla, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(chla_coastal_long))

# 2) Build a land mask from the healthshed polygons
madagascar_land <- combined_data_allyears %>%
  summarise(geometry = st_union(geometry))

# 3) Make sure coastline is in the same CRS
coastline_same_crs <- st_transform(coastline, st_crs(chla_pixel_avg))

# 4) Identify pixels that fall on land
on_land <- lengths(st_intersects(chla_pixel_avg, madagascar_land)) > 0

# 5) Compute distance from each pixel to the nearest coastline segment
nearest_coast_idx <- st_nearest_feature(chla_pixel_avg, coastline_same_crs)

dist_to_coast_m <- as.numeric(
  st_distance(
    chla_pixel_avg,
    coastline_same_crs[nearest_coast_idx, ],
    by_element = TRUE
  )
)

# 6) Remove only inland pixels that are more than 5 km from the coast
chla_pixel_avg_clean <- chla_pixel_avg %>%
  mutate(
    on_land = on_land,
    dist_to_coast_m = dist_to_coast_m
  ) %>%
  filter(!(on_land & dist_to_coast_m > 5000))

cat("Pixels removed:", sum(on_land & dist_to_coast_m > 5000, na.rm = TRUE), "\n")

# 7) Plot
ggplot() +
  geom_sf(
    data = chla_pixel_avg_clean,
    aes(color = log1p(avg_chla)),
    size = 0.5
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = "Log chlorophyll-a\n(mg/m³)",
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Average chlorophyll-a by pixel",
    subtitle = "Inland pixels >5 km from coast removed"
  ) +
  coord_sf()




library(raster)
library(sf)
library(tidyverse)
library(ggplot2)

# 1) Load full chlorophyll raster stack
chla <- stack("./data/chla_bc.nc")

# 2) Convert to dataframe of pixels and average across all monthly layers
chla_df <- as.data.frame(rasterToPoints(chla))
chla_df$avg_chla <- rowMeans(chla_df[, 3:ncol(chla_df)], na.rm = TRUE)

# 3) Convert to sf and match CRS of your map objects
chla_full_avg <- st_as_sf(chla_df, coords = c("x", "y"), crs = 4326) %>%
  st_transform(st_crs(combined_data_allyears))

# 4) Make a wider plotting window around Madagascar
bbox_expanded <- st_bbox(combined_data_allyears)
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - 400000
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + 400000
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - 400000
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + 400000

chla_full_avg <- st_crop(chla_full_avg, bbox_expanded)

# 5) Build land mask from healthshed polygons
madagascar_land <- combined_data_allyears %>%
  summarise(geometry = st_union(geometry))

# 6) Make sure coastline is in same CRS
coastline_same_crs <- st_transform(coastline, st_crs(chla_full_avg))

# 7) Identify pixels on land
on_land <- lengths(st_intersects(chla_full_avg, madagascar_land)) > 0

# 8) Distance from each pixel to nearest coastline segment
nearest_coast_idx <- st_nearest_feature(chla_full_avg, coastline_same_crs)

dist_to_coast_m <- as.numeric(
  st_distance(
    chla_full_avg,
    coastline_same_crs[nearest_coast_idx, ],
    by_element = TRUE
  )
)

# 9) Remove only inland pixels that are more than 5 km from the coast
chla_full_avg_clean <- chla_full_avg %>%
  mutate(
    on_land = on_land,
    dist_to_coast_m = dist_to_coast_m
  ) %>%
  filter(!(on_land & dist_to_coast_m > 5000))

cat("Pixels removed:", sum(on_land & dist_to_coast_m > 5000, na.rm = TRUE), "\n")

# 10) Plot
ggplot() +
  geom_sf(
    data = chla_full_avg_clean,
    aes(color = log1p(avg_chla)),
    size = 0.35
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = "Log chlorophyll-a\n(mg/m³)",
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Average chlorophyll-a by pixel",
    subtitle = "Expanded ocean view around Madagascar"
  ) +
  coord_sf()



# ===============================
# FIGURE 2 SETUP
# Builds:
# 1) chla_full_avg_clean      = full-ocean chlorophyll pixel map
# 2) chla_hs_avg              = chlorophyll mapped to healthsheds
# 3) sst_full_avg_clean       = full-ocean SST pixel map
# 4) sst_hs_avg               = SST mapped to healthsheds
# ===============================



#####PRODUCING FIGURE 2#####
library(raster)
library(sf)
library(tidyverse)
library(ggplot2)

# ---------- Common geometry helpers ----------
madagascar_land <- combined_data_allyears %>%
  summarise(geometry = st_union(geometry))

bbox_expanded <- st_bbox(combined_data_allyears)
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - 400000
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + 400000
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - 400000
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + 400000

# ---------- 1) Chlorophyll full-ocean pixel map ----------
chla <- stack("./data/chla_bc.nc")

chla_df <- as.data.frame(rasterToPoints(chla))
chla_df$avg_chla <- rowMeans(chla_df[, 3:ncol(chla_df)], na.rm = TRUE)

chla_full_avg <- st_as_sf(chla_df, coords = c("x", "y"), crs = 4326) %>%
  st_transform(st_crs(combined_data_allyears)) %>%
  st_crop(bbox_expanded)

coastline_same_crs_chla <- st_transform(coastline, st_crs(chla_full_avg))

on_land_chla <- lengths(st_intersects(chla_full_avg, madagascar_land)) > 0
nearest_coast_idx_chla <- st_nearest_feature(chla_full_avg, coastline_same_crs_chla)

dist_to_coast_m_chla <- as.numeric(
  st_distance(
    chla_full_avg,
    coastline_same_crs_chla[nearest_coast_idx_chla, ],
    by_element = TRUE
  )
)

chla_full_avg_clean <- chla_full_avg %>%
  mutate(
    on_land = on_land_chla,
    dist_to_coast_m = dist_to_coast_m_chla
  ) %>%
  filter(!(on_land & dist_to_coast_m > 5000))

# ---------- 2) Chlorophyll mapped to healthsheds ----------
chla_hs_avg <- cases_with_all %>%
  group_by(clinic_ID, coastal) %>%
  summarise(
    avg_chlorophyll = mean(max_chla, na.rm = TRUE),
    geometry = first(geom),
    .groups = "drop"
  ) %>%
  st_as_sf(crs = st_crs(cases_with_all)) %>%
  mutate(
    avg_chlorophyll = ifelse(coastal == 0, NA, avg_chlorophyll)
  )

# ---------- 3) SST full-ocean pixel map ----------
exposure_stack <- stack("./data/exposure_data_2024.grib")
full_names <- names(exposure_stack)

sst_layers <- grep("sea.surface.temperature", full_names, ignore.case = TRUE)
sst <- subset(exposure_stack, sst_layers)

sst_df <- as.data.frame(rasterToPoints(sst))
sst_df$avg_sst <- rowMeans(sst_df[, 3:ncol(sst_df)], na.rm = TRUE) - 273.15

sst_full_avg <- st_as_sf(sst_df, coords = c("x", "y"), crs = 4326) %>%
  st_transform(st_crs(combined_data_allyears)) %>%
  st_crop(bbox_expanded)

coastline_same_crs_sst <- st_transform(coastline, st_crs(sst_full_avg))

on_land_sst <- lengths(st_intersects(sst_full_avg, madagascar_land)) > 0
nearest_coast_idx_sst <- st_nearest_feature(sst_full_avg, coastline_same_crs_sst)

dist_to_coast_m_sst <- as.numeric(
  st_distance(
    sst_full_avg,
    coastline_same_crs_sst[nearest_coast_idx_sst, ],
    by_element = TRUE
  )
)

sst_full_avg_clean <- sst_full_avg %>%
  mutate(
    on_land = on_land_sst,
    dist_to_coast_m = dist_to_coast_m_sst
  ) %>%
  filter(!(on_land & dist_to_coast_m > 5000))

# ---------- 4) SST mapped to healthsheds ----------
sst_hs_avg <- cases_with_all %>%
  group_by(clinic_ID, coastal) %>%
  summarise(
    avg_sst = mean(sea_surface_temp, na.rm = TRUE),
    geometry = first(geom),
    .groups = "drop"
  ) %>%
  st_as_sf(crs = st_crs(cases_with_all)) %>%
  mutate(
    avg_sst = ifelse(coastal == 0, NA, avg_sst)
  )

cat("Setup complete.\n")
cat("CHLA ocean pixels:", nrow(chla_full_avg_clean), "\n")
cat("CHLA healthsheds:", nrow(chla_hs_avg), "\n")
cat("SST ocean pixels:", nrow(sst_full_avg_clean), "\n")
cat("SST healthsheds:", nrow(sst_hs_avg), "\n")




ggplot() +
  geom_sf(
    data = chla_full_avg_clean,
    aes(color = log1p(avg_chla)),
    size = 0.35
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = "Log chlorophyll-a\n(mg/m³)",
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Average chlorophyll-a by pixel",
    subtitle = "Expanded ocean view around Madagascar"
  ) +
  coord_sf()


ggplot(chla_hs_avg) +
  geom_sf(
    aes(fill = avg_chlorophyll),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = 1,
    na.value = "white",
    name = "Avg chlorophyll-a\n(mg/m³)"
  ) +
  theme_minimal() +
  labs(
    title = "Average chlorophyll-a by healthshed",
    subtitle = "Coastal healthsheds only"
  )


# Convert SST pixel sf object to a regular dataframe with coordinates
sst_full_df <- sst_full_avg_clean %>%
  st_transform(4326) %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# Estimate grid spacing
res_x <- min(diff(sort(unique(sst_full_df$x))))
res_y <- min(diff(sort(unique(sst_full_df$y))))

# Plot as tiles instead of points
ggplot(sst_full_df, aes(x = x, y = y, fill = avg_sst)) +
  geom_tile(width = res_x, height = res_y) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1,
    name = "Avg SST (°C)",
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Average sea surface temperature by pixel",
    subtitle = "Expanded ocean view around Madagascar"
  ) +
  coord_equal()

ggplot(sst_hs_avg) +
  geom_sf(
    aes(fill = avg_sst),
    color = NA
  ) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1,
    na.value = "white",
    name = "Avg SST (°C)"
  ) +
  theme_minimal() +
  labs(
    title = "Average sea surface temperature by healthshed",
    subtitle = "Coastal healthsheds only"
  )



# ===============================
# FIGURE 2 - final composed version
# Nature-style multipanel assembly
# ===============================

library(sf)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(grid)

# -------------------------------
# 1) Robust healthshed-level summaries
#    joined back by clinic_ID
# -------------------------------

clinic_geom <- combined_data_allyears %>%
  select(clinic_ID, geometry) %>%
  distinct(clinic_ID, .keep_all = TRUE)

clinic_exposure_avg <- cases_with_all %>%
  st_drop_geometry() %>%
  group_by(clinic_ID) %>%
  summarise(
    coastal = first(coastal),
    avg_chlorophyll = mean(max_chla, na.rm = TRUE),
    avg_sst = mean(sea_surface_temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    avg_chlorophyll = ifelse(coastal == 0, NA, avg_chlorophyll),
    avg_sst = ifelse(coastal == 0, NA, avg_sst)
  )

chla_hs_avg <- clinic_geom %>%
  inner_join(
    clinic_exposure_avg %>% select(clinic_ID, avg_chlorophyll),
    by = "clinic_ID"
  ) %>%
  st_as_sf()

sst_hs_avg <- clinic_geom %>%
  inner_join(
    clinic_exposure_avg %>% select(clinic_ID, avg_sst),
    by = "clinic_ID"
  ) %>%
  st_as_sf()

# -------------------------------
# 2) Helper: convert sf point layers
#    to regular tile data frames
# -------------------------------

sf_points_to_tile_df <- function(x) {
  x_ll <- st_transform(x, 4326)
  
  coords <- st_coordinates(x_ll)
  
  df <- x_ll %>%
    st_drop_geometry() %>%
    mutate(
      x = coords[, 1],
      y = coords[, 2]
    )
  
  res_x <- min(diff(sort(unique(df$x))))
  res_y <- min(diff(sort(unique(df$y))))
  
  list(
    data = df,
    res_x = res_x,
    res_y = res_y
  )
}

chla_tile <- sf_points_to_tile_df(chla_full_avg_clean)
sst_tile  <- sf_points_to_tile_df(sst_full_avg_clean)

# -------------------------------
# 3) Bounding boxes for plotting
# -------------------------------

combined_ll <- st_transform(combined_data_allyears, 4326)
bbox_mada_ll <- st_bbox(combined_ll)

# small visual buffer so the island is not cramped
x_buffer <- 0.25
y_buffer <- 0.25

bbox_tight_ll <- c(
  xmin = bbox_mada_ll["xmin"] - x_buffer,
  xmax = bbox_mada_ll["xmax"] + x_buffer,
  ymin = bbox_mada_ll["ymin"] - y_buffer,
  ymax = bbox_mada_ll["ymax"] + y_buffer
)

# -------------------------------
# 4) Common panel theme
# -------------------------------

panel_theme <- theme_void(base_size = 9) +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.28, "cm"),
    legend.key.width = unit(1.0, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0)
  )

# -------------------------------
# 5) Panel A - chlorophyll by pixel
#    full/ocean-expanded view
# -------------------------------

p2a <- ggplot(
  chla_tile$data,
  aes(x = x, y = y, fill = log1p(avg_chla))
) +
  geom_tile(
    width = chla_tile$res_x,
    height = chla_tile$res_y
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Log chlorophyll-a\n(mg/m³)",
    na.value = "white"
  ) +
  coord_equal(
    xlim = c(bbox_tight_ll["xmin"], bbox_tight_ll["xmax"]),
    ylim = c(bbox_tight_ll["ymin"], bbox_tight_ll["ymax"]),
    expand = FALSE
  ) +
  panel_theme

# -------------------------------
# 6) Panel B - chlorophyll by healthshed
# -------------------------------

p2b <- ggplot() +
  geom_sf(
    data = chla_hs_avg,
    aes(fill = avg_chlorophyll),
    color = NA
  ) +
  geom_sf(
    data = coastline,
    fill = NA,
    color = "grey85",
    linewidth = 0.3
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Avg chlorophyll-a\n(mg/m³)",
    na.value = "white"
  ) +
  coord_sf(
    xlim = c(bbox_tight_ll["xmin"], bbox_tight_ll["xmax"]),
    ylim = c(bbox_tight_ll["ymin"], bbox_tight_ll["ymax"]),
    expand = FALSE,
    datum = NA
  ) +
  panel_theme

# -------------------------------
# 7) Panel C - SST by pixel
#    full/ocean-expanded view
# -------------------------------

p2c <- ggplot(
  sst_tile$data,
  aes(x = x, y = y, fill = avg_sst)
) +
  geom_tile(
    width = sst_tile$res_x,
    height = sst_tile$res_y
  ) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1,
    name = "Avg SST (°C)",
    na.value = "white"
  ) +
  coord_equal(
    xlim = c(bbox_tight_ll["xmin"], bbox_tight_ll["xmax"]),
    ylim = c(bbox_tight_ll["ymin"], bbox_tight_ll["ymax"]),
    expand = FALSE
  ) +
  panel_theme

# -------------------------------
# 8) Panel D - SST by healthshed
# -------------------------------

p2d <- ggplot() +
  geom_sf(
    data = sst_hs_avg,
    aes(fill = avg_sst),
    color = NA
  ) +
  geom_sf(
    data = coastline,
    fill = NA,
    color = "grey85",
    linewidth = 0.3
  ) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1,
    name = "Avg SST (°C)",
    na.value = "white"
  ) +
  coord_sf(
    xlim = c(bbox_tight_ll["xmin"], bbox_tight_ll["xmax"]),
    ylim = c(bbox_tight_ll["ymin"], bbox_tight_ll["ymax"]),
    expand = FALSE,
    datum = NA
  ) +
  panel_theme

# -------------------------------
# 9) Assemble with cowplot
# -------------------------------

figure2_final <- plot_grid(
  p2a, p2b, p2c, p2d,
  labels = c("A", "B", "C", "D"),
  label_size = 12,
  label_fontface = "bold",
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

figure2_final

# -------------------------------
# 10) Save
# -------------------------------

ggsave(
  filename = "./results/figure2_final.png",
  plot = figure2_final,
  width = 8.5,
  height = 9,
  units = "in",
  dpi = 500,
  bg = "white"
)

ggsave(
  filename = "./results/figure2_final.pdf",
  plot = figure2_final,
  width = 8.5,
  height = 9,
  units = "in",
  device = "pdf",
  bg = "white"
)


library(dplyr)
library(sf)

cat("\n==============================\n")
cat("CORE SAMPLE DESCRIPTION\n")
cat("==============================\n")

# basic size
cat("Clinic-month observations in final analytic sample:",
    nrow(cases_with_all), "\n")

cat("Unique clinic/healthshed units:",
    n_distinct(cases_with_all$clinic_ID), "\n")

# time span
cat("Years in final analytic sample:",
    paste(range(cases_with_all$year, na.rm = TRUE), collapse = " - "), "\n")

cat("Distinct year-months in final analytic sample:",
    n_distinct(cases_with_all$month_year), "\n")

cat("First 5 months:",
    paste(sort(unique(cases_with_all$month_year))[1:5], collapse = ", "), "\n")

cat("Last 5 months:",
    paste(tail(sort(unique(cases_with_all$month_year)), 5), collapse = ", "), "\n")

# outcome totals
cat("\n--- OUTCOMES ---\n")
cat("Total MFP cases:",
    sum(cases_with_all$icam_total, na.rm = TRUE), "\n")

cat("Total MFP events (binary outcome = 1):",
    sum(cases_with_all$icam_event, na.rm = TRUE), "\n")

cat("Total large MFP events:",
    sum(cases_with_all$large_icam_event, na.rm = TRUE), "\n")

cat("Share of clinic-month observations with any MFP event:",
    mean(cases_with_all$icam_event, na.rm = TRUE), "\n")

cat("Share of clinic-month observations with a large MFP event:",
    mean(cases_with_all$large_icam_event, na.rm = TRUE), "\n")

# coastal structure
cat("\n--- COASTAL / INLAND ---\n")
cat("Coastal clinic-month observations:",
    sum(cases_with_all$coastal == 1, na.rm = TRUE), "\n")

cat("Inland clinic-month observations:",
    sum(cases_with_all$coastal == 0, na.rm = TRUE), "\n")

cat("Unique coastal clinic/healthshed units:",
    cases_with_all %>% 
      st_drop_geometry() %>%
      filter(coastal == 1) %>%
      summarise(n = n_distinct(clinic_ID)) %>%
      pull(n), "\n")

cat("Unique inland clinic/healthshed units:",
    cases_with_all %>% 
      st_drop_geometry() %>%
      filter(coastal == 0) %>%
      summarise(n = n_distinct(clinic_ID)) %>%
      pull(n), "\n")

cat("Total MFP events in coastal observations:",
    sum(cases_with_all$icam_event[cases_with_all$coastal == 1], na.rm = TRUE), "\n")

cat("Total MFP events in inland observations:",
    sum(cases_with_all$icam_event[cases_with_all$coastal == 0], na.rm = TRUE), "\n")

# facility type
cat("\n--- FACILITY TYPE ---\n")
print(
  cases_with_all %>%
    st_drop_geometry() %>%
    distinct(clinic_ID, fs_type) %>%
    count(fs_type, name = "n_units")
)



# any-event units
cat("\n--- SPATIAL BURDEN ---\n")
cat("Clinic/healthshed units with at least one MFP event:",
    cases_with_all %>%
      st_drop_geometry() %>%
      group_by(clinic_ID) %>%
      summarise(any_event = as.integer(sum(icam_event, na.rm = TRUE) > 0)) %>%
      summarise(n = sum(any_event)) %>%
      pull(n), "\n")

cat("Clinic/healthshed units with at least one large MFP event:",
    cases_with_all %>%
      st_drop_geometry() %>%
      group_by(clinic_ID) %>%
      summarise(any_large_event = as.integer(sum(large_icam_event, na.rm = TRUE) > 0)) %>%
      summarise(n = sum(any_large_event)) %>%
      pull(n), "\n")





#########
coastal_summary <- cases_with_all %>%
  st_drop_geometry() %>%
  group_by(coastal) %>%
  summarise(
    obs = n(),
    units = n_distinct(clinic_ID),
    total_cases = sum(icam_total, na.rm = TRUE),
    total_events = sum(icam_event, na.rm = TRUE),
    event_rate = mean(icam_event, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(coastal = ifelse(coastal == 1, "Coastal", "Inland"))

print(coastal_summary)


top_regions <- cases_with_all %>%
  st_drop_geometry() %>%
  group_by(reg_name) %>%
  summarise(
    total_events = sum(icam_event, na.rm = TRUE),
    total_cases = sum(icam_total, na.rm = TRUE),
    units = n_distinct(clinic_ID),
    .groups = "drop"
  ) %>%
  arrange(desc(total_events))

print(top_regions, n = 10)


summary(events_model)
confint(events_model)



coef_table <- summary(events_model)$p.table %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term")

print(coef_table)

smooth_table <- summary(events_model)$s.table %>%
  as.data.frame() %>%
  tibble::rownames_to_column("smooth_term")

print(smooth_table)
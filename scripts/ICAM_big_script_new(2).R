### big script new ####
#to run after the "preprocessing_new" script####
# load relevant packages

library(writexl)
library(sf)
library(raster)
library(tibble)
library(ggplot2)
library(terra)
library(tidyverse)
library(utils)
library(progress)
library(purrr)
library(mgcv)
library(lubridate)
library(nngeo)
library(viridis)

# set working directory to this file's location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen = 999)
rm(list = ls())

################################################################################
# 1. LOAD DATA
################################################################################

# Load chlorophyll satellite data
chla <- stack("./data/chla_bc.nc")

# Load population data
population_new <- read_csv("./data/christina_population.csv", show_col_types = FALSE) %>%
  dplyr::transmute(
    fs_uid,
    year,
    population = worldpop100m_biascorr_change
  )

# Load healthsheds shapefile
healthsheds_2022 <- st_read("./data/healthsheds2022.shp", quiet = TRUE)
healthsheds_2022 <- st_transform(healthsheds_2022, crs = 3857)

# -------------------------------------------------------------------------
# IMPORTANT:
# Load augmented ICAM database, not the clinic-only database.
# This file contains original clinic ICAM data plus the 8 unmatched
# news-reported events added into the matched healthshed-months.
# -------------------------------------------------------------------------

icam_cases <- st_read(
  "./data/icam_cases_augmented_with_unmatched_news.gpkg",
  layer = "healthshed_month_augmented",
  quiet = TRUE
)

icam_cases <- st_transform(icam_cases, crs = 3857)

# Check required augmented columns exist
required_augmented_cols <- c(
  "clinic_icam_total",
  "clinic_icam_event",
  "clinic_large_icam_event",
  "news_unmatched_cases",
  "news_unmatched_events",
  "news_unmatched_deaths",
  "news_unmatched_large_events",
  "icam_total_augmented",
  "icam_event_augmented",
  "large_icam_event_augmented"
)

missing_augmented_cols <- setdiff(required_augmented_cols, names(icam_cases))

if (length(missing_augmented_cols) > 0) {
  stop(
    "The augmented ICAM file is missing these columns: ",
    paste(missing_augmented_cols, collapse = ", ")
  )
}

# Preserve clinic-only variables and use augmented variables as main outcomes
icam_cases <- icam_cases %>%
  mutate(
    icam_total_clinic_only = clinic_icam_total,
    icam_event_clinic_only = clinic_icam_event,
    large_icam_event_clinic_only = clinic_large_icam_event,
    
    icam_total = icam_total_augmented,
    icam_event = icam_event_augmented,
    large_icam_event = large_icam_event_augmented
  )

cat("\n--- AUGMENTED ICAM INPUT CHECK ---\n")

augmented_input_check <- icam_cases %>%
  st_drop_geometry() %>%
  summarise(
    clinic_cases_total = sum(clinic_icam_total, na.rm = TRUE),
    news_unmatched_cases_total = sum(news_unmatched_cases, na.rm = TRUE),
    augmented_cases_total = sum(icam_total_augmented, na.rm = TRUE),
    
    clinic_events_total = sum(clinic_icam_event, na.rm = TRUE),
    news_unmatched_events_total = sum(news_unmatched_events, na.rm = TRUE),
    augmented_events_total = sum(icam_event_augmented, na.rm = TRUE),
    
    clinic_large_events_total = sum(clinic_large_icam_event, na.rm = TRUE),
    news_unmatched_large_events_total =
      sum(news_unmatched_large_events, na.rm = TRUE),
    augmented_large_events_total =
      sum(large_icam_event_augmented, na.rm = TRUE)
  )

print(augmented_input_check, width = 1000)

################################################################################
# 2. PREPARE CHLOROPHYLL DATA AND MONTH LABELS
################################################################################

# Convert raster to dataframe format
chla_df <- as.data.frame(rasterToPoints(chla))

# Define coordinate columns
coords <- chla_df[, 1:2]

# Subset chlorophyll columns
chlorophyll_columns <- chla_df[, 3:length(names(chla_df))]

# Create month-year combination
icam_cases <- icam_cases %>%
  mutate(month_year = paste(sprintf("%02d", cPeriode), cAnnee, sep = "-"))

cat("\n--- CHECK 1: raw month_year labels from icam_cases ---\n")
print(head(sort(unique(icam_cases$month_year)), 20))

month_dates_check <- as.Date(
  paste0("01-", unique(icam_cases$month_year)),
  format = "%d-%m-%Y"
)

cat("\n--- CHECK 2: chronologically sorted month_year labels ---\n")
print(head(format(sort(month_dates_check), "%m-%Y"), 20))

unique_months <- unique(icam_cases$month_year)

sorted_months <- format(
  sort(as.Date(paste0("01-", unique_months), format = "%d-%m-%Y")),
  "%m-%Y"
)

if (ncol(chlorophyll_columns) != length(sorted_months)) {
  stop(
    "Number of chlorophyll raster month columns does not match number of ICAM months. ",
    "Chlorophyll columns: ", ncol(chlorophyll_columns),
    "; ICAM months: ", length(sorted_months)
  )
}

colnames(chlorophyll_columns) <- sorted_months
chla_df <- cbind(coords, chlorophyll_columns)

# Convert chlorophyll dataframe to sf and transform
chla_sf <- st_as_sf(chla_df, coords = c("x", "y"), crs = 4326)
chla_sf <- st_transform(chla_sf, crs = 3857)

################################################################################
# 3. CAP CASE COUNTS FOR MODELING
################################################################################

# Cap very large ICAM totals to mitigate outliers
# This now caps the augmented total, including the added news events.
x <- 30

icam_cases <- icam_cases %>%
  mutate(
    icam_total_uncapped_augmented = icam_total,
    icam_total = pmin(icam_total, x),
    icam_event = as.integer(icam_total > 0),
    large_icam_event = as.integer(icam_total > 3)
  )

cat("\n--- MODEL OUTCOME CHECK AFTER CAPPING ---\n")

model_outcome_check <- icam_cases %>%
  st_drop_geometry() %>%
  summarise(
    model_cases_total_capped = sum(icam_total, na.rm = TRUE),
    model_events_total = sum(icam_event, na.rm = TRUE),
    model_large_events_total = sum(large_icam_event, na.rm = TRUE),
    news_rows_added = sum(news_unmatched_events, na.rm = TRUE)
  )

print(model_outcome_check, width = 1000)

table(icam_cases$icam_total)

################################################################################
# 4. SUMMARY MAP DATA
################################################################################

summary_by_clinic <- icam_cases %>%
  group_by(clinic_ID) %>%
  summarise(
    icam_total_sum = sum(icam_total, na.rm = TRUE),
    icam_event_sum = sum(icam_event, na.rm = TRUE),
    icam_large_event_sum = sum(large_icam_event, na.rm = TRUE),
    news_unmatched_cases_sum = sum(news_unmatched_cases, na.rm = TRUE),
    news_unmatched_events_sum = sum(news_unmatched_events, na.rm = TRUE),
    .groups = "drop"
  )

combined_data_allyears <- healthsheds_2022 %>%
  rename(clinic_ID = fs_uid) %>%
  inner_join(
    summary_by_clinic %>%
      st_drop_geometry(),
    by = "clinic_ID"
  )

cat("\n--- POST-FIX JOIN CHECK ---\n")
cat("Rows in combined_data_allyears:", nrow(combined_data_allyears), "\n")
cat("Distinct clinic_ID:", dplyr::n_distinct(combined_data_allyears$clinic_ID), "\n")

dup_check <- combined_data_allyears %>%
  st_drop_geometry() %>%
  count(clinic_ID) %>%
  filter(n > 1)

cat("Number of duplicated clinic_ID after join:", nrow(dup_check), "\n")
print(head(dup_check, 20))

if (!inherits(combined_data_allyears, "sf")) {
  combined_data_allyears <- st_as_sf(combined_data_allyears)
}

################################################################################
# 5. COASTAL BUFFER AND COASTAL HEALTHSHEDS
################################################################################

coastline <- st_read("./data/madagascar_coastline.shp", quiet = TRUE)
coastline <- st_transform(coastline, st_crs(chla_sf))

coastline_combined <- st_combine(coastline)

madagascar_coastline_buffer_line <- st_boundary(
  st_buffer(coastline_combined, dist = 50000)
)

madagascar_coastline_buffer <- st_buffer(coastline_combined, dist = 50000)

madagascar_coastline_buffer_large <- st_buffer(
  coastline_combined,
  dist = 50000
)

chla_coastal <- chla_sf[
  lengths(st_intersects(chla_sf, madagascar_coastline_buffer)) > 0,
]

chla_coastal_large <- chla_sf[
  lengths(st_intersects(chla_sf, madagascar_coastline_buffer_large)) > 0,
]

icam_cases_coastal <- icam_cases %>%
  mutate(
    coastal = as.integer(
      lengths(st_intersects(., madagascar_coastline_buffer)) > 0
    )
  )

# Extract coordinates from coastal chlorophyll pixels
chla_coords <- st_coordinates(chla_coastal)

chla_coastal <- chla_coastal %>%
  mutate(
    lon = chla_coords[, 1],
    lat = chla_coords[, 2]
  )

chla_coastal_long <- chla_coastal %>%
  pivot_longer(
    cols = matches("^\\d{2}-\\d{4}$"),
    names_to = "month_year",
    values_to = "chla"
  )

# Only keep coastal healthsheds before running distance search
icam_cases_near_water <- icam_cases_coastal %>%
  filter(coastal == 1)

rm(icam_cases)

################################################################################
# 6. FAST CHLOROPHYLL MATCHING
################################################################################

search_radius <- 50000

cat("Running the optimized chlorophyll matching method with a",
    search_radius, "m radius...\n")

unique_clinic_ids <- icam_cases_near_water %>%
  st_drop_geometry() %>%
  distinct(clinic_ID)

first_occurrence_indices <- match(
  unique_clinic_ids$clinic_ID,
  icam_cases_near_water$clinic_ID
)

unique_healthsheds_sf <- icam_cases_near_water[first_occurrence_indices, ]

nearby_indices_list <- st_is_within_distance(
  unique_healthsheds_sf,
  chla_coastal,
  dist = search_radius
)

names(nearby_indices_list) <- unique_healthsheds_sf$clinic_ID

chla_coastal_df <- st_drop_geometry(chla_coastal)

stats <- purrr::map2(
  .x = icam_cases_near_water$clinic_ID,
  .y = icam_cases_near_water$month_year,
  .f = function(current_clinic, current_month) {
    
    indices <- nearby_indices_list[[current_clinic]]
    
    if (length(indices) == 0) {
      return(
        tibble(
          mean_chla_new = NA_real_,
          max_chla_new = NA_real_,
          n_chla_obs_new = 0,
          lon_new = NA_real_,
          lat_new = NA_real_
        )
      )
    }
    
    if (!(current_month %in% names(chla_coastal_df))) {
      stop("Month ", current_month, " not found in chlorophyll data.")
    }
    
    chla_values <- chla_coastal_df[[current_month]][indices]
    
    n_obs <- sum(!is.na(chla_values))
    
    if (n_obs == 0) {
      mean_val <- NA_real_
      max_val <- NA_real_
    } else {
      mean_val <- mean(chla_values, na.rm = TRUE)
      max_val <- max(chla_values, na.rm = TRUE)
    }
    
    first_match_lon <- chla_coastal_df$lon[indices[1]]
    first_match_lat <- chla_coastal_df$lat[indices[1]]
    
    tibble(
      mean_chla_new = mean_val,
      max_chla_new = max_val,
      n_chla_obs_new = n_obs,
      lon_new = first_match_lon,
      lat_new = first_match_lat
    )
  }
) %>%
  bind_rows()

icam_chla_summary <- icam_cases_near_water %>%
  st_drop_geometry() %>%
  bind_cols(stats) %>%
  dplyr::select(
    -any_of(c("max_chla", "mean_chla", "n_chla_obs", "lon", "lat"))
  ) %>%
  rename(
    max_chla = max_chla_new,
    mean_chla = mean_chla_new,
    n_chla_obs = n_chla_obs_new,
    lon = lon_new,
    lat = lat_new
  )

cat("Chlorophyll matching finished. Object: icam_chla_summary\n")

icam_chla_summary_df <- icam_chla_summary %>%
  dplyr::select(clinic_ID, month_year, max_chla, mean_chla, n_chla_obs)

icam_final_sf <- icam_cases_coastal %>%
  left_join(icam_chla_summary_df, by = c("clinic_ID", "month_year")) %>%
  mutate(
    max_chla = ifelse(is.na(max_chla), 0, max_chla),
    mean_chla = ifelse(is.na(mean_chla), 0, mean_chla),
    n_chla_obs = ifelse(is.na(n_chla_obs), 0, n_chla_obs)
  )

################################################################################
# 7. ADD TIME, POPULATION, DENSITY
################################################################################

icam_chla_summary_seasonal <- icam_final_sf %>%
  mutate(
    month = factor(substr(month_year, 1, 2), levels = sprintf("%02d", 1:12)),
    year = as.numeric(substr(month_year, 4, 7))
  )

icam_chla_summary_seasonal <- icam_chla_summary_seasonal %>%
  left_join(population_new, by = c("clinic_ID" = "fs_uid", "year" = "year"))

icam_chla_summary_seasonal <- icam_chla_summary_seasonal %>%
  mutate(
    month_num = as.numeric(
      factor(
        month,
        levels = c(
          "01", "02", "03", "04", "05", "06",
          "07", "08", "09", "10", "11", "12"
        )
      )
    ),
    time = (cAnnee - min(cAnnee)) * 12 + month_num - min(month_num) + 1
  )

cases_with_all <- icam_chla_summary_seasonal %>%
  mutate(
    area_km2 = as.numeric(st_area(.)) / 1e6,
    pop_density = population / area_km2
  )

cases_with_all <- cases_with_all %>%
  mutate(
    population_10k = population / 10000,
    pop_density_1000 = pop_density / 1000,
    max_chla_10 = max_chla / 10
  )

################################################################################
# 8. ADD CLIMATE EXPOSURE VARIABLES
################################################################################

exposure_stack <- stack("./data/exposure_data_2024.grib")

full_names <- names(exposure_stack)

temp_2m_layers <- grep("2.metre.temperature", full_names, ignore.case = TRUE)
sst_layers <- grep("sea.surface.temperature", full_names, ignore.case = TRUE)
precip_layers <- grep("total.precipitation", full_names, ignore.case = TRUE)

t2m <- subset(exposure_stack, temp_2m_layers)
sst <- subset(exposure_stack, sst_layers)
tp <- subset(exposure_stack, precip_layers)

tp_df <- as.data.frame(rasterToPoints(tp))
t2m_df <- as.data.frame(rasterToPoints(t2m))
sst_df <- as.data.frame(rasterToPoints(sst))

make_monthly_raster_df <- function(raster_df, sorted_months) {
  coords <- raster_df[, 1:2]
  value_columns <- raster_df[, 3:ncol(raster_df)]
  
  if (ncol(value_columns) != length(sorted_months)) {
    stop(
      "Raster month columns do not match ICAM months. ",
      "Raster columns: ", ncol(value_columns),
      "; ICAM months: ", length(sorted_months)
    )
  }
  
  colnames(value_columns) <- sorted_months
  cbind(coords, value_columns)
}

tp_df <- make_monthly_raster_df(tp_df, sorted_months)
t2m_df <- make_monthly_raster_df(t2m_df, sorted_months)
sst_df <- make_monthly_raster_df(sst_df, sorted_months)

tp_sf <- st_as_sf(tp_df, coords = c("x", "y"), crs = 4326) %>%
  st_transform(3857)

t2m_sf <- st_as_sf(t2m_df, coords = c("x", "y"), crs = 4326) %>%
  st_transform(3857)

sst_sf <- st_as_sf(sst_df, coords = c("x", "y"), crs = 4326) %>%
  st_transform(3857)

nearest_tp <- st_nearest_feature(cases_with_all, tp_sf)
nearest_t2m <- st_nearest_feature(cases_with_all, t2m_sf)
nearest_sst <- st_nearest_feature(cases_with_all, sst_sf)

extract_values <- function(sf_object, nearest_indices, date_col) {
  sapply(seq_along(nearest_indices), function(i) {
    sf_object[[date_col[i]]][nearest_indices[i]]
  })
}

process_chunk <- function(
    chunk,
    chunk_indices,
    tp_sf,
    t2m_sf,
    sst_sf,
    nearest_tp,
    nearest_t2m,
    nearest_sst
) {
  chunk %>%
    mutate(
      precipitation = extract_values(tp_sf, nearest_tp[chunk_indices], month_year),
      temperature_2m = extract_values(t2m_sf, nearest_t2m[chunk_indices], month_year),
      sea_surface_temp = extract_values(sst_sf, nearest_sst[chunk_indices], month_year)
    )
}

chunk_size <- 1000
n_chunks <- ceiling(nrow(cases_with_all) / chunk_size)

cases_with_all_processed <- list()

for (i in 1:n_chunks) {
  start_index <- (i - 1) * chunk_size + 1
  end_index <- min(i * chunk_size, nrow(cases_with_all))
  chunk_indices <- start_index:end_index
  
  chunk <- cases_with_all[chunk_indices, ]
  
  processed_chunk <- process_chunk(
    chunk,
    chunk_indices,
    tp_sf,
    t2m_sf,
    sst_sf,
    nearest_tp,
    nearest_t2m,
    nearest_sst
  )
  
  cases_with_all_processed[[i]] <- processed_chunk
  
  cat("Processed chunk", i, "of", n_chunks, "\n")
}

cases_with_all <- bind_rows(cases_with_all_processed) %>%
  st_as_sf()

cases_with_all$precipitation <- cases_with_all$precipitation * 1000

################################################################################
# 9. ADD WEALTH INDEX AND FINAL MODEL COVARIATES
################################################################################

cases_with_all$clinic_ID <- as.factor(cases_with_all$clinic_ID)
cases_with_all$reg_uid <- as.factor(cases_with_all$reg_uid)
cases_with_all$dist_uid <- as.factor(cases_with_all$dist_uid)

wealth_index_matched <- read_csv("./data/wealth_index_analysis_ready.csv", show_col_types = FALSE)

cases_with_all <- cases_with_all %>%
  left_join(
    dplyr::select(wealth_index_matched, fs_uid, wealth_index),
    by = c("clinic_ID" = "fs_uid")
  )

cases_with_all <- cases_with_all %>%
  mutate(
    sea_surface_temp = sea_surface_temp - 273.15,
    temperature_2m = temperature_2m - 273.15
  )

mean_sst_C <- mean(cases_with_all$sea_surface_temp, na.rm = TRUE)

cases_with_all <- cases_with_all %>%
  mutate(
    sea_surface_temp_centered = sea_surface_temp - mean_sst_C
  )

cases_with_all$wealth_index_5 <- cases_with_all$wealth_index/5

centroids <- st_centroid(st_geometry(cases_with_all))
centroid_coords <- st_coordinates(centroids)

cases_with_all <- cases_with_all %>%
  mutate(
    longitude = centroid_coords[, 1],
    latitude = centroid_coords[, 2]
  )

# Correct falsely coastal healthsheds
cases_with_all$coastal[cases_with_all$max_chla == 0] <- 0

cases_with_all$clinic_ID_factor <- as.factor(cases_with_all$clinic_ID)

################################################################################
# 10. SAVE ANALYSIS-READY OBJECTS
################################################################################

analysis_bundle <- list(
  cases_with_all = cases_with_all,
  combined_data_allyears = combined_data_allyears,
  coastline = coastline,
  madagascar_coastline_buffer_line = madagascar_coastline_buffer_line,
  chla_coastal_long = chla_coastal_long,
  search_radius = search_radius
)

saveRDS(analysis_bundle, "./data/analysis_bundle_50km_augmented.rds")

cat("\nSaved analysis bundle to: ./data/analysis_bundle_50km_augmented.rds\n")
print(names(analysis_bundle))

################################################################################
# 11. MAIN MODEL - AUGMENTED EVENT OUTCOME
################################################################################

events_model <- gam(
  icam_event ~
    coastal +
    I(coastal * max_chla_10) +
    s(time, bs = "ps", k = 20) +
    wealth_index_5 +
    population_10k +
    pop_density_1000 +
    fs_type +
    temperature_2m +
    s(precipitation, bs = "ps", k = 20) +
    I(coastal * sea_surface_temp_centered),
  data = cases_with_all,
  family = "binomial"
)

summary(events_model)

saveRDS(events_model, "./data/events_model_main_50km_augmented.rds")

cat("\nSaved main event model to: ./data/events_model_main_50km_augmented.rds\n")

################################################################################
# 12. MAIN MODEL PLOTS
################################################################################

plot(
  events_model,
  select = 2,
  shade = TRUE,
  shade.col = "lightblue",
  xlab = "Precipitation",
  ylab = "Smooth function",
  main = "Effect of Precipitation on MFP events",
  xlim = c(0, 30),
  ylim = c(-1.5, 1.5)
)

rug(cases_with_all$precipitation)
abline(h = 0, lty = 2, col = "grey50")

plot(
  events_model,
  select = 1,
  shade = TRUE,
  shade.col = "lightblue",
  xlab = "Month",
  ylab = "Smooth function",
  main = "Smooth time trend of MFP events"
)

abline(h = 0, lty = 2, col = "grey50")


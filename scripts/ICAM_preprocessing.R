###NEW ICAM_PREPROCESSING PIPELINE#########


# ------------------------------------------------------------
# Mapping of ICAM events
# Clinic ICAM data + unmatched news-reported marine intoxication events
# ------------------------------------------------------------

library(feather)
library(sf)
library(tidyverse)
library(readxl)
library(lubridate)

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen = 999)
rm(list = ls())


# ------------------------------------------------------------
# 1. Load core data
# ------------------------------------------------------------

healthsheds_2022 <- st_read("./data/healthsheds2022.shx", quiet = TRUE)

file5 <- read_feather("./data/file5.feather")

jp <- read_excel(
  "./data/[FINAL] Madagascar HABs+Marine Intoxication Lit+Field Review Extraction Sheet.xlsx"
)


# ------------------------------------------------------------
# 2. Build clinic ICAM healthshed-month database
# ------------------------------------------------------------

ICAM_cases <- file5 %>%
  dplyr::select(
    clinic_ID,
    cAnnee,
    cPeriode,
    fname,
    V410,
    V411,
    V412,
    V413,
    V414,
    V415,
    V416,
    V417,
    V418,
    V419,
    V420,
    V421,
    V422,
    V423
  ) %>%
  filter(
    cAnnee >= 2016,
    cAnnee <= 2024
  ) %>%
  mutate(
    across(
      V410:V423,
      ~ replace(.x, .x == -1, 0)
    )
  ) %>%
  mutate(
    across(
      V410:V423,
      ~ replace_na(.x, 0)
    )
  ) %>%
  mutate(
    icam_total = rowSums(
      dplyr::select(., V410:V423),
      na.rm = TRUE
    )
  )

combined_data <- ICAM_cases %>%
  inner_join(
    healthsheds_2022,
    by = c("clinic_ID" = "fs_uid")
  )

if (!inherits(combined_data, "sf")) {
  combined_data <- st_as_sf(combined_data)
}

combined_data <- combined_data %>%
  filter(fs_type %in% c("CSB1", "CSB2")) %>%
  mutate(
    icam_event = if_else(icam_total > 0, 1, 0),
    large_icam_event = if_else(icam_total > 3, 1, 0)
  )

export_data <- combined_data %>%
  dplyr::select(
    clinic_ID,
    cAnnee,
    cPeriode,
    icam_total,
    icam_event,
    reg_name,
    dist_name,
    reg_uid,
    dist_uid,
    large_icam_event,
    fs_pop,
    fs_type,
    geometry
  )

# Export original clinic-only ICAM healthshed-month layer
st_write(
  export_data,
  "./data/icam_cases_shapefile.gpkg",
  layer = "icam_cases_shapefile",
  delete_layer = TRUE,
  quiet = TRUE
)


# ------------------------------------------------------------
# 3. Clean news-reported marine intoxication events
#    Keep Madagascar events only, 2016-2024
#    This excludes La Reunion and other non-Madagascar coordinates
# ------------------------------------------------------------

jp_events <- jp %>%
  select(
    `Event Type`,
    `Number of People Impacted (Total)`,
    `# of Deaths`,
    `Lat, Long`,
    `Start Date of Event (MM/DD/YYYY)`,
    `End Date of Event (MM/DD/YYY)`
  ) %>%
  rename(
    event_type = `Event Type`,
    no_impacted = `Number of People Impacted (Total)`,
    no_death = `# of Deaths`,
    lat_long = `Lat, Long`,
    start_date = `Start Date of Event (MM/DD/YYYY)`,
    end_date = `End Date of Event (MM/DD/YYY)`
  ) %>%
  mutate(
    event_type = as.character(event_type),
    no_impacted = suppressWarnings(
      readr::parse_number(as.character(no_impacted))
    ),
    no_death = suppressWarnings(
      readr::parse_number(as.character(no_death))
    ),
    lat_long = as.character(lat_long),
    start_date = as.Date(start_date),
    end_date = as.Date(end_date),
    end_date = if_else(is.na(end_date), start_date, end_date)
  ) %>%
  filter(
    start_date >= as.Date("2016-01-01"),
    start_date <= as.Date("2024-12-31"),
    str_detect(
      event_type,
      regex("marine intoxication|hospital case", ignore_case = TRUE)
    )
  ) %>%
  separate(
    lat_long,
    into = c("lat", "lon"),
    sep = ",\\s*",
    convert = TRUE,
    remove = FALSE
  ) %>%
  filter(
    lon >= 43,
    lon <= 51,
    lat >= -26,
    lat <= -11
  ) %>%
  arrange(start_date) %>%
  mutate(
    event_id = row_number(),
    start_month = floor_date(start_date, "month"),
    end_month = floor_date(end_date, "month")
  )


# ------------------------------------------------------------
# 4. Prepare healthshed and clinic-month data for matching
# ------------------------------------------------------------

healthsheds_public <- healthsheds_2022 %>%
  filter(fs_type %in% c("CSB1", "CSB2")) %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid() %>%
  select(
    fs_uid,
    fs_type,
    reg_name,
    dist_name,
    reg_uid,
    dist_uid,
    fs_pop,
    geometry
  )

clinic_month <- export_data %>%
  st_drop_geometry() %>%
  select(
    clinic_ID,
    cAnnee,
    cPeriode,
    icam_total,
    icam_event,
    large_icam_event,
    reg_uid,
    dist_uid,
    reg_name,
    dist_name,
    fs_type
  )

district_month <- clinic_month %>%
  group_by(dist_uid, cAnnee, cPeriode) %>%
  summarise(
    dist_icam_cases = sum(icam_total, na.rm = TRUE),
    dist_icam_events = sum(icam_event, na.rm = TRUE),
    dist_large_icam_events = sum(large_icam_event, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 5. Assign each news event to nearest CSB1/CSB2 healthshed
# ------------------------------------------------------------

jp_points <- jp_events %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

nearest_idx <- st_nearest_feature(jp_points, healthsheds_public)

jp_hs <- bind_cols(
  jp_points,
  healthsheds_public[nearest_idx, ] %>%
    st_drop_geometry() %>%
    rename(
      matched_clinic_ID = fs_uid,
      matched_fs_type = fs_type,
      matched_reg_name = reg_name,
      matched_dist_name = dist_name,
      matched_reg_uid = reg_uid,
      matched_dist_uid = dist_uid,
      matched_fs_pop = fs_pop
    )
) %>%
  mutate(
    distance_to_healthshed_km =
      as.numeric(st_distance(
        geometry,
        healthsheds_public[nearest_idx, ],
        by_element = TRUE
      )) / 1000
  )


# ------------------------------------------------------------
# 6. Build exact and +/- 1 month matching windows
# ------------------------------------------------------------

make_event_months <- function(events, lag_months, window_name) {
  events %>%
    st_drop_geometry() %>%
    mutate(
      window = window_name,
      window_start = start_month %m-% months(lag_months),
      window_end = end_month %m+% months(lag_months),
      month_date = map2(
        window_start,
        window_end,
        ~ seq.Date(.x, .y, by = "month")
      )
    ) %>%
    select(
      event_id,
      window,
      matched_clinic_ID,
      matched_dist_uid,
      month_date
    ) %>%
    unnest(month_date) %>%
    mutate(
      cAnnee = year(month_date),
      cPeriode = month(month_date)
    )
}

event_months <- bind_rows(
  make_event_months(jp_hs, 0, "exact"),
  make_event_months(jp_hs, 1, "pm1")
)


# ------------------------------------------------------------
# 7. Match news events to clinic data
# ------------------------------------------------------------

hs_match <- event_months %>%
  left_join(
    clinic_month,
    by = c(
      "matched_clinic_ID" = "clinic_ID",
      "cAnnee",
      "cPeriode"
    )
  ) %>%
  group_by(event_id, window) %>%
  summarise(
    hs_icam_cases = sum(replace_na(icam_total, 0)),
    hs_positive_months = sum(replace_na(icam_event, 0)),
    hs_large_positive_months = sum(replace_na(large_icam_event, 0)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = window,
    values_from = c(
      hs_icam_cases,
      hs_positive_months,
      hs_large_positive_months
    ),
    names_glue = "{window}_{.value}",
    values_fill = 0
  )

dist_match <- event_months %>%
  left_join(
    district_month,
    by = c(
      "matched_dist_uid" = "dist_uid",
      "cAnnee",
      "cPeriode"
    )
  ) %>%
  group_by(event_id, window) %>%
  summarise(
    dist_icam_cases = sum(replace_na(dist_icam_cases, 0)),
    dist_positive_months = sum(replace_na(dist_icam_events, 0)),
    dist_large_icam_events = sum(replace_na(dist_large_icam_events, 0)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = window,
    values_from = c(
      dist_icam_cases,
      dist_positive_months,
      dist_large_icam_events
    ),
    names_glue = "{window}_{.value}",
    values_fill = 0
  )

event_match_table <- jp_hs %>%
  st_drop_geometry() %>%
  select(
    event_id,
    event_type,
    no_impacted,
    no_death,
    start_date,
    end_date,
    lat,
    lon,
    matched_clinic_ID,
    matched_fs_type,
    matched_reg_name,
    matched_dist_name,
    matched_reg_uid,
    matched_dist_uid,
    distance_to_healthshed_km
  ) %>%
  left_join(hs_match, by = "event_id") %>%
  left_join(dist_match, by = "event_id") %>%
  mutate(
    across(
      matches("^(exact|pm1)_"),
      ~ replace_na(.x, 0)
    ),
    match_status = case_when(
      exact_hs_icam_cases > 0 ~
        "Strong match: same healthshed, same month",
      
      pm1_hs_icam_cases > 0 ~
        "Plausible match: same healthshed, +/- 1 month",
      
      exact_dist_icam_cases > 0 ~
        "Weak match: same district, same month",
      
      pm1_dist_icam_cases > 0 ~
        "Weak match: same district, +/- 1 month",
      
      TRUE ~
        "No clinic match found"
    )
  ) %>%
  arrange(start_date)


# ------------------------------------------------------------
# 8. Identify unmatched news events to add
# ------------------------------------------------------------

unmatched_news_events <- event_match_table %>%
  filter(match_status == "No clinic match found") %>%
  mutate(
    news_cAnnee = year(start_date),
    news_cPeriode = month(start_date),
    news_unmatched_cases = no_impacted,
    news_unmatched_deaths = no_death,
    news_unmatched_event = 1L,
    news_unmatched_large_event = if_else(
      !is.na(no_impacted) & no_impacted > 3,
      1L,
      0L
    )
  )

news_additions_by_healthshed_month <- unmatched_news_events %>%
  group_by(
    matched_clinic_ID,
    news_cAnnee,
    news_cPeriode
  ) %>%
  summarise(
    news_unmatched_events = sum(news_unmatched_event, na.rm = TRUE),
    news_unmatched_cases = sum(news_unmatched_cases, na.rm = TRUE),
    news_unmatched_deaths = sum(news_unmatched_deaths, na.rm = TRUE),
    news_unmatched_large_events = sum(news_unmatched_large_event, na.rm = TRUE),
    news_event_ids = paste(event_id, collapse = ", "),
    news_event_dates = paste(as.character(start_date), collapse = ", "),
    .groups = "drop"
  ) %>%
  rename(
    clinic_ID = matched_clinic_ID,
    cAnnee = news_cAnnee,
    cPeriode = news_cPeriode
  )


# ------------------------------------------------------------
# 9. Ensure unmatched news healthshed-months exist in database
#    If the clinic file lacks a row for that clinic-month, create one
# ------------------------------------------------------------

existing_healthshed_months <- export_data %>%
  st_drop_geometry() %>%
  select(
    clinic_ID,
    cAnnee,
    cPeriode
  )

news_additions_missing_rows <- news_additions_by_healthshed_month %>%
  anti_join(
    existing_healthshed_months,
    by = c("clinic_ID", "cAnnee", "cPeriode")
  )

news_rows_missing <- healthsheds_public %>%
  rename(clinic_ID = fs_uid) %>%
  inner_join(
    news_additions_missing_rows %>%
      select(clinic_ID, cAnnee, cPeriode),
    by = "clinic_ID"
  ) %>%
  transmute(
    clinic_ID,
    cAnnee,
    cPeriode,
    icam_total = 0,
    icam_event = 0,
    reg_name,
    dist_name,
    reg_uid,
    dist_uid,
    large_icam_event = 0,
    fs_pop,
    fs_type,
    geometry
  )

export_data_base_augmented <- bind_rows(
  export_data,
  news_rows_missing
)


# ------------------------------------------------------------
# 10. Add unmatched news events as transparent source-specific columns
# ------------------------------------------------------------

export_data_augmented <- export_data_base_augmented %>%
  mutate(
    clinic_icam_total = icam_total,
    clinic_icam_event = icam_event,
    clinic_large_icam_event = large_icam_event
  ) %>%
  left_join(
    news_additions_by_healthshed_month,
    by = c("clinic_ID", "cAnnee", "cPeriode")
  ) %>%
  mutate(
    news_unmatched_events = replace_na(news_unmatched_events, 0),
    news_unmatched_cases = replace_na(news_unmatched_cases, 0),
    news_unmatched_deaths = replace_na(news_unmatched_deaths, 0),
    news_unmatched_large_events = replace_na(news_unmatched_large_events, 0),
    news_event_ids = replace_na(news_event_ids, ""),
    news_event_dates = replace_na(news_event_dates, ""),
    
    has_unmatched_news_event = news_unmatched_events > 0,
    
    icam_total_augmented =
      clinic_icam_total + news_unmatched_cases,
    
    icam_event_augmented = if_else(
      clinic_icam_event == 1 | news_unmatched_events > 0,
      1,
      0
    ),
    
    large_icam_event_augmented = if_else(
      clinic_large_icam_event == 1 |
        news_unmatched_large_events > 0 |
        icam_total_augmented > 3,
      1,
      0
    )
  )


# ------------------------------------------------------------
# 11. Create unmatched news event point layer
# ------------------------------------------------------------

unmatched_news_points <- unmatched_news_events %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )


# ------------------------------------------------------------
# 12. Create all-years augmented healthshed summary layer
# ------------------------------------------------------------

summary_by_clinic_augmented <- export_data_augmented %>%
  st_drop_geometry() %>%
  group_by(clinic_ID) %>%
  summarise(
    clinic_icam_total_sum = sum(clinic_icam_total, na.rm = TRUE),
    clinic_icam_event_sum = sum(clinic_icam_event, na.rm = TRUE),
    clinic_large_icam_event_sum = sum(clinic_large_icam_event, na.rm = TRUE),
    
    news_unmatched_cases_sum = sum(news_unmatched_cases, na.rm = TRUE),
    news_unmatched_events_sum = sum(news_unmatched_events, na.rm = TRUE),
    news_unmatched_deaths_sum = sum(news_unmatched_deaths, na.rm = TRUE),
    news_unmatched_large_events_sum =
      sum(news_unmatched_large_events, na.rm = TRUE),
    
    icam_total_augmented_sum = sum(icam_total_augmented, na.rm = TRUE),
    icam_event_augmented_sum = sum(icam_event_augmented, na.rm = TRUE),
    large_icam_event_augmented_sum =
      sum(large_icam_event_augmented, na.rm = TRUE),
    .groups = "drop"
  )

combined_data_allyears_augmented <- healthsheds_2022 %>%
  inner_join(
    summary_by_clinic_augmented,
    by = c("fs_uid" = "clinic_ID")
  )


# ------------------------------------------------------------
# 13. Export augmented database
# ------------------------------------------------------------

output_gpkg <- "./data/icam_cases_augmented_with_unmatched_news.gpkg"

if (file.exists(output_gpkg)) {
  file.remove(output_gpkg)
}

st_write(
  export_data_augmented,
  output_gpkg,
  layer = "healthshed_month_augmented",
  quiet = TRUE
)

st_write(
  unmatched_news_points,
  output_gpkg,
  layer = "unmatched_news_events_points",
  quiet = TRUE
)

st_write(
  combined_data_allyears_augmented,
  output_gpkg,
  layer = "healthshed_all_years_augmented",
  quiet = TRUE
)

write_csv(
  event_match_table,
  "./data/news_clinic_event_match_table_madagascar_only.csv"
)

write_csv(
  st_drop_geometry(unmatched_news_events),
  "./data/unmatched_news_events_added_to_icam_database.csv"
)

write_csv(
  news_additions_by_healthshed_month,
  "./data/unmatched_news_additions_by_healthshed_month.csv"
)


# ------------------------------------------------------------
# 14. Quick diagnostic summaries
# ------------------------------------------------------------

cat("\nClinic-only ICAM totals:\n")
print(
  export_data %>%
    st_drop_geometry() %>%
    summarise(
      clinic_cases_total = sum(icam_total, na.rm = TRUE),
      clinic_events_total = sum(icam_event, na.rm = TRUE),
      clinic_large_events_total = sum(large_icam_event, na.rm = TRUE)
    )
)

cat("\nNews-clinic match status:\n")
print(
  event_match_table %>%
    count(match_status) %>%
    mutate(share = n / sum(n))
)

cat("\nUnmatched news events added:\n")
print(
  unmatched_news_events %>%
    summarise(
      n_unmatched_events = n(),
      total_news_unmatched_cases = sum(news_unmatched_cases, na.rm = TRUE),
      total_news_unmatched_deaths = sum(news_unmatched_deaths, na.rm = TRUE)
    )
)

cat("\nAugmented ICAM totals:\n")
print(
  export_data_augmented %>%
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
)

cat("\nRows with unmatched news additions:\n")
print(
  export_data_augmented %>%
    filter(has_unmatched_news_event) %>%
    st_drop_geometry() %>%
    select(
      clinic_ID,
      cAnnee,
      cPeriode,
      reg_name,
      dist_name,
      clinic_icam_total,
      news_unmatched_cases,
      icam_total_augmented,
      news_unmatched_events,
      news_unmatched_deaths,
      news_event_ids,
      news_event_dates
    ) %>%
    arrange(cAnnee, cPeriode, clinic_ID)
)

cat("\nFiles written:\n")
cat("- ./data/icam_cases_shapefile.gpkg\n")
cat("- ./data/icam_cases_augmented_with_unmatched_news.gpkg\n")
cat("- ./data/news_clinic_event_match_table_madagascar_only.csv\n")
cat("- ./data/unmatched_news_events_added_to_icam_database.csv\n")
cat("- ./data/unmatched_news_additions_by_healthshed_month.csv\n")

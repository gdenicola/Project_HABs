library(tidyverse)
library(readxl)
library(sf)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(ggrepel)

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen = 999)

# If export_data is not already in memory, read the saved ICAM clinic-month file
if (!exists("export_data")) {
  export_data <- st_read("./data/icam_cases_shapefile.gpkg", quiet = TRUE)
}

healthsheds_2022 <- st_read("./data/healthsheds2022.shx", quiet = TRUE)


# ------------------------------------------------------------
# 1. Clean news-reported event data
#    Keep only Madagascar marine intoxication events, 2016-2024
#    La Reunion and other non-Madagascar points are excluded here
# ------------------------------------------------------------

jp <- read_excel(
  "./data/[FINAL] Madagascar HABs+Marine Intoxication Lit+Field Review Extraction Sheet.xlsx"
)

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
# 2. Prepare CSB1/CSB2 healthsheds and clinic-month data
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
    geometry
  )

clinic_month <- export_data %>%
  st_drop_geometry() %>%
  filter(fs_type %in% c("CSB1", "CSB2")) %>%
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
# 3. Assign each news-reported event to nearest CSB1/CSB2 healthshed
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
      matched_dist_uid = dist_uid
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
# 4. Build exact and +/- 1 month matching windows
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
# 5. Match to clinic-month data: healthshed level and district level
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
    dist_large_positive_months = sum(replace_na(dist_large_icam_events, 0)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = window,
    values_from = c(
      dist_icam_cases,
      dist_positive_months,
      dist_large_positive_months
    ),
    names_glue = "{window}_{.value}",
    values_fill = 0
  )


# ------------------------------------------------------------
# 6. Final event-level match table
# ------------------------------------------------------------

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
    distance_to_healthshed_km
  ) %>%
  left_join(hs_match, by = "event_id") %>%
  left_join(dist_match, by = "event_id") %>%
  mutate(
    across(
      contains("icam"),
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
    ),
    
    simple_status = case_when(
      match_status == "Strong match: same healthshed, same month" ~
        "Strong match",
      
      str_detect(match_status, "Weak match") ~
        "Weak match",
      
      match_status == "No clinic match found" ~
        "No match",
      
      TRUE ~ match_status
    ),
    
    clinic_match_size = case_when(
      match_status == "Strong match: same healthshed, same month" ~
        exact_hs_icam_cases,
      
      match_status == "Plausible match: same healthshed, +/- 1 month" ~
        pm1_hs_icam_cases,
      
      match_status == "Weak match: same district, same month" ~
        exact_dist_icam_cases,
      
      match_status == "Weak match: same district, +/- 1 month" ~
        pm1_dist_icam_cases,
      
      TRUE ~ 0
    ),
    
    clinic_size_definition = case_when(
      match_status == "Strong match: same healthshed, same month" ~
        "Same healthshed, same month",
      
      match_status == "Plausible match: same healthshed, +/- 1 month" ~
        "Same healthshed, +/- 1 month",
      
      match_status == "Weak match: same district, same month" ~
        "Same district, same month",
      
      match_status == "Weak match: same district, +/- 1 month" ~
        "Same district, +/- 1 month",
      
      TRUE ~ "No clinic match"
    ),
    
    clinic_to_news_ratio = if_else(
      !is.na(no_impacted) & no_impacted > 0,
      clinic_match_size / no_impacted,
      NA_real_
    )
  ) %>%
  arrange(start_date)


# ------------------------------------------------------------
# 7. Export matched and non-matched tables to one Excel sheet
# ------------------------------------------------------------

matched_events_table <- event_match_table %>%
  filter(match_status != "No clinic match found") %>%
  select(
    event_id,
    start_date,
    end_date,
    matched_reg_name,
    matched_dist_name,
    news_reported_people_impacted = no_impacted,
    news_reported_deaths = no_death,
    clinic_match_size,
    clinic_size_definition,
    clinic_to_news_ratio,
    exact_hs_icam_cases,
    pm1_hs_icam_cases,
    exact_dist_icam_cases,
    pm1_dist_icam_cases,
    match_status,
    lat,
    lon
  ) %>%
  arrange(event_id)

nonmatched_events_table <- event_match_table %>%
  filter(match_status == "No clinic match found") %>%
  select(
    event_id,
    start_date,
    end_date,
    matched_reg_name,
    matched_dist_name,
    news_reported_people_impacted = no_impacted,
    news_reported_deaths = no_death,
    clinic_match_size,
    clinic_size_definition,
    exact_hs_icam_cases,
    pm1_hs_icam_cases,
    exact_dist_icam_cases,
    pm1_dist_icam_cases,
    match_status,
    lat,
    lon
  ) %>%
  arrange(event_id)

wb <- createWorkbook()

addWorksheet(wb, "Madagascar comparison")

title_style <- createStyle(
  textDecoration = "bold",
  fontSize = 14
)

writeData(
  wb,
  sheet = "Madagascar comparison",
  x = "Matched news-reported Madagascar events",
  startRow = 1,
  startCol = 1
)

addStyle(
  wb,
  sheet = "Madagascar comparison",
  style = title_style,
  rows = 1,
  cols = 1
)

writeDataTable(
  wb,
  sheet = "Madagascar comparison",
  x = matched_events_table,
  startRow = 3,
  startCol = 1,
  tableStyle = "TableStyleMedium2"
)

second_table_start <- nrow(matched_events_table) + 7

writeData(
  wb,
  sheet = "Madagascar comparison",
  x = "Non-matched news-reported Madagascar events",
  startRow = second_table_start,
  startCol = 1
)

addStyle(
  wb,
  sheet = "Madagascar comparison",
  style = title_style,
  rows = second_table_start,
  cols = 1
)

writeDataTable(
  wb,
  sheet = "Madagascar comparison",
  x = nonmatched_events_table,
  startRow = second_table_start + 2,
  startCol = 1,
  tableStyle = "TableStyleMedium4"
)

setColWidths(
  wb,
  sheet = "Madagascar comparison",
  cols = 1:25,
  widths = "auto"
)

freezePane(
  wb,
  sheet = "Madagascar comparison",
  firstActiveRow = 4
)

saveWorkbook(
  wb,
  file = "./data/news_reported_clinic_match_tables_madagascar_only.xlsx",
  overwrite = TRUE
)


# ------------------------------------------------------------
# 8. Produce Madagascar-only event map
#    Coincident event locations are slightly offset for visibility
# ------------------------------------------------------------

offset_degrees <- 0.08

jp_points_plot_df <- event_match_table %>%
  mutate(
    jp_size_for_plot = if_else(
      is.na(no_impacted) | no_impacted <= 0,
      1,
      no_impacted
    ),
    
    simple_status = factor(
      simple_status,
      levels = c("No match", "Weak match", "Strong match")
    )
  ) %>%
  group_by(lat, lon) %>%
  arrange(simple_status, event_id, .by_group = TRUE) %>%
  mutate(
    n_same_location = n(),
    location_index = row_number(),
    
    angle = if_else(
      n_same_location > 1,
      2 * pi * (location_index - 1) / n_same_location,
      0
    ),
    
    plot_lon = if_else(
      n_same_location > 1,
      lon + offset_degrees * cos(angle),
      lon
    ),
    
    plot_lat = if_else(
      n_same_location > 1,
      lat + offset_degrees * sin(angle),
      lat
    )
  ) %>%
  ungroup()

jp_points_plot <- jp_points_plot_df %>%
  st_as_sf(
    coords = c("plot_lon", "plot_lat"),
    crs = 4326,
    remove = FALSE
  )

news_event_map <- ggplot() +
  geom_sf(
    data = healthsheds_public,
    fill = "grey95",
    color = "grey80",
    linewidth = 0.1
  ) +
  
  geom_sf(
    data = jp_points_plot %>% filter(simple_status == "No match"),
    aes(
      fill = simple_status,
      size = jp_size_for_plot
    ),
    shape = 21,
    color = "black",
    stroke = 0.8,
    alpha = 0.85
  ) +
  
  geom_sf(
    data = jp_points_plot %>% filter(simple_status == "Weak match"),
    aes(
      fill = simple_status,
      size = jp_size_for_plot
    ),
    shape = 21,
    color = "black",
    stroke = 1.0,
    alpha = 0.95
  ) +
  
  geom_sf(
    data = jp_points_plot %>% filter(simple_status == "Strong match"),
    aes(
      fill = simple_status,
      size = jp_size_for_plot
    ),
    shape = 21,
    color = "black",
    stroke = 1.0,
    alpha = 0.95
  ) +
  
  geom_label_repel(
    data = jp_points_plot_df,
    aes(
      x = plot_lon,
      y = plot_lat,
      label = event_id
    ),
    size = 4,
    fontface = "bold",
    fill = "white",
    color = "black",
    label.size = 0.2,
    min.segment.length = 0,
    box.padding = 0.45,
    point.padding = 0.9,
    max.overlaps = Inf,
    seed = 123
  ) +
  
  scale_fill_manual(
    name = "Match status",
    values = c(
      "No match" = "#D55E00",
      "Weak match" = "#0072B2",
      "Strong match" = "#009E73"
    ),
    breaks = c(
      "No match",
      "Weak match",
      "Strong match"
    )
  ) +
  
  scale_size_continuous(
    name = "News-reported\npeople impacted",
    range = c(3, 10)
  ) +
  
  labs(
    title = "News-reported marine intoxication events and clinic-data matches",
    subtitle = "Madagascar events only; La Reunion events excluded. Coincident event locations are slightly offset for visibility."
  ) +
  
  coord_sf(
    xlim = c(43, 51),
    ylim = c(-26, -11),
    expand = FALSE
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey88")
  )

news_event_map

ggsave(
  filename = "./data/news_reported_events_match_status_map_madagascar_only_with_ids.png",
  plot = news_event_map,
  width = 10,
  height = 11,
  dpi = 300
)



# ------------------------------------------------------------
# 9. Quick checks
# ------------------------------------------------------------

event_match_table %>%
  count(match_status) %>%
  mutate(share = n / sum(n))

matched_events_table %>%
  count(match_status)

nonmatched_events_table %>%
  count(match_status)

jp_points_plot_df %>%
  filter(n_same_location > 1) %>%
  select(
    event_id,
    start_date,
    no_impacted,
    match_status,
    lat,
    lon,
    plot_lat,
    plot_lon
  )

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
options(scipen=999)
rm(list=ls())

################################################################################

#load cholorophyll satellite data
chla_old <- stack("./data/clorophyll_monthly_mean.nc")
chla <- stack("./data/chla_bc.nc")


#load population data
landscan_population <- read_csv("./data/landscan_population.csv")

#load healthsheds shapefile of madagascar
healthsheds_2022 <- st_read("./data/healthsheds2022.shx")
healthsheds_2022 <- st_transform(healthsheds_2022, crs = 3857)

# load cases data (icam_cases)

#this is data saved with the function st_write(data_file,"filename.gpkg")
icam_cases <- st_read("./data/icam_cases_shapefile.gpkg")
#change coordinate system to 3857 like the other file
icam_cases <- st_transform(icam_cases, crs = 3857)

# convert raster to dataframe format 
chla_df <- as.data.frame(rasterToPoints(chla))
chla_df_old <- as.data.frame(rasterToPoints(chla_old))

# define coordinate columns
coords <- chla_df[, 1:2]

#subset relevant clorophyll columns, i.e. years 2016-2022
chlorophyll_columns <- chla_df[ , 3:length(names(chla_df))]

#create column with month-year combination
icam_cases <- icam_cases %>%
  mutate(month_year = paste(sprintf("%02d", cPeriode), cAnnee, sep = "-"))

# we now change the largest icam events to a given value x (30 seems reasonable
# to preserve the vast majority of the data while moderating the 8 remaining outliers,
# only 4 of which on the coast)

#create a single category for icam events > x to mitigate outliers
x <- 30
icam_cases$icam_total[icam_cases$icam_total>x] <- x
table(icam_cases$icam_total)


unique_months <- unique(icam_cases$month_year)

sorted_months <- sort(unique_months)

colnames(chlorophyll_columns) <- sorted_months

chla_df <- cbind(coords, chlorophyll_columns)

# convert new dataframe to sf format and transform to proper coordinate system
chla_sf <- st_as_sf(chla_df, coords = c("x", "y"), crs = 4326)
chla_sf <- st_transform(chla_sf, crs = 3857)


# save chla_sf as shapefile in documents
#st_write(chla_sf, "./data/chla_sf_new.shp",append=FALSE)

#plot of all of icam events on all healthsheds

# Summarize data by clinic_ID, summing up the icam_total across all years
summary_by_clinic <- icam_cases %>%
  group_by(clinic_ID) %>%
  summarise(icam_total_sum = sum(icam_total, na.rm = TRUE),
            icam_event_sum = sum(icam_event, na.rm = TRUE),
            icam_large_event_sum = sum(large_icam_event, na.rm = TRUE))

combined_data_allyears <- summary_by_clinic %>%
  st_join(healthsheds_2022, by = c("clinic_ID" = "fs_uid"))


# Convert combined_data_allyears back to an sf object
if (!inherits(combined_data_allyears, "sf")) {
  combined_data_allyears <- st_as_sf(combined_data_allyears)
}



#load coastline shapefile 
#(to find all sea pixels and healthsheds adjacent to the coast)
coastline <- st_read("./data/madagascar_coastline.shp")

# Transform coastline to match chla_sf's CRS (EPSG:3857)
#coastline <- st_transform(coastline, st_crs(chla_sf))

# First, combine the coastline into a single multilinestring
coastline_combined <- st_combine(coastline)

# Create buffer and extract its boundary (to obtain line, for plotting only)
madagascar_coastline_buffer_line <- st_boundary(st_buffer(coastline_combined, dist = 50000))


# #create buffer polygons (not for plotting, but needed for filtering later)
madagascar_coastline_buffer <- st_buffer(coastline_combined, dist = 50000)
#save shapefile of buffer
#st_write(madagascar_coastline_buffer, "madagascar_coastline_buffer.shp", delete_dsn = TRUE)


# #create buffer polygons (just for plotting chla)
madagascar_coastline_buffer_large <- st_buffer(coastline_combined, dist = 50000, append = FALSE)


# Filter chlorophyll points within the buffer
chla_coastal <- chla_sf[st_intersects(chla_sf, madagascar_coastline_buffer, sparse = FALSE), ]
# (just for plotting chla)
chla_coastal_large <- chla_sf[st_intersects(chla_sf, madagascar_coastline_buffer_large, sparse = FALSE), ]



# Filter healthsheds that intersect with the Madagascar coastline buffer
icam_cases_coastal_only <- icam_cases %>%
  filter(st_intersects(., madagascar_coastline_buffer, sparse = FALSE))

# Create a new variable "coastal" that checks for intersection with the coastline buffer
# INSTEAD of filtering out non-coastal entries
icam_cases_coastal <- icam_cases %>%
  mutate(coastal = as.integer(lengths(st_intersects(., madagascar_coastline_buffer)) > 0))




#we now successfully created (a) an sf object containing the counts of 
#icam events for all coastal healthsheds, called, "icam_cases_coastal", 
#and (b) another sf object containing clorophyll levels for all water 
#pixels near the coast, called  "chla_coastal". what i would now like 
#is to match each healthshed in icam_cases_coastal with the water pixels
#it's close to. specifically, we can do the matching for a radius of 10km. 
#obviously, each healthshed will be matched with multiple water pixels, and 
#therefore multiple cholorphyl measures. what i would like to obtain is to 
#have just to measures of clorophyll for each healthshed, and namely: 
#(1) the average clorophyll level in the pixels to which it is matched to, 
#in a specific year_month period; and (2), the maximum clorophyll level 
#among all pixels that it is matched to, in a specific year_month period. 


# Extract coordinates from chla_coastal before joining, to keep them
chla_coastal <- chla_coastal %>%
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2])


chla_coastal_long <- chla_coastal %>%
  pivot_longer(
    cols = starts_with(c("0", "1")),  # Select columns starting with 0 or 1
    names_to = "month_year",
    values_to = "chla"
  )


# Only keep coastal healthsheds (coastal == 1) before running st_join()
icam_cases_near_water <- icam_cases_coastal %>%
  filter(coastal == 1)

rm(icam_cases_coastal_only, icam_cases)

#saveRDS(icam_cases_near_water, "./data/icam_cases_near_water.rds")
#saveRDS(chla_coastal, "./data/chla_coastal_with_coords.rds")  

#Now run st_join() only on this smaller subset - takes about 90 minutes with 50km radius
icam_chla_join <- st_join(icam_cases_near_water, chla_coastal,
                          join = st_is_within_distance, dist = 50000)


#icam_chla_join <- readRDS("./data/icam_chla_join_50km.rds")


# Perform a spatial join to find water pixels within 50 km of each healthshed
# takes about XX minutes (used to be 30 with just coastal stuff)
# icam_chla_join <- st_join(
#   icam_cases_coastal, 
#   chla_coastal, 
#   join = st_is_within_distance, 
#   dist = 50000 # 50 km radius
# )

#####################

# Identify date columns
date_columns <- grep("^\\d{2}-\\d{4}$", names(icam_chla_join), value = TRUE)

# Process the data
icam_chla_filtered <- icam_chla_join %>%
  mutate(
    row_id = row_number(),
    chla_value = map2_dbl(month_year, row_id, function(my, id) {
      if (my %in% date_columns) {
        value <- icam_chla_join[[my]][id]
        if(is.numeric(value)) value else as.numeric(NA)
      } else {
        as.numeric(NA)
      }
    })
  ) %>%
  dplyr::select(-all_of(date_columns), -row_id)

#clear large (and now useless) object from memory 
rm(icam_chla_join)

#now calculate mean average monthly chlorophyll (probably useless) and 
# max average monthly cholorophyll per healthshed (probably useful)

# Step 1: Extract non-spatial data and calculate max and mean
non_spatial_data <- st_drop_geometry(icam_chla_filtered)

icam_chla_summary <- non_spatial_data %>%
  group_by(clinic_ID, month_year) %>%
  summarise(
    max_chla = if(all(is.na(chla_value))) NA_real_ else max(chla_value, na.rm = TRUE),
    mean_chla = if(all(is.na(chla_value))) NA_real_ else mean(chla_value, na.rm = TRUE),
    n_chla_obs = sum(!is.na(chla_value)),  # Count of non-NA observations
    across(everything(), ~if(is.numeric(.)) first(.) else first(na.omit(.))),
    .groups = 'drop'
  ) %>%
  dplyr::select(-chla_value)  # Remove the original chla_value column if you don't need it

# Step 2: Join with spatial data
spatial_data <- icam_chla_filtered %>% 
  dplyr::select(clinic_ID, geom) %>% 
  group_by(clinic_ID) %>%
  slice(1) %>%  # Take the first geometry for each clinic_ID
  ungroup()

icam_chla_summary_sf <- icam_chla_summary %>%
  left_join(spatial_data, by = "clinic_ID") %>%
  st_as_sf()


#this seems to have worked, i have to do a few checks just to
# see if these are really mean and max. 

#now NEED to rejoin the non-coastal healthsheds in this, with
#chlorophyll value 0:

# Step 1: Remove geometry from `icam_chla_summary_sf` before joining
icam_chla_summary_df <- icam_chla_summary_sf %>%
  st_drop_geometry() %>%  # Convert to a regular dataframe
  select(clinic_ID, month_year, max_chla, mean_chla, n_chla_obs)

# Step 2: Perform a left join, keeping all healthsheds
icam_final_sf <- icam_cases_coastal %>%
  left_join(icam_chla_summary_df, by = c("clinic_ID", "month_year"))

# Step 3: Replace NA values for non-coastal healthsheds
icam_final_sf <- icam_final_sf %>%
  mutate(
    max_chla = ifelse(is.na(max_chla), 0, max_chla),
    mean_chla = ifelse(is.na(mean_chla), 0, mean_chla),
    n_chla_obs = ifelse(is.na(n_chla_obs), 0, n_chla_obs)
  )

#create month variable that is actually in date format
icam_chla_summary_seasonal <- icam_final_sf %>%
  mutate(
    month = factor(substr(month_year, 1, 2), levels = sprintf("%02d", 1:12)),
    year = as.numeric(substr(month_year, 4, 7))
  )

# Now, let's join the population data
icam_chla_summary_seasonal <- icam_chla_summary_seasonal %>%
  left_join(landscan_population, by = c("clinic_ID" = "fs_uid", "year" = "year"))


#Create month_num variable
icam_chla_summary_seasonal <- icam_chla_summary_seasonal %>%
  mutate(month_num = as.numeric(factor(month, levels = c("01", "02", "03", "04", "05", "06", 
                                                         "07", "08", "09", "10", "11", "12"))))

#create continuous time variable (from 1 to 84, the total number of months)
icam_chla_summary_seasonal <- icam_chla_summary_seasonal %>%
  mutate(
    time = (cAnnee - min(cAnnee)) * 12 + month_num - min(month_num) + 1
  )


#now let's add population density:
# Calculate area in square kilometers
cases_with_all <- icam_chla_summary_seasonal %>%
  mutate(area_km2 = as.numeric(st_area(geom)) / 1e6)  # Convert m^2 to km^2

# Calculate population density (people per km^2)
cases_with_all <- cases_with_all %>%
  mutate(pop_density = landscan_pop / area_km2)


#include climate exposure variables
# Load all layers from the GRIB file
exposure_stack <- stack("./data/exposure_data.grib")

# Get full names of the layers
full_names <- names(exposure_stack)

# Try to identify patterns for each variable (case-insensitive)
temp_2m_layers <- grep("2.metre.temperature", full_names, ignore.case = TRUE)
sst_layers <- grep("sea.surface.temperature", full_names, ignore.case = TRUE)
precip_layers <- grep("total.precipitation", full_names, ignore.case = TRUE)

# Create separate stacks for each variable
t2m <- subset(exposure_stack, temp_2m_layers)
sst <- subset(exposure_stack, sst_layers)
tp <- subset(exposure_stack, precip_layers)

# convert raster to dataframe format for all variables
tp_df <- as.data.frame(rasterToPoints(tp))
t2m_df <- as.data.frame(rasterToPoints(t2m))
sst_df <- as.data.frame(rasterToPoints(sst))


# define relevant columns for total precipitation
coords_tp <- tp_df[, 1:2]
tp_columns <- tp_df[, 3:ncol(tp_df)]
unique_months <- unique(cases_with_all$month_year)
# Convert to Date objects
dates <- as.Date(paste0("01-", unique_months), format="%d-%m-%Y")
# Sort the dates
sorted_dates <- sort(dates)
# Convert back to the original format
sorted_months <- format(sorted_dates, "%m-%Y")
colnames(tp_columns) <- sorted_months
tp_df <- cbind(coords_tp, tp_columns)

# define relevant columns for 2 meters temperature
coords_t2m <- t2m_df[, 1:2]
t2m_columns <- t2m_df[, 3:ncol(t2m_df)]
colnames(t2m_columns) <- sorted_months
t2m_df <- cbind(coords_t2m, t2m_columns)

# define relevant columns for sea surface temperature
coords_sst <- sst_df[, 1:2]
sst_columns <- sst_df[, 3:ncol(sst_df)]
colnames(sst_columns) <- sorted_months
sst_df <- cbind(coords_sst, sst_columns)

# convert new dataframes to sf format and set coordinate reference system again (EPSG:3857) for all three variables
tp_sf <- st_as_sf(tp_df, coords = c("x", "y"), crs = 4326)
t2m_sf <- st_as_sf(t2m_df, coords = c("x", "y"), crs = 4326)
sst_sf <- st_as_sf(sst_df, coords = c("x", "y"), crs = 4326)
tp_sf <- st_transform(tp_sf, 3857)
t2m_sf <- st_transform(t2m_sf, 3857)
sst_sf <- st_transform(sst_sf, 3857)

# Find the nearest neighbor for each point in cases_with_all
nearest_tp <- st_nearest_feature(cases_with_all, tp_sf)
nearest_t2m <- st_nearest_feature(cases_with_all, t2m_sf)
nearest_sst <- st_nearest_feature(cases_with_all, sst_sf)


# Function to extract values for a specific date
extract_values <- function(sf_object, nearest_indices, date_col) {
  sapply(seq_along(nearest_indices), function(i) {
    sf_object[[date_col[i]]][nearest_indices[i]]
  })
}

# Function to process a chunk of data
process_chunk <- function(chunk, chunk_indices, tp_sf, t2m_sf, sst_sf, nearest_tp, nearest_t2m, nearest_sst) {
  chunk %>%
    mutate(
      precipitation = extract_values(tp_sf, nearest_tp[chunk_indices], month_year),
      temperature_2m = extract_values(t2m_sf, nearest_t2m[chunk_indices], month_year),
      sea_surface_temp = extract_values(sst_sf, nearest_sst[chunk_indices], month_year)
    )
}

# Process data in chunks
chunk_size <- 1000  # Adjust this value based on your available memory
n_chunks <- ceiling(nrow(cases_with_all) / chunk_size)

cases_with_all_processed <- list()

for(i in 1:n_chunks) {
  start_index <- (i-1) * chunk_size + 1
  end_index <- min(i * chunk_size, nrow(cases_with_all))
  chunk_indices <- start_index:end_index
  
  chunk <- cases_with_all[chunk_indices, ]
  processed_chunk <- process_chunk(chunk, chunk_indices, tp_sf, t2m_sf, sst_sf, nearest_tp, nearest_t2m, nearest_sst)
  cases_with_all_processed[[i]] <- processed_chunk
  
  cat("Processed chunk", i, "of", n_chunks, "\n")
}

# Combine all processed chunks
cases_with_all_new <- bind_rows(cases_with_all_processed)

# Ensure the result is still an sf object
cases_with_all_new <- st_as_sf(cases_with_all_new)
cases_with_all <- cases_with_all_new
rm(cases_with_all_new)
cases_with_all$precipitation <- cases_with_all$precipitation*1000


#in case we want to include spatial effects by region/district/clinic
cases_with_all$clinic_ID <- as.factor(icam_chla_summary_seasonal$clinic_ID)
cases_with_all$reg_uid <- as.factor(icam_chla_summary_seasonal$reg_uid)
cases_with_all$dist_uid <- as.factor(icam_chla_summary_seasonal$dist_uid)


#let's load analysis ready wealth index values (pre-matched by dimeji)
wealth_index_matched <- read_csv("./data/wealth_index_analysis_ready.csv")

# Merge wealth_index_matched into cases_with_all by clinic_ID
cases_with_all <- cases_with_all %>%
  left_join(select(wealth_index_matched, fs_uid, wealth_index), by = c("clinic_ID" = "fs_uid"))

# Convert sea surface temperature and 2m temperature to Celsius
cases_with_all <- cases_with_all %>%
  mutate(
    sea_surface_temp = sea_surface_temp - 273.15,
    temperature_2m = temperature_2m - 273.15
  )

# save cases_with_all (includes all new variables tp, t2m, sst and wealth_index_new) as geopackage in documents
#st_write(cases_with_all, "./data/cases_with_all_20km.gpkg", append = F)
cases_with_all <- st_read("./data/cases_with_all_50km.gpkg")
##############################################################
#create alternative version with capped chlorophyll
#icam_chla_capped <- cases_with_all
#icam_chla_capped$max_chla[icam_chla_capped$max_chla > 15] <- 15

#transform sea surface temperature so that it is centered around zero
#that is to ensure the coastal effect is meaningful to interpret as
#the effect of a healthshed being coastal with chl.a of 0 and average sst

# Compute mean sea surface temperature in Celsius
mean_sst_C <- mean(cases_with_all$sea_surface_temp, na.rm = TRUE)

# Center the sea surface temperature around zero
cases_with_all <- cases_with_all %>%
  mutate(sea_surface_temp_centered = sea_surface_temp - mean_sst_C)

#get longitude and latitude to compute smooth spatial
# Compute centroids
cases_with_all <- cases_with_all %>%
  mutate(
    centroid = st_centroid(geom),  # Get centroids
    longitude = st_coordinates(centroid)[, 1],  # Extract X (longitude)
    latitude = st_coordinates(centroid)[, 2]   # Extract Y (latitude)
  )

#detect and correct "falsely coastal" healthsheds
cases_with_all$coastal[cases_with_all$max_chla==0] <- 0

#GAM for MFP events (i.e. 0 or 1) with all of our variables (finally)
events_model <- gam(icam_event ~  
                      coastal +
                      s(I(coastal*max_chla), bs = 'ps', k = 20) +
                      #I(coastal*max_chla) +
                      month + #max_chla +
                      #reg_name + 
                      #s(dist_uid, bs = "re") +
                      s(time, bs = 'ps', k = 20) + 
                      wealth_index +
                      landscan_pop + 
                      #pop_density +
                      #area_km2 +
                      fs_type + 
                      #s(longitude, latitude, bs = "sos", k = 30) +
                      temperature_2m +
                      s(precipitation, bs = 'ps', k = 20) +
                      I(coastal*sea_surface_temp_centered), 
                    data = cases_with_all,
                    #method = 'discrete',
                    #discrete = T,
                    family = 'binomial' 
                    )
summary(events_model)
# Plot the smooth effect of max_chla
plot(events_model, select = 1, shade = TRUE, shade.col = "lightblue",
     xlab = "Chlorophyll-a", ylab = "Smooth function",
     main = "Effect of Chlorophyll-a on MFP Events", xlim=c(0,30),ylim = c(-1,1.5))

# Add a rug plot to show the distribution of the data
rug(cases_with_all$max_chla)

# Add a grey dashed line at y = 0
abline(h = 0, lty = 2, col = "grey50")


# Plot the smooth effect of precipitation
plot(events_model, select = 3, shade = TRUE, shade.col = "lightblue",
     xlab = "Precipitation", ylab = "Smooth function",
     main = "Effect of Precipitation on MFP events", xlim=c(0,30),ylim = c(-1.5,1.5))

# Add a rug plot to show the distribution of the data
rug(cases_with_all$max_chla)

# Add a grey dashed line at y = 0
abline(h = 0, lty = 2, col = "grey50")


#GAM with more smooths
events_model <- gam(icam_event ~  
                      coastal +
                      s(I(coastal*max_chla), bs = 'ps', k = 20) +
                      #I(coastal*max_chla) +
                      month + #max_chla +
                      #reg_name + 
                      #s(dist_uid, bs = "re") +
                      s(time, bs = 'ps', k = 20) + 
                      s(wealth_index, bs = 'ps', k = 20) +
                      landscan_pop + 
                      pop_density +
                      #area_km2 +
                      fs_type + 
                      s(temperature_2m, bs = 'ps', k = 20) +
                      #sea_surface_temp +
                      s(precipitation, bs='ps', k=20), 
                    data = cases_with_all, family = 'binomial', link = 'logit')
#,discrete = T)
summary(events_model)




#BAM variant for fitting random intercepts fast
events_model <- bam(icam_event ~  
                      coastal +
                      s(I(coastal*max_chla), bs = 'ps', k = 20) +
                      #I(coastal*max_chla) +
                      month + #max_chla +
                      #reg_name + 
                      #s(clinic_ID, bs = "re") +
                      s(time, bs = 'ps', k = 20) + 
                      mean_est_iwi +
                      landscan_pop + 
                      pop_density +
                      #area_km2 +
                      fs_type + 
                      s(temperature_2m, bs = 'ps', k= 20) +
                      #sea_surface_temp +
                      precipitation, 
                  data = cases_with_all, 
                  family = 'binomial', 
                  discrete = T)
summary(events_model)
plot(events_model)
# Plot the smooth effect of max_chla
plot(events_model, select = 1, shade = TRUE, shade.col = "lightblue",
     xlab = "Chlorophyll-a", ylab = "Smooth function",
     main = "Effect of Chlorophyll-a on ICAM Events", xlim=c(0,22),ylim = c(-2,2))

# Add a rug plot to show the distribution of the data
rug(cases_with_all$max_chla)

# Add a grey dashed line at y = 0
abline(h = 0, lty = 2, col = "grey50")

#update column large_ICAM_event, equal to 1 if there was an ICAM event of size > x, 
#and 0 otherwise, meaning if ICAM_total = 0 or otherwise
x = 4
cases_with_all <- mutate(cases_with_all, large_icam_event = ifelse(icam_total > x, 1, 0))

#GAM for events (i.e. 0 or 1) with all of our variables (finally)
large_events_model <- gam(large_icam_event ~  
                      coastal +
                      s(I(coastal*max_chla), bs = 'ps', k = 20) +
                      #I(coastal*max_chla) +
                      month + #max_chla +
                      #reg_name + 
                      #s(dist_uid, bs = "re") +
                      s(time, bs = 'ps', k = 20) + 
                      wealth_index +
                      landscan_pop + 
                      #pop_density +
                      #area_km2 +
                      fs_type + 
                      #s(longitude, latitude, bs = "sos", k = 30) +
                      temperature_2m +
                      s(precipitation, bs = 'ps', k = 20) +
                      I(coastal*sea_surface_temp_centered), 
                    data = cases_with_all,
                    #method = 'discrete',
                    #discrete = T,
                    family = 'binomial' 
)
summary(large_events_model)
#plot(events_model, select = 1, xlim = c(0,20), ylim = c(-1.5,1.5))

# Plot the smooth effect of max_chla
plot(large_events_model, select = 1, shade = TRUE, shade.col = "lightblue",
     xlab = "Chlorophyll-a", ylab = "Smooth function",
     main = "Effect of Chlorophyll-a on ICAM Events", xlim=c(0,22))

# Add a rug plot to show the distribution of the data
rug(cases_with_all$max_chla)

# Add a grey dashed line at y = 0
abline(h = 0, lty = 2, col = "grey50")

#GAM for cases counts (with cases censored at 50) with all of our variables
#does not converge with region
cases_model <- bam(icam_total ~ s(max_chla, bs = 'ps', k = 20) + 
                     month + #max_chla +
                     #reg_name + 
                     s(time, bs = 'ps', k = 20) + 
                     mean_est_iwi +
                     landscan_pop + 
                     pop_density +
                     #area_km2 +
                     fs_type + temperature_2m +
                     precipitation + sea_surface_temp, 
                   data = cases_with_all, family = 'ziP', discrete = T)
summary(cases_model)
# Plot the smooth effect of max_chla
plot(cases_model, select = 1, shade = TRUE, shade.col = "lightblue",
     xlab = "Chlorophyll-a", ylab = "Smooth function",
     main = "Effect of Chlorophyll-a on ICAM Events", xlim=c(0,22))

# Add a rug plot to show the distribution of the data
rug(cases_with_all$max_chla)

# Add a grey dashed line at y = 0
abline(h = 0, lty = 2, col = "grey50")



######PLOTTING ONLY FROM NOW ON ##############


# Summarize data by clinic_ID, summing up the icam_total across all years
summary_by_clinic <- icam_cases_coastal %>%
  group_by(clinic_ID) %>%
  summarise(
    icam_total_sum = sum(icam_total, na.rm = TRUE),
    icam_event_sum = sum(icam_event, na.rm = TRUE),
    icam_large_event_sum = sum(large_icam_event, na.rm = TRUE),
    geom = st_union(geom)  # Combine geometries for each clinic_ID
  )




# Plot Total ICAM Cases
ggplot(combined_data_allyears) +
  geom_sf(aes(fill = ifelse(icam_total_sum == 0, NA, icam_total_sum)), 
          color = "grey95", size = 0.05) +  # Reduce border size
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +  # Adjust color scale
  theme_minimal() +
  labs(fill = "MFP Cases") +
  labs(
    title = "Total MFP Cases by Clinic",
    subtitle = "2016-2024"
  )



# Plot Total ICAM Events
ggplot(combined_data_allyears) +
  geom_sf(aes(fill = ifelse(icam_event_sum == 0, NA, icam_event_sum)), 
          color = "grey95", size = 0.05) +  # Reduce border size
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +  # Adjust color scale
  theme_minimal() +
  labs(fill = "MFP Events") +
  labs(
    title = "Total MFP Events by Clinic",
    subtitle = "2016-2024"
  )





####Chlorophyll Impact Zone
ggplot() +
  # Plot original coastline
  geom_sf(data = coastline, color = "blue") +
  # Plot buffered coastline boundary
  geom_sf(data = madagascar_coastline_buffer_line, color = "red") +
  # Optional: Add a title
  labs(title = "HABs Impact Zone",
       subtitle = "Blue: Original coastline, Red: 50km buffer") +
  # Improve the theme
  theme_minimal()


#plot chlorophyll:

# Calculate average chlorophyll concentration
chla_avg <- chla_coastal_long %>%
  group_by(lon, lat) %>%
  summarise(avg_chla = mean(chla, na.rm = TRUE)) %>%
  ungroup()

# Create the plot
# Create the plot with log-transformed Chlorophyll-a
ggplot() +
  geom_sf(data = chla_avg, aes(color = log1p(avg_chla), geometry = geometry), size = 0.5) +  # Log-transform avg_chla
  scale_color_viridis(option = "plasma", name = "Log-Chlorophyll-a\n(mg/m³)", na.value = "white") +  # Adjusted color scale
  theme_minimal() +
  labs(
    title = "Average Chlorophyll-a Concentration",
    subtitle = "2016-2022",
    caption = "Source: Copernicus Marine Health Services"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  coord_sf()


###Wealth Index
# Create new sf for dropping things
cases_with_all_new_sf <- st_as_sf(cases_with_all)

# Keep only one record per clinic_ID (since wealth index is time-invariant)
wealth_index_sf <- cases_with_all_new_sf %>%
  filter(!is.na(wealth_index)) %>%  # Remove any missing wealth index values
  group_by(clinic_ID) %>%
  summarise(wealth_index = first(wealth_index), geometry = first(geom), .groups = "drop")

ggplot() +
  # Wealth Index Layer
  geom_sf(data = wealth_index_sf, aes(fill = log1p(wealth_index)), color = NA) +  # No color on polygons
  # Borders Layer (Thinner Lines)
  #geom_sf(data = wealth_index_sf, fill = NA, color = "black", size = 0.05) +  # Thin border
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Log Wealth Index", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Wealth Index by Healthshed",
    subtitle = "Log-scale",
    caption = "Source: World Bank"
  )



# Compute average values per clinic over the entire 2016-2022 period
cases_avg <- cases_with_all %>%
  group_by(clinic_ID, coastal) %>%  # Retain coastal classification
  summarise(
    avg_precipitation = mean(precipitation, na.rm = TRUE),
    avg_temperature_2m = mean(temperature_2m, na.rm = TRUE),
    avg_sea_surface_temp = mean(sea_surface_temp, na.rm = TRUE),
    avg_chlorophyll = mean(max_chla, na.rm = TRUE), 
    avg_pop_density = mean(pop_density, na.rm = TRUE),  # Compute average population density
    geometry = first(geom)  # Retain spatial data
  ) %>%
  ungroup()

# Convert back to sf object
cases_avg <- st_as_sf(cases_avg, crs = st_crs(cases_with_all))

# Set non-coastal areas to NA for SST & Chlorophyll-a
cases_avg <- cases_avg %>%
  mutate(
    avg_sea_surface_temp = ifelse(coastal == 0, NA, avg_sea_surface_temp),
    avg_chlorophyll = ifelse(coastal == 0, NA, avg_chlorophyll),
    log_avg_pop_density = log1p(avg_pop_density)  # Apply log transformation to population density
  )


ggplot(cases_avg) +
  geom_sf(aes(fill = log_avg_pop_density), color = NA) +  
  scale_fill_viridis_c(option = "viridis", direction = -1, na.value = "white") +  
  theme_minimal() +
  labs(fill = "Log Population Density") +
  labs(
    title = "Population Density by Healthshed",
    subtitle = "Log-scale"
  )




# Plot 1: Average Precipitation (2016-2022) (old auto-scale)
# ggplot(cases_avg) +
#   geom_sf(aes(fill = avg_precipitation), color = NA) +  # Completely remove borders
#   scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
#   theme_minimal() +
#   labs(fill = "Avg. Precipitation (mm/day)") +
#   labs(
#     title = "Average Precipitation by Healthshed",
#     subtitle = "2016-2022"
#   )

# Plot 1: Average Precipitation (2016-2022) (new)
ggplot(cases_avg) +
  geom_sf(aes(fill = avg_precipitation), color = NA) +  
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white", 
                       limits = c(0.78, 9.99)) +  # Ensure scale covers full range
  theme_minimal() +
  labs(fill = "Avg. Precipitation (mm/day)") +
  labs(
    title = "Average Precipitation by Healthshed",
    subtitle = "2016-2022"
  )

# Plot 2: Average 2m Temperature (2016-2022)
ggplot(cases_avg) +
  geom_sf(aes(fill = avg_temperature_2m), color = NA) +  # Completely remove borders
  scale_fill_distiller(palette = "RdBu", direction = -1, na.value = "white") +  # Blue for cold, Red for warm
  theme_minimal() +
  labs(fill = "Avg. 2m Temperature (°C)") +
  labs(
    title = "Average 2m Temperature by Healthshed",
    subtitle = "2016-2022"
  )



# Plot 3: Average Sea Surface Temperature (2016-2022)
ggplot(cases_avg) +
  geom_sf(aes(fill = avg_sea_surface_temp), color = NA) +  # Completely remove borders
  scale_fill_distiller(palette = "RdBu", direction = -1, na.value = "white") +  # Blue for cold, Red for warm
  theme_minimal() +
  labs(fill = "Avg. Sea Surface Temp (°C)") +
  labs(
    title = "Average Sea Surface Temperature by Healthshed",
    subtitle = "2016-2022"
  )

# Plot 4: Average Chlorophyll-a Concentration (2016-2022)
ggplot(cases_avg) +
  geom_sf(aes(fill = avg_chlorophyll), color = NA) +  # Completely remove borders
  scale_fill_viridis_c(option = "plasma", direction = 1, na.value = "white") +  
  theme_minimal() +
  labs(fill = "Avg. Chlorophyll-a (mg/m³)") +
  labs(
    title = "Maximum Average Chlorophyll-a by Healthshed",
    subtitle = "2016-2022"
  )

# Plot empty healthsheds with borders only
ggplot(combined_data_allyears) +
  geom_sf(fill = "grey98", color = "grey75", size = 0.05) +  # No fill, only borders
  theme_minimal() +
  labs(
    title = "Healthshed Boundaries"
  )







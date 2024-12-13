
# load relevant packages

library(writexl)
library(sf)
library(raster)
library(tibble)
library(ggplot2)
library(terra)
library(tidyverse)



# set working directory to this file's location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen=999)
rm(list=ls())

################################################################################

#load cholorophyll satellite data
chla <- stack("./data/clorophyll_monthly_mean.nc")

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

# define coordinate columns
coords <- chla_df[, 1:2]

#subset relevant clorophyll columns, i.e. years 2016-2022
chlorophyll_columns <- chla_df[ , 223:306]

#create column with month-year combination
icam_cases <- icam_cases %>%
  mutate(month_year = paste(sprintf("%02d", cPeriode), cAnnee, sep = "-"))

unique_months <- unique(icam_cases$month_year)

sorted_months <- sort(unique_months)

colnames(chlorophyll_columns) <- sorted_months

chla_df <- cbind(coords, chlorophyll_columns)

# convert new dataframe to sf format and transform to proper coordinate system
chla_sf <- st_as_sf(chla_df, coords = c("x", "y"), crs = 4326)
chla_sf <- st_transform(chla_sf, crs = 3857)


# save chla_sf as shapefile in documents
#st_write(chla_sf, "./data/chla_sf.shp",append=FALSE)


#load coastline shapefile 
#(to find all sea pixels and healthsheds adjacent to the coast)
coastline <- st_read("./data/madagascar_coastline.shp")

# Transform coastline to match chla_sf's CRS (EPSG:3857)
#coastline <- st_transform(coastline, st_crs(chla_sf))

# First, combine the coastline into a single multilinestring
coastline_combined <- st_combine(coastline)

# Create buffer and extract its boundary (to obtain line, for plotting only)
madagascar_coastline_buffer_line <- st_boundary(st_buffer(coastline_combined, dist = 20000))


# Plot using ggplot
ggplot() +
  # Plot original coastline
  geom_sf(data = coastline, color = "blue") +
  # Plot buffered coastline boundary
  geom_sf(data = madagascar_coastline_buffer_line, color = "red") +
  # Optional: Add a title
  labs(title = "Madagascar Coastline: Original and 20km Buffer Line",
       subtitle = "Blue: Original Coastline, Red: 20km Buffer Boundary") +
  # Improve the theme
  theme_minimal()


# #create buffer polygons (not for plotting, but needed for filtering later)
madagascar_coastline_buffer <- st_buffer(coastline_combined, dist = 20000)


# Filter chlorophyll points within the buffer
chla_coastal <- chla_sf[st_intersects(chla_sf, madagascar_coastline_buffer, sparse = FALSE), ]



# Filter healthsheds that intersect with the Madagascar coastline buffer
icam_cases_coastal <- icam_cases %>%
  filter(st_intersects(., madagascar_coastline_buffer, sparse = FALSE))




# Summarize data by clinic_ID, summing up the icam_total across all years
summary_by_clinic <- icam_cases_coastal %>%
  group_by(clinic_ID) %>%
  summarise(
    icam_total_sum = sum(icam_total, na.rm = TRUE),
    icam_event_sum = sum(icam_event, na.rm = TRUE),
    icam_large_event_sum = sum(large_icam_event, na.rm = TRUE),
    geom = st_union(geom)  # Combine geometries for each clinic_ID
  )


#plot of count of icam events on coastal healthsheds only
ggplot(summary_by_clinic) +
  geom_sf(aes(fill = ifelse(icam_event_sum == 0, NA, icam_event_sum)), color = "grey80") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  theme_void() +
  labs(fill = "Total ICAM Events") +
  ggtitle("Total ICAM Events by Clinic")
# use magma, plasma or inferno with direction = -1


#we now successfully created (a) an sf object containing the counts of 
#icam events for all coastal healthsheds, called, "icam_cases_coastal", 
#and (b) another sf object containing clorophyll levels for all water 
#pixels near the coast, called  "chla_coastal". what i would now like 
#is to match each healthshed in icam_cases_coastal with the water pixels
#it's close to. specifically, we can do the matching for a radius of 20km. 
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


# system.time({
#   # Perform the spatial join
#   icam_chla_join <- st_join(
#     icam_cases_coastal, 
#     chla_coastal_long, 
#     join = st_is_within_distance, 
#     dist = 20000 # 20 km radius
#   ) %>%
#     # Filter to keep only rows where month_year matches
#     filter(month_year.x == month_year.y) %>%
#     # Clean up column names
#     rename(month_year = month_year.x) %>%
#     dplyr::select(-month_year.y)
# })

# Perform a spatial join to find water pixels within 20 km of each healthshed
# takes about 30 minutes
icam_chla_join <- st_join(
  icam_cases_coastal, 
  chla_coastal, 
  join = st_is_within_distance, 
  dist = 20000 # 20 km radius
)

# First, identify the columns named after dates
date_columns <- grep("^\\d{2}-\\d{4}$", names(icam_chla_join), value = TRUE)

icam_chla_filtered <- icam_chla_join %>%
  rowwise() %>%
  mutate(
    chla_value = {
      col_name <- month_year
      if (col_name %in% names(.)) {
        value <- .[[col_name]]
        if (length(value) > 1) mean(value, na.rm = TRUE) else value
      } else {
        NA_real_
      }
    }
  ) %>%
  ungroup() %>%
  # Select all columns except the date columns
  select(-all_of(date_columns))





#the following step 2 is pure chatgpt output, to be checked after running step 1
# Step 2: Calculate average and maximum chlorophyll levels for each healthshed and year_month
icam_chla_summary <- icam_chla_join %>%
  group_by(healthshed_id, year_month) %>% # Replace 'healthshed_id' and 'year_month' with your actual column names
  summarize(
    avg_chlorophyll = mean(chlorophyll_level, na.rm = TRUE), # Replace 'chlorophyll_level' with the correct column name
    max_chlorophyll = max(chlorophyll_level, na.rm = TRUE),
    geometry = first(geometry) # Retain healthshed geometry
  ) %>%
  ungroup()


# create empty lists for mean and max chla values
mean_values <- vector("list", length = nrow(icam_cases))
max_values <- vector("list", length = nrow(icam_cases))

# go through all rows in icam_cases
for (i in 1:nrow(icam_cases)) {
  print(i)
  polygon <- icam_cases[i, ]
  
  # extract current month
  
  current_month_year <- icam_cases$month_year[i]
  
  # create buffer around polygon (4000 meters)
  
  buffer <- st_buffer(polygon, dist = 4000)
  
  # find raster points within buffer
  
  points_in_buffer <- chla_sf[st_intersects(chla_sf, buffer, sparse = FALSE), ]
  
  # temporary vectors for mean and max values per month
  
  monthly_means <- c()
  monthly_maxs <- c()
  
  # go through all columns/month in chla_sf
  
  for (month_column in names(chla_sf)[1:84]) { 
    
    # extract value for relevant month
    
    values <- points_in_buffer[[month_column]]
    
    # calculate mean and max, ignoring NAs
    
    mean_value <- mean(values, na.rm = TRUE)
    max_value <- max(values, na.rm = TRUE)
    
    # if only NAs: result shall be NA
    if (all(is.na(values))) {
      mean_value <- NA
      max_value <- NA
    }
    
    monthly_means <- c(monthly_means, mean_value)
    monthly_maxs <- c(monthly_maxs, max_value)
  }
  
  # save calculated mean and max in lists
  
  mean_values[[i]] <- monthly_means
  max_values[[i]] <- monthly_maxs
}

# convert lists to dataframes

mean_df <- data.frame(mean_chla = mean_values)
max_df <- data.frame(max_chla = max_values)

# save as csv

write.csv(mean_df, "mean_values.csv", row.names = FALSE)
write.csv(max_df, "max_values.csv", row.names = FALSE)

# convert index column to regular column (otherwise error occurs when the dataframe is transposed)

mean_df_no_index <- rownames_to_column(mean_df, var = "index")
max_df_no_index <- rownames_to_column(max_df, var = "index")

# transpose dataframe (change columns to rows and rows to columns) and delete first column (= former index column)

mean_df_transposed <- t(mean_df_no_index[,-1]) 
max_df_transposed <- t(max_df_no_index[,-1])

# rename columns with month_year

unique_months <- unique(icam_cases$month_year)

colnames(mean_df_transposed) <- as.character(unique_months)
colnames(max_df_transposed) <- as.character(unique_months)

# convert month_year to character in icam_cases

icam_cases$month_year <- as.character(icam_cases$month_year)

# after transposing, mean_df_transposed and max_df_transposed are in matrix format, we convert it to dataframe again

mean_df_final <- as.data.frame(mean_df_transposed)
max_df_final <- as.data.frame(max_df_transposed)

# save mean_df_final and max_df_final as excel

write_xlsx(mean_df_final, "mean_df_final.xlsx")
write_xlsx(max_df_final, "max_df_final.xlsx")

# create new column in icam_cases to store mean_chla

cases_with_chla <- icam_cases %>%
  mutate(mean_chla = sapply(1:nrow(icam_cases), function(i) {
    
    # extract relevant month
    
    month_col <- as.character(icam_cases$month_year[i])
    
    # look for this month in mean_df_final
    
    if (month_col %in% colnames(mean_df_final)) {
      return(mean_df_final[i, month_col])  # get value from relevant row and column
    } else {
      return(NA)  # NA if nothing found
    }
  }))

# also create new column in cases_with_chla to store max_chla

cases_with_chla <- cases_with_chla %>%
  mutate(max_chla = sapply(1:nrow(cases_with_chla), function(i) {
    
    # extract relevant month
    
    month_col <- as.character(cases_with_chla$month_year[i])
    
    # look for this month in max_df_final
    
    if (month_col %in% colnames(max_df_final)) {
      return(max_df_final[i, month_col])  # get value from relevant row and column
    } else {
      return(NA)  # NA if nothing found
    }
  })) 

# save final product

st_write(cases_with_chla, "cases_with_chla.gpkg",append = FALSE)




# 
# madagascar_coastline_buffer_line <- st_transform(madagascar_coastline_buffer_line, 
#                                                              crs = "+proj=longlat +datum=WGS84")
# # Filter chlorophyll points within the buffer
# chla_sf_buffered <- st_crop(chla_sf, madagascar_coastline_buffer_line)

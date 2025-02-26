# date: 01/2025
# name: Kira Hülsdünker
# topic: wealth index new

# load relevant packages

install.packages("terra")
install.packages("raster")
install.packages("writexl")
install.packages("sp")
install.packages("nngeo")
library(writexl)
library(dplyr)
library(sf)
library(raster)
library(tibble)
library(nngeo)

################################################################################

# set working directory to this file's location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen=999)
rm(list=ls())

################################################################################

# load new wealth_index

wealth_index_new <- st_read("/Users/kira/Documents/Masterarbeit/basic_data/data_files/Madagascar_estimated_wealth_index_with_explanatory_variables.shp")

# load cases_with_sst

cases_with_sst <- st_read("/Users/kira/Documents/Masterarbeit/data/cases_with_sst.gpkg")

# change crs of wealth_index_new to EPSG:3857

wealth_index_new <- st_transform(wealth_index_new, st_crs(cases_with_sst))

################################################################################

# select relevant columns

wealth_index_relevant <- wealth_index_new %>%
  dplyr::select(geometry, dist_road, est_iwi, pop5km)

# wealth_index_new containes many point geometries within one healthshed
# we need to match points to healthsheds by choosing the points within one healthshed

points_in_polygons <- st_join(wealth_index_relevant, cases_with_sst, join = st_within)

########################################## weighted ############################

# calculate mean for dist_roads and est_iwi for each polygon (weighted)

weighted_summary_stats <- points_in_polygons %>%
  group_by(clinic_ID) %>%
  
  mutate(population_sum = sum(pop5km, na.rm = TRUE)) %>%
  
  mutate(weight = pop5km / population_sum) %>%
  
  summarise(
    weighted_mean_dist_road = sum(dist_road * weight, na.rm = TRUE),
    weighted_mean_est_iwi = sum(est_iwi * weight, na.rm = TRUE)
  ) %>%
  st_drop_geometry()

# join cases_with_sst with new dataset summary_stats by geometry

cases_with_wealth_index_new <- cases_with_sst %>%
  left_join(weighted_summary_stats, by = c("clinic_ID" = "clinic_ID"))

# overtake values of nearest neighbors for healthsheds that do not have points in them

cases_with_missing_values <- cases_with_wealth_index_new %>%
  filter(is.na(weighted_mean_est_iwi) | is.na(weighted_mean_dist_road))

nearest_neighbors_missing <- st_nearest_feature(cases_with_missing_values)

cases_with_missing_values <- cases_with_missing_values %>%
  mutate(
    weighted_mean_est_iwi = ifelse(is.na(weighted_mean_est_iwi), 
                                   cases_with_wealth_index_new$weighted_mean_est_iwi[nearest_neighbors_missing], 
                                   weighted_mean_est_iwi),
    weighted_mean_dist_road = ifelse(is.na(weighted_mean_dist_road), 
                                     cases_with_wealth_index_new$weighted_mean_dist_road[nearest_neighbors_missing], 
                                     weighted_mean_dist_road)
  )

cases_with_all_weighted <- cases_with_wealth_index_new %>%
  filter(!(is.na(weighted_mean_est_iwi) | is.na(weighted_mean_dist_road))) %>%
  bind_rows(cases_with_missing_values)

# save cases_with_all (includes all new variables tp, t2m, sst and wealth_index_new) as geopackage in documents

st_write(cases_with_all_weighted, "/Users/kira/Documents/Masterarbeit/data/cases_with_all_weighted.gpkg")

########################################## not weighted ########################

# calculate mean and median for dist_roads and est_iwi for each polygon (not weighted)

summary_stats <- points_in_polygons %>%
  group_by(clinic_ID) %>%  
  summarise(
    mean_dist_road = mean(dist_road, na.rm = TRUE),  
    median_dist_road = median(dist_road, na.rm = TRUE),  
    mean_est_iwi = mean(est_iwi, na.rm = TRUE),  
    median_est_iwi = median(est_iwi, na.rm = TRUE)  
  ) %>%
  st_drop_geometry()  

# join cases_with_sst with new dataset summary_stats by geometry

cases_with_wealth_index_new <- cases_with_sst %>%
  left_join(summary_stats, by = c("clinic_ID" = "clinic_ID"))

# overtake values of nearest neighbors for healthsheds that do not have points in them

cases_with_missing_values <- cases_with_wealth_index_new %>%
  filter(is.na(mean_est_iwi) | is.na(median_est_iwi) | is.na(mean_dist_road)| is.na(median_dist_road))

nearest_neighbors_missing <- st_nearest_feature(cases_with_missing_values)

cases_with_missing_values <- cases_with_missing_values %>%
  mutate(
    mean_est_iwi = ifelse(is.na(mean_est_iwi), 
                          cases_with_wealth_index_new$mean_est_iwi[nearest_neighbors_missing], 
                          mean_est_iwi),
    median_est_iwi = ifelse(is.na(median_est_iwi), 
                          cases_with_wealth_index_new$median_est_iwi[nearest_neighbors_missing], 
                          median_est_iwi),
    mean_dist_road = ifelse(is.na(mean_dist_road), 
                            cases_with_wealth_index_new$mean_dist_road[nearest_neighbors_missing], 
                            mean_dist_road),
    median_dist_road = ifelse(is.na(median_dist_road), 
                            cases_with_wealth_index_new$median_dist_road[nearest_neighbors_missing], 
                            median_dist_road)
  )

cases_with_all <- cases_with_wealth_index_new %>%
  filter(!(is.na(mean_est_iwi) | is.na(median_est_iwi) | is.na(mean_dist_road) | is.na(median_dist_road))) %>%
  bind_rows(cases_with_missing_values)

# save cases_with_all (includes all new variables tp, t2m, sst and wealth_index_new) as geopackage in documents

st_write(cases_with_all, "/Users/kira/Documents/Masterarbeit/data/cases_with_all.gpkg")

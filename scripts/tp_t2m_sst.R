# date: 01/2025
# name: Kira Hülsdünker
# topic: satellite data (total precipitation, 2m temperature and sea surface temperature)

# load relevant packages

install.packages("terra")
install.packages("raster")
install.packages("writexl")
install.packages("sp")
library(writexl)
library(dplyr)
library(sf)
library(raster)
library(tibble)

# set working directory to this file's location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen=999)
rm(list=ls())

################################################################################

# downloaded data here: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=download
# total precipitation, 2 meter temperature and sea surface temperature
# downloaded as "monthly averaged reanalysis", globally, netCDF4

################################################################################

# In QGIS: 

# original dataset of each variable has been transformed into EPSG:3857 and safed as tp_global_3857, t2m_global_3857 and sst_global_3857
# then a buffer of 50 km has been created around the coastline
# the raster dataset has been reduced using "clip raster by mask layer" and the 15 km buffer around the coast was used as the mask layer for all three variables

################################################################################

# load satellite data (total precipitation) with stack function (to load all bands)

tp <- stack("/Users/kira/Documents/Masterarbeit/data/tp_50km_selected.tif")

# load satellite data (2 m temperature) with stack function (to load all bands)

t2m <- stack("/Users/kira/Documents/Masterarbeit/data/t2m_50km_selected.tif")

# load satellite data (sea surface temperature) with stack function (to load all bands)

sst <- stack("/Users/kira/Documents/Masterarbeit/data/sst_50km_selected.tif")

# load cases data (cases_with_chla_6000_meters)

cases_with_chla_6000_meters <- st_read("/Users/kira/Documents/Masterarbeit/data/cases_with_chla_6000_meters.gpkg")

# convert raster to dataframe format for all variables

tp_df <- as.data.frame(rasterToPoints(tp))

t2m_df <- as.data.frame(rasterToPoints(t2m))

sst_df <- as.data.frame(rasterToPoints(sst))

# define relevant columns for total precipitation

coords_tp <- tp_df[, 1:2]

tp_columns <- tp_df[, 3:ncol(tp_df)]

unique_months <- unique(cases_with_chla_6000_meters$month_year)

sorted_months <- sort(unique_months)

colnames(tp_columns) <- sorted_months

tp_df <- cbind(coords_tp, tp_columns)

colnames(tp_df)

# define relevant columns for 2 meters temperature

coords_t2m <- t2m_df[, 1:2]

t2m_columns <- t2m_df[, 3:ncol(t2m_df)]

unique_months <- unique(cases_with_chla_6000_meters$month_year)

sorted_months <- sort(unique_months)

colnames(t2m_columns) <- sorted_months

t2m_df <- cbind(coords_t2m, t2m_columns)

colnames(t2m_df)

# define relevant columns for sea surface temperature

coords_sst <- sst_df[, 1:2]

sst_columns <- sst_df[, 3:ncol(sst_df)]

unique_months <- unique(cases_with_chla_6000_meters$month_year)

sorted_months <- sort(unique_months)

colnames(sst_columns) <- sorted_months

sst_df <- cbind(coords_sst, sst_columns)

colnames(sst_df)

# convert new dataframes to sf format and set coordinate reference system again (EPSG:3857) for all three variables

tp_sf <- st_as_sf(tp_df, coords = c("x", "y"), crs = 3857)

t2m_sf <- st_as_sf(t2m_df, coords = c("x", "y"), crs = 3857)

sst_sf <- st_as_sf(sst_df, coords = c("x", "y"), crs = 3857)

# save as shapefiles in documents

st_write(tp_sf, "/Users/kira/Documents/Masterarbeit/data/tp_sf.shp")

st_write(t2m_sf, "/Users/kira/Documents/Masterarbeit/data/t2m_sf.shp")

st_write(sst_sf, "/Users/kira/Documents/Masterarbeit/data/sst_sf.shp")

# match by nearest neighbor (nearest raster pixel to each healthshed-centroid) and by year_month and add new columns tp, t2m and sst to cases_with_chla
# in tp_df/t2m_df/sst_df each column is one month_year, so we have 156 tp/t2m/sst columns, plus 2 columns each with x and y coordinates

# first, match total precipitation (tp) to cases_with_chla_6000_meters

cases_with_tp <- cases_with_chla_6000_meters

nearest_tp_indices <- st_nearest_feature(cases_with_tp, tp_sf)

cases_with_tp$month_year <- as.character(cases_with_tp$month_year)

tp_nearest_values <- vector("numeric", length = nrow(cases_with_tp))

for (i in 1:nrow(cases_with_tp)) {
  col_name <- cases_with_tp$month_year[i]
  if (col_name %in% colnames(tp_sf)) {
    tp_nearest_values[i] <- as.numeric(tp_sf[nearest_tp_indices[i], col_name])
  } else {
    tp_nearest_values[i] <- NA
  }
}

cases_with_tp$tp <- tp_nearest_values

head(cases_with_tp)

# second, match 2 meters temperature (t2m) to cases_with_tp

cases_with_t2m <- cases_with_tp

nearest_t2m_indices <- st_nearest_feature(cases_with_t2m, t2m_sf)

cases_with_t2m$month_year <- as.character(cases_with_t2m$month_year)

t2m_nearest_values <- vector("numeric", length = nrow(cases_with_t2m))

for (i in 1:nrow(cases_with_t2m)) {
  col_name <- cases_with_t2m$month_year[i]
  if (col_name %in% colnames(t2m_sf)) {
    t2m_nearest_values[i] <- as.numeric(t2m_sf[nearest_t2m_indices[i], col_name])
  } else {
    t2m_nearest_values[i] <- NA
  }
}

cases_with_t2m$t2m <- t2m_nearest_values - 273.15 # transfer from Kelvin to Celsius

head(cases_with_t2m)

# third, match sea surface temperature (sst) to cases_with_t2m

cases_with_sst <- cases_with_t2m

nearest_sst_indices <- st_nearest_feature(cases_with_sst, sst_sf)

cases_with_sst$month_year <- as.character(cases_with_sst$month_year)

sst_nearest_values <- vector("numeric", length = nrow(cases_with_sst))

for (i in 1:nrow(cases_with_sst)) {
  col_name <- cases_with_sst$month_year[i]
  if (col_name %in% colnames(sst_sf)) {
    sst_nearest_values[i] <- as.numeric(sst_sf[nearest_sst_indices[i], col_name])
  } else {
    sst_nearest_values[i] <- NA
  }
}

cases_with_sst$sst <- sst_nearest_values - 273.15 # transfer from Kelvin to Celsius

head(cases_with_sst)

# save cases_with_sst (includes all three new variables tp, t2m and sst) as geopackage in documents

st_write(cases_with_sst, "/Users/kira/Documents/Masterarbeit/data/cases_with_sst.gpkg")


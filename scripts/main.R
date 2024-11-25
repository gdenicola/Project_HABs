#Mapping of ICAM events

library(feather)
library(sf)
library(tidyverse)


# library(arrow)
# library(dplyr)
# library(ggplot2)
# library(sf)
# library(zip)
# library(viridis)
# library(networkD3)
# library(tableone)
# library(leaflet)
# library(lubridate)
# library(zoo)

# set working directory to this file's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen=999)
rm(list=ls())

#read diagnosis dictionary
diagnosis_dictionary <- read_csv("./data/diagnosis_dictionary_translated_kevin.csv")


#load healthsheds shapefile of madagascar
healthsheds_2022 <- st_read("./data/healthsheds2022.shx")

# load clinic_spatial_metadata file (includes 4022 clinics, each only once)
clinic_spatial_metadata <- read.csv("./data/clinic_spatial_metadata.csv")

# load clinic_reporting_metadata file (includes 4022 clinics)
clinic_reporting_metadata <- read.csv("./data/clinic_reporting_metadata.csv")

#variables V410-V423 contain ICAM cases (V424-V425 ICAM "referrals")
file5 <- read_feather("./data/file5.feather")

# file16 <- read_feather("./data/file16.feather")
# file16_smaller <- read_feather("./data/file16_smaller.feather")

# file1 <- read_feather("./data/file1.feather")
# file1_smaller <- read_feather("./data/file1_new.feather")


#file29 <- read_feather("./data/file29.feather")
#file39 <- read_feather("./data/file39.feather")


#create smaller dataset for ICAM cases only
ICAM_cases <- file5 %>% select(clinic_ID,cAnnee, cPeriode, fname, V410, V411, V412, V413, V414, V415, V416, V417, V418, V419, V420, V421, V422, V423) 

#ICAM data not availabke before 2015, so remove previous years
ICAM_cases <- ICAM_cases %>%
  filter(cAnnee >= 2015 & cAnnee <= 2022)

#replace -1 values with 0s (missing and not reported are equivalent for our purposes - waves 1,2) 
ICAM_cases <- ICAM_cases %>%
  mutate(across(V410:V423, ~ replace(., . == -1, 0)))

#replace NA values with 0s (missing and not reported are equivalent for our purposes - wave 3) 
ICAM_cases <- ICAM_cases %>%
  mutate(across(V410:V423, ~ replace_na(., 0)))


#create a column with total icam events for all ages and genders
ICAM_cases <- ICAM_cases %>%
  mutate(icam_total = rowSums(select(., V410:V423), na.rm = TRUE))

#combine ICAM cases with healthshed spatial data
combined_data <- ICAM_cases %>%
  inner_join(healthsheds_2022, by = c("clinic_ID" = "fs_uid"))

n_distinct(combined_data$clinic_ID)

#keep only public clinics/hospitals
combined_data <- combined_data %>% filter(fs_type %in% c("CSB1", "CSB2")) 

n_distinct(combined_data$clinic_ID)

# Summarize data by year, month and clinic_ID, summing up the icam_total
summary_by_month_clinic <- combined_data %>%
  group_by(clinic_ID, cAnnee, cPeriode) %>%
  summarise(icam_total_sum = sum(icam_total, na.rm = TRUE))

# Summarize data by clinic_ID, summing up the icam_total across all years
summary_by_clinic <- combined_data %>%
  group_by(clinic_ID) %>%
  summarise(icam_total_sum = sum(icam_total, na.rm = TRUE))

combined_data_allyears <- summary_by_clinic %>%
  inner_join(healthsheds_2022, by = c("clinic_ID" = "fs_uid"))


# Convert combined_data_allyears back to an sf object
if (!inherits(combined_data_allyears, "sf")) {
  combined_data_allyears <- st_as_sf(combined_data_allyears)
}

ggplot(combined_data_allyears) +
  geom_sf(aes(fill = ifelse(icam_total_sum == 0, NA, icam_total_sum)), color = "grey30") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  theme_void() +
  labs(fill = "Total ICAM Events") +
  ggtitle("Total ICAM Events by Clinic")
# use magma, plasma or inferno with direction = -1


#plot eet
ggplot(extremely_summarized_data) + 
  geom_sf(aes(fill = icam_total), color = "grey30") + 
  theme_classic() +
  scale_fill_viridis_c(option = "plasma")  # Optional: to use a color scale for better visualization


#plot healthsheds
ggplot(healthsheds_2022) +
     geom_sf(fill = "#69b3a2", color = "white") +
     theme_void()

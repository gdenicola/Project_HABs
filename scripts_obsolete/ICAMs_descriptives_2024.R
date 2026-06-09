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
diagnosis_dictionary <- read_csv("./data/diagnosis_dictionary_translated.csv")


#load healthsheds shapefile of madagascar
healthsheds_2022 <- st_read("./data/healthsheds2022.shx")

# load clinic_spatial_metadata file (includes 4022 clinics, each only once)
clinic_spatial_metadata <- read.csv("./data/clinic_spatial_metadata.csv")

# load clinic_reporting_metadata file (includes 4022 clinics)
clinic_reporting_metadata <- read.csv("./data/clinic_reporting_metadata.csv")

#variables V410-V423 contain ICAM cases (V424-V425 ICAM "referrals")
file5 <- read_feather("./data/file5.feather")

#create smaller dataset for ICAM cases only
ICAM_cases <- file5 %>% dplyr::select(clinic_ID,cAnnee, cPeriode, fname, V410, V411, V412, V413, V414, V415, V416, V417, V418, V419, V420, V421, V422, V423) 

#ICAM data not available before 2015, so remove previous years
ICAM_cases <- ICAM_cases %>%
  filter(cAnnee >= 2016 & cAnnee <= 2024)

#replace -1 values with 0s (missing and not reported are equivalent for our purposes - waves 1,2) 
ICAM_cases <- ICAM_cases %>%
  mutate(across(V410:V423, ~ replace(., . == -1, 0)))

#replace NA values with 0s (missing and not reported are equivalent for our purposes - wave 3) 
ICAM_cases <- ICAM_cases %>%
  mutate(across(V410:V423, ~ replace_na(., 0)))

#create a column with total icam events for all ages and genders
ICAM_cases <- ICAM_cases %>%
  mutate(icam_total = rowSums(dplyr::select(., V410:V423), na.rm = TRUE))

#combine ICAM cases with healthshed spatial data
combined_data <- ICAM_cases %>%
  inner_join(healthsheds_2022, by = c("clinic_ID" = "fs_uid"))

n_distinct(combined_data$clinic_ID)

#keep only public clinics/hospitals
combined_data <- combined_data %>% filter(fs_type %in% c("CSB1", "CSB2")) 


n_distinct(combined_data$clinic_ID)


#create column ICAM_event, equal to 1 if there was an ICAM event of any size, 
#and 0 otherwise, meaning if ICAM_total = 0 or otherwise
combined_data <- mutate(combined_data, icam_event = ifelse(icam_total > 0, 1, 0))


#create column large_ICAM_event, equal to 1 if there was an ICAM event of size > 3, 
#and 0 otherwise, meaning if ICAM_total = 0 or otherwise
combined_data <- mutate(combined_data, large_icam_event = ifelse(icam_total > 3, 1, 0))


sum(combined_data$large_icam_event)
sum(combined_data$icam_event)
sum(combined_data$icam_total)



# Summarize data by year, month and clinic_ID, summing up the icam_total, icam_event, and large_icam_event
summary_by_month_clinic <- combined_data %>%
  group_by(cAnnee, cPeriode) %>%
  summarise(icam_total_sum = sum(icam_total, na.rm = TRUE),icam_event_sum = sum(icam_event, na.rm = TRUE),icam_large_event_sum = sum(large_icam_event, na.rm = TRUE)) 


# Spaghetti plot with x-axis labels as month names
ggplot(summary_by_month_clinic, aes(x = cPeriode, y = icam_total_sum, group = cAnnee, color = as.factor(cAnnee))) +
  geom_line() +
  scale_x_continuous(
    breaks = 1:12, 
    labels = c("January", "February", "March", "April", "May", "June", 
               "July", "August", "September", "October", "November", "December")
  ) +
  labs(
    title = "Spaghetti Plot of ICAM Totals by Year",
    x = "Month",
    y = "Total ICAM",
    color = "Year"
  ) +
  theme_minimal()


# Spaghetti plot with x-axis labels as month names
ggplot(summary_by_month_clinic, aes(x = cPeriode, y = icam_event_sum, group = cAnnee, color = as.factor(cAnnee))) +
  geom_line() +
  scale_x_continuous(
    breaks = 1:12, 
    labels = c("January", "February", "March", "April", "May", "June", 
               "July", "August", "September", "October", "November", "December")
  ) +
  labs(
    title = "Number of ICAM events by month",
    x = "Month",
    y = "Total ICAM events",
    color = "Year"
  ) +
  theme_minimal()


# Spaghetti plot with x-axis labels as month names
ggplot(summary_by_month_clinic, aes(x = cPeriode, y = icam_large_event_sum, group = cAnnee, color = as.factor(cAnnee))) +
  geom_line() +
  scale_x_continuous(
    breaks = 1:12, 
    labels = c("January", "February", "March", "April", "May", "June", 
               "July", "August", "September", "October", "November", "December")
  ) +
  labs(
    title = "Number of large ICAM events by month",
    x = "Month",
    y = "Total ICAM large events",
    color = "Year"
  ) +
  theme_minimal()




#counting the number of cases
total_by_year <- summary_by_month_clinic %>%
  group_by(cAnnee) %>%
  summarise(total_icam = sum(icam_total_sum, na.rm = TRUE))

# View the result
print(total_by_year)




#counting the number of events
total_events_by_year <- summary_by_month_clinic %>%
  group_by(cAnnee) %>%
  summarise(total_icam = sum(icam_event_sum, na.rm = TRUE))


# View the result
print(total_events_by_year)


#no real trend, this is good for investigating relationship as it's likely that 
#bias didn't change much over the years, so we have constant bias/under-reporting


#calculate median cases by month
median_by_month <- summary_by_month_clinic %>%
  group_by(cPeriode) %>%
  summarise(median_icam_total = median(icam_total_sum, na.rm = TRUE))


# View the result
print(median_by_month)


#calculate mean events by month
mean_events_by_month <- summary_by_month_clinic %>%
  group_by(cPeriode) %>%
  summarise(median_icam_events = mean(icam_event_sum, na.rm = TRUE))


# View the result
print(mean_events_by_month)


# ranges from 4.43 to 7.57 events per month over the whole country on average
# apparently no real seasonality which is a bit weird

#let's check the large events

#calculate mean large events by month
mean_large_events_by_month <- summary_by_month_clinic %>%
  group_by(cPeriode) %>%
  summarise(median_large_icam_events = mean(icam_large_event_sum, na.rm = TRUE))

# View the result
print(mean_large_events_by_month)
# ranges from 0.286 large events in november to 1.57 in october
# over the whole country, on average


# Summarize data by clinic_ID, summing up the icam_total across all years
summary_by_clinic <- combined_data %>%
  group_by(clinic_ID) %>%
  summarise(icam_total_sum = sum(icam_total, na.rm = TRUE),
            icam_event_sum = sum(icam_event, na.rm = TRUE),
            icam_large_event_sum = sum(large_icam_event, na.rm = TRUE))


combined_data_allyears <- summary_by_clinic %>%
  inner_join(healthsheds_2022, by = c("clinic_ID" = "fs_uid"))


# Convert combined_data_allyears back to an sf object
if (!inherits(combined_data_allyears, "sf")) {
  combined_data_allyears <- st_as_sf(combined_data_allyears)
}

#plot of icam totals --> least informative because of large events
ggplot(combined_data_allyears) +
  geom_sf(aes(fill = ifelse(icam_total_sum == 0, NA, icam_total_sum)), color = "grey90") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  theme_void() +
  labs(fill = "Total ICAM cases") +
  ggtitle("Total ICAM cases by Clinic")
# use magma, plasma or inferno with direction = -1

#plot of large (>3) icam events 
ggplot(combined_data_allyears) +
  geom_sf(aes(fill = ifelse(icam_large_event_sum == 0, NA, icam_large_event_sum)), color = "grey90") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  theme_void() +
  labs(fill = "Total large ICAM Events") +
  ggtitle("Total large (>3 cases) ICAM Events by Clinic")
# use magma, plasma or inferno with direction = -1



#plot of icam events --> most informative
ggplot(combined_data_allyears) +
  geom_sf(aes(fill = ifelse(icam_event_sum == 0, NA, icam_event_sum)), color = "grey90") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  theme_void() +
  labs(fill = "Total ICAM Events") +
  ggtitle("Total ICAM Events by Clinic")
# use magma, plasma or inferno with direction = -1



#create and export shapefile with cases and coordinates
export_data <- dplyr::select(combined_data, clinic_ID, cAnnee, cPeriode,
                      icam_total, icam_event, reg_name, dist_name, reg_uid, 
                      dist_uid, large_icam_event, fs_pop, fs_type,
                      geometry)
#export_data <- combined_data
st_write(export_data,"./data/icam_cases_shapefile.gpkg", append = FALSE)




#plot healthsheds
# ggplot(healthsheds_2022) +
#      geom_sf(fill = "#69b3a2", color = "white") +
#      theme_void()

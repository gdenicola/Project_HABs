library(tidyverse)
library(readxl)
library(sf)

# set working directory to this file's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen=999)
rm(list=ls())


jp <- read_excel("./data/[FINAL] Madagascar HABs+Marine Intoxication Lit+Field Review Extraction Sheet.xlsx")

# select relevant columns, rename them, mutate data type and filter for years 2010 - 2022

jp_data <- jp %>%
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
    no_impacted = as.numeric(gsub("[^0-9.]", "", no_impacted)),  
    no_death = as.numeric(gsub("[^0-9.]", "", no_death)),  
    lat_long = as.character(lat_long),  
    start_date = as.Date(start_date),  
    end_date = as.Date(end_date)  
  ) %>%
  filter(start_date >= "2010-01-01" & start_date <= "2022-12-31")


#filter extra for ICAMs analysis, which start in 2015
#jp_data <- jp_data %>%
#  filter(start_date >= "2015-01-01" & start_date <= "2022-12-31")


#load healthsheds shapefile of madagascar
healthsheds_2022 <- st_read("./data/healthsheds2022.shx")


#combine ICAM cases with healthshed spatial data
# combined_data <- jp_data %>%
#   inner_join(healthsheds_2022, by = c("clinic_ID" = "fs_uid"))


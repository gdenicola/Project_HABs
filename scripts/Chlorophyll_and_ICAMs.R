
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



# set working directory to this file's location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen=999)
rm(list=ls())

################################################################################

#load cholorophyll satellite data
chla <- stack("./data/clorophyll_monthly_mean.nc")

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

# define coordinate columns
coords <- chla_df[, 1:2]

#subset relevant clorophyll columns, i.e. years 2016-2022
chlorophyll_columns <- chla_df[ , 223:306]

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
#st_write(chla_sf, "./data/chla_sf.shp",append=FALSE)

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
madagascar_coastline_buffer_line <- st_boundary(st_buffer(coastline_combined, dist = 10000))


# Plot using ggplot
ggplot() +
  # Plot original coastline
  geom_sf(data = coastline, color = "blue") +
  # Plot buffered coastline boundary
  geom_sf(data = madagascar_coastline_buffer_line, color = "red") +
  # Optional: Add a title
  labs(title = "Madagascar Coastline: Original and 10km Buffer Line",
       subtitle = "Blue: Original Coastline, Red: 10km Buffer Boundary") +
  # Improve the theme
  theme_minimal()


# #create buffer polygons (not for plotting, but needed for filtering later)
madagascar_coastline_buffer <- st_buffer(coastline_combined, dist = 10000)


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


#plot of icam totals (with largest grouped to solve outliers)
ggplot(combined_data_allyears) +
  geom_sf(aes(fill = ifelse(icam_total_sum == 0, NA, icam_total_sum)), color = "grey90") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  theme_void() +
  labs(fill = "Total ICAM cases") +
  ggtitle("Total ICAM cases by Clinic")
# use magma, plasma or inferno with direction = -1

#plot of icam events on all healthsheds
ggplot(combined_data_allyears) +
  geom_sf(aes(fill = ifelse(icam_event_sum == 0, NA, icam_event_sum)), color = "grey90") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  theme_void() +
  labs(fill = "Total ICAM Events") +
  ggtitle("Total ICAM Events by Clinic")
# use magma, plasma or inferno with direction = -1

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



# Perform a spatial join to find water pixels within 10 km of each healthshed
# takes about 30 minutes
icam_chla_join <- st_join(
  icam_cases_coastal, 
  chla_coastal, 
  join = st_is_within_distance, 
  dist = 10000 # 10 km radius
)



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

##basic analyses: average comparison

# 1. Prepare the data
analysis_data <- icam_chla_summary_sf %>%
  filter(!is.na(max_chla) & !is.na(icam_event)) %>%
  mutate(icam_event = factor(icam_event))  # Ensure icam_event is a factor

# 2. Calculate averages
avg_chla <- analysis_data %>%
  group_by(icam_event) %>%
  summarise(
    avg_max_chla = mean(max_chla, na.rm = TRUE),
    sd_max_chla = sd(max_chla, na.rm = TRUE),
    n = n()
  )

print(avg_chla)

# 3. Conduct t-test
t_test_result <- t.test(max_chla ~ icam_event, data = analysis_data)

print(t_test_result)

#linear model
linear_model <- gam(icam_event ~ max_chla, data = icam_chla_summary_sf, family = binomial)
summary(linear_model)


gam_model <- gam(icam_event ~ s(max_chla, bs = 'ps', k = 20), data = icam_chla_summary_sf, family = binomial)
summary(gam_model)
plot(gam_model)

###produce a better plot
# Create a new dataset for prediction, handling potential non-finite values
max_chla_min <- min(icam_chla_summary_sf$max_chla[is.finite(icam_chla_summary_sf$max_chla)], na.rm = TRUE)
max_chla_max <- min(25, max(icam_chla_summary_sf$max_chla[is.finite(icam_chla_summary_sf$max_chla)], na.rm = TRUE))

new_data <- data.frame(max_chla = seq(max_chla_min, max_chla_max, length.out = 100))

# Predict using the GAM model
pred <- predict(gam_model, newdata = new_data, se.fit = TRUE, type = "link")

# Calculate confidence intervals
new_data$fit <- pred$fit
new_data$lower <- pred$fit - 1.96 * pred$se.fit
new_data$upper <- pred$fit + 1.96 * pred$se.fit

# Convert to probability scale
new_data$fit_prob <- plogis(new_data$fit)
new_data$lower_prob <- plogis(new_data$lower)
new_data$upper_prob <- plogis(new_data$upper)

# Create the plot
ggplot(new_data, aes(x = max_chla)) +
  geom_ribbon(aes(ymin = lower_prob, ymax = upper_prob), alpha = 0.2) +
  geom_line(aes(y = fit_prob), color = "blue") +
  labs(x = "Max Chlorophyll", y = "Probability of ICAM Event") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 20))  # This limits the x-axis to 25


library(segmented)
logit_model <- glm(icam_event ~ max_chla, data = icam_chla_summary_sf, family = binomial)
segmented_model <- segmented(logit_model, seg.Z = ~max_chla)
summary(segmented_model)

#create month variable that is actually in date format
icam_chla_summary_seasonal <- icam_chla_summary_sf %>%
  mutate(
    month = factor(substr(month_year, 1, 2), levels = sprintf("%02d", 1:12)),
    year = as.numeric(substr(month_year, 4, 7))
  )

# Now, let's join the population data
icam_chla_summary_seasonal <- icam_chla_summary_seasonal %>%
  left_join(landscan_population, by = c("clinic_ID" = "fs_uid", "year" = "year"))


#fit model with month as a dummy variable
seasonal_model <- glm(icam_event ~ max_chla + month, 
                      data = icam_chla_summary_seasonal, family = binomial)
summary(seasonal_model)
# months not significant, probably because seasonality is mostly included in 
# chlorophyll levels (and data sparsity as well)


#model with population as a predictor normally (no offset cause modeling probability)
pop_model <- gam(icam_event ~ s(max_chla, bs = 'ps', k= 30) + landscan_pop + month, 
                 data = icam_chla_summary_seasonal, family = binomial)
summary(pop_model)
plot(pop_model,xlim = c(0,20))


#model the number of cases per month (not just the occurence of an event)
cases_model <- gam(icam_total ~ s(max_chla, bs = 'ps', k= 30) + landscan_pop + month, 
                 data = icam_chla_summary_seasonal, family = 'nb', link = 'log')
summary(cases_model)
plot(cases_model,xlim = c(0,20))

#model the number of cases per month (not just the occurence of an event) 
#with offset(log(pop)) to model the rate of cases
cases_model <- gam(icam_total ~ s(max_chla, bs = 'ps', k= 30) + month + offset(log(landscan_pop)), 
                   data = icam_chla_summary_seasonal, family = 'nb', link = 'log')
summary(cases_model)
plot(cases_model,xlim = c(0,20))

#Create month_num variable
icam_chla_summary_seasonal <- icam_chla_summary_seasonal %>%
  mutate(month_num = as.numeric(factor(month, levels = c("01", "02", "03", "04", "05", "06", 
                                                         "07", "08", "09", "10", "11", "12"))))

#create continuous time variable (from 1 to 84, the total number of months)
icam_chla_summary_seasonal <- icam_chla_summary_seasonal %>%
  mutate(
    time = (cAnnee - min(cAnnee)) * 12 + month_num - min(month_num) + 1
  )


#add smooth time trend for events model
events_model <- gam(icam_event ~ s(max_chla, bs = 'ps', k = 20) + month + s(time, bs = 'ps', k = 20) + landscan_pop, 
                   data = icam_chla_summary_seasonal, family = 'binomial', link = 'logit')
summary(events_model)
plot(events_model,xlim = c(0,20))

#add smooth time trend for cases model
cases_model <- gam(icam_total ~ s(max_chla, bs = 'ps', k = 20) + month + s(time, bs = 'ps', k = 20) + offset(log(landscan_pop)), 
                   data = icam_chla_summary_seasonal, family = 'nb', link = 'log')
summary(cases_model)
plot(cases_model,xlim = c(0,20))


#add district effect and CSB type for events model
events_model <- gam(icam_event ~ s(max_chla, bs = 'ps', k = 20) + month + 
                      reg_name + s(time, bs = 'ps', k = 20) + 
                      landscan_pop + fs_type, 
                    data = icam_chla_summary_seasonal, family = 'binomial', link = 'logit')
summary(events_model)
plot(events_model,xlim = c(0,20))

#add district effect and CSB type for cases model
cases_model <- gam(icam_total ~ s(max_chla, bs = 'ps', k = 20) + month + 
                      reg_name + s(time, bs = 'ps', k = 20) + 
                      offset(log(landscan_pop)) + fs_type, 
                    data = icam_chla_summary_seasonal, family = 'nb', link = 'log')
summary(cases_model)
plot(cases_model,xlim = c(0,20))


#add clinic ID random intercept
icam_chla_summary_seasonal$clinic_ID <- as.factor(icam_chla_summary_seasonal$clinic_ID)
icam_chla_summary_seasonal$reg_uid <- as.factor(icam_chla_summary_seasonal$reg_uid)
icam_chla_summary_seasonal$dist_uid <- as.factor(icam_chla_summary_seasonal$dist_uid)

cases_model_re <- bam(icam_total ~ s(max_chla, bs = 'ps', k = 20) + 
                        s(time, bs = 'ps', k = 20) +
                        s(clinic_ID, bs = "re") +
                        month + offset(log(landscan_pop)),
                      data = icam_chla_summary_seasonal, 
                      family = 'nb',
                      method = "fREML",
                      discrete = TRUE)

summary(cases_model_re)
plot(cases_model_re, select = 1, xlim = c(0,20))  # This plots only the smooth term for max_chla
# 
# #code for plotting estimated random effects
# # Extract the coefficients
# all_coeffs <- coef(cases_model_re)
# 
# # Identify which coefficients correspond to the random intercepts
# random_intercepts <- all_coeffs[grep("clinic_ID", names(all_coeffs))]
# 
# # Create a data frame
# random_effects_df <- data.frame(
#   clinic_ID = names(random_intercepts),
#   effect = random_intercepts
# )
# 
# # Histogram
# ggplot(random_effects_df, aes(x = effect)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#   theme_minimal() +
#   labs(title = "Distribution of Random Intercepts by Clinic",
#        x = "Random Intercept",
#        y = "Count")
# 
# # Density plot
# ggplot(random_effects_df, aes(x = effect)) +
#   geom_density(fill = "skyblue", alpha = 0.7) +
#   theme_minimal() +
#   labs(title = "Density of Random Intercepts by Clinic",
#        x = "Random Intercept",
#        y = "Density")
# 
# # QQ plot
# ggplot(random_effects_df, aes(sample = effect)) +
#   stat_qq() +
#   stat_qq_line() +
#   theme_minimal() +
#   labs(title = "Q-Q Plot of Random Intercepts",
#        x = "Theoretical Quantiles",
#        y = "Sample Quantiles")
# 
# # Box plot
# ggplot(random_effects_df, aes(y = effect)) +
#   geom_boxplot() +
#   theme_minimal() +
#   labs(title = "Box Plot of Random Intercepts",
#        y = "Random Intercept")

# #let's try segmented regression again
# negbin_model <- gam(icam_total ~ max_chla + landscan_pop + month,
#                     data = icam_chla_cases_agg, family = 'nb')
# segmented_model <- segmented(negbin_model, seg.Z = ~max_chla)
# summary(segmented_model)


#let's include random effect for clinic ID

icam_chla_summary_seasonal$clinic_ID <- as.factor(icam_chla_summary_seasonal$clinic_ID)


re_model <- bam(icam_event ~ max_chla + month +
    s(clinic_ID, bs = "re"),
  data = icam_chla_summary_seasonal, 
  family = binomial,
  method = "fREML",
  discrete = TRUE
)

summary(re_model)

# Fit the model with p-splines for max_chla, cyclic p-spline for month, and random effect for clinic_ID
full_spline_model <- bam(
  icam_event ~ 
    s(max_chla, bs = "ps", k = 10) +  # p-spline for max_chla
    s(month_num, bs = "cp", k = 12) +  # cyclic p-spline for month
    s(clinic_ID, bs = "re"),
  data = icam_chla_summary_seasonal, 
  family = binomial,
  method = "fREML",
  discrete = TRUE
)

summary(full_spline_model)


#let's make some chlorophyll descriptives

# Convert month_year to Date format
chla_coastal_long <- chla_coastal_long %>%
  mutate(date = as.Date(paste0(month_year, "-01"), format="%m-%Y-%d"))

# Calculate mean chla by month
chla_monthly_mean <- chla_coastal_long %>%
  group_by(date) %>%
  summarise(mean_chla = mean(chla, na.rm = TRUE))

# Create the time series plot
ggplot(chla_monthly_mean, aes(x = date, y = mean_chla)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Chlorophyll-a Concentration Over Time",
       x = "Date",
       y = "Mean Chlorophyll-a (mg/m³)") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####adding trendline
# Calculate mean chla by month
chla_monthly_mean <- chla_coastal_long %>%
  group_by(date) %>%
  summarise(mean_chla = mean(chla, na.rm = TRUE))

# Create the time series plot with trendline
ggplot(chla_monthly_mean, aes(x = date, y = mean_chla)) +
  geom_line(alpha = 0.7) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "pink") +
  labs(title = "Mean Chlorophyll-a Concentration Over Time",
       subtitle = "With trend line",
       x = "Date",
       y = "Mean Chlorophyll-a (mg/m³)") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###now seasonality plot
# Extract month from the date
chla_coastal_long <- chla_coastal_long %>%
  mutate(month = month(date, label = TRUE, abbr = FALSE))

# Calculate mean chla by month across all years
chla_monthly_seasonal <- chla_coastal_long %>%
  group_by(month) %>%
  summarise(mean_chla = mean(chla, na.rm = TRUE),
            sd_chla = sd(chla, na.rm = TRUE))

# Create the seasonality plot
ggplot(chla_monthly_seasonal, aes(x = month, y = mean_chla, group = 1)) +
  geom_line() +
  geom_point() +
  #geom_errorbar(aes(ymin = mean_chla - sd_chla, ymax = mean_chla + sd_chla), width = 0.2) +
  labs(title = "Seasonal Pattern of Chlorophyll-a Concentration",
       x = "Month",
       y = "Mean Chlorophyll-a (mg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = month.name)

# Assuming we're using the chla_coastal_long dataset
# If not, replace chla_coastal_long with your dataset name
library(moments)  # For skewness and kurtosis

# Calculate summary statistics
chla_summary <- chla_coastal_long %>%
  summarise(
    n = n(),
    mean = mean(chla, na.rm = TRUE),
    median = median(chla, na.rm = TRUE),
    sd = sd(chla, na.rm = TRUE),
    min = min(chla, na.rm = TRUE),
    max = max(chla, na.rm = TRUE),
    q25 = quantile(chla, 0.25, na.rm = TRUE),
    q75 = quantile(chla, 0.75, na.rm = TRUE),
    skewness = skewness(chla, na.rm = TRUE),
    kurtosis = kurtosis(chla, na.rm = TRUE)
  )

print(chla_summary)

# Create a histogram
p1 <- ggplot(chla_coastal_long, aes(x = chla)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Chlorophyll Levels",
       x = "Chlorophyll Level",
       y = "Frequency") +
  theme_minimal()

print(p1)

# If the distribution is highly skewed, we might want to look at a log-transformed version
chla_coastal_long$log_chla <- log(chla_coastal_long$chla + 1)  # +1 to handle zeros

p4 <- ggplot(chla_coastal_long, aes(x = log_chla)) +
  geom_histogram(bins = 50, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Log-Transformed Chlorophyll Levels",
       x = "Log(Chlorophyll Level + 1)",
       y = "Frequency") +
  theme_minimal()

print(p4)

#start with average chla per month overall
# Calculate monthly averages from the original dataset
monthly_avg_chla <- chla_coastal_long %>%
  mutate(month = as.numeric(substr(month_year, 1, 2))) %>%
  group_by(month) %>%
  summarise(
    avg_chla = mean(chla, na.rm = TRUE),
    se_chla = sd(chla, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Create the plot
ggplot(monthly_avg_chla, aes(x = month, y = avg_chla)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = avg_chla - se_chla, ymax = avg_chla + se_chla), 
                width = 0.2, color = "blue", alpha = 0.5) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(title = "Seasonal Pattern of Chlorophyll Levels",
       subtitle = "Based on all coastal pixels",
       x = "Month",
       y = "Average Chlorophyll Level") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# If you want to see the actual values
print(monthly_avg_chla)

####let's filter west coast only
# Define the longitude range for the west coast (adjust as needed)
# Function to convert Web Mercator X to longitude
x_to_lon <- function(x) {
  (x / 20037508.34) * 180
}

# Calculate longitude range
lon_range <- range(x_to_lon(st_coordinates(chla_coastal_long)[,1]))
print(paste("Longitude range:", lon_range[1], "to", lon_range[2]))

# Define the longitude range for the west coast (adjust as needed based on the printed range)
west_lon_min <- 43
west_lon_max <- 45

# Filter the data for west coast pixels
west_coast_chla <- chla_coastal_long %>%
  filter(x_to_lon(st_coordinates(geometry)[,1]) >= west_lon_min & 
           x_to_lon(st_coordinates(geometry)[,1]) <= west_lon_max)

# Calculate monthly averages for west coast
west_coast_monthly_avg <- west_coast_chla %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(avg_chla = mean(chla, na.rm = TRUE),
            se_chla = sd(chla, na.rm = TRUE) / sqrt(n()))

# Print the results to check
print(west_coast_monthly_avg)

# Create the plot (only if we have data)
if (nrow(west_coast_monthly_avg) > 0) {
  ggplot(west_coast_monthly_avg, aes(x = factor(month), y = avg_chla)) +
    geom_line(group = 1, size = 1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = avg_chla - 2*se_chla, ymax = avg_chla + 2*se_chla), width = 0.2) +
    labs(title = "Monthly Average Chlorophyll-a Concentrations (West Coast)",
         x = "Month",
         y = "Chlorophyll-a (mg/m³)") +
    theme_minimal() +
    scale_x_discrete(labels = month.abb) +
    ylim(min(west_coast_monthly_avg$avg_chla) * 0.9, 
         max(west_coast_monthly_avg$avg_chla) * 1.1)

#let's do some ICAM descriptives

# Calculate summary statistics
mfp_summary <- icam_chla_summary_seasonal %>%
  summarise(
    n = n(),
    total_events = sum(icam_event, na.rm = TRUE),
    mean = mean(icam_event, na.rm = TRUE),
    variance = var(icam_event, na.rm = TRUE),
    min = min(icam_event, na.rm = TRUE),
    max = max(icam_event, na.rm = TRUE)
  )

print(mfp_summary)

# Calculate frequency of MFP events
mfp_freq <- icam_chla_summary_seasonal %>%
  group_by(icam_event) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

print(mfp_freq)


# Create a histogram of MFP events per clinic
mfp_per_clinic <- icam_chla_summary_seasonal %>%
  group_by(clinic_ID) %>%
  summarise(total_events = sum(icam_event, na.rm = TRUE))

p2 <- ggplot(mfp_per_clinic, aes(x = total_events)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of MFP Events per Clinic",
       x = "Total MFP Events",
       y = "Number of Clinics") +
  theme_minimal()

print(p2)

table(mfp_per_clinic$total_events)


#now, seasonality of MFPs
# Calculate monthly totals of ICAM events
monthly_icam_counts <- icam_chla_summary_seasonal %>%
  mutate(month = as.numeric(month)) %>%
  group_by(month) %>%
  summarise(
    total_icam = sum(icam_event, na.rm = TRUE),
    n_samples = n()
  ) %>%
  ungroup()

# Create the plot
ggplot(monthly_icam_counts, aes(x = month, y = total_icam)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(breaks = function(x) pretty(x, n = 10)) +
  labs(title = "Seasonal Pattern of MFP Events",
       subtitle = "Total count across all years and healthsheds",
       x = "Month",
       y = "Total MFP Events") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Print the summary statistics
print(monthly_icam_counts)


# Calculate the average max chlorophyll level for each healthshed over the 7-year period
avg_chlorophyll_map <- icam_chla_summary_seasonal %>%
  group_by(clinic_ID) %>%
  summarise(
    avg_chla = mean(max_chla, na.rm = TRUE),
    geometry = first(geom),  # Assuming the geometry doesn't change for each clinic_ID
    .groups = 'drop'
  ) %>%
  st_as_sf()

# Create the map
ggplot() +
  geom_sf(data = avg_chlorophyll_map, aes(fill = avg_chla), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Avg Chlorophyll") +
  theme_minimal() +
  labs(title = "Average Chlorophyll Levels by Healthshed (2016-2022)",
       subtitle = "Coastal Areas of Madagascar") +
  theme(legend.position = "right")

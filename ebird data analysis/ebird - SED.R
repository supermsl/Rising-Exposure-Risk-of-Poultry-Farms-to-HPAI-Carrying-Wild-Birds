if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ebird/ebird-best-practices")
#
rm(list=ls())
library(auk)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(readr)
library(sf)

f_sed <- "ebd_US_mallar3_202201_202312_smp_relSep-2024_sampling.txt"
observations <- read_sampling(f_sed)
glimpse(observations)

# 
counties <- st_read("cb_2022_us_county_500k.shp")

#
coordinates <- observations %>%
  filter(!is.na(longitude), !is.na(latitude),
         longitude >= -180 & longitude <= 180,
         latitude >= -90 & latitude <= 90)

#
points_sf <- st_as_sf(coordinates, coords = c("longitude", "latitude"), crs = 4326)

#
counties <- st_transform(counties, crs = st_crs(points_sf))

#
points_in_counties <- st_join(points_sf, counties)
points_in_counties <- points_in_counties %>%
  filter(!is.na(NAME))

#
state_column <- "STATE_NAME"
county_column <- "NAME"
state_number <- "STATEFP"
county_number <- "COUNTYFP"
date_column <- "observation_date"

#
points_in_counties[[date_column]] <- as.Date(points_in_counties[[date_column]])

#
zf_filtered <- points_in_counties %>%
  filter(protocol_type %in% c("Stationary", "Traveling"),
         duration_minutes  <= 360,
         effort_distance_km <= 10,
         number_observers <= 10,
         all_species_reported == 1)  

#
zf_filtered[[observation_column]] <- as.numeric(zf_filtered[[observation_column]])

# 
data <- st_drop_geometry(zf_filtered)

#
data <- data %>%
  mutate(year = year(.data[[date_column]]))

#
result_2022 <- data %>%
  filter(year == 2022) %>%
  group_by(across(all_of(c(state_number, county_number)))) %>%
  summarise(
    count_2022 = n(),
    .groups = "drop"
  )

result_2023 <- data %>%
  filter(year == 2023) %>%
  group_by(across(all_of(c(state_number, county_number)))) %>%
  summarise(
    count_2023 = n(),
    .groups = "drop"
  )

result_2022_2023 <- data %>%
  filter(year %in% c(2022, 2023)) %>%
  group_by(across(all_of(c(state_number, county_number)))) %>%
  summarise(
    count_2022_2023 = n(),
    .groups = "drop"
  )

#
final_result <- result_2022 %>%
  full_join(result_2023, by = c(state_number, county_number)) %>%
  full_join(result_2022_2023, by = c(state_number, county_number))

print(final_result)

#
write.csv(final_result, 'analysis for 2022, 2023, and 2022-2023.csv', row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)

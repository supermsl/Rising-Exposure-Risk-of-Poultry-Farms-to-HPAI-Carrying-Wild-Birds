library(readr)
library(sf)
library(dplyr) 
rm(list=ls())

counties <- st_read("cb_2022_us_county_500k.shp", crs = 4326)
counties.contiguous <- subset(counties, !counties$STATE_NAME %in% c('Alaska'
                                                                  , "Hawaii"
                                                                  , "Puerto Rico" 
                                                                  , "Commonwealth of the Northern Mariana Islands"
                                                                  , "American Samoa"
                                                                  , "United States Virgin Islands"  
                                                                  , "Guam" 
                                                                  ))
counties.only <- counties.contiguous[c('STATEFP', 'COUNTYFP')]


coordinates <- read_csv("SUM.csv")
library(stringr)
# 
coordinates$STATEFP <- as.numeric(coordinates$STATEFP)
coordinates$COUNTYFP <- as.numeric(coordinates$COUNTYFP)

# 
coordinates$STATEFP <- sprintf("%02d", coordinates$STATEFP)   # 
coordinates$COUNTYFP <- sprintf("%03d", coordinates$COUNTYFP) # 

counties.only$STATEFP <- as.character(counties.only$STATEFP)
counties.only$COUNTYFP <- as.character(counties.only$COUNTYFP)

coordinates$trend <- vector('character', length = nrow(coordinates))
coordinates$trend1 <- vector('character', length = nrow(coordinates))


library(dplyr)
counties.contiguous.trend <- dplyr::left_join(counties.only, coordinates, by = c("STATEFP", "COUNTYFP"))

library(ggplot2)
library(patchwork)
library(scales)
# 
counties.contiguous.trend$WARCP45A2_trans <- sign(counties.contiguous.trend$A1B452033) * sqrt(abs(counties.contiguous.trend$A2452033))
counties.contiguous.trend$WARCP45A1B_trans <- sign(counties.contiguous.trend$A1B452033) * sqrt(abs(counties.contiguous.trend$A1B452033))
counties.contiguous.trend$WARCP45B2_trans <- sign(counties.contiguous.trend$A1B452033) * sqrt(abs(counties.contiguous.trend$B2452033))
counties.contiguous.trend$WARCP45B1_trans <- sign(counties.contiguous.trend$A1B452033) * sqrt(abs(counties.contiguous.trend$B1452033))
counties.contiguous.trend$WARCP85A2_trans <- sign(counties.contiguous.trend$A1B452033) * sqrt(abs(counties.contiguous.trend$A2852033))
counties.contiguous.trend$WARCP85A1B_trans <- sign(counties.contiguous.trend$A1B452033) * sqrt(abs(counties.contiguous.trend$A1B852033))
counties.contiguous.trend$WARCP85B2_trans <- sign(counties.contiguous.trend$A1B452033) * sqrt(abs(counties.contiguous.trend$B2852033))
counties.contiguous.trend$WARCP85B1_trans <- sign(counties.contiguous.trend$A1B452033) * sqrt(abs(counties.contiguous.trend$B1852033))
# 
min_val <- sign(-30) * sqrt(abs(-30)) 
max_val <- sign(15) * sqrt(abs(15)) 

#
map.trend1 <- ggplot() +
  geom_sf(data = counties.contiguous.trend, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = counties.contiguous.trend, aes(fill = WARCP45A2_trans), color = NA) +
  scale_fill_gradientn(colors = c("#2171B5", "#FFFFFF", "#B2182B"),
                       values = scales::rescale(c(min_val, 0, max_val)),
                       name = expression(sign(Delta) * sqrt(abs(Delta))),
                       labels = number_format(accuracy = 0.1)) +
  theme_void() +
  labs(title = "A2rcp 4.5 (2033) - Current (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(5, 5, 5, 5)
  )

#
map.trend2 <- ggplot() +
  geom_sf(data = counties.contiguous.trend, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = counties.contiguous.trend, aes(fill = WARCP45A1B_trans), color = NA) +
  scale_fill_gradientn(colors = c("#2171B5", "#FFFFFF", "#B2182B"),
                       values = scales::rescale(c(min_val, 0, max_val)),
                       name = expression(sign(Delta) * sqrt(abs(Delta))),
                       labels = number_format(accuracy = 0.1)) +
  theme_void() +
  labs(title = "A1Brcp 4.5 (2033) - Current (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(5, 5, 5, 5)
  )


#
map.trend3 <- ggplot() +
  geom_sf(data = counties.contiguous.trend, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = counties.contiguous.trend, aes(fill = WARCP45B2_trans), color = NA) +
  scale_fill_gradientn(colors = c("#2171B5", "#FFFFFF", "#B2182B"),
                       values = scales::rescale(c(min_val, 0, max_val)),
                       name = expression(sign(Delta) * sqrt(abs(Delta))),
                       labels = number_format(accuracy = 0.1)) +
  theme_void() +
  labs(title = "B2rcp 4.5 (2033) - Current (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(5, 5, 5, 5)
  )

#
map.trend4 <- ggplot() +
  geom_sf(data = counties.contiguous.trend, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = counties.contiguous.trend, aes(fill = WARCP45B1_trans), color = NA) +
  scale_fill_gradientn(colors = c("#2171B5", "#FFFFFF", "#B2182B"),
                       values = scales::rescale(c(min_val, 0, max_val)),
                       name = expression(sign(Delta) * sqrt(abs(Delta))),
                       labels = number_format(accuracy = 0.1)) +
  theme_void() +
  labs(title = "B1rcp 4.5 (2033) - Current (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(5, 5, 5, 5)
  )

#
map.trend5 <- ggplot() +
  geom_sf(data = counties.contiguous.trend, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = counties.contiguous.trend, aes(fill = WARCP85A2_trans), color = NA) +
  scale_fill_gradientn(colors = c("#2171B5", "#FFFFFF", "#B2182B"),
                       values = scales::rescale(c(min_val, 0, max_val)),
                       name = expression(sign(Delta) * sqrt(abs(Delta))),
                       labels = number_format(accuracy = 0.1)) +
  theme_void() +
  labs(title = "A2rcp 8.5 (2033) - Current (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(5, 5, 5, 5)
  )

#
map.trend6 <- ggplot() +
  geom_sf(data = counties.contiguous.trend, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = counties.contiguous.trend, aes(fill = WARCP85A1B_trans), color = NA) +
  scale_fill_gradientn(colors = c("#2171B5", "#FFFFFF", "#B2182B"),
                       values = scales::rescale(c(min_val, 0, max_val)),
                       name = expression(sign(Delta) * sqrt(abs(Delta))),
                       labels = number_format(accuracy = 0.1)) +
  theme_void() +
  labs(title = "A1Brcp 8.5 (2033) - Current (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(5, 5, 5, 5)
  )

#
map.trend7 <- ggplot() +
  geom_sf(data = counties.contiguous.trend, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = counties.contiguous.trend, aes(fill = WARCP85B2_trans), color = NA) +
  scale_fill_gradientn(colors = c("#2171B5", "#FFFFFF", "#B2182B"),
                       values = scales::rescale(c(min_val, 0, max_val)),
                       name = expression(sign(Delta) * sqrt(abs(Delta))),
                       labels = number_format(accuracy = 0.1)) +
  theme_void() +
  labs(title = "B1rcp 8.5 (2033) - Current (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(5, 5, 5, 5)
  )

#
map.trend8 <- ggplot() +
  geom_sf(data = counties.contiguous.trend, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = counties.contiguous.trend, aes(fill = WARCP85B1_trans), color = NA) +
  scale_fill_gradientn(colors = c("#2171B5", "#FFFFFF", "#B2182B"),
                       values = scales::rescale(c(min_val, 0, max_val)),
                       name = expression(sign(Delta) * sqrt(abs(Delta))),
                       labels = number_format(accuracy = 0.1)) +
  theme_void() +
  labs(title = "B2rcp 8.5 (2033) - Current (2023)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(5, 5, 5, 5)
  )
#
library(patchwork)
final_plot <- map.trend1 + map.trend2 + map.trend3 + map.trend4 + 
  map.trend5 + map.trend6 + map.trend7 + map.trend8 + 
  plot_layout(ncol = 2, nrow = 4)

final_plot



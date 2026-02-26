#
library(readr)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

#
rm(list = ls())

# 
counties <- st_read("cb_2022_us_county_500k.shp", quiet = TRUE)

# 
counties.contiguous <- subset(counties, !STATE_NAME %in% c(
  'Alaska', 'Hawaii', 'Puerto Rico',
  'Commonwealth of the Northern Mariana Islands',
  'American Samoa', 'United States Virgin Islands', 'Guam'
))

counties.only <- counties.contiguous[c('STATEFP', 'COUNTYFP', 'geometry')]

coordinates <- read_csv("3062farm2033.csv")

#
coordinates$STATEFP <- sprintf("%02d", as.numeric(coordinates$STATEFP))
coordinates$COUNTYFP <- sprintf("%03d", as.numeric(coordinates$COUNTYFP))
counties.only$STATEFP <- as.character(counties.only$STATEFP)
counties.only$COUNTYFP <- as.character(counties.only$COUNTYFP)

#
counties.merged <- left_join(counties.only, coordinates, by = c("STATEFP", "COUNTYFP"))

#
counties_proj <- st_transform(counties.merged, crs = 5070)

# 
centroids_proj <- st_centroid(counties_proj$geometry)

#
coords_mat <- st_coordinates(centroids_proj)
counties_proj$centroid_x <- coords_mat[, 1]
counties_proj$centroid_y <- coords_mat[, 2]

#
all_vars <- c(
  "ER2023", "Any_poultry",
  "A1B45", "A1B85", "A245", "A285", "B145", "B185", "B245", "B285"
)

centers <- data.frame()
for (var in all_vars) {
  df <- counties_proj %>%
    filter(!is.na(.data[[var]]), !is.na(centroid_x), !is.na(centroid_y)) %>%
    mutate(w = .data[[var]])
  
  total_weight <- sum(df$w, na.rm = TRUE)
  lon_weighted <- sum(df$centroid_x * df$w, na.rm = TRUE) / total_weight
  lat_weighted <- sum(df$centroid_y * df$w, na.rm = TRUE) / total_weight
  
  centers <- rbind(centers, data.frame(
    name = var,
    centroid_x = lon_weighted,
    centroid_y = lat_weighted
  ))
}

# 
centers.sf_proj <- st_as_sf(centers, coords = c("centroid_x", "centroid_y"), crs = 5070)

#
centers.sf <- st_transform(centers.sf_proj, crs = 4326)

#
reference <- "Any_poultry"
centers.sf_proj$distance_to_poultry <- NA
ref_point_proj <- centers.sf_proj[centers.sf_proj$name == reference, ]

for (i in seq_len(nrow(centers.sf_proj))) {
  if (centers.sf_proj$name[i] != reference) {
    centers.sf_proj$distance_to_poultry[i] <- as.numeric(st_distance(centers.sf_proj[i, ], ref_point_proj))
  }
}

#
centers.sf$distance_to_poultry <- centers.sf_proj$distance_to_poultry

#
centers.sf$name <- factor(centers.sf$name, levels = all_vars)

centers.sf$shape <- case_when(
  centers.sf$name == reference ~ "Poultry",
  centers.sf$name == "ER2023" ~ "Current",
  grepl("45", centers.sf$name) ~ "RCP 4.5",
  grepl("85", centers.sf$name) ~ "RCP 8.5"
)

centers.sf$color <- case_when(
  grepl("A2",  centers.sf$name) ~ "#56457f",   #
  grepl("A1B", centers.sf$name) ~ "#9182ae",   # 
  grepl("B2",  centers.sf$name) ~ "#b8c6ba",   # 
  grepl("B1",  centers.sf$name) ~ "#dde8f3",   # 
  centers.sf$name == reference ~ "#d73027",    # 
  centers.sf$name == "ER2023"  ~ "#4575b4"     # 
)

shape_map <- c(
  "Poultry" = 21,
  "Current" = 22,
  "RCP 4.5" = 24,
  "RCP 8.5" = 23
)

#
bbox <- st_bbox(centers.sf)
expand_ratio <- 0.15
#
xrange <- bbox$xmax - bbox$xmin
yrange <- bbox$ymax - bbox$ymin

#
xrange_expand <- xrange * (1 + 2 * expand_ratio)
yrange_expand <- yrange * (1 + 2 * expand_ratio)

#
max_range <- max(xrange_expand, yrange_expand)

#
xmid <- (bbox$xmax + bbox$xmin) / 2
ymid <- (bbox$ymax + bbox$ymin) / 2

#
xlim_zoom <- c(xmid - max_range / 2, xmid + max_range / 2)
ylim_zoom <- c(ymid - max_range / 2, ymid + max_range / 2)

#
states.sf <- counties.contiguous %>%
  group_by(STATEFP, STATE_NAME) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  st_transform(crs = 4326)

#
p_main <- ggplot() +
  geom_sf(data = counties.only, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = states.sf, fill = "#feefe4", color = "white", size = 0.4) +
  geom_sf(data = centers.sf, aes(fill = color, shape = shape), size = 4, color = "black", stroke = 0.4) +
  scale_shape_manual(name = "Type", values = shape_map) +
  scale_fill_identity(name = "Scenario", guide = "legend") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right") +
  ggtitle("Full US Map with Weighted Centroids")

print(p_main)

#
p_zoom <- ggplot() +
  geom_sf(data = counties.only %>% st_transform(4326), fill = "#ddecf8", color = "white", size = 0.1) +
  geom_sf(data = centers.sf, aes(fill = color, shape = shape), size = 5, color = "black", stroke = 0.6) +
  geom_text_repel(
    data = centers.sf,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.size = 0.4
  ) +
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom) +
  scale_shape_manual(name = "Type", values = shape_map) +
  scale_fill_identity(name = "Scenario", guide = "legend") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    aspect.ratio = 1 
  ) +
  ggtitle("Zoomed-in Weighted Centroids")

print(p_zoom)

#
centers.sf %>%
  select(name, distance_to_poultry) %>%
  arrange(desc(distance_to_poultry)) %>%
  print()


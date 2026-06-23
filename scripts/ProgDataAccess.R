# Lesson on programmatic data access in R
# https://nceas-learning-hub.github.io/2026_delta_week3/s02_r_programmatic_data_access.html

#install.packages("pak") # package to install packages faster
# making a slight change to test a new branch
# load packages
library(pak)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(sf)
library(dataRetrieval)
library(tigris) #downloads census data
library(httr2)

# Get all stream gauge stations in California
ca_streams <- read_waterdata_monitoring_location(
  state_name = "California",
  site_type = "Stream"
)
# under the hood, this function is requesting: https://api.waterdata.usgs.gov/ogcapi/v0/collections/monitoring-locations/items?f=json&lang=en-US&state_name=California&site_type=Stream&limit=50000

# Build a bounding box for a region of interest, then filter spatially
delta_bbox <- sf::st_bbox(c(xmin = -122.1, ymin = 37.8,
                            xmax = -121.2, ymax = 38.5),
                          crs = sf::st_crs(4326))
delta_sites <- sf::st_filter(ca_streams, sf::st_as_sfc(delta_bbox))

# How many stations?
nrow(delta_sites)

# Preview results (geometry column is retained automatically)
delta_sites |>
  select(monitoring_location_id, monitoring_location_name) |>
  head(10)

# let's look at one example of a gauge location
options(tigris_use_cache = TRUE)

# Station of interest (already an sf object from delta_sites)
freeport <- delta_sites |>
  filter(monitoring_location_id == "USGS-11447650")

# Compute map extent: ±0.35° around the station
lon <- as.numeric(sf::st_coordinates(freeport)[1, "X"])
lat <- as.numeric(sf::st_coordinates(freeport)[1, "Y"])
buf <- 0.35
map_xlim <- c(lon - buf, lon + buf)
map_ylim <- c(lat - buf, lat + buf)

# Find counties intersecting the visual extent
vis_rect <- sf::st_sfc(
  sf::st_polygon(list(matrix(c(
    map_xlim[1], map_ylim[1], map_xlim[2], map_ylim[1],
    map_xlim[2], map_ylim[2], map_xlim[1], map_ylim[2],
    map_xlim[1], map_ylim[1]
  ), ncol = 2, byrow = TRUE))),
  crs = sf::st_crs(4326)
)
all_ca <- counties(state = "CA", year = 2022, progress_bar = FALSE)
area_counties <- sf::st_filter(all_ca,
    sf::st_transform(vis_rect, sf::st_crs(all_ca)))

# Water-body polygons for those counties
area_water_sf <- map(area_counties$NAME,
    ~area_water("CA", county = .x, year = 2022)) |>
  bind_rows()

# Extract Freeport station coordinates for the text label (avoids
# geom_sf_text CRS warning)
site_coords <- as.data.frame(sf::st_coordinates(freeport))

ggplot() +
  geom_sf(data = area_counties, fill = "#f5f0e8",
          color = "gray60", linewidth = 0.3) +
  geom_sf(data = area_water_sf, fill = "#a6cee3",
          color = "#6baed6", linewidth = 0.1) +
  geom_sf(data = freeport, color = "#e31a1c",
          size = 4, shape = 21, fill = "#e31a1c") +
  geom_text(data = site_coords,
            aes(x = X, y = Y, label = "Freeport\n(11447650)"),
            nudge_x = 0.05, hjust = 0, size = 2.8) +
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
  labs(title = "Sacramento River at Freeport (USGS 11447650)",
       x = NULL,
       y = NULL) +
  theme_light() +
  theme(panel.background = element_rect(fill = "#e8f4f8"))

# What data does Freeport have?
freeport_avail <- read_waterdata_combined_meta(
  monitoring_location_id = "USGS-11447650"
)

# Which parameters are available, and for how long?
freeport_avail |>
  select(parameter_code, parameter_name, statistic_id, begin, end) |>
  arrange(parameter_code, statistic_id)

# Using http2 package to make API requests
# this package allows you to make an API request without download a specific package for the data

# Build the same request that read_waterdata_daily() constructs
req <- request("https://api.waterdata.usgs.gov/ogcapi/v0") |>
  req_url_path_append("/collections/daily/items") |>
  req_url_query(
    monitoring_location_id = "USGS-11447650",
    parameter_code = "00010",
    statistic_id = "00003",
    time = "2020-01-01/2020-01-05"  # ISO 8601 interval format
  )

# Preview the request without actually sending it
req_dry_run(req)

# Send the request, returning a response
resp <- req_perform(req)

# Check the HTTP status code
resp_status(resp)  # 200 = success

# Parse the JSON response body
resp_body_json(resp)

# Pull the observation list out of the GeoJSON structure
resp_data <- resp_body_json(resp) |>
  purrr::pluck("features") |>
  purrr::map("properties") |>
  dplyr::bind_rows()

head(resp_data)

# Exercise
# Download this information for multiple sites

sj_river <- request("https://api.waterdata.usgs.gov/ogcapi/v0") |>
  req_url_path_append("/collections/daily/items") |>
  req_url_query(
    monitoring_location_id = "USGS-11303500",
    parameter_code = "00010",
    statistic_id = "00003",
    time = "2020-01-01/2020-01-05"  # ISO 8601 interval format
  )

# Preview the request without actually sending it
req_dry_run(sj_river)

# Send the request, returning a response
sj_river <- req_perform(sj_river)

# Check the HTTP status code
resp_status(sj_river)  # 200 = success

# Parse the JSON response body
resp_body_json(sj_river)

# Pull the observation list out of the GeoJSON structure
sj_river <- resp_body_json(sj_river) |>
  purrr::pluck("features") |>
  purrr::map("properties") |>
  dplyr::bind_rows()

head(sj_river)


cache_slough <- request("https://api.waterdata.usgs.gov/ogcapi/v0") |>
  req_url_path_append("/collections/daily/items") |>
  req_url_query(
    monitoring_location_id = "USGS-11455385",
    parameter_code = "00010",
    statistic_id = "00003",
    time = "2020-01-01/2020-01-05"  # ISO 8601 interval format
  )

# Preview the request without actually sending it
req_dry_run(cache_slough)

# Send the request, returning a response
cache_slough <- req_perform(cache_slough)

# Check the HTTP status code
resp_status(cache_slough)  # 200 = success

# Parse the JSON response body
resp_body_json(cache_slough)

# Pull the observation list out of the GeoJSON structure
cache_slough <- resp_body_json(cache_slough) |>
  purrr::pluck("features") |>
  purrr::map("properties") |>
  dplyr::bind_rows()

head(cache_slough)

sac_river <- request("https://api.waterdata.usgs.gov/ogcapi/v0") |>
  req_url_path_append("/collections/daily/items") |>
  req_url_query(
    monitoring_location_id = "USGS-11455420",
    parameter_code = "00010",
    statistic_id = "00003",
    time = "2020-01-01/2020-01-05"  # ISO 8601 interval format
  )

# Preview the request without actually sending it
req_dry_run(sac_river)

# Send the request, returning a response
sac_river <- req_perform(sac_river)

# Check the HTTP status code
resp_status(sac_river)  # 200 = success

# Parse the JSON response body
resp_body_json(sac_river)

# Pull the observation list out of the GeoJSON structure
sac_river <- resp_body_json(sac_river) |>
  purrr::pluck("features") |>
  purrr::map("properties") |>
  dplyr::bind_rows()

head(sac_river)

all <- rbind(sj_river, cache_slough, sac_river)

# Water quality data from all agencies at a USGS site
wq_sac <- readWQPdata(
  siteid = "USGS-11455420",
  characteristicName = "Turbidity"
)

sj_river %>%
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  facet_wrap(~parameter_code, scales = "free_y", ncol = 1)

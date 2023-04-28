# setup ####
library(here)
library(tidyverse)
library(janitor)
library(zipcodeR) # https://gavinrozzi.github.io/zipcodeR/
library(tigris) # https://github.com/walkerke/tigris
library(tidycensus) # https://github.com/walkerke/tidycensus/
library(sf)
library(scales)
library(cowplot)
library(pins)

# options ####
options(tigris_use_cache = TRUE)

# load pinboard ####
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data ####
# get list of zip codes in coconino county from zipcodeR: https://gavinrozzi.github.io/zipcodeR/
coconino_zip_codes <- search_county(county_name = "Coconino", state_abb = "AZ")

# get spatial zip codes from tigris; package info: https://github.com/walkerke/tigris
coco_zip_codes_spatial <- zctas() %>%
  clean_names()

coco_zip_codes_spatial <- coco_zip_codes_spatial %>%
  filter(zcta5ce20 %in% coconino_zip_codes$zipcode)

# get spatial county boundaries from tigris
coco_county_spatial <- counties(state = "AZ") %>%
  clean_names() %>%
  filter(name == "Coconino")

# get primary roads from tigris
roads_spatial <- primary_secondary_roads(state = "04") %>%
  filter(RTTYP == "I" | RTTYP == "U" | RTTYP == "S" | RTTYP == "C") %>% # https://www.census.gov/library/reference/code-lists/route-type-codes.html
  clean_names()

# read coconino county municipal boundaries: https://data-coconinocounty.opendata.arcgis.com/datasets/488748ccfa7842fc8cde6fc1599a53aa_0
gis_coco_muni_bound <- suicide_data %>%
  pin_read("gis_coconino_municipal_boundaries")

# ensure all coordinate reference systems match
st_crs(coco_zip_codes_spatial)
st_crs(roads_spatial)
st_crs(coco_county_spatial)
st_crs(gis_coco_muni_bound)

# convert municipal boundaries to EPSG:4269
gis_coco_muni_bound <- st_set_crs(gis_coco_muni_bound, value = 4269) %>%
  st_transform(value = 4269)

coco_county_spatial_bbox <- st_as_sfc(st_bbox(coco_county_spatial), crs = "4269")

# spatial join intersection such that
# only roads in coconino county
coco_county_roads_spatial <- st_intersection(
  x = coco_county_spatial_bbox,
  y = roads_spatial
)

# such that only zip codes within coconino county (because there is at least one zip code that exceeds the county boundary)
coco_zip_codes_spatial <- st_intersection(
  x = coco_county_spatial,
  y = coco_zip_codes_spatial
)

# read supervisor districts from pin board
gis_coconino_supervisor_districts <- suicide_data %>%
  pin_read("gis_coconino_supervisor_districts")

# check my work, does the spatial data display correctly?
# view coconino county roads
ggplot() +
  geom_sf(data = coco_county_spatial, fill = "#D9D6D0") +
  geom_sf(data = gis_coconino_supervisor_districts, mapping = aes(fill = NAME), alpha = 1 / 3) +
  geom_sf(data = coco_zip_codes_spatial, fill = "#8CBDB8", alpha = (1 / 6)) +
  geom_sf(data = coco_county_roads_spatial, alpha = 2 / 3, size = 2 / 3, color = "#545234")

# get population denominator for each zip code for most recent available year
# from tidycensus: https://walker-data.com/tidycensus/
zip_code_population <- get_acs(
  geography = "zcta",
  variables = c("total_population" = "S0101_C01_001"),
  year = 2021,
  survey = "acs5"
) %>%
  clean_names() %>%
  mutate(
    year = "2021",
    source = "ACS 5-year subject tables"
  )

# extract zipcode from the name
zip_code_population$zipcode <- str_extract(zip_code_population$name, "[:digit:]{5}")

# create data frame showing population denominator for each coconino county zip code
coco_county_zipcode_pop_denom <- right_join(
  x = zip_code_population,
  y = coconino_zip_codes,
  by = c("zipcode" = "zipcode")
)

# read suicide data from pinboard
pin_name <- read_rds(
  file = "data-tidy/suicide-data-pin-name.rds"
)

# inspect
suicide_data %>%
  pin_meta(pin_name)

# azdhs mortality data
azdhs_suicide_data <- suicide_data %>%
  pin_read(pin_name)

# years of analysis
(analysis_year_range <- c(as.character(unique(azdhs_suicide_data$year_analysis_ll):unique(azdhs_suicide_data$year_analysis_ul))))

# data frame of count of death by suicide for each zip code in Coconino county
(count_zipcode <- azdhs_suicide_data %>%
  filter(d_county_resident == "resident") %>% # county residents only
  filter(death_book_year %in% analysis_year_range) %>% # recent years only
  group_by(residence_zip) %>% #
  count(sort = TRUE) %>%
  ungroup())

# data frame of rate of death by suicide for each zipcode
(rate_zipcode <- left_join(
  x = count_zipcode,
  y = coco_county_zipcode_pop_denom,
  by = c("residence_zip" = "zipcode")
) %>%
  filter(n >= 6) %>% # where there are greater than 6 deaths by suicide in zip code during the analysis time range
  mutate(rate_per_100k = 100000 * (n / estimate)) %>%
  select(residence_zip, n, estimate, population, rate_per_100k) %>%
  drop_na(rate_per_100k))

# spatial join with rates and zip codes
rate_zipcode_spatial <- geo_join(
  spatial_data = coco_zip_codes_spatial,
  data_frame = rate_zipcode,
  by_sp = "zcta5ce20",
  by_df = "residence_zip",
  how = "inner"
)

# plot and check my work
ggplot(data = rate_zipcode_spatial) +
  geom_sf(data = coco_county_spatial, fill = "#D9D6D0") +
  geom_sf(mapping = aes(fill = rate_per_100k)) +
  geom_sf(data = coco_zip_codes_spatial, color = "#B4947C", fill = NA) +
  geom_sf(data = coco_county_roads_spatial, color = "#545234", size = 1.25)

# save to pinboard
suicide_data %>%
  pin_write(
    x = coco_county_spatial,
    name = "gis_coco_county_spatial",
    type = "rds",
    title = "Simple feature of Coconino County boundary",
    description = "Geospatial polygon of Coconino County boundary. Geodetic CRS: NAD83",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("gis_coco_county_spatial")

# save to pinboard
suicide_data %>%
  pin_write(
    x = coco_county_roads_spatial,
    name = "gis_coco_county_roads_spatial",
    type = "rds",
    title = "Simple feature of primary roads within Coconino County",
    description = "Simple feature of primary roads within Coconino County. Geodetic CRS: NAD83",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("gis_coco_county_roads_spatial")

# save to pinboard
suicide_data %>%
  pin_write(
    x = coco_zip_codes_spatial,
    name = "gis_coco_zip_codes_spatial",
    type = "rds",
    title = "Simple feature of zip codes within Coconino County",
    description = "Simple feature of zip codes within Coconino County. Geodetic CRS: NAD83",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("gis_coco_zip_codes_spatial")

# pin details
suicide_subset <- azdhs_suicide_data %>%
  filter(d_county_resident == "resident") %>% # county residents only
  filter(death_book_year %in% analysis_year_range)

pin_title <- str_c(
  "Simple feature of rate by zip code within Coconino County (",
  min(suicide_subset$death_book_year),
  "-",
  max(suicide_subset$death_book_year),
  ")"
)

pin_desc <- str_c(
  "Simple feature of suicide mortality rate by zip code within Coconino County. Zip code population estimates are sourced from US Census ACS are included only for analysis time range, Coconino County resident, and n > 6. Geodetic CRS: NAD83 (",
  min(suicide_subset$death_book_year),
  "-",
  max(suicide_subset$death_book_year),
  ")."
)

# save to pinboard
suicide_data %>%
  pin_write(
    x = rate_zipcode_spatial,
    name = "gis_rate_zipcode_spatial",
    type = "rds",
    title = pin_title,
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("gis_rate_zipcode_spatial")

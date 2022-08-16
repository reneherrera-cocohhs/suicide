# Setup ####
# packages
library(here)
library(tidyverse)
library(pins)
library(tidygeocoder)

# load pinboard
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read from pin board
geo_address <- suicide_data %>%
  pin_read("geo_address_list") %>%
  mutate(id = row_number())

# Takes a dataframe containing addresses as an input and returns the results from a specified geocoding service in a dataframe format
geo_address_list <- geo_address %>%
  geocode(
    street = injury_address,
    city = injaddr_city,
    state = injaddr_state,
    postalcode = injaddr_zip,
    method = "osm",
    lat = latitude,
    long = longitude
  ) %>%
  filter(!is.na(latitude)) # keep only what returns a long & lat result 

# inspect
glimpse(geo_address_list)

# save as pin with new name 
geocoded_injury_location_spatial <- geo_address_list

# save to pin board
suicide_data %>%
  pin_write(geocoded_injury_location_spatial,
    title = "Geocoded list of injury addresses",
    type = "rds",
    description = "Geocoded list of injury addresses.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology"
    )
  )

# Setup ####
# packages
library(here)
library(tidyverse)
library(janitor)
library(pins)

# load pin board 
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

# view pin board from source above
suicide_data %>%
  pin_list()

# read data
cdc_wonder <- read_tsv("data-raw/Underlying Cause of Death, 1999-2019_usa.txt",
  n_max = 209
) %>%
  clean_names()

# filter to only usa & arizona
cdc_wonder <- cdc_wonder %>%
  filter(
    state == "Arizona" | notes == "Total",
    year != "NA"
  ) %>%
  mutate(geography = case_when(
    state == "Arizona" ~ "Arizona",
    notes == "Total" ~ "United States"
  )) %>%
  select(year, geography, deaths, population, crude_rate, age_adjusted_rate)

# a more descriptive name for the data frame
cdc_wonder_age_adj_rates_usa_az <- cdc_wonder

# save to pin board
suicide_data %>% # this creates a new folder 'cdc_wonder_age_adj_rates_usa_az' at the path shown in the pin metadata
  pin_write(cdc_wonder_age_adj_rates_usa_az,
    title = "Age adjusted rate of death by suicide for USA & AZ",
    type = "rds",
    description = "Age adjusted rate of death by suicide for USA & AZ, for 2016-2019, from CDC Wonder."
  )

# view pin meta data
suicide_data %>%
  pin_meta("cdc_wonder_age_adj_rates_usa_az")

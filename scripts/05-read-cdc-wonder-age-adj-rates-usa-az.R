# Introduction ####
# read CDC wonder age adjusted mortality rates for USA & AZ
#
# rené dario herrera
# rherrera at coconino dot az dot gov
# coconino county az
# 10 January 2022

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

# read data from CDC Wonder Query
# https://wonder.cdc.gov/
# see below for the query details
cdc_wonder <- read_tsv("data-raw/Underlying Cause of Death, 1999-2020_usa.txt",
  n_max = 261
) %>%
  clean_names()

# filter to only usa & arizona and tidy
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
    description = "Age adjusted rate of death by suicide for USA & AZ, for 2016-2019, from CDC Wonder.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      url = "https://wonder.cdc.gov/"
    )
  )

# view pin meta data
suicide_data %>%
  pin_meta("cdc_wonder_age_adj_rates_usa_az")

# append year 2021 ####
cdc_wonder_age_adj_rates_usa_az <- suicide_data %>%
  pin_read("cdc_wonder_age_adj_rates_usa_az")

cdc_wonder_age_adj_rates_usa_az %>%
  names()

year_2021 <- tibble(
  year = c(2021, 2021),
  geography = c("Arizona", "United States"),
  deaths = c(1475, 48183),
  population = c(7276316, 331893745),
  crude_rate = c(20.3, 14.5),
  age_adjusted_rate = c(19.5, 14.1)
)

cdc_wonder_age_adj_rates_usa_az <- bind_rows(
  cdc_wonder_age_adj_rates_usa_az,
  year_2021
)

# save to pin board
suicide_data %>% # this creates a new folder 'cdc_wonder_age_adj_rates_usa_az' at the path shown in the pin metadata
  pin_write(cdc_wonder_age_adj_rates_usa_az,
    title = "Age adjusted rate of death by suicide for USA & AZ",
    type = "rds",
    description = "Age adjusted rate of death by suicide for USA & AZ, for 2016-2019, from CDC Wonder.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      url = "https://wonder.cdc.gov/"
    )
  )

# Citation
# Centers for Disease Control and Prevention, National Center for Health Statistics. National Vital Statistics System, Mortality 2018-2021 on CDC WONDER Online Database, released in 2021. Data are from the Multiple Cause of Death Files, 2018-2021, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. Accessed at http://wonder.cdc.gov/ucd-icd10-expanded.html on Feb 10, 2023 7:10:32 PM

# query criteria
# Injury Intent:	Suicide
# Year/Month:	2016; 2017; 2018; 2019; 2020
# Group By:	Year; State
# Show Totals:	True
# Show Zero Values:	False
# Show Suppressed:	False
# Standard Population:	2000 U.S. Std. Population
# Calculate Rates Per:	100,000
# Rate Options:	Default intercensal populations for years 2001-2009 (except Infant Age Groups)

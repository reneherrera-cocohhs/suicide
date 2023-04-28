# Introduction ####
# Age adjustment and standardization
# this scripts takes the standard population file from NIH SEER, imports it, and tidies it for inclusion in later analysis
#
# rené dario herrera
# rherrera at coconino dot az dot gov
# coconino county az
# 10 January 2022

# Setup ####
# packages
library(here)
library(tidyverse)
library(pins)

# load pin board
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

# view pin board
# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# source of data for standard population is: https://seer.cancer.gov/stdpopulations/
# Read data ####
us_std_pop <- read_fwf(
  "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt",
  fwf_cols(
    standard = c(1, 3),
    age = c(4, 6),
    standard_pop = c(7, 14)
  )
)

# inspect data
glimpse(us_std_pop)

# code the data
# Tidy ####
# us standard population
us_std_pop <- us_std_pop %>%
  filter(standard == "204") %>%
  mutate(
    standard = case_when( # recode values
      standard == "204" ~ "2000 U.S. Std Population (18 age groups - Census P25-1130)",
      TRUE ~ as.character(standard)
    ),
    age = case_when( # criteria ~ new value
      age == "000" ~ "0 years",
      age == "001" ~ "0-4 years",
      age == "002" ~ "5-9 years",
      age == "003" ~ "10-14 years",
      age == "004" ~ "15-19 years",
      age == "005" ~ "20-24 years",
      age == "006" ~ "25-29 years",
      age == "007" ~ "30-34 years",
      age == "008" ~ "35-39 years",
      age == "009" ~ "40-44 years",
      age == "010" ~ "45-49 years",
      age == "011" ~ "50-54 years",
      age == "012" ~ "55-59 years",
      age == "013" ~ "60-64 years",
      age == "014" ~ "65-69 years",
      age == "015" ~ "70-74 years",
      age == "016" ~ "75-79 years",
      age == "017" ~ "80-84 years",
      age == "018" ~ "85+ years",
      TRUE ~ as.character(age)
    ),
    standard_pop = as.numeric(standard_pop)
  ) %>%
  rename(age_group = age)

us_std_pop

# Write to pin board ####
# add data to raw data pin board
suicide_data %>% # this creates a new folder 'us_std_pop' at the path shown in the pin metadata
  pin_write(us_std_pop,
    title = "US Standard population",
    type = "rds",
    description = "US Standard Population - 18 age groups. Standard populations, often referred to as standard millions, are the age distributions used as weights to create age-adjusted statistics. https://seer.cancer.gov/stdpopulations/",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      url = "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt"
    )
  )

# view the pin metadata ####
# historical
suicide_data %>%
  pin_meta("us_std_pop")

# Introduction ####
# Read data from the AZDHS Mortality Extract
# rené dario herrera
# rherrera at coconino dot az dot gov
# coconino county az
# 10 January 2022

# Setup ####
# load packages
library(here) # project oriented workflow
library(tidyverse) # data reading, wrangling, and tidying
library(lubridate) # dates
library(haven) # SAS
library(janitor) # clean
library(pins) # data access

# load pinboard 
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

# Read data ####
# read historical data from the shared network drive
####
# WARNING, reading the data will take several minutes (especially true if on a slow connection)
####
death_data_historical <- read_sas(data = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/all_deaths.sas7bdat") %>%
  clean_names()

# read year to date data
death_data_ytd <- read_sas(data = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/coconino2021ytddeaths.sas7bdat") %>%
  clean_names()

# Pins ####
# loaded from source above
# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# write data to the pin board ####
# historical
suicide_data %>% # this creates a new folder 'death_data_historical' at the path shown in the pin metadata
  pin_write(death_data_historical,
    title = "AZDHS mortality extract, historical",
    type = "rds",
    description = "Historical (starting in 2016) all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County."
  )

# YTD
suicide_data %>% # this creates a new folder 'death_data_ytd' at the path shown in the pin metadata
  pin_write(death_data_ytd,
    title = "AZDHS mortality extract, year to date",
    type = "rds",
    description = "Year-to-date all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County."
  )

# view the pin metadata ####
# historical
suicide_data %>%
  pin_meta("death_data_historical")

# YTD
suicide_data %>%
  pin_meta("death_data_ytd")

# Inspect the data ####
glimpse(death_data_historical)
glimpse(death_data_ytd)

# Introduction ####
# Read data from the AZDHS Mortality Extract and save to environment
# rené dario herrera
# rherrera at coconino dot az dot gov
# coconino county az
# 10 January 2022

# Setup ####
# load packages
library(here) # project oriented workflow
library(tidyverse) # data reading, wrangling, and tidying
library(lubridate) # dates
# library(haven) # SAS; import
library(janitor) # clean
library(pins) # data access

# source mortality data
# read mortality data
source("C:/Users/rherrera/projects/mortality-data/scripts/03-read-mortality-data.R", encoding = 'UTF-8')

# tidy
source("C:/Users/rherrera/projects/mortality-data/scripts/04-tidy.R", encoding = 'UTF-8')

# transform
source("C:/Users/rherrera/projects/mortality-data/scripts/05-transform.R", encoding = 'UTF-8')

# load pinboard
mortality_folder <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data")
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

# Read data ####
mortality_folder %>%
  pin_list()

mortality_data <- mortality_folder %>%
  pin_read("mortality-data-tidy-transformed-2010-2023")


# # read historical data from the shared network drive
# ####
# # WARNING, reading the data will take several minutes (especially true if on a slow connection)
# ####
# death_data_historical <- read_sas(data = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/all_deaths.sas7bdat") %>%
#   clean_names()
#
# # read 2021 year to date data
# death_data_ytd_2021 <- read_sas(data = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/coconino2021ytddeaths.sas7bdat") %>%
#   clean_names()
#
# # read 2022 year to date data
# death_data_ytd <- read_sas(data = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/coconino2022ytddeaths.sas7bdat") %>%
#   clean_names()

# Pins ####
# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# # write data to the pin board ####
# # historical
# suicide_data %>% # this creates a new folder 'death_data_historical' at the path shown in the pin metadata
#   pin_write(death_data_historical,
#     title = "AZDHS mortality extract, historical",
#     type = "rds",
#     description = "Historical (starting in 2016) all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County.",
#     metadata = list(
#       owner = "Coconino HHS",
#       department = "Epidemiology",
#       user = "rherrera"
#     )
#   )
#
# # year 2021
# suicide_data %>% # this creates a new folder 'death_data_ytd_2021' at the path shown in the pin metadata
#   pin_write(death_data_ytd_2021,
#             title = "AZDHS mortality extract, year to date, 2021",
#             type = "rds",
#             description = "Year-to-date all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County.",
#             metadata = list(
#               owner = "Coconino HHS",
#               department = "Epidemiology",
#               user = "rherrera"
#             )
#   )
#
# # YTD
# suicide_data %>% # this creates a new folder 'death_data_ytd' at the path shown in the pin metadata
#   pin_write(death_data_ytd,
#     title = "AZDHS mortality extract, year to date",
#     type = "rds",
#     description = "Year-to-date all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County.",
#     metadata = list(
#       owner = "Coconino HHS",
#       department = "Epidemiology",
#       user = "rherrera"
#     )
#   )
#
# # view the pin metadata ####
# # historical
# suicide_data %>%
#   pin_meta("death_data_historical")
#
# suicide_data %>%
#   pin_meta("death_data_ytd_2021")
#
# # YTD
# suicide_data %>%
#   pin_meta("death_data_ytd")
#
# # Inspect the data ####
# glimpse(death_data_historical)
# glimpse(death_data_ytd)

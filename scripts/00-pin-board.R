# run this script once, after project setup it isn't necessary to run it again
# This script sets up the pin board; The pins package helps you publish data sets, models, and other R objects, making it easy to share them across projects and with your colleagues.
# rené dario herrera
# 13 May 2022
# rherrera at coconino dot az dot gov

# Setup ####
# load packages
pacman::p_load(
  here, # project oriented workflow
  pins # data access
)

# library(here) # project oriented workflow
# library(pins) # data access

# Pins ####
# create a pin board ####
# here for now, need to consider where the best place for this should really be
suicide_data <- board_folder("")

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# use pin_meta to view the metadata of each pin ####
# for example, the prepared suicide data
suicide_data %>%
  pin_meta("azdhs_suicide_data_extract")

# the age adjusted rates
suicide_data %>%
  pin_meta("death_by_suicide_coconino_resident_age_adjusted_rate")

# the crude rate by zipcode
suicide_data %>%
  pin_meta("gis_rate_zipcode_spatial")

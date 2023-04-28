# run this script once, after project setup it isn't necessary to run it again
# This script sets up the pin board; The pins package helps you publish data sets, models, and other R objects, making it easy to share them across projects and with your colleagues.
# and loads the color palettes
# rené dario herrera
# 13 May 2022
# rherrera at coconino dot az dot gov

# Setup ####
# load packages
# pacman::p_load(
#   here, # project oriented workflow
#   pins # data access
# )

library(here) # project oriented workflow
library(pins) # data access

# Pins ####
# create a pin board ####
# here for now, need to consider where the best place for this should really be
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

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

# Coconino Color Palette  ####
color_palette_coco_4 <- c(
  "#545234", # Verdigris
  "#D9D6D0", # Timberwolf
  "#8CBDB8", # Shadow Green
  "#B4947C" # Mongoose
)

# save color palette to pin board
suicide_data %>%
  pin_write(
    x = color_palette_coco_4,
    type = "rds",
    title = "Color Palette: Coconino",
    description = "Four value color palette. Verdigris, Timberwolf, Shadow Green, and Mongoose."
  )

# gray color palette
color_palette_gray_7 <- c(
  "#3B3B3B",
  "#525252",
  "#696969",
  "#808080",
  "#979797",
  "#AEAEAE",
  "#C5C5C5"
)

# save color palette to pin board
suicide_data %>%
  pin_write(
    x = color_palette_gray_7,
    type = "rds",
    title = "Color Palette: Gray",
    description = "Seven value color palette. Source: https://www.canva.com/colors/color-meanings/gray/"
  )

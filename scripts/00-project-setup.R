# run this script once, after project setup it isn't necessary to run it again
# This script checks if necessary packages are installed and installs the packages if not 
# rené dario herrera 
# 13 May 2022 
# rherrera at coconino dot az dot gov 

# Setup ####

# List of packages needed #### 
packages_needed_list <- c(
  "here", # https://github.com/r-lib/here
  "tidyverse", # https://github.com/tidyverse/tidyverse
  "pins", # https://github.com/rstudio/pins
  "lubridate", # https://github.com/tidyverse/lubridate
  "haven", # https://github.com/tidyverse/haven
  "janitor", # https://github.com/sfirke/janitor
  "readxl", # https://github.com/tidyverse/readxl
  "curl", # https://github.com/jeroen/curl
  "purrr", # https://github.com/tidyverse/purrr
  "scales", # https://github.com/r-lib/scales
  "tidycensus", # https://github.com/walkerke/tidycensus
  "zipcodeR", # https://github.com/gavinrozzi/zipcodeR/
  "tigris", # https://github.com/walkerke/tigris
  "sf", # https://github.com/r-spatial/sf/
  "cowplot", # https://github.com/wilkelab/cowplot
  "tidygeocoder", # https://jessecambon.github.io/tidygeocoder/index.html
  "slider" # https://davisvaughan.github.io/slider/
)

# function #### source: https://gist.github.com/stevenworthington/3178163
# check to see if packages are installed. Install them if they are not
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] # check to see if packages are installed
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE) # Install them if they are not
  # sapply(pkg, require, character.only = TRUE) # , then load them into the R session.
}

# call function #### 
ipak(packages_needed_list)

# Coconino Color Palette 
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

# run this script once, after project setup it isn't necessary to run it again
# This script checks if necessary packages are installed and installs the packages if not
# it also initiates the environment
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
  "slider", # https://davisvaughan.github.io/slider/
  "renv",
  "styler",
  "tidytext",
  "stopwords"
)

# function #### source: https://gist.github.com/stevenworthington/3178163
# check to see if packages are installed. Install them if they are not
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] # check to see if packages are installed
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  } # Install them if they are not
  # sapply(pkg, require, character.only = TRUE) # , then load them into the R session.
}

# call function ####
ipak(packages_needed_list)

# initiate R environment https://rstudio.github.io/renv/articles/renv.html
renv::init()

# R environment status
renv::status()

# add packages to lockfile
renv::snapshot()

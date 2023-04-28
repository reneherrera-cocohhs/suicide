# Introduction ####
# Download, read, and tidy Population Health and Vital Statistics Population Denominators from AZDHS
# These are the denominators needed to calculate crude and age-adjusted rates and race-specific rates
#
# rené dario herrera
# rherrera at coconino dot az dot gov
# coconino county az
# 10 January 2022

# Setup ####
# packages
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(curl)
library(lubridate)
library(pins)

# load pin board
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

# view pin board from source above
# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# Read data ####

# read 2020 data
# source: https://pub.azdhs.gov/health-stats/menu/info/pop/index.php
# Population of Infants, Children (1-14 Years), Adolescents (15-19 Years),
# Young Adults (20-44 Years), Middle-Aged Adults (45-64 Years),
# and Elderly (65+) by Gender, and County of Residence

# create function to read data
read_azdhs_pop_data <- function(x, y, z) {
  # set url
  url <- x

  # set path to save downloaded file
  path_dest <- "data-raw/"

  # set file name to save downloaded file
  path_file <- y

  # download the file and save to designated path
  curl_download(url, destfile = paste(path_dest, "azdhs_pop_", path_file, sep = ""))

  # read the excel file
  mydata <- read_excel(paste(path_dest, "azdhs_pop_", path_file, sep = ""),
    skip = 4,
    n_max = 48
  ) %>%
    clean_names()

  # tidy the data
  # update columan variable names
  names(mydata) <- c("area", "sex", "<1", "1-14", "15-19", "20-44", "45-64", "65+", "total")

  # make table long and add year variable
  mydata <- mydata %>%
    pivot_longer(
      cols = c(3:9),
      names_to = "age_group",
      values_to = "estimate"
    ) %>%
    mutate(year = as.character(z))

  # add missing area names
  mydata[8:21, 1] <- "Arizona"
  mydata[71:84, 1] <- "Coconino"

  mydata <- mydata %>%
    filter(area == "Arizona" | area == "Coconino")

  mydata
}

# call function to read data for year 2021
azdhs_pop_data_2021 <- read_azdhs_pop_data(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2021/t10a1_21.xlsx",
  y = "2021_t10a1_21.xlsx",
  z = "2021"
)

# call function to read data for year 2020
azdhs_pop_data_2020 <- read_azdhs_pop_data(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2020/t10a1_20.xlsx",
  y = "2020_t10a1_20.xlsx",
  z = "2020"
)

# call function to read data for year 2019
azdhs_pop_data_2019 <- read_azdhs_pop_data(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2019/t10a1_19.xlsx",
  y = "2019_t10a1_19.xlsx",
  z = "2019"
)

# call function to read data for year 2018
azdhs_pop_data_2018 <- read_azdhs_pop_data(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2018/t10a1_18.xlsx",
  y = "2018_t10a1_18.xlsx",
  z = "2018"
)

# call function to read data for year 2017
azdhs_pop_data_2017 <- read_azdhs_pop_data(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2017/t10a1_17.xlsx",
  y = "2017_t10a1_17.xlsx",
  z = "2017"
)

# call function to read data for year 2016
azdhs_pop_data_2016 <- read_azdhs_pop_data(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2016/t10a1_16.xlsx",
  y = "2016_t10a1_16.xlsx",
  z = "2016"
)

# combine all
azdhs_pop_by_life_stage <- bind_rows(
  azdhs_pop_data_2016,
  azdhs_pop_data_2017,
  azdhs_pop_data_2018,
  azdhs_pop_data_2019,
  azdhs_pop_data_2020,
  azdhs_pop_data_2021
)

# save to pinboard ####
suicide_data %>% # this creates a new folder 'death_data_ytd' at the path shown in the pin metadata
  pin_write(azdhs_pop_by_life_stage,
    title = "AZDHS Population Denominators, life stage",
    type = "rds",
    description = "Population health and vital statistics, population denominators by sex and age group for years 2016-2020 for Arizona and Coconino County. Population of Infants, Children (1-14 Years), Adolescents (15-19 Years), Young Adults (20-44 Years), Middle-Aged Adults (45-64 Years), and Elderly (65+) by Gender, and County of Residence"
  )

# view the pin metadata ####
# historical
suicide_data %>%
  pin_meta("azdhs_pop_by_life_stage")

# save combined to disk
write_rds(
  x = azdhs_pop_by_life_stage,
  file = "data-output-tidy-processed/azdhs_population_denominators_by_life_stage.rds"
)

# Population by Five-Year Age Groups, County, Gender, and Race/Ethnicity
# create function
read_azdhs_pop_data_sex_gender <- function(x, y, z) {
  # set url
  url <- x

  # set path to save downloaded file
  path_dest <- "data-raw/"

  # set file name to save downloaded file
  path_file <- y

  # download the file and save to designated path
  curl_download(url, destfile = paste(path_dest, "azdhs_pop_age_county_gender_", path_file, sep = ""))

  # read the excel file
  mydata <- read_excel(paste(path_dest, "azdhs_pop_age_county_gender_", path_file, sep = "")) %>%
    clean_names()

  # tidy the data
  # update columan variable names
  names(mydata) <- c(
    "area", "race_ethnicity", "sex",
    "<1", "1-4", "5-9",
    "10-14", "15-19",
    "20-24", "25-29",
    "30-34", "35-39",
    "40-44", "45-49",
    "50-54", "55-59",
    "60-64", "65-69",
    "70-74", "75-79",
    "80-84", "85+",
    "total"
  )

  # add missing area names
  mydata[5:21, 1] <- "Arizona"
  mydata[63:79, 1] <- "Coconino"

  # add missing race ethnicity categories
  # for arizona
  mydata[5:6, 2] <- "All groups"
  mydata[8:9, 2] <- "White non-Hispanic"
  mydata[11:12, 2] <- "Hispanic or Latino"
  mydata[14:15, 2] <- "Black or African American"
  mydata[17:18, 2] <- "American Indian or Alaska Native"
  mydata[20:21, 2] <- "Asian or Pacific Islander"

  # for coconino
  mydata[62:64, 2] <- "All groups"
  mydata[66:67, 2] <- "White non-Hispanic"
  mydata[69:70, 2] <- "Hispanic or Latino"
  mydata[72:73, 2] <- "Black or African American"
  mydata[75:76, 2] <- "American Indian or Alaska Native"
  mydata[78:79, 2] <- "Asian or Pacific Islander"

  # make table long and add year variable
  mydata <- mydata %>%
    filter(
      area == "Arizona" | area == "Coconino"
    ) %>%
    pivot_longer(
      cols = c(4:23),
      names_to = "age_group",
      values_to = "estimate"
    ) %>%
    mutate(year = as.character(z))

  mydata
}

# call function to read data for year 2021
azdhs_pop_data_sex_gender_2021 <- read_azdhs_pop_data_sex_gender(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2021/t10d3_21.xlsx",
  y = "2021_t10d3_21.xlsx",
  z = "2021"
)

# call function to read data for year 2020
azdhs_pop_data_sex_gender_2020 <- read_azdhs_pop_data_sex_gender(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2020/t10d3_20.xlsx",
  y = "2020_t10d3_20.xlsx",
  z = "2020"
)

# call function to read data for year 2019
azdhs_pop_data_sex_gender_2019 <- read_azdhs_pop_data_sex_gender(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2019/t10d3_19.xlsx",
  y = "2019_t10d3_19.xlsx",
  z = "2019"
)

# call function to read data for year 2018
azdhs_pop_data_sex_gender_2018 <- read_azdhs_pop_data_sex_gender(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2018/t10d3_18.xlsx",
  y = "2018_t10d3_18.xlsx",
  z = "2018"
)

# call function to read data for year 2017
azdhs_pop_data_sex_gender_2017 <- read_azdhs_pop_data_sex_gender(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2017/t10d3_17.xlsx",
  y = "2017_t10d3_17.xlsx",
  z = "2017"
)

# call function to read data for year 2016
azdhs_pop_data_sex_gender_2016 <- read_azdhs_pop_data_sex_gender(
  x = "https://pub.azdhs.gov/health-stats/menu/info/pop/2016/t10d3_16.xlsx",
  y = "2016_t10d3_16.xlsx",
  z = "2016"
)

# combine all
azdhs_pop_data_by_sex_gender <- bind_rows(
  azdhs_pop_data_sex_gender_2016,
  azdhs_pop_data_sex_gender_2017,
  azdhs_pop_data_sex_gender_2018,
  azdhs_pop_data_sex_gender_2019,
  azdhs_pop_data_sex_gender_2020,
  azdhs_pop_data_sex_gender_2021
)

# save to pinboard ####
suicide_data %>% # this creates a new folder 'death_data_ytd' at the path shown in the pin metadata
  pin_write(azdhs_pop_data_by_sex_gender,
    title = "AZDHS Population Denominators, 5-year age groups",
    type = "rds",
    description = "Population health and vital statistics, population denominators by race, sex, and age group for years 2016-2020 for Arizona and Coconino County. Population by Five-Year Age Groups, County, Gender, and Race/Ethnicity.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      url = "https://pub.azdhs.gov/health-stats/menu/"
    )
  )

# view the pin metadata ####
# historical
suicide_data %>%
  pin_meta("azdhs_pop_data_by_sex_gender")

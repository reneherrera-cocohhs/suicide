# Introduction ####
# This script will:
# analyze qualitative and categorical variables
#
# rené dario herrera
# 18 May 2022
# coconino county health and human services
# rherrera at coconino dot az dot gov

# Setup ####
# packages
library(here)
library(tidyverse)
library(pins)
library(janitor)

# load pinboard ####
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
pin_name <- read_rds(
  file = "data-tidy/suicide-data-pin-name.rds"
)

# azdhs mortality data
azdhs_suicide_data_extract <- suicide_data %>%
  pin_read(pin_name)

suicide_data %>%
  pin_meta(pin_name)

# inspect
glimpse(azdhs_suicide_data_extract)

# years of analysis
(analysis_year_range <- c(as.character(unique(azdhs_suicide_data_extract$year_analysis_ll):unique(azdhs_suicide_data_extract$year_analysis_ul))))

# # filter to years of analysis ####
azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  filter(death_book_year %in% analysis_year_range)

# Quantile age groups?
hist(azdhs_suicide_data_extract$calc_age, breaks = 10)

age_quantiles <- quantile(azdhs_suicide_data_extract$calc_age)

# new variable to categorize age groups
azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  mutate(age_group_5_quant_bins = cut(
    calc_age,
    breaks = as.vector(age_quantiles), # alternative breaks = 5
    include.lowest = TRUE
  ))

# view data
azdhs_suicide_data_extract %>%
  tabyl(age_group_5_quant_bins)

# view plot of data
plot(azdhs_suicide_data_extract$age_group_5_quant_bins)

# create a new variable where race is lumped
# show only the 3 most populated race categories;
# lump everything else to other
azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  mutate(race_code_lump = fct_lump(d_race_code, n = 3))

# view data
azdhs_suicide_data_extract %>%
  tabyl(race_code_lump)

# view plot of data
plot(azdhs_suicide_data_extract$race_code_lump)

ggplot(
  data = azdhs_suicide_data_extract,
  mapping = aes(
    x = race_code_lump
  )
) +
  geom_bar() +
  coord_flip()

# view table of data (by sex)
azdhs_suicide_data_extract %>%
  tabyl(
    race_code_lump, d_sex
  )

# manner of death ####
# clean up method of suicide
azdhs_suicide_data_extract %>%
  distinct(cod_a) %>%
  arrange(cod_a) %>%
  as.list()

azdhs_suicide_data_extract %>%
  distinct(cdc_injurydesc) %>%
  arrange(cdc_injurydesc) %>%
  as.list()

# code new values cod_a
azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  mutate(
    method_code_a = case_when(
      str_detect(str_to_lower(cod_a), "toxic") ~ "overdose/poison",
      str_detect(str_to_lower(cod_a), "hanging") ~ "hanging",
      str_detect(str_to_lower(cod_a), "anoxic") ~ "asphyxiation",
      str_detect(str_to_lower(cod_a), "hypoxic") ~ "asphyxiation",
      str_detect(str_to_lower(cod_a), "asphyxia") ~ "asphyxiation",
      str_detect(str_to_lower(cod_a), "poison") ~ "overdose/poison",
      str_detect(str_to_lower(cod_a), "overdose") ~ "overdose/poison",
      str_detect(str_to_lower(cod_a), "gun") ~ "firearm",
      str_detect(str_to_lower(cod_a), "exsanguination") ~ "exsanguination",
      str_detect(str_to_lower(cod_a), "drug") ~ "overdose/poison",
      str_detect(str_to_lower(cod_a), "blunt force") ~ "blunt force injuries",
      str_detect(str_to_lower(cod_a), "stab") ~ "stab wound",
      str_detect(str_to_lower(cod_a), "sharp") ~ "stab wound",
      str_detect(str_to_lower(cod_a), "suffocat") ~ "asphyxiation",
      is.na(cod_a) ~ "unknown",
      cod_a == "" ~ "unknown",
      TRUE ~ as.character(cod_a)
    ),
    method_code_a_lump = fct_lump(method_code_a, n = 4)
  )

# view data
azdhs_suicide_data_extract %>%
  tabyl(method_code_a)

# view data
azdhs_suicide_data_extract %>%
  tabyl(method_code_a_lump)

# view table of data (by sex)
azdhs_suicide_data_extract %>%
  tabyl(
    method_code_a_lump, d_sex
  )

# needs to be updated from 07- transform
# code new values cdc
azdhs_suicide_data_extract %>%
  select(contains("cdc")) %>%
  names()

azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  mutate(
    method_code_cdc_lump = fct_lump(d_method_code_cdc, n = 4)
  )

# view data
azdhs_suicide_data_extract %>%
  tabyl(d_method_code_cdc)

# view data
azdhs_suicide_data_extract %>%
  tabyl(method_code_cdc_lump)

# view table of data (by sex)
azdhs_suicide_data_extract %>%
  tabyl(
    method_code_cdc_lump, d_sex
  )

azdhs_suicide_data_extract %>%
  select(contains("_lat")) %>%
  glimpse()

# # geospatial injury address ####
# geo_var_names <- c(
#   "death_book_year",
#   "injury_add_longitude",
#   "injury_add_lattitude",
#   "injury_address",
#   "injaddr_city",
#   "injaddr_state",
#   "injaddr_zip",
#   "injaddr_county",
#   "method_code_cdc_lump",
#   "race_code_lump",
#   "age_group_5_quant_bins",
#   "county_resident",
#   "sex"
# ) %>%
#   paste0(sep = "|")
#
# # select variables of interest
# geo_address_list <- azdhs_suicide_data_extract %>%
#   filter(death_book_year %in% analysis_year_range) %>%
#   select(all_of(geo_var_names)) %>%
#   drop_na(injury_address, injaddr_city, injaddr_state, injaddr_zip)
#
# # save geospatial data to pin board
# suicide_data %>%
#   pin_write(
#     geo_address_list,
#     type = "rds",
#     title = "list of addresses for geocoding",
#     description = "list of addresses for geocoding with Census batch tool",
#     metadata = list(
#       owner = "Coconino HHS",
#       department = "Epidemiology",
#       user = "rherrera"
#     )
#   )
#
# suicide_data %>%
#   pin_meta("geo_address_list")

# pin details
pin_title <- str_c(
  "AZDHS mortality data extract for suicide plus categorical lumps (",
  min(azdhs_suicide_data_extract$death_book_year),
  "-",
  max(azdhs_suicide_data_extract$death_book_year),
  ")"
)

pin_desc <- str_c(
  "AZDHS mortality data extract for death by suicide plus categorical data lumping (",
  min(azdhs_suicide_data_extract$death_book_year),
  "-",
  max(azdhs_suicide_data_extract$death_book_year),
  ")."
)

# save to pin board ####
suicide_data %>%
  pin_write(
    x = azdhs_suicide_data_extract,
    name = "azdhs_suicide_data_extract_plus_categorical_lumps",
    type = "rds",
    title = pin_title,
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("azdhs_suicide_data_extract_plus_categorical_lumps")

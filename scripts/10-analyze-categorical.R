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
suicide_data <- board_folder("") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
# azdhs mortality data
azdhs_suicide_data_extract <- suicide_data %>%
  pin_read("azdhs_suicide_data_extract")

suicide_data %>%
  pin_meta("azdhs_suicide_data_extract")

# years of analysis
(analysis_year_range <- c(as.character(unique(azdhs_suicide_data_extract$year_analysis_ll):unique(azdhs_suicide_data_extract$year_analysis_ul))))

# # filter to years of analysis ####
# azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
#   filter(death_book_year %in% analysis_year_range)

# Quantile age groups?
hist(azdhs_suicide_data_extract$age_calc, breaks = 5)

age_quantiles <- quantile(azdhs_suicide_data_extract$age_calc)

# new variable to categorize age groups
azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  mutate(age_group_5_quant_bins = cut(
    age_calc,
    breaks = as.vector(age_quantiles), # alternative breaks = 5
    include.lowest = TRUE
  )) 

# view data
azdhs_suicide_data_extract %>%
  count(age_group_5_quant_bins)

# view plot of data
plot(azdhs_suicide_data_extract$age_group_5_quant_bins)

# create a new variable where race is lumped
# show only the 3 most populated race categories;
# lump everything else to other
azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  mutate(race_code_lump = fct_lump(race_code, n = 3))

# view data
azdhs_suicide_data_extract %>%
  count(race_code_lump, sort = TRUE)

# view plot of data
plot(azdhs_suicide_data_extract$race_code_lump)

# view table of data (by sex)
azdhs_suicide_data_extract %>%
  tabyl(
    race_code_lump, sex
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
      str_detect(cod_a, "toxic") ~ "overdose/poison",
      str_detect(cod_a, "hanging") ~ "hanging",
      str_detect(cod_a, "anoxic") ~ "asphyxiation",
      str_detect(cod_a, "hypoxic") ~ "asphyxiation",
      str_detect(cod_a, "asphyxia") ~ "asphyxiation",
      str_detect(cod_a, "poison") ~ "overdose/poison",
      str_detect(cod_a, "overdose") ~ "overdose/poison",
      str_detect(cod_a, "gun") ~ "firearm",
      str_detect(cod_a, "exsanguination") ~ "exsanguination",
      str_detect(cod_a, "drug") ~ "overdose/poison",
      str_detect(cod_a, "blunt force") ~ "blunt force injuries",
      str_detect(cod_a, "stab") ~ "stab wound",
      str_detect(cod_a, "sharp") ~ "stab wound",
      str_detect(cod_a, "suffocat") ~ "asphyxiation",
      is.na(cod_a) ~ "unknown",
      cod_a == "" ~ "unknown",
      TRUE ~ as.character(cod_a)
    ),
    method_code_a_lump = fct_lump(method_code_a, n = 4)
  )

# view data
azdhs_suicide_data_extract %>%
  count(method_code_a, sort = TRUE)

# view data
azdhs_suicide_data_extract %>%
  count(method_code_a_lump, sort = TRUE)

# view table of data (by sex)
azdhs_suicide_data_extract %>%
  tabyl(
    method_code_a_lump, sex
  )

# code new values cdc
azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  mutate(
    method_code_cdc_lump = fct_lump(method_code_cdc, n = 4)
  )

# view data
azdhs_suicide_data_extract %>%
  count(method_code_cdc, sort = TRUE)

# view data
azdhs_suicide_data_extract %>%
  count(method_code_cdc_lump, sort = TRUE)

# view table of data (by sex)
azdhs_suicide_data_extract %>%
  tabyl(
    method_code_cdc_lump, sex
  )

# geospatial injury address #### 
geo_var_names <- c(
  "death_book_year",
  "injury_address",
  "injaddr_city",
  "injaddr_state",
  "injaddr_zip",
  "injaddr_county",
  "method_code_cdc_lump",
  "race_code_lump",
  "age_group_5_quant_bins",
  "county_resident",
  "sex"
)

# select variables of interest 
geo_address_list <- azdhs_suicide_data_extract %>%
  filter(death_book_year %in% analysis_year_range) %>%
  select(all_of(geo_var_names)) %>%
  drop_na(injury_address, injaddr_city, injaddr_state, injaddr_zip)

# save geocoding to pin board 
suicide_data %>%
  pin_write(
    geo_address_list,
    type = "rds",
    title = "list of addresses for geocoding",
    description = "list of addresses for geocoding with Census batch tool",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("geo_address_list")

# rename for pin board
azdhs_suicide_data_extract_plus_categorical_lumps <- azdhs_suicide_data_extract

# save to pin board ####
suicide_data %>%
  pin_write(
    x = azdhs_suicide_data_extract_plus_categorical_lumps,
    type = "rds",
    title = "AZDHS mortality data extract for suicide plus categorical lumps",
    description = "AZDHS mortality data extract for death by suicide plus categorical data lumping.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("azdhs_suicide_data_extract_plus_categorical_lumps")

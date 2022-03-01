# qualitative and categorical variables

# Setup ####
# packages
library(here)
library(tidyverse)
library(pins)

# load pinboard ####
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
# azdhs mortality data
azdhs_mortality_extract <- suicide_data %>%
  pin_read("azdhs_mortality_extract")

suicide_data %>%
  pin_meta("azdhs_mortality_extract")

# years of analysis
(analysis_year_range <- c(as.character(unique(azdhs_mortality_extract$year_analysis_ll):unique(azdhs_mortality_extract$year_analysis_ul))))

# filter to years of analysis ####
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  filter(death_book_year %in% analysis_year_range)

#### 

# Quantile age groups?
hist(azdhs_mortality_extract$age_calc, breaks = 5)

age_quantiles <- quantile(azdhs_mortality_extract$age_calc, na.rm = TRUE)

####

# new variable to categorize age groups
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(age_group_5_quant_bins = cut(
    age_calc,
    breaks = as.vector(age_quantiles), # alternative breaks = 5
    include.lowest = TRUE
  )) 

# view data
azdhs_mortality_extract %>%
  count(age_group_5_quant_bins)

# view plot of data
plot(azdhs_mortality_extract$age_group_5_quant_bins)

# create a new variable where race is lumped
# show only the 3 most populated race categories;
# lump everything else to other
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(race_code_lump = fct_lump(race_code, n = 3))

# view data
azdhs_mortality_extract %>%
  count(race_code_lump, sort = TRUE)

# view plot of data
plot(azdhs_mortality_extract$race_code_lump)

# manner of death ####
# clean up method of suicide
azdhs_mortality_extract %>%
  distinct(cod_a) %>%
  arrange(cod_a) %>%
  as.list()

azdhs_mortality_extract %>%
  distinct(cdc_injurydesc) %>%
  arrange(cdc_injurydesc) %>%
  as.list()

# code new values cod_a
azdhs_mortality_extract <- azdhs_mortality_extract %>%
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
azdhs_mortality_extract %>%
  count(method_code_a, sort = TRUE)

# view table of data (by sex)
ftable(azdhs_mortality_extract$sex ~ azdhs_mortality_extract$method_code_a)

# view data
azdhs_mortality_extract %>%
  count(method_code_a_lump, sort = TRUE)

# view table of data (by sex)
ftable(azdhs_mortality_extract$sex ~ azdhs_mortality_extract$method_code_a_lump)

# code new values cdc
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    method_code_cdc_lump = fct_lump(method_code_cdc, n = 4)
  )

# view data
azdhs_mortality_extract %>%
  count(method_code_cdc, sort = TRUE)

# view table of data (by sex)
ftable(azdhs_mortality_extract$sex ~ azdhs_mortality_extract$method_code_cdc)

# view data
azdhs_mortality_extract %>%
  count(method_code_cdc_lump, sort = TRUE)

# view table of data (by sex)
ftable(azdhs_mortality_extract$sex ~ azdhs_mortality_extract$method_code_cdc_lump)

# code edu to lumps 
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    edu_code_lump = fct_lump(edu_code, n = 4)
  )

# view data
azdhs_mortality_extract %>%
  count(edu_code_lump, sort = TRUE)

# save to pinboard 
suicide_data %>%
  pin_write(
    azdhs_mortality_extract,
    type = "rds",
    title = "AZDHS Mortality Extract, tidy and transformed",
    description = "The tidy and transformed version of the original AZDHS mortality extract."
  )

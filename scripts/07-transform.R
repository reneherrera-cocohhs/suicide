# Introduction #### 
# This script will:
# code new qualitative and categorical variables
# rené dario herrera 
# coconino county health and human services 
# rherrera at coconino dot az dot gov 

# Setup ####
# package libraries 
library(here)
library(tidyverse)
library(lubridate)
library(pins)
library(janitor)

# load pinboard ####
suicide_data <- board_folder("") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
# azdhs mortality data
azdhs_mortality_extract <- suicide_data %>%
  pin_read("azdhs_mortality_extract")

suicide_data %>%
  pin_meta("azdhs_mortality_extract")

# Change geographic variables to character Title Case
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(across(
    .cols = starts_with("birth_"),
    str_to_title
  )) %>%
  mutate(across(
    .cols = starts_with("residence_"),
    str_to_title
  )) %>%
  mutate(
    death_city_name = str_to_title(death_city_name),
    death_county_name = str_to_title(death_county_name),
    death_state_name = str_to_title(death_state_name),
    funeral_home = str_to_title(funeral_home)
  ) %>%
  mutate( # update the date of death "99" values to "01" or else they will appear as NA when age calculates
    date_of_death = str_replace(date_of_death,
                                "99$",
                                "01")
  ) 

# code new variables ####

## age ####
# calculate age for age group
# convert to date
azdhs_mortality_extract$decedent_dob <- ymd(azdhs_mortality_extract$decedent_dob)
azdhs_mortality_extract$date_of_death <- ymd(azdhs_mortality_extract$date_of_death)

# save new variable, age in years
# save calculated variable to data
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(age_calc = time_length(x = difftime(azdhs_mortality_extract$date_of_death, azdhs_mortality_extract$decedent_dob), unit = "years"))

## day, date, month, and year #### 
# extract month of death to plot on a time series and create a new variable
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(date_of_death_month = month(date_of_death))

# extract year of death and create a new variable
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(date_of_death_year = year(date_of_death))

# extract day of death and create a new variable
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(date_of_death_day_week = wday(date_of_death, label = TRUE))

# extract death day of month and create a new variable
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(date_of_death_day_month = mday(date_of_death)) 

## age groups ####
# create levels for age group coding as factor below
age_group_5yr_levels <- c(
  "0-4 years",
  "5-9 years",
  "10-14 years",
  "15-19 years",
  "20-24 years",
  "25-29 years",
  "30-34 years",
  "35-39 years",
  "40-44 years",
  "45-49 years",
  "50-54 years",
  "55-59 years",
  "60-64 years",
  "65-69 years",
  "70-74 years",
  "75-79 years",
  "80-84 years",
  "85+ years"
)

age_group_7cat_levels <- c(
  "Under 5 years",
  "5 to 14 years",
  "15 to 19 years",
  "20 to 44 years",
  "45 to 64 years",
  "65 to 69 years",
  "70 years and over"
)

age_group_6cat_levels <- c(
  "Under 14 years",
  "14 to 17 years",
  "18 to 25 years",
  "26 to 44 years",
  "45 to 64 years",
  "65 years and over"
)

# code age as age group categorical variable (factor)
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    age_group_5yr = factor(case_when( # for age adjustment
      age_calc < 5 ~ "0-4 years",
      age_calc < 10 ~ "5-9 years",
      age_calc < 15 ~ "10-14 years",
      age_calc < 20 ~ "15-19 years",
      age_calc < 25 ~ "20-24 years",
      age_calc < 30 ~ "25-29 years",
      age_calc < 35 ~ "30-34 years",
      age_calc < 40 ~ "35-39 years",
      age_calc < 45 ~ "40-44 years",
      age_calc < 50 ~ "45-49 years",
      age_calc < 55 ~ "50-54 years",
      age_calc < 60 ~ "55-59 years",
      age_calc < 65 ~ "60-64 years",
      age_calc < 70 ~ "65-69 years",
      age_calc < 75 ~ "70-74 years",
      age_calc < 80 ~ "75-79 years",
      age_calc < 85 ~ "80-84 years",
      age_calc >= 85 ~ "85+ years",
      TRUE ~ as.character(age_calc)
    ), levels = age_group_5yr_levels, ordered = TRUE), #
    age_group_7cat = factor(case_when( # for reporting
      age_calc < 5 ~ "Under 5 years",
      age_calc < 15 ~ "5 to 14 years",
      age_calc < 20 ~ "15 to 19 years",
      age_calc < 45 ~ "20 to 44 years",
      age_calc < 65 ~ "45 to 64 years",
      age_calc < 70 ~ "65 to 69 years",
      age_calc >= 70 ~ "70 years and over",
      TRUE ~ as.character(age_calc)
    ), levels = age_group_7cat_levels, ordered = TRUE),
    age_group_6cat = factor(case_when( # for reporting
      age_calc < 14 ~ "Under 14 years",
      age_calc < 18 ~ "14 to 17 years",
      age_calc < 26 ~ "18 to 25 years",
      age_calc < 45 ~ "26 to 44 years",
      age_calc < 65 ~ "45 to 64 years",
      age_calc >= 65 ~ "65 years and over",
      TRUE ~ as.character(age_calc)
    ), levels = age_group_6cat_levels, ordered = TRUE)
  )

# check my work 
glimpse(azdhs_mortality_extract)

# what is listed for age category
# check levels
azdhs_mortality_extract %>%
  distinct(age_group_5yr) %>%
  arrange(age_group_5yr) %>%
  as.list()

azdhs_mortality_extract %>%
  count(age_group_5yr)

plot(x = azdhs_mortality_extract$age_group_5yr)

azdhs_mortality_extract %>%
  distinct(age_group_7cat) %>%
  arrange(age_group_7cat) %>%
  as.list()

azdhs_mortality_extract %>%
  count(age_group_7cat)

plot(x = azdhs_mortality_extract$age_group_7cat)

azdhs_mortality_extract %>%
  distinct(age_group_6cat) %>%
  arrange(age_group_6cat) %>%
  as.list()

azdhs_mortality_extract %>%
  count(age_group_6cat)

plot(x = azdhs_mortality_extract$age_group_6cat)

# inspect to confirm accurate coding 
azdhs_mortality_extract %>%
  select(starts_with("age_")) %>%
  glimpse()

## analysis date range ####
# The suicide report focuses on the most recent 5 years
# create values for new variable to determine the range of which years to include in reports
year_analysis_ul <- if_else(month(Sys.Date()) < 7, (year(Sys.Date())) - 1, year(Sys.Date())) # for analysis purposes, this is the upper limit of date range
year_analysis_current <- year(Sys.Date()) # for analysis purposes, this is the current year
year_analysis_ll <- year_analysis_ul - 4 # for analysis purposes, this is the lower limit of the date range

# add new variables to data 
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    year_analysis_ul = year_analysis_ul,
    year_analysis_current = year_analysis_current,
    year_analysis_ll = year_analysis_ll
  )

## race ####
# read from pin board
us_census_race_groups <- suicide_data %>%
  pin_read("us_census_race_groups")

us_census_race_groups <- as.list(us_census_race_groups$variable)

# explore and list all the race categories
azdhs_mortality_extract %>%
  distinct(race_vsims) %>%
  arrange(race_vsims)

# Commented this out because it isn't needed to tidy the data; I used this to search for variables indicating race and ethnicity

# map(azdhs_mortality_extract, ~ unique(.x[which(str_detect(.x, "hisp"))]))
# map(azdhs_mortality_extract, ~ unique(.x[which(str_detect(.x, "black"))]))
# map(azdhs_mortality_extract, ~ unique(.x[which(str_detect(.x, "ameri"))]))
# map(azdhs_mortality_extract, ~ unique(.x[which(str_detect(.x, "asia"))]))
# map(azdhs_mortality_extract, ~ unique(.x[which(str_detect(.x, "paci"))]))

azdhs_mortality_extract %>%
  filter(if_any(contains("race"), ~ str_detect(.x, "asian"))) %>%
  select(starts_with("race"))

azdhs_mortality_extract %>%
  filter(if_any(contains("race"), ~ str_detect(.x, "hawa"))) %>%
  select(starts_with("race"))

azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(race_code = factor(
    case_when(
      race_vsims == "hispanic or latino" ~ "Hispanic or Latino (any race)",
      race_vsims == "white non-hispanic" ~ "White Non-Hispanic",
      race_vsims == "black or african american" ~ "Black or African American",
      race_vsims == "american indian or alaska native" ~ "American Indian and Alaska Native",
      race_vsims == "asian or pacific islander" ~ "Asian or Native Hawaiian and Other Pacific Islander",
      race_vsims == "unknown/other/refused" ~ "Other",
      is.na(race_vsims) ~ "Other",
      TRUE ~ "Other"
    ),
    exclude = NA,
    levels = c(
      "White Non-Hispanic",
      "American Indian and Alaska Native",
      "Hispanic or Latino (any race)",
      "Black or African American",
      "Asian or Native Hawaiian and Other Pacific Islander",
      "Other"
    )
  ))

# view
glimpse(azdhs_mortality_extract)
levels(azdhs_mortality_extract$race_code)

azdhs_mortality_extract %>%
  distinct(race_code) %>%
  arrange(race_code) %>%
  as.list()

azdhs_mortality_extract %>%
  count(race_code, sort = TRUE)

## sex and gender ####
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(sex = factor(case_when(
    decedent_gender_desc == "male" ~ "Male",
    decedent_gender_desc == "female" ~ "Female",
    is.na(decedent_gender_desc) ~ "Not yet determined",
    TRUE ~ "Not yet determined"
  ), levels = c("Female", "Male", "Not yet determined")))

azdhs_mortality_extract %>%
  count(sex, sort = TRUE)

## residence county ####
# view the county names
azdhs_mortality_extract %>%
  distinct(residence_county_name) %>%
  arrange(residence_county_name)

azdhs_mortality_extract %>%
  count(residence_county_name, sort = TRUE) %>%
  slice_max(order_by = n, n = 10) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(residence_county_name, n), y = n)) +
  coord_flip()

# add a new variable to show if the decedent is a coconino county resident
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(county_resident = if_else(
    str_detect(azdhs_mortality_extract$residence_county_name, "Coconino"),
    "Resident",
    "Non-Resident"
  ))

# inspect
azdhs_mortality_extract %>%
  count(county_resident)

## tribal member ####
# check what possible values are
unique(azdhs_mortality_extract$residence_on_reservation)
unique(azdhs_mortality_extract$resident_address_system_code_de1)

azdhs_mortality_extract %>%
  filter(residence_on_reservation == 1) %>%
  distinct(resident_address_system_code_de1)

# recode
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(tribal_code = case_when(
    resident_address_system_code_de1 == NA ~ "no",
    resident_address_system_code_de1 == "" ~ "no",
    resident_address_system_code_de1 == "no" ~ "no",
    resident_address_system_code_de1 == "unknown" ~ "no",
    TRUE ~ "yes"
  ))

azdhs_mortality_extract %>%
  count(tribal_code, sort = TRUE)

## education ####
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    edu_code = factor
    (case_when(
      str_detect(education_desc, "8th grade or less") ~ "8th grade or less",
      str_detect(education_desc, "9th") ~ "9th-12th grade; no diploma",
      str_detect(education_desc, "associate degree") ~ "Associate degree",
      str_detect(education_desc, "bachelor's degree") ~ "Bachelor's degree",
      str_detect(education_desc, "doctorate") ~ "Doctorate or professional degree",
      str_detect(education_desc, "high school graduate") ~ "High school graduate or GED",
      str_detect(education_desc, "master's degree") ~ "Master's degree",
      str_detect(education_desc, "not classifiable") ~ "Unknown",
      str_detect(education_desc, "some college credit") ~ "Some college, but no degree",
      str_detect(education_desc, "unknown") ~ "Unknown",
      is.na(education_desc) ~ "Unknown",
      TRUE ~ "Unknown"
    )),
    hs_grad = factor
    (case_when(
      str_detect(education_desc, "8th grade or less") ~ "No",
      str_detect(education_desc, "9th") ~ "No",
      str_detect(education_desc, "associate degree") ~ "Yes",
      str_detect(education_desc, "bachelor's degree") ~ "Yes",
      str_detect(education_desc, "doctorate") ~ "Yes",
      str_detect(education_desc, "high school graduate") ~ "Yes",
      str_detect(education_desc, "master's degree") ~ "Yes",
      str_detect(education_desc, "not classifiable") ~ "Unknown",
      str_detect(education_desc, "some college credit") ~ "Yes",
      str_detect(education_desc, "unknown") ~ "Unknown",
      is.na(education_desc) ~ "Unknown",
      TRUE ~ "Unknown"
    )),
    college_grad = factor(
      case_when(
        str_detect(education_desc, "8th grade or less") ~ "8th grade or less",
        str_detect(education_desc, "9th") ~ "9th-12th grade; no diploma",
        str_detect(education_desc, "associate degree") ~ "Associate degree",
        str_detect(education_desc, "bachelor's degree") ~ "yes",
        str_detect(education_desc, "doctorate") ~ "yes",
        str_detect(education_desc, "high school graduate") ~ "High school graduate or GED",
        str_detect(education_desc, "master's degree") ~ "yes",
        str_detect(education_desc, "not classifiable") ~ "Unknown",
        str_detect(education_desc, "some college credit") ~ "Some college, but no degree",
        str_detect(education_desc, "unknown") ~ "Unknown",
        is.na(education_desc) ~ "Unknown",
        TRUE ~ "Unknown"
      )
    )
  )

azdhs_mortality_extract %>%
  count(edu_code, sort = TRUE)

azdhs_mortality_extract %>%
  count(hs_grad, sort = TRUE)

azdhs_mortality_extract %>%
  count(college_grad, sort = TRUE)

## recode marital description ####
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(marital_code = factor
         (case_when(
           str_detect(marital_desc, "divorced") ~ "Divorced",
           str_detect(marital_desc, "married but") ~ "Married, but separated",
           str_detect(marital_desc, "never married") ~ "Never married, single",
           str_detect(marital_desc, "married") ~ "Married",
           str_detect(marital_desc, "widowed") ~ "Widowed",
           str_detect(marital_desc, "unknown") ~ "Unknown",
           is.na(marital_desc) ~ "Unknown",
           TRUE ~ "Unknown"
         )))

azdhs_mortality_extract %>%
  count(marital_code, sort = TRUE)

## recode occupation ####
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(occupation_code = factor
         (str_to_lower(case_when(
           str_detect(occupation_description, "college student") ~ "Student, College",
           str_detect(occupation_description, "student") ~ "Student",
           str_detect(occupation_description, "labor") ~ "Laborer",
           is.na(occupation_description) ~ "Unknown",
           TRUE ~ as.character(occupation_description)
         ))))

azdhs_mortality_extract %>%
  count(occupation_code, sort = TRUE)

## recode industry ####
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(industry_code = factor
         (str_to_lower(case_when(
           str_detect(industry_description, "education") ~ "Education",
           str_detect(industry_description, "high school") ~ "Education",
           str_detect(industry_description, "university") ~ "Education",
           str_detect(industry_description, "college") ~ "Education",
           str_detect(industry_description, "health") ~ "Health Care",
           str_detect(industry_description, "medical") ~ "Health Care",
           str_detect(industry_description, "hospital") ~ "Health Care",
           str_detect(industry_description, "dental") ~ "Health Care",
           str_detect(industry_description, "restaurant") ~ "Hospitality",
           str_detect(industry_description, "auto") ~ "Automotive",
           is.na(industry_description) ~ "Unknown",
           TRUE ~ as.character(industry_description)
         ))))

azdhs_mortality_extract %>%
  count(industry_code, sort = TRUE)

# residence_zip
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    residence_zip = case_when(
      is.na(residence_zip) ~ "unknown",
      TRUE ~ residence_zip
    )
  )

azdhs_mortality_extract %>%
  count(residence_zip, sort = TRUE)

## recode funeral home ####
azdhs_mortality_extract <- azdhs_mortality_extract %>% # rename for pin board
  mutate(funeral_code = factor
         (case_when(
           str_detect(funeral_home, "Norvel Owens") ~ "Norven Owens Mortuary of Flagstaff",
           str_detect(funeral_home, "Affordable Burial") ~ "Affordable Burial and Cremation LLC",
           funeral_home == "" ~ "Unknown",
           is.na(funeral_home) ~ "Unknown",
           TRUE ~ as.character(funeral_home)
         )))

azdhs_mortality_extract %>%
  count(funeral_code, sort = TRUE)

## prisoner or inmate? ####
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    cdc_placeofinjury_text = factor(case_when(
      cdc_placeofinjury == "a" ~ "Home",
      cdc_placeofinjury == "b" ~ "Farm",
      cdc_placeofinjury == "c" ~ "Residential institution",
      cdc_placeofinjury == "f" ~ "Unknown",
      cdc_placeofinjury == "g" ~ "Industrial and construction",
      cdc_placeofinjury == "i" ~ "Trade and service area",
      cdc_placeofinjury == "k" ~ "Street/highway",
      cdc_placeofinjury == "n" ~ "Sports and athletics area",
      cdc_placeofinjury == "p" ~ "School, other institution, public administrative area",
      cdc_placeofinjury == "q" ~ "Unknown",
      cdc_placeofinjury == "" ~ "Unknown",
      is.na(cdc_placeofinjury) ~ "Unknown"
    ))
  )

azdhs_mortality_extract %>%
  count(cdc_placeofinjury_text, sort = TRUE)

azdhs_mortality_extract %>%
  count(cdc_placeofinjury) %>%
  arrange(cdc_placeofinjury)

azdhs_mortality_extract %>%
  count(cdc_injuryplace_desc) %>%
  arrange(cdc_injuryplace_desc)

azdhs_mortality_extract %>%
  count(death_status_desc) %>%
  arrange(death_status_desc)

azdhs_mortality_extract %>%
  count(death_place_type_other) %>%
  arrange(death_place_type_other)

str_detect(azdhs_mortality_extract$death_place_type_other, "prison")
str_detect(azdhs_mortality_extract$death_place_type_other, "jail")

azdhs_mortality_extract %>%
  count(place_of_injury_other) %>%
  arrange(place_of_injury_other)

str_detect(azdhs_mortality_extract$place_of_injury_other, "prison")
str_detect(azdhs_mortality_extract$place_of_injury_other, "jail")
str_detect(azdhs_mortality_extract$place_of_injury_other, "correction")
str_detect(azdhs_mortality_extract$place_of_injury_other, "institution")

## Manner of death, injury description ####
# code new values cdc
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    method_code_cdc = case_when(
      str_detect(cdc_injurydesc, "poison") ~ "overdose/poison",
      str_detect(cdc_injurydesc, "hang") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "hung") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "ligature") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "around neck") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "around his neck") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "around her neck") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "around their neck") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "firearm") ~ "firearm",
      str_detect(cdc_injurydesc, "shot self") ~ "firearm",
      str_detect(cdc_injurydesc, "shot him") ~ "firearm",
      str_detect(cdc_injurydesc, "shot her") ~ "firearm",
      str_detect(cdc_injurydesc, "shot them") ~ "firearm",
      str_detect(cdc_injurydesc, "overdose") ~ "overdose/poison",
      str_detect(cdc_injurydesc, "jump") ~ "jump/fall",
      str_detect(cdc_injurydesc, "fall") ~ "jump/fall",
      str_detect(cdc_injurydesc, "fell") ~ "jump/fall",
      str_detect(cdc_injurydesc, "cliff") ~ "jump/fall",
      str_detect(cdc_injurydesc, "ingest") ~ "overdose/poison",
      str_detect(cdc_injurydesc, "gun") ~ "firearm",
      str_detect(cdc_injurydesc, "cut") ~ "cut/stabbed self",
      str_detect(cdc_injurydesc, "incision") ~ "cut/stabbed self",
      str_detect(cdc_injurydesc, "drug") ~ "overdose/poison",
      str_detect(cdc_injurydesc, "excess of") ~ "overdose/poison",
      str_detect(cdc_injurydesc, "blunt force") ~ "blunt force injuries",
      str_detect(cdc_injurydesc, "stab") ~ "cut/stabbed self",
      str_detect(cdc_injurydesc, "sharp") ~ "cut/stabbed self",
      str_detect(cdc_injurydesc, "wound") ~ "cut/stabbed self",
      str_detect(cdc_injurydesc, "suffocat") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "asphyxiation") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "plastic bag") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "gas ") ~ "hanged self/asphyxiation",
      str_detect(cdc_injurydesc, "train") ~ "train/vehicle",
      str_detect(cdc_injurydesc, "vehicle") ~ "train/vehicle",
      str_detect(cdc_injurydesc, "car") ~ "train/vehicle",
      str_detect(cdc_injurydesc, "truck") ~ "train/vehicle",
      str_detect(cdc_injurydesc, "fire") ~ "fire",
      is.na(cdc_injurydesc) ~ "unknown",
      cdc_injurydesc == "" ~ "unknown",
      TRUE ~ as.character(cdc_injurydesc)
    )) 

## manner of death ####
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

# Quantile age groups?
hist(azdhs_mortality_extract$age_calc, breaks = 5)

(age_quantiles <- quantile(azdhs_mortality_extract$age_calc, na.rm = TRUE))

# new variable to categorize age groups
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  drop_na(age_calc) %>%
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

# view table of data (by sex)
azdhs_mortality_extract %>%
  tabyl(
    race_code_lump, sex
  )

# view data
azdhs_mortality_extract %>%
  count(method_code_a_lump, sort = TRUE)

# view table of data (by sex)
# view table of data (by sex)
azdhs_mortality_extract %>%
  tabyl(
    method_code_a_lump, sex
  )

# code new values cdc
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    method_code_cdc_lump = fct_lump(method_code_cdc, n = 4)
  )

# view data
azdhs_mortality_extract %>%
  count(method_code_cdc, sort = TRUE)

# view data
azdhs_mortality_extract %>%
  count(method_code_cdc_lump, sort = TRUE)

# view table of data (by sex)
azdhs_mortality_extract %>%
  tabyl(
    method_code_cdc_lump, sex
  )

# code edu to lumps 
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    edu_code_lump = fct_lump(edu_code, n = 4)
  )

# view data
azdhs_mortality_extract %>%
  count(edu_code_lump, sort = TRUE)

# view table of data (by sex)
azdhs_mortality_extract %>%
  tabyl(
    edu_code_lump, sex
  )

glimpse(azdhs_mortality_extract)

# save to pinboard 
suicide_data %>%
  pin_write(
    azdhs_mortality_extract,
    type = "rds",
    title = "AZDHS Mortality Extract, tidy and transformed",
    description = "The tidy and transformed version of the original AZDHS mortality extract.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("azdhs_mortality_extract")

# subset to death by suicide only 
azdhs_suicide_data_extract <- azdhs_mortality_extract %>% # create suicide variable
  filter(suicide == "yes") %>% # filter by suicide variable
  arrange(desc(date_of_death), medical_examiner_record_no) %>%
  distinct(cert_me_record_date = str_c( # take out duplicates
    death_certificate_number,
    medical_examiner_record_no, 
    date_of_death,
                 sep = "-"), .keep_all = TRUE)

# inspect tidy data ####
glimpse(azdhs_suicide_data_extract)

azdhs_suicide_data_extract %>%
  arrange(desc(cert_me_record_date))

# save to pin board ####
suicide_data %>%
  pin_write(azdhs_suicide_data_extract,
            title = "AZDHS Mortality Extract (Suicide subset)",
            type = "rds",
            description = "Subset of the AZDHS Mortality Extract (2016-2021) including only death by suicide and pending medical examiner cases.",
            metadata = list(
              owner = "Coconino HHS",
              department = "Epidemiology",
              user = "rherrera"
            )
  )

suicide_data %>%
  pin_meta("azdhs_suicide_data_extract")

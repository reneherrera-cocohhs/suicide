

# Setup ####
# packages
library(here) # project oriented workflow
library(tidyverse) # 
# library(janitor) # tabyl
library(scales) # date_breaks & date_minor_breaks
library(pins) # read data 

# load pinboard ####
suicide_data <- board_folder("") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
# azdhs mortality data
azdhs_suicide_data_extract <- suicide_data %>%
  pin_read("azdhs_suicide_data_extract")

# pin info
suicide_data %>%
  pin_meta("azdhs_suicide_data_extract")

smr_selection_year <- as.character(unique(azdhs_suicide_data_extract$year_analysis_ul))

# filter to case selection year
cases_for_smr <- azdhs_suicide_data_extract %>%
  filter(death_book_year %in% smr_selection_year) %>%
  filter(county_resident == "Resident") %>% # county resident only
  filter(injaddr_county == "coconino") %>% # injury occurred within Coconino county
  filter(!is.na(death_certificate_number)) %>% # only cases where a death certificate number exists
  filter(!is.na(medical_examiner_record_no)) %>% # only cases where a medical examiner number exists
  filter(medical_examiner_record_no != "") %>%
  mutate(age_calc = round(age_calc, digits = 0))

# convert "" to NA
cases_for_smr[cases_for_smr == ""] <- NA

# inspect 
glimpse(cases_for_smr)

# filter to cases for first part of year; Jan-Jun
sample_cases_for_smr <- cases_for_smr %>%
  filter(date_of_death_month < 7) %>%
  # slice_sample(prop = 0.8) %>%
  select(
    death_certificate_number,
    medical_examiner_record_no,
    sex,
    date_of_death,
    decedent_dob,
    age_calc,
    years_in_arizona,
    race_code,
    birth_city,
    birth_state_name,
    birth_country,
    residence_city_name,
    residence_state,
    residence_zip,
    residence_county_name,
    residence_country,
    death_status_desc,
    death_place_type_other,
    cdc_injuryplace_desc,
    injury_description_other,
    place_of_injury_other,
    cdc_injurydesc,
    cdc_placeofinjury_text,
    injury_address,
    injaddr_city,
    injaddr_state,
    injaddr_zip,
    injaddr_county,
    death_city_name,
    death_county_name,
    death_state_name,
    place_of_death_zip,
    method_code_cdc,
    cod_a,
    military_service,
    funeral_home,
    edu_code,
    occupation_description,
    industry_description,
    marital_code
  )

# save to pin board ####
suicide_data %>%
  pin_write(sample_cases_for_smr,
            title = "Sample of cases for review by SMR",
            type = "rds",
            description = "Sample of the subset of the AZDHS Mortality Extract including only death by suicide.",
            metadata = list(
              owner = "Coconino HHS",
              department = "Epidemiology"
            )
  )

# Introduction #### 
# This script will join all AZDHS mortality data,
# select only the variables of interest, 
# remove duplicates, 
# and save the updated data to the pin board 
# rené dario herrera 
# 18 may 2022 
# Coconino County Health and Human Services 
# rherrera at coconino dot az dot gov 

# Setup ####
# package libraries 
library(here)
library(tidyverse)
library(lubridate)
library(pins)

# Read data #### 
# load pinboard
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

## list the pins located on the pin board ####
suicide_data %>%
  pin_list()

## read data from pin board ####
# historical
death_data_historical <- suicide_data %>%
  pin_read("death_data_historical") %>%
  mutate(across(everything(), as.character)) # ensure capability to bind_row

# view
glimpse(death_data_historical)

# count for each year 
death_data_historical %>%
  count(death_book_year)

# year to date
death_data_ytd_2021 <- suicide_data %>%
  pin_read("death_data_ytd_2021") %>%
  mutate(across(everything(), as.character)) # ensure capability to bind_row

# view
glimpse(death_data_ytd_2021)

# count for each year 
death_data_ytd_2021 %>%
  count(death_book_year)

# year to date
death_data_ytd <- suicide_data %>%
  pin_read("death_data_ytd") %>%
  mutate(across(everything(), as.character)) # ensure capability to bind_row

# view
glimpse(death_data_ytd)

# count for each year 
death_data_ytd %>%
  count(death_book_year)

# check the date of the most recent death in the record 
death_data_ytd %>%
  summarise(max(ymd(date_of_death)))

# Join data ####
azdhs_combined <- bind_rows(
  death_data_historical,
  death_data_ytd_2021,
  death_data_ytd
) %>%
  distinct(
    death_certificate_number, # remove duplicates ####
    .keep_all = TRUE
  ) %>%
  mutate(across(where(is.character), str_to_lower)) %>%# change character strings to lower 
  mutate(suicide = if_else(cdc_mannerofdeath_desc == "suicide", "yes", "no")) # new variable code of suicide 

# check count for each year 
azdhs_combined %>%
  count(death_book_year)

glimpse(azdhs_combined)

# Select variables of interest #### 
# select the variables that contain ICD-10 codes 
(var_icd <- azdhs_combined %>%
   select(contains("record_axis")) %>%
   names())

# variables of interest ####
variables_of_interest <- c(
  "death_certificate_number", # unique identifier; Death registration file number (Example- 102-2017-000001D)
  "medical_examiner_record_no", # Death Medical Examiner record Number
  "death_book_year", # Year of death registration
  "date_of_death", # Death date. The table gives you year,month, and day
  "decedent_dob", # Date of Birth
  "birth_city", # Birth City- free text field
  "birth_state_name", # Birth state name- free text field
  "birth_country", # Birth Country- free text field
  "residence_city_name", # Residence city name
  "residence_zip", # Residence zipcode
  "residence_county_name", # Residence county name
  "residence_state", # Residence state name
  "residence_country", # Residence country name
  "residence_on_reservation", # tribal?
  "death_city_name", # Free text field. Death address city place name
  "death_county_name", # Free text field. Death address county name
  "death_state_name", # Free text field. Death address state name
  "place_of_death_zip", # Zipcode
  "new_race_desc", #
  "race_vsims", # Race VSIMS (American Indian or alaska Native, Asian or Pacific Islander, Black or African American, Hispanic or latino, Unknown/other/Refused, White Non-Hispanic)
  "race_desc", # Race description (American Indian/Alaska Native, Asian/Pacific Islander, Blacl/African american, Other race/Refused/Not obtainable, White)
  "race_phs", # Race PHS  (American Indian or alaska Native, Asian or Pacific Islander, Black or African American, Hispanic or latino, Unknown/other/Refused, White Non-Hispanic)
  "nchs_race_code", # NCHS race code
  "hispanic_desc", # If the person is  Hispanic or non- hispanic
  "decedent_gender_desc", # It is coded as Female, Male, and Not yet determined
  "acme_underlying_cause", # Primary cause of Death
  "cdc_mannerofdeath_desc", # A= Accident, C= Undetermined, H= Homocide, N= Natural Death, P= Pending Investigation, S= Suicide
  "cod_a", # Cause of death line a (free text field)
  "cod_b", # Cause of death line b (free text field)
  "cod_c", # Cause of death line c (free text field)
  "cod_d", # Cause of death line d (free text field)
  var_icd,
  "military_service", # Decedent veteran status. Coded as Yes,No and Unknown
  "funeral_home", # Funeral home business unit name
  "education_desc", # 1= 8th Grade, 2=9th, 3= High School, 4= Some college, 5= Associate, 6= Bachelor, 7= Master, 8= Doctor, 9= Unk
  "occupation_description", # Decedent occupation description- Free Text field
  "industry_description", # Decedent industry description- free text field
  "years_in_arizona", # Number of years lived in Arizona
  "marital_desc", # Marital Status Description- 1= Never married, single   2= Married  3= Widowed   4= Divorced   9= Unknown
  "death_status_desc", # Death status description (free text field) (Dead on arrival, decedent's residence, emergency, emergency room/outpatient, hospice facility, inpatient, not classifiable, nursing home/long term care, other,specify))
  "death_place_type_other", # Other place of Death
  "resident_address_system_code_de1", # tribal?
  "cdc_placeofinjury", # Place of Injury CDC code. It is coded as A,B,C,G,I,K,N,P, and Q, A= Home, B= farm, C= Residential Instituition, G= Industrial and Construction, I= Trade and Service area, K= Street/Highway, N= Sports and Athletics Area, P= School, other institution, public administrative area, Q= Unknown
  "cdc_injuryplace_desc", # Place of Injury Description
  "injury_description_other", # Description of Other Place of Injury
  "place_of_injury_other", # Other place of Injury
  "cdc_injurydesc", # Injury description (free text field)
  "injury_add_lattitude", # Injury address lattitude
  "injury_add_longitude", # Injury address longitude
  "injury_address", # Place of injury full address.
  "injaddr_city", # Injury address city
  "injaddr_state", # Injury address State
  "injaddr_zip", # Injury address zipcode
  "injaddr_county",
  "suicide",
  "autopsy",
  "certifier_name"
)

# select only the variables of interest
azdhs_mortality_extract <- azdhs_combined %>%
  select(all_of(variables_of_interest))

# Save updated data to the pin board ####
suicide_data %>%
  pin_write(
    azdhs_mortality_extract,
    type = "rds",
    title = "AZDHS Mortality Extract, tidy",
    description = "The more tidy version of the original AZDHS mortality extract, includes data starting in 2016 and only variables of interest.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("azdhs_mortality_extract")

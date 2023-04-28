# Introduction ####
# source script to read mortality data to R environment;
# tidy data (i.e., clean up dates and remove dummy variables)
# transform (create new dummy variables)
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
library(janitor)

# source
source(
  "scripts/07-read-mortality-extract.R",
  echo = TRUE
)

# # Read data ####
# # # load pinboard
# mortality_folder <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data")
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

# Read data ####
mortality_folder %>%
  pin_list()

# # this should change when the year changes.
# # for example, when year 2024 data is available then update the pin read accordingly
mortality_data <- mortality_folder %>%
  pin_read("mortality-data-tidy-transformed-2010-2023")

# inpsect
glimpse(mortality_data)

## list the pins located on the pin board ####
suicide_data %>%
  pin_list()

## read data from pin board ####
# # historical
# death_data_historical <- suicide_data %>%
#   pin_read("death_data_historical") %>%
#   mutate(across(everything(), as.character)) # ensure capability to bind_row
#
# # view
# glimpse(death_data_historical)
#
# # count for each year
# death_data_historical %>%
#   tabyl(death_book_year)
#
# # year to date
# death_data_ytd_2021 <- suicide_data %>%
#   pin_read("death_data_ytd_2021") %>%
#   mutate(across(everything(), as.character)) # ensure capability to bind_row
#
# # view
# glimpse(death_data_ytd_2021)
#
# # count for each year
# death_data_ytd_2021 %>%
#   tabyl(death_book_year)
#
# # year to date
# death_data_ytd <- suicide_data %>%
#   pin_read("death_data_ytd") %>%
#   mutate(across(everything(), as.character)) # ensure capability to bind_row
#
# # view
# glimpse(death_data_ytd)
#
# # count for each year
# death_data_ytd %>%
#   tabyl(death_book_year)
#
# # check the date of the most recent death in the record
# death_data_ytd %>%
#   summarise(max(ymd(date_of_death)))

# Join data ####
# azdhs_combined <- bind_rows(
#   death_data_historical,
#   death_data_ytd_2021,
#   death_data_ytd
# ) %>%
#   distinct(
#     death_certificate_number, # remove duplicates ####
#     .keep_all = TRUE
#   ) %>%
#   mutate(across(where(is.character), str_to_lower)) %>%# change character strings to lower
#   mutate(suicide = if_else(cdc_mannerofdeath_desc == "suicide", "yes", "no")) # new variable code of suicide
#
# # check count for each year
# azdhs_combined %>%
#   tabyl(death_book_year)
#
# glimpse(azdhs_combined)
#
# # Select variables of interest ####
# # select the variables that contain ICD-10 codes
# (var_icd <- azdhs_combined %>%
#    select(contains("record_axis")) %>%
#    names())
#
# # comment this out because i need to keep all variables, for now
# # # variables of interest ####
# # variables_of_interest <- c(
# #   "death_certificate_number", # unique identifier; Death registration file number (Example- 102-2017-000001D)
# #   "medical_examiner_record_no", # Death Medical Examiner record Number
# #   "death_book_year", # Year of death registration
# #   "date_of_death", # Death date. The table gives you year,month, and day
# #   "decedent_dob", # Date of Birth
# #   "birth_city", # Birth City- free text field
# #   "birth_state_name", # Birth state name- free text field
# #   "birth_country", # Birth Country- free text field
# #   "residence_city_name", # Residence city name
# #   "residence_zip", # Residence zipcode
# #   "residence_county_name", # Residence county name
# #   "residence_state", # Residence state name
# #   "residence_country", # Residence country name
# #   "residence_on_reservation", # tribal?
# #   "death_city_name", # Free text field. Death address city place name
# #   "death_county_name", # Free text field. Death address county name
# #   "death_state_name", # Free text field. Death address state name
# #   "place_of_death_zip", # Zipcode
# #   "new_race_desc", #
# #   "race_vsims", # Race VSIMS (American Indian or alaska Native, Asian or Pacific Islander, Black or African American, Hispanic or latino, Unknown/other/Refused, White Non-Hispanic)
# #   "race_desc", # Race description (American Indian/Alaska Native, Asian/Pacific Islander, Blacl/African american, Other race/Refused/Not obtainable, White)
# #   "race_phs", # Race PHS  (American Indian or alaska Native, Asian or Pacific Islander, Black or African American, Hispanic or latino, Unknown/other/Refused, White Non-Hispanic)
# #   "nchs_race_code", # NCHS race code
# #   "hispanic_desc", # If the person is  Hispanic or non- hispanic
# #   "decedent_gender_desc", # It is coded as Female, Male, and Not yet determined
# #   "acme_underlying_cause", # Primary cause of Death
# #   "cdc_mannerofdeath_desc", # A= Accident, C= Undetermined, H= Homocide, N= Natural Death, P= Pending Investigation, S= Suicide
# #   "cod_a", # Cause of death line a (free text field)
# #   "cod_b", # Cause of death line b (free text field)
# #   "cod_c", # Cause of death line c (free text field)
# #   "cod_d", # Cause of death line d (free text field)
# #   var_icd,
# #   "military_service", # Decedent veteran status. Coded as Yes,No and Unknown
# #   "funeral_home", # Funeral home business unit name
# #   "education_desc", # 1= 8th Grade, 2=9th, 3= High School, 4= Some college, 5= Associate, 6= Bachelor, 7= Master, 8= Doctor, 9= Unk
# #   "occupation_description", # Decedent occupation description- Free Text field
# #   "industry_description", # Decedent industry description- free text field
# #   "years_in_arizona", # Number of years lived in Arizona
# #   "marital_desc", # Marital Status Description- 1= Never married, single   2= Married  3= Widowed   4= Divorced   9= Unknown
# #   "death_status_desc", # Death status description (free text field) (Dead on arrival, decedent's residence, emergency, emergency room/outpatient, hospice facility, inpatient, not classifiable, nursing home/long term care, other,specify))
# #   "death_place_type_other", # Other place of Death
# #   "resident_address_system_code_de1", # tribal?
# #   "cdc_placeofinjury", # Place of Injury CDC code. It is coded as A,B,C,G,I,K,N,P, and Q, A= Home, B= farm, C= Residential Instituition, G= Industrial and Construction, I= Trade and Service area, K= Street/Highway, N= Sports and Athletics Area, P= School, other institution, public administrative area, Q= Unknown
# #   "cdc_injuryplace_desc", # Place of Injury Description
# #   "injury_description_other", # Description of Other Place of Injury
# #   "place_of_injury_other", # Other place of Injury
# #   "cdc_injurydesc", # Injury description (free text field)
# #   "injury_add_lattitude", # Injury address lattitude
# #   "injury_add_longitude", # Injury address longitude
# #   "injury_address", # Place of injury full address.
# #   "injaddr_city", # Injury address city
# #   "injaddr_state", # Injury address State
# #   "injaddr_zip", # Injury address zipcode
# #   "injaddr_county",
# #   "suicide",
# #   "autopsy",
# #   "certifier_name"
# # )
# #
# # # select only the variables of interest
# # azdhs_mortality_extract <- azdhs_combined %>%
# #   select(all_of(variables_of_interest))

# Save updated data to the pin board ####
# suicide_data %>%
#   pin_write(
#     x = mortality_data,
#     name = "azdhs_mortality_extract",
#     type = "rds",
#     title = "AZDHS Mortality Extract, tidy and transformed",
#     description = "The more tidy version of the original AZDHS mortality extract.",
#     metadata = list(
#       owner = "Coconino HHS",
#       department = "Epidemiology",
#       user = "rherrera"
#     )
#   )

suicide_data %>%
  pin_meta("azdhs_mortality_extract")

# rename
azdhs_mortality_extract <- mortality_data

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
  )

## analysis date range ####
# The suicide report focuses on the most recent 5 years
# create values for new variable to determine the range of which years to include in reports
(year_analysis_ul <- if_else(
  condition = month(Sys.Date()) < 7,
  true = (year(Sys.Date())) - 1,
  false = year(Sys.Date())
)
)

# for analysis purposes, this is the upper limit of date range
(year_analysis_current <- year(Sys.Date())) # for analysis purposes, this is the current year
(year_analysis_ll <- year_analysis_ul - 4) # for analysis purposes, this is the lower limit of the date range

# add new variables to data
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    year_analysis_ul = year_analysis_ul,
    year_analysis_current = year_analysis_current,
    year_analysis_ll = year_analysis_ll
  )

# years of analysis
(analysis_year_range <- c(as.character(unique(azdhs_mortality_extract$year_analysis_ll):unique(azdhs_mortality_extract$year_analysis_current))))

azdhs_mortality_extract %>%
  tabyl(death_book_year)

# change "Resident" to "resident"
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    d_county_resident = str_to_lower(d_county_resident)
  )


# subset to most recent years
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  filter(death_book_year %in% analysis_year_range)

azdhs_mortality_extract %>%
  tabyl(death_book_year)

# inspect dummy variables
azdhs_mortality_extract %>%
  select(starts_with("d_")) %>%
  glimpse()

## Manner of death, injury description ####
# code new values cdc
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    d_method_code_cdc = case_when(
      str_detect(str_to_lower(cdc_injurydesc), "poison") ~ "overdose/poison",
      str_detect(str_to_lower(cdc_injurydesc), "hang") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "hung") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "ligature") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "around neck") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "around his neck") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "around her neck") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "around their neck") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "firearm") ~ "firearm",
      str_detect(str_to_lower(cdc_injurydesc), "shot self") ~ "firearm",
      str_detect(str_to_lower(cdc_injurydesc), "shot him") ~ "firearm",
      str_detect(str_to_lower(cdc_injurydesc), "shot her") ~ "firearm",
      str_detect(str_to_lower(cdc_injurydesc), "shot them") ~ "firearm",
      str_detect(str_to_lower(cdc_injurydesc), "gsw ") ~ "firearm",
      str_detect(str_to_lower(cdc_injurydesc), "overdose") ~ "overdose/poison",
      str_detect(str_to_lower(cdc_injurydesc), "jump") ~ "jump/fall",
      str_detect(str_to_lower(cdc_injurydesc), "fall") ~ "jump/fall",
      str_detect(str_to_lower(cdc_injurydesc), "fell") ~ "jump/fall",
      str_detect(str_to_lower(cdc_injurydesc), "cliff") ~ "jump/fall",
      str_detect(str_to_lower(cdc_injurydesc), "ingest") ~ "overdose/poison",
      str_detect(str_to_lower(cdc_injurydesc), "gun") ~ "firearm",
      str_detect(str_to_lower(cdc_injurydesc), "cut") ~ "cut/stabbed self",
      str_detect(str_to_lower(cdc_injurydesc), "incision") ~ "cut/stabbed self",
      str_detect(str_to_lower(cdc_injurydesc), "drug") ~ "overdose/poison",
      str_detect(str_to_lower(cdc_injurydesc), "excess of") ~ "overdose/poison",
      str_detect(str_to_lower(cdc_injurydesc), "self administered") ~ "overdose/poison",
      str_detect(str_to_lower(cdc_injurydesc), "blunt force") ~ "blunt force injuries",
      str_detect(str_to_lower(cdc_injurydesc), "stab") ~ "cut/stabbed self",
      str_detect(str_to_lower(cdc_injurydesc), "sharp") ~ "cut/stabbed self",
      str_detect(str_to_lower(cdc_injurydesc), "wound") ~ "cut/stabbed self",
      str_detect(str_to_lower(cdc_injurydesc), "suffocat") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "asphyxiation") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "plastic bag") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), " gas") ~ "hanged self/asphyxiation",
      str_detect(str_to_lower(cdc_injurydesc), "train") ~ "train/vehicle",
      str_detect(str_to_lower(cdc_injurydesc), "vehicle") ~ "train/vehicle",
      str_detect(str_to_lower(cdc_injurydesc), "car") ~ "train/vehicle",
      str_detect(str_to_lower(cdc_injurydesc), "truck") ~ "train/vehicle",
      str_detect(str_to_lower(cdc_injurydesc), "fire") ~ "fire",
      # is.na(cdc_injurydesc) ~ "unknown",
      cdc_injurydesc == "" ~ NA_character_,
      TRUE ~ as.character(cdc_injurydesc)
    )
  )

azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate(
    d_firearm = if_else(
      condition = str_detect(d_method_code_cdc, "firearm"),
      true = TRUE,
      false = FALSE
    )
  )

azdhs_mortality_extract <- azdhs_mortality_extract %>%
  mutate( # replace blank values with NA
    across(
      .cols = everything(),
      .fns = ~ na_if(x = ., y = "")
    )
  )

# remove duplicate death certificate number
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  distinct(
    death_certificate_number,
    .keep_all = TRUE
  )

# write description string for pin board meta data
pin_description <- str_c(
  "The tidy and transformed version of the original AZDHS mortality extract; inclusive of: ",
  paste(analysis_year_range, collapse = ", ")
)

# save to pinboard
suicide_data %>%
  pin_write(
    azdhs_mortality_extract,
    type = "rds",
    title = "AZDHS Mortality Extract, tidy and transformed",
    description = pin_description,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("azdhs_mortality_extract")

azdhs_mortality_extract %>%
  tabyl(death_book_year)

# subset to death by suicide only ####
azdhs_suicide_data <- azdhs_mortality_extract %>% # create suicide variable
  filter(d_suicide == 1) %>% # filter by suicide variable
  arrange(desc(date_of_death), medical_examiner_record_no)

# inspect tidy data
glimpse(azdhs_suicide_data)

azdhs_suicide_data %>%
  tabyl(death_book_year, d_county_resident)

pin_name <- str_c(
  "azdhs-suicide-data-",
  as.character(min(azdhs_suicide_data$death_book_year)),
  "-",
  as.character(max(azdhs_suicide_data$death_book_year))
)

# save pin name for use in project
write_rds(
  x = pin_name,
  file = "data-tidy/suicide-data-pin-name.rds"
)

pin_title <- str_c(
  "AZDHS Mortality Extract, tidy, transformed, suicide subset (",
  as.character(min(azdhs_suicide_data$death_book_year)),
  "-",
  as.character(max(azdhs_suicide_data$death_book_year)),
  ")"
)

pin_description <- str_c(
  "The tidy and transformed version of the AZDHS suicide mortality subset; inclusive of: ",
  paste(analysis_year_range, collapse = ", ")
)

# save to pin board
suicide_data %>%
  pin_write(
    x = azdhs_suicide_data,
    name = pin_name,
    type = "rds",
    title = pin_title,
    description = pin_description,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

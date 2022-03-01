# Setup ####
# packages
library(here)
library(tidyverse)
library(lubridate)
library(pins)

# load pinboard
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
# historical
death_data_historical <- suicide_data %>%
  pin_read("death_data_historical") %>%
  mutate(across(everything(), as.character)) # ensure capability to bind_row

# view
glimpse(death_data_historical)

# year to date
death_data_ytd <- suicide_data %>%
  pin_read("death_data_ytd") %>%
  mutate(across(everything(), as.character)) # ensure capability to bind_row

# view
glimpse(death_data_ytd)

# me brief
me_brief <- suicide_data %>%
  pin_read("me_brief")

# view
glimpse(me_brief)

# combine historical and ytd data frames ####
azdhs_combined <- bind_rows(
  death_data_historical,
  death_data_ytd
)

# Explore variables ####
# unique identifier
azdhs_combined %>%
  select(contains("cert")) %>%
  glimpse()

# Date, explore ####
# which variables contain "year" in the name?
azdhs_combined %>%
  select(contains("year")) %>%
  glimpse()

# which years (death book year) are present in the data ?
azdhs_combined %>%
  distinct(death_book_year)

# which variables have age
azdhs_combined %>%
  select(contains("age")) %>%
  glimpse()

# which variables have date
azdhs_combined %>%
  select(contains("date")) %>%
  glimpse()

# which variables have birth or dob
azdhs_combined %>%
  select(contains("dob")) %>%
  glimpse()

# Geography, explore ####
# which variables have county
azdhs_combined %>%
  select(contains("birth")) %>%
  glimpse()

azdhs_combined %>%
  select(contains("residence")) %>%
  glimpse()

azdhs_combined %>%
  select(contains("death")) %>%
  glimpse()

azdhs_combined %>%
  select(contains("county")) %>%
  glimpse()

# which variables have city
azdhs_combined %>%
  select(contains("city")) %>%
  glimpse()

azdhs_combined %>%
  select(contains("state")) %>%
  glimpse()

# which variables have zip code
azdhs_combined %>%
  select(contains("zip")) %>%
  glimpse()

# which city names exist?
azdhs_combined %>%
  distinct(residence_city_name) %>%
  arrange(residence_city_name) %>%
  as.list()

# latitude and longitude? 
# which variables have zip code
azdhs_combined %>%
  select(contains("lat")) %>%
  glimpse()

azdhs_combined %>%
  select(contains("lon")) %>%
  glimpse()

# address? 
azdhs_combined %>%
  select(contains("addr")) %>%
  glimpse()

# Race and Ethnicity, explore ####
# which variables contain race
azdhs_combined %>%
  select(contains("race")) %>%
  glimpse()

azdhs_combined %>%
  select(contains("hisp")) %>%
  glimpse()

# Sex and Gender, explore ####
azdhs_combined %>%
  select(contains("gender")) %>%
  glimpse()

# Method of death, explore ####
azdhs_combined %>%
  select(contains("death")) %>%
  glimpse()

azdhs_combined %>%
  select(contains("cod")) %>%
  glimpse()

# Military service, explore ####
azdhs_combined %>%
  select(contains("military")) %>%
  glimpse()

# Religious affiliation and funeral home ####
azdhs_combined %>%
  select(contains("funeral")) %>%
  glimpse()

# Education, explore ####
azdhs_combined %>%
  select(contains("education")) %>%
  glimpse()

# occupation, explore ####
azdhs_combined %>%
  select(contains("occup")) %>%
  glimpse()

# inmate or correctional facility, explore ####

####

# commented out because it isn't needed to tidy the data, this is what I used to search for some indicator of

# search_for_inmate_string <- map(azdhs_combined, ~ unique(.x[which(str_detect(.x, "inmate"))]))
# search_for_inmate_string <- map(azdhs_combined, ~ unique(.x[which(str_detect(.x, "INMATE"))]))
# search_for_inmate_string <- map(azdhs_combined, ~ unique(.x[which(str_detect(.x, "correct"))]))
# search_for_inmate_string <- map(azdhs_combined, ~ unique(.x[which(str_detect(.x, "CORRECT"))]))
# search_for_inmate_string <- map(azdhs_combined, ~ unique(.x[which(str_detect(.x, "prison"))]))
# search_for_inmate_string <- map(azdhs_combined, ~ unique(.x[which(str_detect(.x, "PRISON"))]))
# search_for_inmate_string <- map(azdhs_combined, ~ unique(.x[which(str_detect(.x, "incar"))]))
# search_for_inmate_string <- map(azdhs_combined, ~ unique(.x[which(str_detect(.x, "INCAR"))]))

# injury
azdhs_combined %>%
  select(contains("injury")) %>%
  glimpse()

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
  "cdc_mannerofdeath_desc", # A= Accident, C= Undetermined, H= Homocide, N= Natural Death, P= Pending Investigation, S= Suicide
  "cod_a", # Cause of death line a (free text field)
  "cod_b", # Cause of death line b (free text field)
  "cod_c", # Cause of death line c (free text field)
  "cod_d", # Cause of death line d (free text field)
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
  "injaddr_county"
)

# total number of all deaths in Coconino County ####
(azdhs_all_cause_mortality_coconino_resident_count <- azdhs_combined %>%
  mutate(across(where(is.character), str_to_lower)) %>% # all variables to lower case
  select(all_of(variables_of_interest)) %>% # select variables of interest
  mutate(suicide = if_else(cdc_mannerofdeath_desc == "suicide", "yes", "no")) %>% # new variable of suicide 
  filter(str_detect(residence_county_name, "coconino")) %>% # county resident only 
  count(death_book_year))

# save to pinboard
suicide_data %>%
  pin_write(
    azdhs_all_cause_mortality_coconino_resident_count,
    type = "rds",
    title = "Count of all cause mortality of Coconino residents",
    description = "Count of all deaths of Coconino County residents provided by AZDHS mortality extract by year."
  )

# total number of all military service deaths in Coconino County #### 
(azdhs_all_cause_mortality_coconino_military_count <- azdhs_combined %>%
   mutate(across(where(is.character), str_to_lower)) %>% # all variables to lower case
   select(all_of(variables_of_interest)) %>% # select variables of interest
   mutate(suicide = if_else(cdc_mannerofdeath_desc == "suicide", "yes", "no")) %>% # new variable of suicide 
   filter(str_detect(residence_county_name, "coconino")) %>% # county resident only 
   filter(military_service == "yes") %>% # military service only
   count(death_book_year))

# save to pinboard
suicide_data %>%
  pin_write(
    azdhs_all_cause_mortality_coconino_military_count,
    type = "rds",
    title = "Count of all cause mortality of military Coconino residents",
    description = "Count of all deaths of Coconino County residents provided by AZDHS mortality extract by year."
  )

# select only the variables of interest
# filter subset to death by suicide only ####
azdhs_suicide_data <- azdhs_combined %>%
  mutate(across(where(is.character), str_to_lower)) %>% # all variables to lower case
  distinct(death_certificate_number, .keep_all = TRUE) %>% # use identifier to remove duplicates
  select(all_of(variables_of_interest)) %>% # select variables of interest
  mutate(suicide = if_else(cdc_mannerofdeath_desc == "suicide", "yes", "no"))

# inspect
glimpse(azdhs_suicide_data)

# Dates ####
# calculate age for age group
# convert to date
azdhs_suicide_data$decedent_dob <- ymd(azdhs_suicide_data$decedent_dob)
azdhs_suicide_data$date_of_death <- ymd(azdhs_suicide_data$date_of_death)

# save new variable, age in years
# save calculated variable to data
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(age_calc = time_length(x = difftime(azdhs_suicide_data$date_of_death, azdhs_suicide_data$decedent_dob), unit = "years"))

# add me brief to data frame
azdhs_suicide_data <- full_join(
  x = azdhs_suicide_data, # rename for saving to pin
  y = me_brief,
) %>%
  mutate(me_brief = case_when(
    me_brief == "yes" ~ "yes",
    is.na(me_brief) ~ "no",
  ))

# add new variable to determine the range of which years to include in reports
year_analysis_ul <- if_else(month(Sys.Date()) < 7, (year(Sys.Date())) - 1, year(Sys.Date())) # for analysis purposes, this is the upper limit of date range
year_analysis_current <- year(Sys.Date()) # for analysis purposes, this is the current year
year_analysis_ll <- year_analysis_ul - 4 # for analysis purposes, this is the lower limit of the date range

azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(
    year_analysis_ul = year_analysis_ul,
    year_analysis_current = year_analysis_current,
    year_analysis_ll = year_analysis_ll
  )

# view
glimpse(azdhs_suicide_data)

# years of analysis
analysis_year_range <- c(as.character(unique(azdhs_suicide_data$year_analysis_ll):unique(azdhs_suicide_data$year_analysis_ul)))

###

# how to ensure there is no double counting?
# look at the recent deaths to see if there is any overlap between what is in the ME pending cases and the mortality extract

azdhs_suicide_data %>%
  arrange(desc(date_of_death)) %>%
  slice_head(n = 20) %>%
  select(death_certificate_number,
         medical_examiner_record_no,
         date_of_death,
         decedent_dob,
         residence_city_name,
         death_city_name,
         funeral_home)

####

# extract month of death to plot on a time series and create a new variable
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(date_of_death_month = month(date_of_death))

# extract year of death and create a new variable
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(date_of_death_year = year(date_of_death))

# extract day of death and create a new variable
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(date_of_death_day_week = wday(date_of_death, label = TRUE))

# extract death day of month and create a new variable
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(date_of_death_day_month = mday(date_of_death)) 

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
azdhs_suicide_data <- azdhs_suicide_data %>%
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

# what is listed for age category
# check levels
azdhs_suicide_data %>%
  distinct(age_group_5yr) %>%
  arrange(age_group_5yr) %>%
  as.list()

azdhs_suicide_data %>%
  count(age_group_5yr)

plot(x = azdhs_suicide_data$age_group_5yr)

azdhs_suicide_data %>%
  distinct(age_group_7cat) %>%
  arrange(age_group_7cat) %>%
  as.list()

azdhs_suicide_data %>%
  count(age_group_7cat)

plot(x = azdhs_suicide_data$age_group_7cat)

azdhs_suicide_data %>%
  distinct(age_group_6cat) %>%
  arrange(age_group_6cat) %>%
  as.list()

azdhs_suicide_data %>%
  count(age_group_6cat)

plot(x = azdhs_suicide_data$age_group_6cat)

# inspect to confirm accurate coding 
azdhs_suicide_data %>%
  select(starts_with("age_"))

# Change geographic variables to character Title Case
azdhs_suicide_data <- azdhs_suicide_data %>%
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

# residence county ####
# view the county names
azdhs_suicide_data %>%
  distinct(residence_county_name) %>%
  arrange(residence_county_name)

azdhs_suicide_data %>%
  count(residence_county_name, sort = TRUE) %>%
  slice_max(order_by = n, n = 10) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(residence_county_name, n), y = n)) +
  coord_flip()

# add a new variable to show if the decedent is a coconino county resident
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(county_resident = if_else(
    str_detect(azdhs_suicide_data$residence_county_name, "Coconino"),
    "Resident",
    "Non-Resident"
  ))

# inspect
azdhs_suicide_data %>%
  count(county_resident)

# race ####
# read from pin board
us_census_race_groups <- suicide_data %>%
  pin_read("us_census_race_groups")

us_census_race_groups <- as.list(us_census_race_groups$variable)

# explore and list all the race categories
azdhs_suicide_data %>%
  distinct(race_vsims) %>%
  arrange(race_vsims)

####

# Commented this out because it isn't needed to tidy the data; I used this to search for variables indicating race and ethnicity

# map(azdhs_suicide_data, ~ unique(.x[which(str_detect(.x, "hisp"))]))
# map(azdhs_suicide_data, ~ unique(.x[which(str_detect(.x, "black"))]))
# map(azdhs_suicide_data, ~ unique(.x[which(str_detect(.x, "ameri"))]))
# map(azdhs_suicide_data, ~ unique(.x[which(str_detect(.x, "asia"))]))
# map(azdhs_suicide_data, ~ unique(.x[which(str_detect(.x, "paci"))]))

azdhs_suicide_data %>%
  filter(if_any(contains("race"), ~ str_detect(.x, "asian"))) %>%
  select(starts_with("race"))

azdhs_suicide_data %>%
  filter(if_any(contains("race"), ~ str_detect(.x, "hawa"))) %>%
  select(starts_with("race"))

azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(race_code = factor(
    case_when(
      race_vsims == "hispanic or latino" ~ "Hispanic or Latino (any race)",
      race_vsims == "white non-hispanic" ~ "White Non-Hispanic",
      race_vsims == "black or african american" ~ "Black or African American",
      race_vsims == "american indian or alaska native" ~ "American Indian and Alaska Native",
      race_vsims == "asian or pacific islander" ~ "Asian or Native Hawaiian and Other Pacific Islander",
      race_vsims == "unknown/other/refused" ~ "Other",
      is.na(race_vsims) ~ "Other",
      TRUE ~ as.character(race_vsims)
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
glimpse(azdhs_suicide_data)
levels(azdhs_suicide_data$race_code)

azdhs_suicide_data %>%
  distinct(race_code) %>%
  arrange(race_code) %>%
  as.list()

azdhs_suicide_data %>%
  count(race_code, sort = TRUE)

# sex and gender ####
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(sex = factor(case_when(
    decedent_gender_desc == "male" ~ "Male",
    decedent_gender_desc == "female" ~ "Female",
    is.na(decedent_gender_desc) ~ "Not yet determined",
    TRUE ~ as.character(decedent_gender_desc)
  ), levels = c("Female", "Male", "Not yet determined")))

azdhs_suicide_data %>%
  count(sex, sort = TRUE)

# tribal member ####
# check what possible values are
unique(azdhs_suicide_data$residence_on_reservation)
unique(azdhs_suicide_data$resident_address_system_code_de1)

azdhs_suicide_data %>%
  filter(residence_on_reservation == 1) %>%
  distinct(resident_address_system_code_de1)

# recode
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(tribal_code = case_when(
    resident_address_system_code_de1 == NA ~ "no",
    resident_address_system_code_de1 == "" ~ "no",
    resident_address_system_code_de1 == "no" ~ "no",
    resident_address_system_code_de1 == "unknown" ~ "no",
    TRUE ~ "yes"
  ))

azdhs_suicide_data %>%
  count(tribal_code, sort = TRUE)

# education ####
azdhs_suicide_data <- azdhs_suicide_data %>%
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

azdhs_suicide_data %>%
  count(edu_code, sort = TRUE)

azdhs_suicide_data %>%
  count(hs_grad, sort = TRUE)

azdhs_suicide_data %>%
  count(college_grad, sort = TRUE)

# recode marital description ####
azdhs_suicide_data <- azdhs_suicide_data %>%
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

azdhs_suicide_data %>%
  count(marital_code, sort = TRUE)

# recode occupation ####
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(occupation_code = factor
  (str_to_lower(case_when(
      str_detect(occupation_description, "college student") ~ "Student, College",
      str_detect(occupation_description, "student") ~ "Student",
      str_detect(occupation_description, "labor") ~ "Laborer",
      is.na(occupation_description) ~ "Unknown",
      TRUE ~ as.character(occupation_description)
  ))))

azdhs_suicide_data %>%
  count(occupation_code, sort = TRUE)

# recode industry ####
azdhs_suicide_data <- azdhs_suicide_data %>%
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

azdhs_suicide_data %>%
  count(industry_code, sort = TRUE)

# residence_zip
azdhs_suicide_data <- azdhs_suicide_data %>%
  mutate(
    residence_zip = case_when(
      is.na(residence_zip) ~ "unknown",
      TRUE ~ residence_zip
    )
  )

azdhs_suicide_data %>%
  count(residence_zip, sort = TRUE)

# recode funeral home ####
azdhs_suicide_data <- azdhs_suicide_data %>% # rename for pin board
  mutate(funeral_code = factor
  (case_when(
      str_detect(funeral_home, "Norvel Owens") ~ "Norven Owens Mortuary of Flagstaff",
      str_detect(funeral_home, "Affordable Burial") ~ "Affordable Burial and Cremation LLC",
      funeral_home == "" ~ "Unknown",
      is.na(funeral_home) ~ "Unknown",
      TRUE ~ as.character(funeral_home)
  )))

azdhs_suicide_data %>%
  count(funeral_code, sort = TRUE)

# prisoner or inmate? ####
azdhs_suicide_data %>%
  count(cdc_injuryplace_desc) %>%
  arrange(cdc_injuryplace_desc)

azdhs_suicide_data %>%
  count(death_status_desc) %>%
  arrange(death_status_desc)

azdhs_suicide_data %>%
  count(death_place_type_other) %>%
  arrange(death_place_type_other)

str_detect(azdhs_suicide_data$death_place_type_other, "prison")
str_detect(azdhs_suicide_data$death_place_type_other, "jail")

azdhs_suicide_data %>%
  count(place_of_injury_other) %>%
  arrange(place_of_injury_other)

str_detect(azdhs_suicide_data$place_of_injury_other, "prison")
str_detect(azdhs_suicide_data$place_of_injury_other, "jail")

# Manner of death, injury description ####
# code new values cdc
azdhs_suicide_data <- azdhs_suicide_data %>%
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

# create a new data table for 2 x 2 analysis and modeling 
azdhs_mortality_extract <- azdhs_suicide_data

# save to pinboard 
suicide_data %>%
  pin_write(
    azdhs_mortality_extract,
    type = "rds",
    title = "AZDHS Mortality Extract, tidy",
    description = "The more tidy version of the original AZDHS mortality extract."
  )

# subset to death by suicide only 
azdhs_suicide_data <- azdhs_suicide_data %>% # create suicide variable
  filter(suicide == "yes") # filter by suicide variable

# rename for pin board 
azdhs_suicide_data_extract <- azdhs_suicide_data

# inspect tidy data ####
glimpse(azdhs_suicide_data_extract)

azdhs_suicide_data_extract %>%
  arrange(desc(medical_examiner_record_no))

# save to pin board ####
suicide_data %>%
  pin_write(azdhs_suicide_data_extract,
    title = "AZDHS Mortality Extract (Suicide subset)",
    type = "rds",
    description = "Subset of the AZDHS Mortality Extract (2016-2021) including only death by suicide and pending medical examiner cases."
  )

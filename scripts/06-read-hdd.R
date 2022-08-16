# setup ####

# library packages ####
library(here)
library(tidyverse)
library(janitor)
library(pins) # data access
library(fs)
library(lubridate)

# Pins ####
# create a pin board ####
# here for now, need to consider where the best place for this should really be
folder_mortality <- board_folder("")
folder_hdd <- board_folder("")

# list the pins located on the pin board ####
folder_mortality %>%
  pin_list()

folder_hdd %>%
  pin_list()

# CC & DD Categories ####
# used to identify which ICD10 codes to look for

# CDC Suicidal Ideation v1
# Definition:
#   Discharge Diagnosis, Admit Reason Combo, and Chief Complaint History TERMS: (,^[;/ ]R45.851^,or,^[;/ ]R45851^,or,^[;/ ]V62.84^,or,^[;/ ]V6284^,or,^6471006^,or,^102911000^,or,^401229000^,or,^247650009^,or,^225457007^,or,^425104003^,or,^304594002^,or,^225444004^,or,^401206008^,),or,(,(,SI,OR,SI[/ ;.]^,OR,^[/ ;.]SI,OR,^[/ ;.]SI[/ ;.]^,OR,(,^WANT^,AND,^DIE ^,),OR,(,^LIFE^,AND,^ END^,),OR,(,(,^SELF^,AND,(,^HARM^,OR,^HURT^,OR,(,^HANG^,ANDNOT,^CHANG^,),OR,^INFLICT^,OR,^KILL^,OR,^LACERA^,OR,^MUTILAT^,OR,^SHOOT^,OR,^STAB^,OR,^IDEAT^,OR,^ CUT^,),),AND,(,^PLAN^,or,^WANT^,or,^GOING TO^,),),OR,^SUICIDE^,OR,^SUIC^,OR,^SUCI^,OR,(,^SUSCI^,ANDNOT,^RESUSCI^,),OR,^SUISID^,OR,^ END IT^,OR,^IDEATION^,),ANDNOT,(,^END OF LIFE^,OR,^END OF BATTERY LIFE^,OR,^DENIE[SD] SELF HARM^,OR,^NO SELF HARM^,OR,^ACCIDENT^,OR,^HOMICI^,OR,^DENIE[SD] SI[ \/;.]^,OR,^DENIE[SD] SI,OR,^DENIE[SD] ANY SI[ \/;.]^,OR,^DENIE[SD] ANY SI,OR,^DENIE[SD] CURRENT SI[ \/;.]^,OR,^DENIE[SD] CURRENT SI,OR,^NO SI[ \/;.]^,OR,^NO SI,OR,^NOT SI[\/;.]^,OR,^NOT SI,OR,^DENIE[SD] SUIC^,OR,^DENIE[SD] CURRENT SUIC^,OR,^DENIE[SD] ANY SUIC^,OR,^DENIE[SD] S/H^,OR,^RT SI[ \/;.]^,OR,^RT SI,OR,^RIGHT SI[\ /;.]^,OR,^RIGHT SI,OR,^NOT SUIC^,),)
# Description:
#
#   Created by partners in the CDCs National Center for Injury Prevention to support states and jurisdictions to query visits related to suicidal ideation, or thoughts or plans of engaging in suicide-related behavior.

# CDC Suicide Attempt v1
# Definition:
#   Discharge Diagnosis, Admit Reason Combo, and Chief Complaint History TERMS: (,^[;/ ]T14.91^,or,^[;/ ]T1491^,or,^[;/ ]X7[1-9]^,or,^[;/ ]X8[0-3]^,or,^[;/ ]T3[6-9].[X0-9][X0-9]2^,or,^[;/ ]T3[6-9][X0-9][X0-9]2^,or,^[;/ ]T[4-5][0-9].[X0-9][X0-9]2^,or,^[;/ ]T[4-5][0-9][X0-9][X0-9]2^,or,^[;/ ]T6[0-5].[X0-9][X0-9]2^,or,^[;/ ]T6[0-5][X0-9][X0-9]2^,or,^[;/ ]T71.[X0-9][X0-9]2^,or,^[;/ ]T71[X0-9][X0-9]2^,or,^[;/ ]T3[6-9].[X0-9]2X[ADS]^,or,^[;/ ]T3[6-9][X0-9]2X[ADS]^,or,^[;/ ]T[4-5][0-9].[X0-9]2X[ADS]^,or,^[;/ ]T[4-5][0-9][X0-9]2X[ADS]^,or,^[;/ ]T6[0-5].[X0-9]2X[ADS]^,or,^[;/ ]T6[0-5][X0-9]2X[ADS]^,or,^[;/ ]T71.[X0-9]2X[ADS]^,or,^[;/ ]T71[X0-9]2X[ADS]^,or,^ [;/ ]T50.[ABZ][129]2^, or,^ [;/ ]T50[ABZ][129]2^,or,^[;/ ]E95[0-9]^,or,^82313006^,or,^55554002^,or,^287181000^,or,^891003^,or,^44301001^,or,^53846008^,or^274228002^,or,^86849004^,or,^287182007^,or,^287190007^,or,^269725004^,or,^287181000^,or,^460991000124106^,or,^59274003^,),or,(,(,(,^ATTEMPT^,ANDNOT,^NO ATTEMPT^,),OR,^[;/ ]TRY^,OR,TRY^,OR,^TRIED TO^,OR,(,^INTENTIONAL^,ANDNOT,(,^UNINTENTIONAL^,OR,^ACCIDENTAL^,),),),AND,(,^KILL^,OR,(,^HANG^,ANDNOT,^CHANG^,),OR,^SHOOT^,OR,^OVERDOSE^,OR,^[/ ;.]OD[/ ;.]^,OR,OD,OR,OD[/ ;.]^,OR,^[/ ;.]OD,OR,^ END^LIFE^,OR,^SUICIDE^,OR,^SUIC^,OR,^SUCI^,OR,(,^SUSCI^,ANDNOT,^RESUSCI^,),OR,^SUISID^,),or,SUICIDE,),ANDNOT,(,^END OF LIFE^,OR,^END OF BATTERY LIFE^,OR,^DENIE[SD] SELF HARM^,OR,^NO SELF HARM^,OR,^ACCIDENT^,OR,^HOMICI^,OR,^DENIE[SD] SI[ \/;.]^,OR,^DENIE[SD] SI,OR,^DENIE[SD] ANY SI[ \/;.]^,OR,^DENIE[SD] ANY SI,OR,^DENIE[SD] CURRENT SI[ \/;.]^,OR,^DENIE[SD] CURRENT SI,OR ^NO SI[ \/;.]^,OR,^NO SI,OR,^NOT SI[\/;.]^,OR,^NOT SI,OR,^DENIE[SD] SUIC^,OR,^DENIE[SD] CURRENT SUIC^,OR,^DENIE[SD] ANY SUIC^,OR,^DENIE[SD] S/H^,OR,^RT SI[ \/;.]^,OR,^RT SI,OR,^RIGHT SI[\ /;.]^,OR,^RIGHT SI,OR,^NOT SUIC^,)
# Description:
#
#   Created by partners in the CDCs National Center for Injury Prevention to support states and jurisdictions to query visits related to a suicide attempt, or self-directed and potentially injurious behavior with any intent to die as a result of the behavior.

cdc_suicidal_ideation <- c(
  "R45.851",
  "R45851",
  "V62.84",
  "V6284"
) %>%
  paste(collapse = "|")

cdc_suicidal_attempt <- c(
  "T14.91",
  "T1491",
  "X7",
  "X8",
  "T3",
  "X0",
  "T71.",
  "T71",
  "T6",
  "T50.",
  "T50",
  "E95"
) %>%
  paste(collapse = "|")

(csv_files <- dir_ls(
  path = "",
  recurse = TRUE,
  regexp = "\\.csv$"
))

hdd_data <- csv_files %>%
  map_dfr(
    read_csv,
    .id = "source",
    col_types = cols(.default = "c")
  ) %>%
  clean_names()

glimpse(hdd_data)

str(hdd_data)

class(hdd_data)



hdd_data <- hdd_data %>%
  mutate(
    across(everything(), str_to_lower)
  ) %>%
  mutate(
    discharge_date = ymd(discharge_date),
    discharge_year = year(discharge_date),
    discharge_month = month(discharge_date),
    admission_date = ymd(admission_date),
    birth_date = ymd(birth_date)
  ) 

hdd_data$age_calc <- round(time_length(x = difftime(hdd_data$discharge_date, hdd_data$birth_date), unit = "years"), digits = 0)

# write mortality data to pin board ####
folder_hdd %>%
  pin_write(
    x = hdd_data,
    type = "rds",
    title = "AZDHS Hospital Discharge Data, tidy and transformed",
    description = "The tidy and transformed version of the AZDHS hospital discharge data.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

folder_hdd %>%
  pin_meta("hdd_data")

# subset filter to most recent five years, inclusive of current year 
hdd_data_current <- hdd_data %>%
  filter(discharge_year >= year(Sys.Date())-5) 

# save to pin board 
folder_hdd %>%
  pin_write(
    x = hdd_data_current,
    type = "rds",
    title = "AZDHS Hospital Discharge Data, tidy and transformed, current",
    description = "The tidy and transformed version of the AZDHS hospital discharge data for the most recent five years.",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

hdd_data_current <- folder_hdd %>%
  pin_read("hdd_data_current")

glimpse(hdd_data_current)

hdd_data_current <- hdd_data_current %>%
  mutate(
    code_cdc_suicide_att = if_any(contains("diagnosis"), ~str_detect(.x, str_to_lower(cdc_suicidal_attempt))),
    code_cdc_suicide_idea = if_any(contains("diagnosis"), ~str_detect(.x, str_to_lower(cdc_suicidal_ideation)))
  )

hdd_data_current %>%
  # select(
  #   discharge_year,
  #   code_cdc_suicide_att,
  #   code_cdc_suicide_idea
  # ) %>%
  group_by(discharge_year) %>%
  count(code_cdc_suicide_att, code_cdc_suicide_idea) %>%
  pivot_longer(
    cols = starts_with("code_"),
    names_to = "code",
    values_to = "values"
  ) %>%
  filter(values == "TRUE") %>%
  group_by(discharge_year, code) %>%
  summarize(n = sum(n)) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = discharge_year,
      y = n,
      fill = code
    ),
    position = "dodge"
  )

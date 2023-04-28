# Introduction ####
# read the data provided by the medical examiner ME BRIEF emails and prepare it for inclusion in the report
#
# rené dario herrera
# rherrera at coconino dot az dot gov <- <- <-
# coconino county az
# 10 January 2022

# Setup ####
# packages
library(here)
library(tidyverse)
library(lubridate)
library(janitor)
library(pins)

# load pinboard
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data
me_brief <- read_csv(
  file = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw/me-brief.csv",
  skip = 1,
  col_names = c(
    "date_of_death",
    "medical_examiner_record_no",
    "me_description",
    "me_date_of_exam",
    "decedent_gender_desc",
    "me_age"
  ),
  col_types = c("DccDcc"),
  na = c("", NA, "NA"),
  skip_empty_rows = TRUE
) %>%
  clean_names() %>%
  drop_na(medical_examiner_record_no) %>%
  mutate(
    me_brief = "yes",
    death_book_year = as.character(year(date_of_death)),
    sex = str_to_title(decedent_gender_desc),
    age_calc = as.numeric(me_age)
  ) %>%
  arrange(date_of_death)

# view
glimpse(me_brief)

me_brief <- me_brief %>%
  mutate( # replace blank values with NA
    across(
      .cols = everything(),
      .fns = ~ na_if(x = ., y = "")
    )
  )

# check the count for each year
me_brief %>%
  count(death_book_year)

tail(me_brief)

# assemble description for pin
pin_desc <- str_c(
  "Pending cases identified by the Medical Examiner briefing (",
  as.character(min(me_brief$date_of_death)),
  " - ",
  as.character(max(me_brief$date_of_death)),
  ")."
)

# save to pin board
suicide_data %>%
  pin_write(
    me_brief,
    type = "rds",
    title = "ME Brief, pending cases",
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

# check metadata of saved pin
suicide_data %>%
  pin_meta("me_brief")

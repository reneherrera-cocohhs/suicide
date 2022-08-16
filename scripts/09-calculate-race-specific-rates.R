# Introduction #### 
# This script will:
# calculate race specific rates 
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

# load pinboard by sourcing setup script
suicide_data <- board_folder("") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
# mortality extract data
azdhs_suicide_data <- suicide_data %>%
  pin_read("azdhs_suicide_data_extract")

# read population denominators
azdhs_pop_data_by_sex_gender <- suicide_data %>%
  pin_read("azdhs_pop_data_by_sex_gender")

# population denominator data frame
azdhs_pop_denominator <- azdhs_pop_data_by_sex_gender %>%
  filter(
    area == "Coconino",
    race_ethnicity != "All groups",
    sex == "Total",
    age_group == "total"
  ) %>%
  mutate(
    race_code = case_when( # race_ethnicity ~ race_code
      race_ethnicity == "American Indian or Alaska Native" ~ "American Indian and Alaska Native",
      race_ethnicity == "Asian or Pacific Islander" ~ "Asian or Native Hawaiian and Other Pacific Islander",
      race_ethnicity == "Black or African American" ~ "Black or African American",
      race_ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino (any race)",
      race_ethnicity == "White non-Hispanic" ~ "White Non-Hispanic"
    ),
    estimate = as.numeric(estimate)
  ) %>%
  group_by(year, race_code) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup()

# copy 2020 values to year 2021 and save to new data frame
add_2021 <- azdhs_pop_denominator %>%
  filter(year == "2020") %>%
  mutate(year = "2021")

add_2022 <- azdhs_pop_denominator %>%
  filter(year == "2020") %>%
  mutate(year = "2022")

# add 2021 values to population denominator data frame
azdhs_pop_denominator <- bind_rows(
  azdhs_pop_denominator,
  add_2021,
  add_2022
) %>%
  mutate(race_code = factor(
    case_when(
      race_code == "American Indian and Alaska Native" ~ "American Indian and Alaska Native",
      race_code == "Asian or Native Hawaiian and Other Pacific Islander" ~ "Other",
      race_code == "Black or African American" ~ "Other",
      race_code == "Hispanic or Latino (any race)" ~ "Hispanic or Latino (any race)",
      race_code == "White Non-Hispanic" ~ "White Non-Hispanic"
    )
  )) %>%
  group_by(year, race_code) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup()

# count of death by suicide for each year and race
(by_race_year <- azdhs_suicide_data %>%
  filter(county_resident == "Resident") %>% # county residents only
  mutate(race_code_lump = fct_lump(race_code, n = 3)) %>%
  group_by(death_book_year, race_code_lump) %>%
  count() %>%
  ungroup())

# create new data frame with population denominator and count
(count_by_race_year <- full_join(
  x = by_race_year,
  y = azdhs_pop_denominator,
  by = c(
    "death_book_year" = "year",
    "race_code_lump" = "race_code"
  )
) %>%
  arrange(death_book_year, race_code_lump))

# change NA values to 0 (zero)
count_by_race_year$n[is.na(count_by_race_year$n)] <- 0

# race specific rate, with count and population denominator
(death_by_suicide_coconino_resident_race_specific_rate <- count_by_race_year %>%
  mutate(rate_per_100k = round(100000 * (n / estimate), digits = 1)) 
  # %>% filter(death_book_year %in% analysis_year_range)
  )

# check with plot 
death_by_suicide_coconino_resident_race_specific_rate %>%
  ggplot() +
  geom_line(mapping = aes(
    x = death_book_year,
    y = rate_per_100k,
    color = race_code_lump,
    group = race_code_lump
  )) +
  ylim(0,NA)

# save to pin board
suicide_data %>%
  pin_write(
    x = death_by_suicide_coconino_resident_race_specific_rate,
    title = "Death by suicide, race specific rates",
    description = "Race specific rates of death by suicide for Coconino County Residents.",
    type = "rds",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("death_by_suicide_coconino_resident_race_specific_rate")

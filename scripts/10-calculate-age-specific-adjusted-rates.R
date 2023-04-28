# Introduction ####
# This script will:
# calculate age specific and age adjusted rates
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
library(lubridate)

# load pinboard ####
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
pin_name <- read_rds(
  file = "data-tidy/suicide-data-pin-name.rds"
)

# azdhs mortality data
azdhs_suicide_data_extract <- suicide_data %>%
  pin_read(pin_name)

# how many records by county by year
azdhs_suicide_data_extract %>%
  tabyl(death_book_year, d_county_resident)

# read population denominators
azdhs_pop_data_by_sex_gender <- suicide_data %>%
  pin_read("azdhs_pop_data_by_sex_gender")

# read standard population
us_std_pop <- suicide_data %>% # this creates a new folder 'us_std_pop' at the path shown in the pin metadata
  pin_read("us_std_pop")

# # medical examiner report ####
# # me brief
# me_brief <- suicide_data %>%
#   pin_read("me_brief") %>%
#   filter(
#     date_of_death > ymd(Sys.Date())-months(3) # subset to include only ME cases less than 3 months
#   )
#
# # join with suicide data
# azdhs_suicide_data_extract <- full_join(
#   azdhs_suicide_data_extract,
#   me_brief
# ) %>%
#   distinct(
#     medical_examiner_record_no,
#     date_of_death,
#     .keep_all = TRUE
#   )

# convert "" to NA
azdhs_suicide_data_extract[azdhs_suicide_data_extract == ""] <- NA

#

# view the age groups in population denominators
unique(azdhs_pop_data_by_sex_gender$age_group)

# view the age groups in standard population
unique(us_std_pop$age_group)

# tidy azdhs & standard population ####
# update azdhs population denominators to match standard population
(azdhs_pop_denominator <- azdhs_pop_data_by_sex_gender %>%
  filter(
    area == "Coconino",
    race_ethnicity == "All groups",
    sex == "Total",
    age_group != "total"
  ) %>%
  mutate(
    age_group = str_c(age_group, " years", sep = ""),
    age_group_us_std = case_when(
      age_group == "<1 years" ~ "0-4 years",
      age_group == "1-4 years" ~ "0-4 years",
      TRUE ~ as.character(age_group)
    ),
    estimate = as.numeric(estimate)
  ) %>%
  group_by(year, age_group_us_std) %>%
  summarise(estimate_population = sum(estimate)) %>%
  ungroup() %>%
  arrange(year, age_group_us_std))

# # copy 2020 values to year 2021 and save to new data frame
# add_2021 <- azdhs_pop_denominator %>%
#   filter(year == "2021") %>%
#   mutate(year = "2021")

# copy 2021 values to year 2022 and save to new data frame
add_2022 <- azdhs_pop_denominator %>%
  filter(year == "2021") %>%
  mutate(year = "2022")

add_2023 <- azdhs_pop_denominator %>%
  filter(year == "2021") %>%
  mutate(year = "2023")

# add 2021 values to population denominator data frame
(azdhs_pop_denominator <- bind_rows(
  azdhs_pop_denominator,
  # add_2021,
  add_2022,
  add_2023
))

# azdhs_pop_denominator %>%
#   filter(year == "2021" | year == "2022") %>%
#   filter(str_detect(age_group_us_std, "-2")) %>%
#   knitr::kable()

# view the death by suicide data frame
glimpse(azdhs_suicide_data_extract)

# view the age groups in standard population
unique(azdhs_suicide_data_extract$d_age_group_5yr)

# years of analysis
(analysis_year_range <- c(as.character(unique(azdhs_suicide_data_extract$year_analysis_ll):unique(azdhs_suicide_data_extract$year_analysis_current))))

# summary analysis ####
# before continuing remove records with no death certificate number

# check about the death cert number
azdhs_suicide_data_extract %>%
  mutate(
    d_cert_exist = if_else(
      condition = is.na(death_certificate_number),
      true = 0,
      false = 1
    )
  ) %>%
  tabyl(d_cert_exist)

# drop observations where death certificate is NA
azdhs_suicide_data_extract <- azdhs_suicide_data_extract %>%
  filter(!is.na(death_certificate_number))

# count of death by suicide each year
azdhs_suicide_data_extract %>%
  tabyl(death_book_year, d_county_resident)

# count of death by year for Coconino residents only
(count_by_year <- azdhs_suicide_data_extract %>%
  filter(d_county_resident == "resident") %>% # county residents only
  filter(death_book_year %in% analysis_year_range) %>%
  count(death_book_year))

# pin details
pin_title <- str_c(
  "Count of death by suicide residents only (",
  min(count_by_year$death_book_year),
  "-",
  max(count_by_year$death_book_year),
  ")"
)

pin_desc <- str_c(
  "Count of death by suicide, Coconino residents only (",
  min(count_by_year$death_book_year),
  "-",
  max(count_by_year$death_book_year),
  ")."
)

# save to pin board
suicide_data %>%
  pin_write(
    x = count_by_year,
    name = "azdhs_death_by_suicide_coco_resident_me",
    type = "rds",
    title = pin_title,
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("azdhs_death_by_suicide_coco_resident_me")

# count of death by suicide grouped by year and age group
azdhs_suicide_data_extract %>%
  filter(death_book_year %in% analysis_year_range) %>%
  filter(d_county_resident == "resident") %>% # county residents only
  tabyl(d_age_group_5yr, death_book_year) %>%
  adorn_totals(c("row", "col"))

(by_year_age_group <- azdhs_suicide_data_extract %>%
  filter(death_book_year %in% analysis_year_range) %>%
  filter(d_county_resident == "resident") %>% # county residents only
  group_by(death_book_year, d_age_group_5yr) %>%
  count() %>%
  ungroup())

# join count of death by suicide with population denominators
(by_age_plus_pop_denom <- right_join(
  x = by_year_age_group,
  y = azdhs_pop_denominator,
  by = c(
    "death_book_year" = "year",
    "d_age_group_5yr" = "age_group_us_std"
  )
) %>%
  arrange(death_book_year, d_age_group_5yr) %>%
  filter(death_book_year %in% analysis_year_range))

# change NA values to 0 (zero)
by_age_plus_pop_denom$n[is.na(by_age_plus_pop_denom$n)] <- 0

# view
by_age_plus_pop_denom

# crude rate by year ####
by_age_plus_pop_denom %>%
  filter(death_book_year %in% analysis_year_range) %>%
  group_by(death_book_year) %>%
  summarise(
    n = sum(n),
    population = sum(estimate_population)
  ) %>%
  mutate(crude_rate_per_100k = 100000 * (n / population)) # should year 2020 population be carried over to 2021?

# age specific rate for each age group by year ####
(death_by_suicide_coconino_resident_age_specific_rate <- by_age_plus_pop_denom %>%
  filter(death_book_year %in% analysis_year_range) %>%
  # mutate(
  #   n = case_when(
  #     n == 0 ~ 0,
  #     n < 6 ~ as.double(NA),
  #     n >=6 ~ n
  #   )) %>%
  mutate(age_specific_rate_per_100k = 100000 * (n / estimate_population))
)

# pin info
pin_title <- str_c(
  "Death by suicide, age specific rates (",
  min(death_by_suicide_coconino_resident_age_specific_rate$death_book_year),
  "-",
  max(death_by_suicide_coconino_resident_age_specific_rate$death_book_year),
  ")"
)

pin_desc <- str_c(
  "Age specific rate of death by suicide for Coconino County Residents. Excludes pending cases from the medical examiner (",
  min(death_by_suicide_coconino_resident_age_specific_rate$death_book_year),
  "-",
  max(death_by_suicide_coconino_resident_age_specific_rate$death_book_year),
  ")."
)

# save to pin board
suicide_data %>%
  pin_write(
    x = death_by_suicide_coconino_resident_age_specific_rate,
    title = pin_title,
    description = pin_desc,
    type = "rds",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("death_by_suicide_coconino_resident_age_specific_rate")

# age adjusted rates for each year ####
# join count with standard population
(death_by_suicide_coconino_resident_age_adjusted_rate <- full_join(
  x = by_age_plus_pop_denom,
  y = us_std_pop,
  by = c("d_age_group_5yr" = "age_group")
) %>%
  filter(death_book_year %in% analysis_year_range) %>%
  group_by(death_book_year) %>%
  mutate(pop_percent = standard_pop / sum(standard_pop)) %>%
  ungroup() %>%
  # mutate(
  #   n = if_else(
  #     condition = n >= 6,
  #     true = n,
  #     false = as.double(NA)
  #   )) %>%
  mutate(
    crude_rate = 100000 * (n / estimate_population),
    standardized_rate = pop_percent * crude_rate
  ) %>%
  group_by(death_book_year) %>%
  summarise(
    count_of_deaths = sum(n),
    population_denominator = sum(estimate_population),
    crude_rate_per_100k = round(100000 * (count_of_deaths / population_denominator), digits = 1),
    age_adj_rate_per_100k = round(sum(standardized_rate), digits = 1)
  ) %>%
  ungroup()
# %>% filter(death_book_year %in% analysis_year_range)
)

# pin info
pin_title <- str_c(
  "Death by suicide, age adjusted rates (",
  min(death_by_suicide_coconino_resident_age_adjusted_rate$death_book_year),
  "-",
  max(death_by_suicide_coconino_resident_age_adjusted_rate$death_book_year),
  ")"
)

pin_desc <- str_c(
  "Age adjusted rates of death by suicide for Coconino County Residents. Excludes pending cases from the medical examiner (",
  min(death_by_suicide_coconino_resident_age_adjusted_rate$death_book_year),
  "-",
  max(death_by_suicide_coconino_resident_age_adjusted_rate$death_book_year),
  ")."
)

# save to pin board
suicide_data %>%
  pin_write(
    x = death_by_suicide_coconino_resident_age_adjusted_rate,
    title = pin_title,
    description = pin_desc,
    type = "rds",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("death_by_suicide_coconino_resident_age_adjusted_rate")

# comparison of age adjusted rates between Coconino, AZ, USA ####
# read CDC wonder data from pin
(cdc_wonder_age_adj_rates_usa_az <- suicide_data %>%
  pin_read("cdc_wonder_age_adj_rates_usa_az") %>%
  mutate(year = as.character(year)) %>%
  select(year, geography, age_adjusted_rate)
)

# prepare coconino rates for joining
(coconino_rates <- death_by_suicide_coconino_resident_age_adjusted_rate %>%
  mutate(geography = "Coconino County") %>%
  select(
    year = death_book_year,
    geography,
    age_adjusted_rate = age_adj_rate_per_100k
  )
)

# join and create new data frame
rates_usa_az_coco <- bind_rows(
  cdc_wonder_age_adj_rates_usa_az,
  coconino_rates
) %>%
  arrange(year, geography) %>%
  filter(year %in% analysis_year_range)

# check my work with a plot
rates_usa_az_coco %>%
  ggplot(mapping = aes(
    x = year,
    y = age_adjusted_rate,
    group = geography
  )) +
  geom_line(mapping = aes(color = geography)) +
  ylim(0, NA)

# pin info
pin_title <- str_c(
  "Age adjusted rates, comparison between USA, AZ, & Coconino (",
  min(rates_usa_az_coco$year),
  "-",
  max(rates_usa_az_coco$year),
  ")"
)

pin_desc <- str_c(
  "Comparison of age adjusted rates of death by suicide for the United States, Arizona, and Coconino (",
  min(rates_usa_az_coco$year),
  "-",
  max(rates_usa_az_coco$year),
  ")."
)

# save data frame to pin board
suicide_data %>%
  pin_write(
    x = rates_usa_az_coco,
    name = "death_by_suicide_rate_comparison",
    title = pin_title,
    description = pin_desc,
    type = "rds",
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("death_by_suicide_rate_comparison")

# number of deaths (count) includes ME pending cases
# rate does not

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
azdhs_suicide_data_extract <- suicide_data %>%
  pin_read("azdhs_suicide_data_extract")

# read population denominators
azdhs_pop_data_by_sex_gender <- suicide_data %>%
  pin_read("azdhs_pop_data_by_sex_gender")

# read standard population
us_std_pop <- suicide_data %>% # this creates a new folder 'us_std_pop' at the path shown in the pin metadata
  pin_read("us_std_pop")

# view the age groups in population denominators
unique(azdhs_pop_data_by_sex_gender$age_group)

# view the age groups in standard population
unique(us_std_pop$age_group)

# tidy azdhs & standard population ####
# update azdhs population denominators to match standard population
azdhs_pop_denominator <- azdhs_pop_data_by_sex_gender %>%
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
  ungroup()

# copy 2020 values to year 2021 and save to new data frame
add_2021 <- azdhs_pop_denominator %>%
  filter(year == "2020") %>%
  mutate(year = "2021")

# copy 2020 values to year 2022 and save to new data frame
add_2022 <- azdhs_pop_denominator %>%
  filter(year == "2020") %>%
  mutate(year = "2022")

# add 2021 values to population denominator data frame
azdhs_pop_denominator <- bind_rows(
  azdhs_pop_denominator,
  add_2021,
  add_2022
)

# view the death by suicide data frame
glimpse(azdhs_suicide_data_extract)

# view the age groups in standard population
unique(azdhs_suicide_data_extract$age_group_5yr)

# years of analysis
(analysis_year_range <- c(as.character(unique(azdhs_suicide_data_extract$year_analysis_ll):unique(azdhs_suicide_data_extract$year_analysis_current))))

# summary analysis ####
# count of death by suicide each year including pending cases from medical examiner
(count_by_year <- azdhs_suicide_data_extract %>%
  filter(county_resident == "Resident") %>% # county residents only
  filter(death_book_year %in% analysis_year_range) %>%
  count(death_book_year))

# rename for pin board 
azdhs_death_by_suicide_coco_resident_me <- count_by_year

# save to pin board
suicide_data %>%
  pin_write(
    x = azdhs_death_by_suicide_coco_resident_me,
    type = "rds",
    title = "Count of death by suicide including pending ME cases",
    description = "Count of death by suicide including pending ME cases"
  )

# years of analysis
(analysis_year_range <- c(as.character(unique(azdhs_suicide_data_extract$year_analysis_ll):unique(azdhs_suicide_data_extract$year_analysis_ul))))

# count of death by suicide grouped by year and age group
by_year_age_group <- azdhs_suicide_data_extract %>%
  filter(death_book_year %in% analysis_year_range) %>%
  filter(me_brief == "no") %>%
  filter(county_resident == "Resident") %>% # county residents only
  group_by(death_book_year, age_group_5yr) %>%
  count() %>%
  ungroup()

# join count of death by suicide with population denominators
by_age_plus_pop_denom <- right_join(
  x = by_year_age_group,
  y = azdhs_pop_denominator,
  by = c(
    "death_book_year" = "year",
    "age_group_5yr" = "age_group_us_std"
  )
) %>%
  arrange(death_book_year, age_group_5yr)

# change NA values to 0 (zero)
by_age_plus_pop_denom$n[is.na(by_age_plus_pop_denom$n)] <- 0

# crude rate by year ####
by_age_plus_pop_denom %>%
  group_by(death_book_year) %>%
  summarise(
    n = sum(n),
    population = sum(estimate_population)
  ) %>%
  mutate(crude_rate_per_100k = 100000 * (n / population)) %>% # should year 2020 population be carried over to 2021?
  filter(death_book_year %in% analysis_year_range)

# age specific rate for each age group by year ####
(death_by_suicide_coconino_resident_age_specific_rate <- by_age_plus_pop_denom %>%
  mutate(age_specific_rate_per_100k = 100000 * (n / estimate_population)) %>%
  filter(death_book_year %in% analysis_year_range))

# save to pin board
suicide_data %>%
  pin_write(
    x = death_by_suicide_coconino_resident_age_specific_rate,
    title = "Death by suicide, age specific rates",
    description = "Age specific rate of death by suicide for Coconino County Residents. Excludes pending cases from the medical examiner.",
    type = "rds"
  )

# age adjusted rates for each year ####
# join count with standard population
(death_by_suicide_coconino_resident_age_adjusted_rate <- full_join(
  x = by_age_plus_pop_denom,
  y = us_std_pop,
  by = c("age_group_5yr" = "age_group")
) %>%
  group_by(death_book_year) %>%
  mutate(pop_percent = standard_pop / sum(standard_pop)) %>%
  ungroup() %>%
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
  ungroup() %>%
  filter(death_book_year %in% analysis_year_range))

# save to pin board
suicide_data %>%
  pin_write(
    x = death_by_suicide_coconino_resident_age_adjusted_rate,
    title = "Death by suicide, age adjusted rates",
    description = "Age adjusted rates of death by suicide for Coconino County Residents. Excludes pending cases from the medical examiner.",
    type = "rds"
  )

# comparison of age adjusted rates between Coconino, AZ, USA ####
# read CDC wonder data from pin
cdc_wonder_age_adj_rates_usa_az <- suicide_data %>%
  pin_read("cdc_wonder_age_adj_rates_usa_az") %>%
  mutate(year = as.character(year)) %>%
  select(year, geography, age_adjusted_rate)

# prepare coconino rates for joining
coconino <- death_by_suicide_coconino_resident_age_adjusted_rate %>%
  mutate(geography = "Coconino County") %>%
  select(year = death_book_year, geography, age_adjusted_rate = age_adj_rate_per_100k)

# join and create new data frame
rates_usa_az_coco <- bind_rows(
  cdc_wonder_age_adj_rates_usa_az,
  coconino
) %>%
  filter(year %in% analysis_year_range)

# check my work with a plot
rates_usa_az_coco %>%
  ggplot(mapping = aes(x = year, y = age_adjusted_rate, group = geography)) +
  geom_line(mapping = aes(color = geography))

# rename
(death_by_suicide_rate_comparison <- rates_usa_az_coco)

# save data frame to pin board
suicide_data %>%
  pin_write(
    x = death_by_suicide_rate_comparison,
    title = "Age adjusted rates, comparison between USA, AZ, & Coconino",
    description = "Comparison of age adjusted rates of death by suicide for the United States, Arizona, and Coconino",
    type = "rds"
  )

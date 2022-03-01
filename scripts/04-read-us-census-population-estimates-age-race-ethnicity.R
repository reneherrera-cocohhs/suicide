# Setup ####
# packages
library(here)
library(tidyverse)
library(tidycensus)
library(scales)
library(pins)

# load pin board 
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw")

# view pin board from source above
suicide_data %>%
  pin_list()

# load variables to search for population
v19 <- load_variables(2019, "acs5/subject", cache = TRUE)

# age group ####
# read census api data for population
coco_pop_2016 <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "total" = "S0101_C01_001",
    "0-4 years" = "S0101_C01_002",
    "5-9 years" = "S0101_C01_003",
    "10-14 years" = "S0101_C01_004",
    "15-19 years" = "S0101_C01_005",
    "20-24 years" = "S0101_C01_006",
    "25-29 years" = "S0101_C01_007",
    "30-34 years" = "S0101_C01_008",
    "35-39 years" = "S0101_C01_009",
    "40-44 years" = "S0101_C01_010",
    "45-49 years" = "S0101_C01_011",
    "50-54 years" = "S0101_C01_012",
    "55-59 years" = "S0101_C01_013",
    "60-64 years" = "S0101_C01_014",
    "65-69 years" = "S0101_C01_015",
    "70-74 years" = "S0101_C01_016",
    "75-79 years" = "S0101_C01_017",
    "80-84 years" = "S0101_C01_018",
    "85+ years" =  "S0101_C01_019"
  ),
  cache_table = TRUE,
  year = 2016,
  state = "AZ",
  county = "Coconino"
)

# estimate values are a percent of the total population
# estimate values should be converted to an amount
coco_pop_2016 <- coco_pop_2016 %>%
  filter(variable != "total") %>%
  mutate(
    estimate = 138064 * (estimate / 100),
    year = "2016"
  )

coco_pop_2017 <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "total" = "S0101_C01_001",
    "0-4 years" = "S0101_C01_002",
    "5-9 years" = "S0101_C01_003",
    "10-14 years" = "S0101_C01_004",
    "15-19 years" = "S0101_C01_005",
    "20-24 years" = "S0101_C01_006",
    "25-29 years" = "S0101_C01_007",
    "30-34 years" = "S0101_C01_008",
    "35-39 years" = "S0101_C01_009",
    "40-44 years" = "S0101_C01_010",
    "45-49 years" = "S0101_C01_011",
    "50-54 years" = "S0101_C01_012",
    "55-59 years" = "S0101_C01_013",
    "60-64 years" = "S0101_C01_014",
    "65-69 years" = "S0101_C01_015",
    "70-74 years" = "S0101_C01_016",
    "75-79 years" = "S0101_C01_017",
    "80-84 years" = "S0101_C01_018",
    "85+ years" =  "S0101_C01_019"
  ),
  cache_table = TRUE,
  year = 2017,
  state = "AZ",
  county = "Coconino"
) %>%
  mutate(year = "2017")

coco_pop_2018 <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "total" = "S0101_C01_001",
    "0-4 years" = "S0101_C01_002",
    "5-9 years" = "S0101_C01_003",
    "10-14 years" = "S0101_C01_004",
    "15-19 years" = "S0101_C01_005",
    "20-24 years" = "S0101_C01_006",
    "25-29 years" = "S0101_C01_007",
    "30-34 years" = "S0101_C01_008",
    "35-39 years" = "S0101_C01_009",
    "40-44 years" = "S0101_C01_010",
    "45-49 years" = "S0101_C01_011",
    "50-54 years" = "S0101_C01_012",
    "55-59 years" = "S0101_C01_013",
    "60-64 years" = "S0101_C01_014",
    "65-69 years" = "S0101_C01_015",
    "70-74 years" = "S0101_C01_016",
    "75-79 years" = "S0101_C01_017",
    "80-84 years" = "S0101_C01_018",
    "85+ years" =  "S0101_C01_019"
  ),
  cache_table = TRUE,
  year = 2018,
  state = "AZ",
  county = "Coconino"
) %>%
  mutate(year = "2018")

coco_pop_2019 <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "total" = "S0101_C01_001",
    "0-4 years" = "S0101_C01_002",
    "5-9 years" = "S0101_C01_003",
    "10-14 years" = "S0101_C01_004",
    "15-19 years" = "S0101_C01_005",
    "20-24 years" = "S0101_C01_006",
    "25-29 years" = "S0101_C01_007",
    "30-34 years" = "S0101_C01_008",
    "35-39 years" = "S0101_C01_009",
    "40-44 years" = "S0101_C01_010",
    "45-49 years" = "S0101_C01_011",
    "50-54 years" = "S0101_C01_012",
    "55-59 years" = "S0101_C01_013",
    "60-64 years" = "S0101_C01_014",
    "65-69 years" = "S0101_C01_015",
    "70-74 years" = "S0101_C01_016",
    "75-79 years" = "S0101_C01_017",
    "80-84 years" = "S0101_C01_018",
    "85+ years" =  "S0101_C01_019"
  ),
  cache_table = TRUE,
  year = 2019,
  state = "AZ",
  county = "Coconino"
) %>%
  mutate(year = "2019")

# copy year-2019 data to year-2020
coco_pop_2020 <- coco_pop_2019 %>%
  mutate(year = "2020")

# combine everything together to make one data frame
acs5_coco_population_by_age <- bind_rows(
  coco_pop_2016,
  coco_pop_2017,
  coco_pop_2018,
  coco_pop_2019,
  coco_pop_2020
)

acs5_coco_population_by_age %>%
  filter(variable != "total") %>%
  write_rds("data-output-tidy-processed/us-census-acs5-coconino-population-by-age.rds")

# save to pinboard ####
suicide_data %>% # this creates a new folder 'death_data_ytd' at the path shown in the pin metadata
  pin_write(acs5_coco_population_by_age,
    title = "US Census ACS 5-year population denominators",
    type = "rds",
    description = "US Census American Community Survey, population denominators by age for calendar years 2016-2019 for Coconino County."
  )

# view the pin metadata ####
# historical
suicide_data %>%
  pin_meta("acs5_coco_population_by_age")

####

# race and ethnicity ####
# read census api data for population
coco_pop_2016 <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "total" = "DP05_0065",
    "Hispanic or Latino (any race)" = "DP05_0066",
    "White Non-Hispanic" = "DP05_0072",
    "Black or African American" = "DP05_0073",
    "American Indian and Alaska Native" = "DP05_0074",
    "Asian" = "DP05_0075",
    "Native Hawaiian and Other Pacific Islander" = "DP05_0076"
  ),
  cache_table = TRUE,
  year = 2016,
  state = "AZ",
  county = "Coconino"
) %>%
  mutate(year = "2016")

coco_pop_2017 <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "total" = "DP05_0070",
    "Hispanic or Latino (any race)" = "DP05_0071",
    "White Non-Hispanic" = "DP05_0077",
    "Black or African American" = "DP05_0078",
    "American Indian and Alaska Native" = "DP05_0079",
    "Asian" = "DP05_0080",
    "Native Hawaiian and Other Pacific Islander" = "DP05_0081"
  ),
  cache_table = TRUE,
  year = 2017,
  state = "AZ",
  county = "Coconino"
) %>%
  mutate(year = "2017")

coco_pop_2018 <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "total" = "DP05_0070",
    "Hispanic or Latino (any race)" = "DP05_0071",
    "White Non-Hispanic" = "DP05_0077",
    "Black or African American" = "DP05_0078",
    "American Indian and Alaska Native" = "DP05_0079",
    "Asian" = "DP05_0080",
    "Native Hawaiian and Other Pacific Islander" = "DP05_0081"
  ),
  cache_table = TRUE,
  year = 2018,
  state = "AZ",
  county = "Coconino"
) %>%
  mutate(year = "2018")

coco_pop_2019 <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "total" = "DP05_0070",
    "Hispanic or Latino (any race)" = "DP05_0071",
    "White Non-Hispanic" = "DP05_0077",
    "Black or African American" = "DP05_0078",
    "American Indian and Alaska Native" = "DP05_0079",
    "Asian" = "DP05_0080",
    "Native Hawaiian and Other Pacific Islander" = "DP05_0081"
  ),
  cache_table = TRUE,
  year = 2019,
  state = "AZ",
  county = "Coconino"
) %>%
  mutate(year = "2019")

# the 2020 data is not available from ACS5, look at Decennial census instead
# source: https://data.census.gov/cedsci/table?t=Age%20and%20Sex%3APopulations%20and%20People%3ARace%20and%20Ethnicity&g=0500000US04005&y=2020&tid=DECENNIALPL2020.P4&hidePreview=true
coco_pop_2020 <- tribble(
  ~"GEOID", ~"NAME", ~"variable", ~"estimate", ~"moe", ~"year",
  "04005", "Coconino County, Arizona", "total", 115992, NA, "2020",
  "04005", "Coconino County, Arizona", "Hispanic or Latino (any race)", 15873, NA, "2020",
  "04005", "Coconino County, Arizona", "White Non-Hispanic", 66001, NA, "2020",
  "04005", "Coconino County, Arizona", "Black or African American", 1607, NA, "2020",
  "04005", "Coconino County, Arizona", "American Indian and Alaska Native", 25294, NA, "2020",
  "04005", "Coconino County, Arizona", "Asian", 2247, NA, "2020",
  "04005", "Coconino County, Arizona", "Native Hawaiian and Other Pacific Islander", 201, NA, "2020"
)

# copy year-2020 to year-201
coco_pop_2021 <- coco_pop_2020 %>%
  mutate(year = "2021")

# combine
acs5_coconino_population_by_race <- bind_rows(
  coco_pop_2016,
  coco_pop_2017,
  coco_pop_2018,
  coco_pop_2019,
  coco_pop_2020
)

# standardize race group names
us_census_race_groups <- acs5_coconino_population_by_race %>%
  filter(variable != "total") %>%
  distinct(variable)

# view
us_census_race_groups

# save race groups to disk
write_rds(us_census_race_groups, "data-output-tidy-processed/us_census_race_groups.rds")

# save to pin board
suicide_data %>% # this creates a new folder 'us_census_race_groups' at the path shown in the pin metadata
  pin_write(us_census_race_groups,
    title = "US Census Race Group Names",
    type = "rds",
    description = "List of US Census race and ethnicity group names. See https://www.census.gov/topics/population/race/about.html for more information."
  )

# view pin meta data
suicide_data %>%
  pin_meta("us_census_race_groups")

# recode race group names to match data provided by AZDHS
# "American Indian and Alaska Native", "Hispanic or Latino (any race)", "White Non-Hispanic", "Other"
coconino_population_by_race <- acs5_coconino_population_by_race %>%
  mutate(race_code = case_when(
    variable == "Asian" ~ "Other",
    variable == "Native Hawaiian and Other Pacific Islander" ~ "Other",
    variable == "Black or African American" ~ "Other",
    TRUE ~ as.character(variable)
  ))

# calculate totals for year-race groups
coconino_population_by_race_recode <- coconino_population_by_race %>%
  group_by(year, race_code) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup() %>%
  mutate(year = as.integer(year))

# test vizualization
coconino_population_by_race_recode %>%
  filter(race_code != "total") %>%
  ggplot() +
  geom_col(mapping = aes(x = year, y = estimate, fill = race_code), position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = str_wrap("Coconino County Population Grouped by Race & Ethnicity", width = 60),
    caption = str_wrap("Source: US Census American Community Survey 5-year population estimates (2016-2019), US Decennial Census (2020)", width = 60),
    x = "Year",
    y = "Percentage"
  )

acs5_coco_population_by_race <- coconino_population_by_race_recode

# save to pin board
suicide_data %>% # this creates a new folder 'acs5_coco_population_by_race' at the path shown in the pin metadata
  pin_write(acs5_coco_population_by_race,
    title = "US CensusACS 5-year population denominators by race",
    type = "rds",
    description = "US Census ACS5 population denominators by race for year 2016-2020."
  )

# view pin meta data
suicide_data %>%
  pin_meta("acs5_coco_population_by_race")

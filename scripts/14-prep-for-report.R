# exploratory analysis
# epidemiology 2 x 2 tables and modeling

# Setup ####
# packages
library(here) # project oriented workflow
library(tidyverse) #
library(janitor) # tabyl
library(scales) # date_breaks & date_minor_breaks
library(pins) # read data
library(slider)
library(lubridate)

# load pinboard ####
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

## read data from pin board ####
# azdhs mortality data
# read death by suicide data with categorical data lumps
suicide_extract <- suicide_data %>%
  pin_read("azdhs_suicide_data_extract_plus_categorical_lumps")

# inspect
glimpse(suicide_extract)

suicide_extract %>%
  tabyl(death_book_year)

# pin info
suicide_data %>%
  pin_meta("azdhs_suicide_data_extract_plus_categorical_lumps")

(analysis_year_range <- c(as.character(unique(suicide_extract$year_analysis_ll):unique(suicide_extract$year_analysis_current))))

suicide_in_coco <- suicide_extract %>%
  filter(d_county_resident == "resident") %>%
  filter(death_book_year %in% analysis_year_range) %>%
  # select(date_of_death) %>%
  # glimpse()
  mutate(
    date_of_death = ymd(date_of_death)
  )

# medical examiner report ####
# join with data from the medical examiner
# medical examiner report
me_brief <- suicide_data %>%
  pin_read("me_brief") %>%
  mutate(
    me_brief = 1
  ) %>%
  filter(
    date_of_death > ymd(Sys.Date()) - months(3) # subset to include only ME cases less than 3 months
  )

# join
suicide_coco <- full_join(
  x = suicide_extract,
  y = me_brief,
  by = c(
    "death_book_year",
    "d_date_of_death" = "date_of_death",
    "d_sex" = "decedent_gender_desc",
    "medical_examiner_record_no",
    "calc_age" = "age_calc"
  )
)

# suicide_coco %>%
#   distinct(cert_me_record_date = str_c( # take out duplicates; this does work for the 5-year report because 2013 data is not included
#     # death_certificate_number,
#     medical_examiner_record_no,
#     date_of_death,
#     sep = "-"),
#     .keep_all = TRUE)

# view
glimpse(suicide_coco)

suicide_coco %>%
  tabyl(death_book_year, me_brief)

# update formatting from ME brief
suicide_coco <- suicide_coco %>%
  mutate(
    d_sex = case_when(
      str_detect(str_to_lower(d_sex), "female") ~ "Female",
      str_detect(str_to_lower(d_sex), "male") ~ "Male"
    ),
    d_sex = factor(
      x = d_sex,
      levels = c("Female", "Male")
    )
  )

# # create new unique id
# suicide_coco <- suicide_coco %>%
#   mutate(
#     d_id = str_c(
#       date_of_death,
#       "-",
#       str_to_lower(str_sub(decedent_gender_desc, 1, 1)),
#       "-",
#       if_else(
#         condition = is.na(me_brief),
#         true = "0",
#         false = "1"
#       ),
#       "-",
#       str_to_lower(str_sub(if_else(
#         condition = is.na(medical_examiner_record_no),
#         true = "Unknown",
#         false = medical_examiner_record_no
#       ), 1,7))
#       # "-",
#       # str_to_lower(if_else(
#       #   condition = is.na(death_certificate_number),
#       #   true = "Unknown",
#       #   false = death_certificate_number
#       # ))
#     )
#   )
# # %>%
#   # select(
#   #   d_id,
#   #   date_of_death,
#   #   death_certificate_number,
#   #   decedent_gender_desc,
#   #   medical_examiner_record_no,
#   #   me_brief
#   # ) %>%
#   # arrange(desc(medical_examiner_record_no)) %>%
#   # distinct(
#   #   d_id,
#   #   .keep_all = TRUE
#   # )

# suicide_coco %>%
#   select(
#     death_book_year,
#     date_of_death,
#     decedent_gender_desc,
#     medical_examiner_record_no,
#     me_brief
#     ) %>%
#   arrange(desc(date_of_death), medical_examiner_record_no)
#   # arrange(desc(cert_me_record_date))

me_pending_cases <- suicide_coco %>%
  filter(me_brief == 1) %>%
  count() %>%
  as.character()

pin_title <- str_c(
  "AZDHS Mortality Extract inc. Suicide + ME (",
  min(suicide_coco$death_book_year),
  "-",
  max(suicide_coco$death_book_year),
  ")."
)

pin_desc <- str_c(
  "Subset of the AZDHS Mortality Extract (",
  min(suicide_coco$death_book_year),
  "-",
  max(suicide_coco$death_book_year),
  ") including only death by suicide and additional ",
  me_pending_cases,
  " pending medical examiner cases."
)

# save to pin board ####
suicide_data %>%
  pin_write(
    x = suicide_coco,
    name = "azdhs_suicide_data_me",
    title = pin_title,
    type = "rds",
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("azdhs_suicide_data_me")

# me_brief <- suicide_data %>%
#   pin_read("me_brief") %>%
#   filter(
#     date_of_death > ymd(Sys.Date())-months(6) # subset to include only ME cases less than 6 months
#   )

glimpse(suicide_in_coco)

suicide_in_coco %>%
  tabyl(death_book_year, d_county_resident)

glimpse(me_brief)

me_brief %>%
  tabyl(death_book_year, sex)

# coconino residents only and medical examiner
suicide_data_coco <- full_join(
  x = suicide_in_coco,
  y = me_brief,
  by = c(
    "death_book_year",
    "date_of_death",
    "d_sex" = "sex",
    "medical_examiner_record_no",
    "calc_age" = "age_calc"
  )
) %>%
  distinct(
    medical_examiner_record_no,
    date_of_death,
    .keep_all = TRUE
  )

suicide_data_coco <- suicide_data_coco %>%
  mutate(
    d_sex = case_when( # sex
      str_detect(str_to_lower(d_sex), "female") ~ "Female",
      str_detect(str_to_lower(d_sex), "male") ~ "Male"
    ),
    d_sex = factor(
      x = d_sex,
      levels = c("Female", "Male")
    )
  )

bind_rows(
  suicide_in_coco,
  me_brief
) %>%
  distinct(
    medical_examiner_record_no,
    date_of_death,
    .keep_all = TRUE
  )

glimpse(suicide_data_coco)

# convert "" to NA
suicide_data_coco[suicide_data_coco == ""] <- NA

# inspect
glimpse(suicide_data_coco)

# count of death by suicide including medical examiner ####
(number_of_deaths_by_suicide_by_year <- suicide_data_coco %>%
  # filter(d_county_resident == "resident" | me_brief == 1) %>%
  count(death_book_year))

suicide_data_coco %>%
  # filter(d_county_resident == "resident" | me_brief == "yes") %>%
  tabyl(death_book_year, me_brief)

# pin details
pin_title <- str_c(
  "Slide 04 - Number of Deaths (",
  min(number_of_deaths_by_suicide_by_year$death_book_year),
  "-",
  max(number_of_deaths_by_suicide_by_year$death_book_year),
  ")"
)

pin_desc <- str_c(
  "Data table for slide 04, number of deaths by suicide among coconino county residents and the medical examiner report by year (",
  min(number_of_deaths_by_suicide_by_year$death_book_year),
  "-",
  max(number_of_deaths_by_suicide_by_year$death_book_year),
  ")."
)

# save to pin board
suicide_data %>%
  pin_write(number_of_deaths_by_suicide_by_year,
    title = pin_title,
    type = "rds",
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("number_of_deaths_by_suicide_by_year")

# death by month in current year
year_current <- suicide_data_coco %>%
  drop_na(year_analysis_ul) %>%
  distinct(year_analysis_ul) %>%
  as.character()

azdhs_death_by_suicide_coco_resident_me_ytd <- suicide_data_coco %>%
  filter(death_book_year == year_current)

# pin details
pin_title <- str_c(
  "Death by suicide YTD, by month (",
  year_current,
  ")"
)

pin_desc <- str_c(
  "Dataset of YTD death by suicide for Coconino County residents and includes ME data (",
  year_current,
  ")"
)

# save pin
suicide_data %>%
  pin_write(
    x = azdhs_death_by_suicide_coco_resident_me_ytd,
    title = pin_title,
    type = "rds",
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("azdhs_death_by_suicide_coco_resident_me_ytd")

# count of deaths sourced from medical examiner brief
me_brief_date_start <- suicide_data_coco %>%
  filter(me_brief == 1)

pin_title <- str_c(
  "Medical examiner count by year, starting ",
  min(me_brief_date_start$date_of_death)
)

pin_desc <- str_c(
  "Count of medical examiner deaths by year, starting ",
  min(me_brief_date_start$date_of_death),
  " up to ",
  max(me_brief_date_start$date_of_death)
)

(me_brief_count <- suicide_data_coco %>%
  filter(me_brief == 1) %>%
  count(death_book_year))

# save pin
suicide_data %>%
  pin_write(me_brief_count,
    title = pin_title,
    type = "rds",
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta("me_brief_count")

suicide_data_coco %>%
  count(death_book_year)

## age group levels?
age_group_6cat_levels <- levels(suicide_data_coco$d_age_group_6cat)

# current year data
# recode with levels
suicide_data_coco_2023 <- suicide_data_coco %>%
  filter(death_book_year == year_current) %>%
  distinct(
    medical_examiner_record_no,
    .keep_all = TRUE
  ) %>%
  mutate(
    age_group_6cat = factor(case_when( # for reporting
      calc_age < 14 ~ "Under 14 years",
      calc_age < 18 ~ "14 to 17 years",
      calc_age < 26 ~ "18 to 25 years",
      calc_age < 45 ~ "26 to 44 years",
      calc_age < 65 ~ "45 to 64 years",
      calc_age >= 65 ~ "65 years and over",
      TRUE ~ as.character(calc_age)
    ), levels = age_group_6cat_levels, ordered = TRUE)
  ) %>%
  drop_na(age_group_6cat) %>%
  # select(calc_age, age_group_6cat) %>%
  # arrange(calc_age) %>%
  # print(n = 46)
  mutate(age_group_6cat = fct_lump_min(
    f = age_group_6cat,
    min = 6
  )) %>%
  count(age_group_6cat)

suicide_data_coco_ytd <- suicide_data_coco %>%
  filter(death_book_year == year_current) %>%
  distinct(
    medical_examiner_record_no,
    .keep_all = TRUE
  ) %>%
  mutate(
    age_group_6cat = factor(case_when( # for reporting
      calc_age < 14 ~ "Under 14 years",
      calc_age < 18 ~ "14 to 17 years",
      calc_age < 26 ~ "18 to 25 years",
      calc_age < 45 ~ "26 to 44 years",
      calc_age < 65 ~ "45 to 64 years",
      calc_age >= 65 ~ "65 years and over",
      TRUE ~ as.character(calc_age)
    ), levels = age_group_6cat_levels, ordered = TRUE)
  ) %>%
  drop_na(age_group_6cat) %>%
  # select(calc_age, age_group_6cat) %>%
  # arrange(calc_age) %>%
  # print(n = 46)
  mutate(age_group_6cat = fct_lump_min(
    f = age_group_6cat,
    min = 6
  ))

# pin details
pin_title <- str_c(
  "Suicide count by age group in current year (",
  min(suicide_data_coco_ytd$death_book_year),
  "), residents only, inc. medical examiner."
)

pin_desc <- pin_title

pin_name <- str_c(
  "suicide-count-x-age-coco-resident-me-",
  min(suicide_data_coco_ytd$death_book_year)
)

suicide_data_coco_ytd <- suicide_data_coco_ytd %>%
  count(age_group_6cat)

# save pin
suicide_data %>%
  pin_write(
    x = suicide_data_coco_ytd,
    name = pin_name,
    title = pin_title,
    type = "rds",
    description = pin_desc,
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

suicide_data %>%
  pin_meta(pin_name)

# # Summary statistics ####
# suicide_data_coco %>%
#   glimpse()
# 
# # age
# summary(suicide_data_coco$calc_age)
# 
# suicide_data_coco %>%
#   drop_na(calc_age, d_sex, race_code_lump) %>%
#   ggplot() +
#   geom_boxplot(
#     mapping = aes(
#       x = death_book_year,
#       y = calc_age,
#       fill = d_sex
#     )
#   ) +
#   facet_wrap(~race_code_lump) +
#   ylim(0,NA)
# 
# # age groups
# suicide_data_coco %>%
#   tabyl(
#     d_age_group_6cat
#   )
# 
# # race and ethnicity
# suicide_data_coco %>%
#   tabyl(
#     d_race_code
#   )
# 
# # manner of death
# suicide_data_coco %>%
#   tabyl(
#     cdc_mannerofdeath_desc
#   )
# 
# suicide_data_coco %>%
#   select(contains("edu")) %>%
#   names()
# 
# # education
# suicide_data_coco %>%
#   tabyl(
#     d_edu_code
#   )
# 
# # occupation
# suicide_data_coco %>%
#   mutate(
#     occupation_code = fct_lump(d_occupation_code, n = 10)
#   ) %>%
#   tabyl(
#     occupation_code,
#     show_na = FALSE
#   )
# 
# # industry
# suicide_data_coco %>%
#   mutate(
#     industry_code = fct_lump(d_industry_code, n = 10)
#   ) %>%
#   tabyl(
#     industry_code,
#     show_na = FALSE
#   )
# 
# # years in arizona
# suicide_data_coco %>%
#   mutate(years_in_arizona = as.numeric(years_in_arizona)) %>%
#   select(years_in_arizona) %>%
#   as_vector() %>%
#   summary()
# 
# # marital code
# suicide_data_coco %>%
#   tabyl(
#     d_marital_code
#   )
# 
# # sex
# suicide_data_coco %>%
#   tabyl(
#     d_sex
#   )
# 
# # county residence
# suicide_data_coco %>%
#   tabyl(
#     d_county_resident
#   )
# 
# # tribal
# suicide_data_coco %>%
#   tabyl(
#     d_tribal_code
#   )
# 
# # funeral code
# suicide_data_coco %>%
#   mutate(
#     funeral_code = fct_lump(d_funeral_code, n = 10)
#   ) %>%
#   tabyl(
#     funeral_code,
#     show_na = FALSE
#   )
# 
# # cross tabulations
# # age x sex
# suicide_data_coco %>%
#   tabyl(
#     d_age_group_6cat, d_sex
#   ) %>%
#   adorn_percentages()
# 
# # age x race
# suicide_data_coco %>%
#   tabyl(
#     d_age_group_6cat, d_race_code
#   ) %>%
#   adorn_percentages()
# 
# # age x edu
# suicide_data_coco %>%
#   filter(d_age_group_6cat %in% c(
#     "26 to 44 years",
#     "45 to 64 years",
#     "65 years and over"
#   )) %>%
#   tabyl(
#     d_age_group_6cat, d_edu_code,
#     show_missing_levels = FALSE
#   ) %>%
#   adorn_percentages()
# 
# # age x marital
# suicide_data_coco %>%
#   tabyl(
#     d_age_group_6cat, d_marital_code
#   ) %>%
#   adorn_percentages()
# 
# # age x manner of death
# table_manner_by_age <- suicide_data_coco %>%
#   drop_na(method_code_cdc_lump, d_age_group_6cat) %>%
#   tabyl(
#     method_code_cdc_lump, d_age_group_6cat
#   ) %>%
#   adorn_percentages()
# 
# fisher.test(table_manner_by_age)
# 
# chisq.test(table_manner_by_age)
# 
# t.test(calc_age ~ sex, data = suicide_data_coco)
# 
# # sex x manner of death
# table_manner_by_sex <- suicide_data_coco %>%
#   drop_na(cdc_mannerofdeath_desc, sex) %>%
#   tabyl(
#     cdc_mannerofdeath_desc, sex
#   ) %>%
#   adorn_percentages()
# 
# chisq.test(table_manner_by_sex)
# 
# fisher.test(table_manner_by_sex)
# 
# # race x manner of death
# table_manner_by_race <- suicide_data_coco %>%
#   drop_na(cdc_mannerofdeath_desc, race_code) %>%
#   tabyl(
#     cdc_mannerofdeath_desc, race_code
#   ) %>%
#   adorn_percentages()
# 
# chisq.test(table_manner_by_race)
# 
# fisher.test(table_manner_by_race)
# 
# # edu x manner of death
# (table_manner_by_edu <- suicide_data_coco %>%
#   drop_na(cdc_mannerofdeath_desc, edu_code) %>%
#   tabyl(
#     cdc_mannerofdeath_desc, edu_code
#   ) %>%
#   adorn_percentages())
# 
# chisq.test(table_manner_by_edu)
# 
# fisher.test(table_manner_by_edu)
# 
# # marital x manner of death
# suicide_data_coco %>%
#   tabyl(
#     cdc_mannerofdeath_desc, marital_code
#   ) %>%
#   adorn_percentages()
# 
# # epi curve ####
# #
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(aes(x = date_of_death))
# 
# # daily
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(x = date_of_death),
#     binwidth = 1
#   ) +
#   labs(
#     title = "Deaths per day (Inclusive of 2017-2021)",
#     subtitle = "Coconino County"
#   )
# 
# # weekly
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(x = date_of_death),
#     binwidth = 7
#   ) +
#   labs(
#     title = "Deaths per Week (Inclusive of 2017-2021)",
#     subtitle = "Coconino County"
#   )
# 
# weekly_breaks <- seq.Date(
#   from = as.Date("2017-01-01"),
#   to = as.Date("2022-01-01"),
#   by = "weeks"
# )
# 
# # monthly
# monthly_breaks <- seq.Date(
#   from = as.Date("2017-01-01"),
#   to = as.Date("2022-01-01"),
#   by = "months"
# )
# 
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(x = date_of_death),
#     breaks = monthly_breaks
#   ) +
#   scale_x_date(
#     date_breaks = "year",
#     date_minor_breaks = "3 months"
#   ) +
#   labs(
#     title = "Deaths per Month (Inclusive of 2017-2021)",
#     subtitle = "Coconino County"
#   )
# 
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(x = date_of_death),
#     breaks = weekly_breaks
#   ) +
#   scale_x_date(
#     date_breaks = "year",
#     date_minor_breaks = "3 months"
#   ) +
#   labs(
#     title = "Deaths per Week (Inclusive of 2017-2021)",
#     subtitle = "Coconino County"
#   )
# 
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(
#       x = date_of_death,
#       group = sex,
#       fill = sex
#     ),
#     breaks = monthly_breaks,
#     color = NA
#   ) +
#   scale_x_date(
#     date_breaks = "year",
#     date_minor_breaks = "3 months"
#   ) +
#   labs(
#     title = "Deaths per Month (Inclusive of 2017-2021)",
#     subtitle = "Coconino County grouped by Sex"
#   ) +
#   theme(legend.position = "bottom")
# 
# suicide_data_coco %>%
#   filter(suicide == "yes") %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(x = date_of_death),
#     breaks = monthly_breaks
#   ) +
#   scale_x_date(
#     date_breaks = "year",
#     date_minor_breaks = "3 months"
#   ) +
#   labs(
#     title = "Suicide Deaths per Month (Inclusive of 2017-2021)",
#     subtitle = "Coconino County"
#   )
# 
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(
#       x = date_of_death,
#       group = race_code_lump,
#       fill = race_code_lump
#     ),
#     breaks = monthly_breaks,
#     color = NA
#   ) +
#   scale_x_date(
#     date_breaks = "year",
#     date_minor_breaks = "3 months"
#   ) +
#   labs(
#     title = "Deaths per Month (Inclusive of 2017-2021)",
#     subtitle = "Coconino County grouped by Race"
#   ) +
#   theme(legend.position = "bottom")
# 
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(
#       x = date_of_death,
#       group = age_group_6cat,
#       fill = age_group_6cat
#     ),
#     breaks = monthly_breaks,
#     color = NA
#   ) +
#   scale_x_date(
#     date_breaks = "year",
#     date_minor_breaks = "3 months"
#   ) +
#   labs(
#     title = "Deaths per Month (Inclusive of 2017-2021)",
#     subtitle = "Coconino County grouped by Age"
#   ) +
#   theme(legend.position = "bottom")
# 
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(
#       x = date_of_death,
#       group = age_group_5yr,
#       fill = age_group_5yr
#     ),
#     breaks = monthly_breaks,
#     color = NA
#   ) +
#   scale_x_date(
#     date_breaks = "year",
#     date_minor_breaks = "3 months"
#   ) +
#   labs(
#     title = "Deaths per Month (Inclusive of 2017-2021)",
#     subtitle = "Coconino County grouped by Age"
#   ) +
#   theme(legend.position = "bottom")
# 
# suicide_data_coco %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(
#       x = date_of_death,
#       group = suicide,
#       fill = suicide
#     ),
#     breaks = monthly_breaks,
#     color = NA
#   ) +
#   scale_x_date(
#     date_breaks = "year",
#     date_minor_breaks = "3 months"
#   ) +
#   labs(
#     title = "Deaths per Month (Inclusive of 2017-2021)",
#     subtitle = "Coconino County grouped by Suicide"
#   ) +
#   theme(legend.position = "bottom")
# 
# # 30 day moving average
# suicide_data_coco %>%
#   count(date_of_death, name = "count") %>%
#   complete(
#     date_of_death = seq.Date(as.Date("2018-01-01"), as.Date("2022-12-31"), by = "day"),
#     fill = list(count = 0),
#     explicit = FALSE
#   ) %>%
#   mutate(
#     avg_30day = slide_index(
#       count,
#       .i = date_of_death,
#       .f = ~ mean(.x, na.rm = TRUE),
#       .before = 29,
#       .complete = FALSE
#     ),
#     avg_30day = unlist(avg_30day)
#   ) %>%
#   ggplot() +
#   geom_histogram(
#     mapping = aes(
#       x = date_of_death,
#       y = count
#     ),
#     stat = "identity",
#     breaks = monthly_breaks
#   ) +
#   geom_line(
#     mapping = aes(
#       x = date_of_death,
#       y = avg_30day,
#       lty = "30 day rolling avg"
#     ),
#     color = "red",
#     size = 1
#   )
# 
# # demographic pyramid
# suicide_data_coco %>%
#   drop_na(age_group_5yr, sex) %>%
#   tabyl(age_group_5yr, sex, show_missing_levels = FALSE)
# 
# # library(apyramid)
# #
# # age_pyramid( # this isn't working yet
# #   data = table_age_pyramid,
# #   age_group = age_group_5yr,
# #   split_by = sex
# # )
# 
# # risk ratios ####
# # install.packages("epitools")
# library(epitools)
# 
# rrtable <- suicide_data_coco %>%
#   filter(military_service != "unknown") %>%
#   drop_na(military_service, suicide) %>%
#   tabyl(
#     suicide, military_service
#   ) %>%
#   as.matrix()
# 
# rrtable <- table(
#   suicide_data_coco$suicide,
#   suicide_data_coco$military_service
# )
# 
# riskratio.wald(
#   rrtable
# )
# 
# # hypothesis testing ####
# # males with no high school education and suicide risk
# # males with no high school diploma are at a higher risk of suicide than males with a high school diploma
# suicide_male_hs <- suicide_data_coco %>%
#   drop_na(sex, hs_grad, suicide) %>%
#   filter(sex == "Male") %>%
#   transmute(
#     male = if_else(
#       sex == "Male", 1, 0
#     ),
#     no_hs = if_else(
#       hs_grad == "No", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   ) %>%
#   mutate(
#     exposure = if_else(
#       male == 1 & no_hs == 1, 1, 0
#     )
#   )
# 
# suicide_male_hs %>%
#   tabyl(
#     exposure,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_male_hs$exposure,
#   suicide_male_hs$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_male_hs$exposure,
#     suicide_male_hs$suicide
#   )
# ) # fail to reject
# 
# # military service and suicide risk
# # death by suicide is more likely to occur with military service
# suicide_military <- suicide_data_coco %>%
#   drop_na(military_service, suicide) %>%
#   transmute(
#     military = if_else(
#       military_service == "yes", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_military %>%
#   tabyl(
#     military,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_military$military,
#   suicide_military$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_military$military,
#     suicide_military$suicide
#   )
# ) # reject the null; therefore this indicates military service may be protective agains death by suicide
# 
# # male sex and suicide risk
# # male are more likely to die by suicide compared to female
# suicide_male <- suicide_data_coco %>%
#   drop_na(sex, suicide) %>%
#   transmute(
#     male = if_else(
#       sex == "Male", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_male %>%
#   tabyl(
#     male,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_male$male,
#   suicide_male$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_male$male,
#     suicide_male$suicide
#   )
# ) # reject the null; male are more likely to die by suicide than female
# 
# # county resident and suicide risk
# # suicide are more likely to be resident than non-resident
# suicide_resident <- suicide_data_coco %>%
#   drop_na(county_resident, suicide) %>%
#   transmute(
#     county_resident = if_else(
#       county_resident == "Resident", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_resident %>%
#   tabyl(
#     county_resident,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_resident$county_resident,
#   suicide_resident$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_resident$county_resident,
#     suicide_resident$suicide
#   )
# ) # fail to reject
# 
# # native american and suicide risk
# # native american are more likely to die from suicide than non native american
# suicide_native <- suicide_data_coco %>%
#   drop_na(race_code_lump, suicide) %>%
#   transmute(
#     native_american = if_else(
#       race_code_lump == "American Indian and Alaska Native", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_native %>%
#   tabyl(
#     native_american,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_native$native_american,
#   suicide_native$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_native$native_american,
#     suicide_native$suicide
#   )
# ) # reject the null; native american race & ethnicity are less likely to die by suicide than others
# 
# # college grad and suicide risk
# # college grad are more likely to die from suicide than non college grad
# suicide_hs <- suicide_data_coco %>%
#   drop_na(college_grad, suicide) %>%
#   transmute(
#     college_grad = if_else(
#       college_grad == "yes", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_hs %>%
#   tabyl(
#     college_grad,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_hs$college_grad,
#   suicide_hs$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_hs$college_grad,
#     suicide_hs$suicide
#   )
# ) # fail to reject
# 
# # married and suicide risk
# # married are less likely to die from suicide than unmarried
# suicide_married <- suicide_data_coco %>%
#   drop_na(marital_code, suicide) %>%
#   transmute(
#     married = if_else(
#       marital_code == "Married", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_married %>%
#   tabyl(
#     married,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_married$married,
#   suicide_married$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_married$married,
#     suicide_married$suicide
#   )
# ) # reject, people who are currently married are less likely to die by suicide
# 
# # unmarried male and suicide risk
# # unmarried males are more likely to die from suicide than unmarried
# suicide_unmarried_male <- suicide_data_coco %>%
#   filter(sex == "Male") %>%
#   drop_na(sex, marital_code, suicide) %>%
#   transmute(
#     unmarried = if_else(
#       marital_code == "Married", 0, 1
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_unmarried_male %>%
#   tabyl(
#     unmarried,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_unmarried_male$unmarried,
#   suicide_unmarried_male$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_unmarried_male$unmarried,
#     suicide_unmarried_male$suicide
#   )
# ) # reject, unmarried males are more likely to die by suicide than married males
# 
# # young adult male and suicide risk
# # young adult males are more likely to die from suicide than males of over ages
# suicide_ya_male <- suicide_data_coco %>%
#   filter(sex == "Male") %>%
#   drop_na(sex, age_group_6cat, suicide) %>%
#   transmute(
#     ya_male = if_else(
#       age_group_6cat == "18 to 25 years", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_ya_male %>%
#   tabyl(
#     ya_male,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_ya_male$ya_male,
#   suicide_ya_male$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_ya_male$ya_male,
#     suicide_ya_male$suicide
#   )
# ) # reject, young males age 18-25 are more likely to die by suicide than males in other age groups
# 
# # teen girls and suicide risk
# # teenage girls are more likely to die from suicide than teenage boys
# suicide_teen_girl <- suicide_data_coco %>%
#   filter(age_group_6cat == "14 to 17 years") %>%
#   drop_na(sex, age_group_6cat, suicide) %>%
#   transmute(
#     teen_girl = if_else(
#       sex == "Female", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_teen_girl %>%
#   tabyl(
#     teen_girl,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_teen_girl$teen_girl,
#   suicide_teen_girl$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_teen_girl$teen_girl,
#     suicide_teen_girl$suicide
#   )
# ) # fail to reject
# 
# # military women and suicide risk
# # women with military service are more likely to die from suicide than non-military service women
# suicide_military_female <- suicide_data_coco %>%
#   filter(sex == "Female") %>%
#   drop_na(sex, military_service, suicide) %>%
#   transmute(
#     military_service = if_else(
#       military_service == "yes", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_military_female %>%
#   tabyl(
#     military_service,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_military_female$military_service,
#   suicide_military_female$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_military_female$military_service,
#     suicide_military_female$suicide
#   )
# ) # fail to reject
# 
# # military men and suicide risk
# # men with military service are more likely to die from suicide than non-military service men
# suicide_military_male <- suicide_data_coco %>%
#   filter(sex == "Male") %>%
#   drop_na(sex, military_service, suicide) %>%
#   transmute(
#     military_service = if_else(
#       military_service == "yes", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_military_male %>%
#   tabyl(
#     military_service,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_military_male$military_service,
#   suicide_military_male$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_military_male$military_service,
#     suicide_military_male$suicide
#   )
# ) # reject, men with history of military service are less likely to die by suicide than men without
# 
# glimpse(suicide_data_coco)
# levels(suicide_data_coco$age_group_6cat)
# 
# # adults 45-64 and suicide risk
# # adults 45-64 are more likely to die from suicide than adults of over ages
# suicide_45_to_64 <- suicide_data_coco %>%
#   filter(age_group_6cat != "Under 14 years" & age_group_6cat != "14 to 17 years") %>%
#   drop_na(age_group_6cat, suicide) %>%
#   transmute(
#     age_45_to_64 = if_else(
#       age_group_6cat == "45 to 64 years", 1, 0
#     ),
#     suicide = if_else(
#       suicide == "yes", 1, 0
#     )
#   )
# 
# suicide_45_to_64 %>%
#   tabyl(
#     age_45_to_64,
#     suicide
#   )
# 
# chisq.test(table(
#   suicide_45_to_64$age_45_to_64,
#   suicide_45_to_64$suicide
# ))
# 
# fisher.test(
#   table(
#     suicide_45_to_64$age_45_to_64,
#     suicide_45_to_64$suicide
#   )
# ) # fail to reject

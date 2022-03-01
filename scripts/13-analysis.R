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

# load pinboard ####
suicide_data <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/Suicide/data-raw") # suicide_data

# list the pins located on the pin board ####
suicide_data %>%
  pin_list()

# read data from pin board ####
# azdhs mortality data
azdhs_mortality_extract <- suicide_data %>%
  pin_read("azdhs_mortality_extract")

# pin info
suicide_data %>%
  pin_meta("azdhs_mortality_extract")

(analysis_year_range <- c(as.character(unique(azdhs_mortality_extract$year_analysis_ll):unique(azdhs_mortality_extract$year_analysis_ul))))

# filter to recent years
azdhs_mortality_extract <- azdhs_mortality_extract %>%
  filter(death_book_year %in% analysis_year_range)

# convert "" to NA
azdhs_mortality_extract[azdhs_mortality_extract == ""] <- NA

# inspect 
glimpse(azdhs_mortality_extract)

# Summary statistics #### 
# age
summary(azdhs_mortality_extract$age_calc)

azdhs_mortality_extract %>%
  drop_na(age_calc, sex) %>%
  ggplot() +
  geom_boxplot(mapping = aes(
    x = death_book_year,
    y = age_calc,
    fill = sex
  )) +
  facet_wrap(~race_code_lump)

# age groups
azdhs_mortality_extract %>%
  tabyl(
    age_group_6cat
  )

# race and ethnicity 
azdhs_mortality_extract %>%
  tabyl(
    race_code
  )

# manner of death 
azdhs_mortality_extract %>%
  tabyl(
    cdc_mannerofdeath_desc
  )

# education 
azdhs_mortality_extract %>%
  tabyl(
    edu_code_lump
  )

# occupation 
azdhs_mortality_extract %>%
  mutate(
    occupation_code = fct_lump(occupation_code, n = 10)
  ) %>%
  tabyl(
    occupation_code,
    show_na = FALSE
  )

# industry
azdhs_mortality_extract %>%
  mutate(
    industry_code = fct_lump(industry_code, n = 10)
  ) %>%
  tabyl(
    industry_code,
    show_na = FALSE
  )

# years in arizona 
azdhs_mortality_extract %>%
  mutate(years_in_arizona = as.numeric(years_in_arizona)) %>%
  select(years_in_arizona) %>%
  as_vector() %>%
  summary()

# marital code 
azdhs_mortality_extract %>%
  tabyl(
    marital_code
  )

# sex 
azdhs_mortality_extract %>%
  tabyl(
    sex
  )

# county residence 
azdhs_mortality_extract %>%
  tabyl(
    county_resident
  )

# tribal
azdhs_mortality_extract %>%
  tabyl(
    tribal_code
  )

# funeral code
azdhs_mortality_extract %>%
  mutate(
    funeral_code = fct_lump(funeral_code, n = 10)
  ) %>%
  tabyl(
    funeral_code,
    show_na = FALSE
  )

# cross tabulations 
# age x sex 
azdhs_mortality_extract %>%
  tabyl(
    age_group_6cat, sex
  ) %>%
  adorn_percentages()

# age x race 
azdhs_mortality_extract %>%
  tabyl(
    age_group_6cat, race_code_lump
  ) %>%
  adorn_percentages()

# age x edu
azdhs_mortality_extract %>%
  filter(age_group_6cat %in% c(
    "26 to 44 years",
    "45 to 64 years",
    "65 years and over"
  )) %>%
  tabyl(
    age_group_6cat, edu_code_lump, show_missing_levels = FALSE
  ) %>%
  adorn_percentages()

# age x marital 
azdhs_mortality_extract %>%
  tabyl(
    age_group_6cat, marital_code
  ) %>%
  adorn_percentages()

# age x manner of death 
table_manner_by_age <- azdhs_mortality_extract %>%
  drop_na(cdc_mannerofdeath_desc, age_group_6cat) %>%
  tabyl(
    cdc_mannerofdeath_desc, age_group_6cat
  ) %>%
  adorn_percentages() 

fisher.test(table_manner_by_age)

chisq.test(table_manner_by_age)

t.test(age_calc ~ sex, data = azdhs_mortality_extract)

# sex x manner of death 
table_manner_by_sex <- azdhs_mortality_extract %>%
  drop_na(cdc_mannerofdeath_desc, sex) %>%
  tabyl(
    cdc_mannerofdeath_desc, sex
  ) %>%
  adorn_percentages()

chisq.test(table_manner_by_sex)

fisher.test(table_manner_by_sex)

# race x manner of death 
table_manner_by_race <- azdhs_mortality_extract %>%
  drop_na(cdc_mannerofdeath_desc, race_code) %>%
  tabyl(
    cdc_mannerofdeath_desc, race_code
  ) %>%
  adorn_percentages()

chisq.test(table_manner_by_race)

fisher.test(table_manner_by_race)

# edu x manner of death 
table_manner_by_edu <- azdhs_mortality_extract %>%
  drop_na(cdc_mannerofdeath_desc, edu_code) %>%
  tabyl(
    cdc_mannerofdeath_desc, edu_code
  ) %>%
  adorn_percentages()

chisq.test(table_manner_by_edu)

fisher.test(table_manner_by_edu)

# marital x manner of death 
azdhs_mortality_extract %>%
  tabyl(
    cdc_mannerofdeath_desc, marital_code
  ) %>%
  adorn_percentages()

# epi curve #### 
# 
azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(aes(x = date_of_death))

# daily 
azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(x = date_of_death),
    binwidth = 1
    ) +
  labs(
    title = "Deaths per day (Inclusive of 2017-2021)",
    subtitle = "Coconino County"
  )

# weekly 
azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(x = date_of_death),
    binwidth = 7
  ) +
  labs(
    title = "Deaths per Week (Inclusive of 2017-2021)",
    subtitle = "Coconino County"
  )

weekly_breaks <- seq.Date(from = as.Date("2017-01-01"),
                           to = as.Date("2022-01-01"),
                           by = "weeks")

# monthly 
monthly_breaks <- seq.Date(from = as.Date("2017-01-01"),
                           to = as.Date("2022-01-01"),
                           by = "months")

azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(x = date_of_death),
    breaks = monthly_breaks
  ) +
  scale_x_date(
    date_breaks = "year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Deaths per Month (Inclusive of 2017-2021)",
    subtitle = "Coconino County"
  )

azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(x = date_of_death),
    breaks = weekly_breaks
  ) +
  scale_x_date(
    date_breaks = "year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Deaths per Week (Inclusive of 2017-2021)",
    subtitle = "Coconino County"
  )

azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(
      x = date_of_death,
      group = sex,
      fill = sex
    ),
    breaks = monthly_breaks,
    color = NA
  ) +
  scale_x_date(
    date_breaks = "year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Deaths per Month (Inclusive of 2017-2021)",
    subtitle = "Coconino County grouped by Sex"
  ) +
  theme(legend.position = "bottom")

azdhs_mortality_extract %>%
  filter(suicide == "yes") %>%
  ggplot() +
  geom_histogram(
    mapping = aes(x = date_of_death),
    breaks = monthly_breaks
  ) +
  scale_x_date(
    date_breaks = "year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Suicide Deaths per Month (Inclusive of 2017-2021)",
    subtitle = "Coconino County"
  )

azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(
      x = date_of_death,
      group = race_code_lump,
      fill = race_code_lump
    ),
    breaks = monthly_breaks,
    color = NA
  ) +
  scale_x_date(
    date_breaks = "year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Deaths per Month (Inclusive of 2017-2021)",
    subtitle = "Coconino County grouped by Race"
  ) +
  theme(legend.position = "bottom")

azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(
      x = date_of_death,
      group = age_group_6cat,
      fill = age_group_6cat
    ),
    breaks = monthly_breaks,
    color = NA
  ) +
  scale_x_date(
    date_breaks = "year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Deaths per Month (Inclusive of 2017-2021)",
    subtitle = "Coconino County grouped by Age"
  ) +
  theme(legend.position = "bottom")

azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(
      x = date_of_death,
      group = age_group_5yr,
      fill = age_group_5yr
    ),
    breaks = monthly_breaks,
    color = NA
  ) +
  scale_x_date(
    date_breaks = "year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Deaths per Month (Inclusive of 2017-2021)",
    subtitle = "Coconino County grouped by Age"
  ) +
  theme(legend.position = "bottom")

azdhs_mortality_extract %>%
  ggplot() +
  geom_histogram(
    mapping = aes(
      x = date_of_death,
      group = suicide,
      fill = suicide
    ),
    breaks = monthly_breaks,
    color = NA
  ) +
  scale_x_date(
    date_breaks = "year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Deaths per Month (Inclusive of 2017-2021)",
    subtitle = "Coconino County grouped by Suicide"
  ) +
  theme(legend.position = "bottom")

# 30 day moving average 
azdhs_mortality_extract %>%
  count(date_of_death, name = "count") %>%
  drop_na(date_of_death) %>%
  mutate(
    avg_30day = slide_index(
      count,
      .i = date_of_death,
      .f = ~mean(.x, na.rm = TRUE),
      .before = 29,
      .complete = FALSE
    ),
    avg_30day = unlist(avg_30day)
  ) %>%
  ggplot() +
  geom_histogram(
    mapping = aes(
      x = date_of_death,
      y = count
    ),
    stat = "identity",
    breaks = monthly_breaks
  ) +
  geom_line(
    mapping = aes(
      x = date_of_death,
      y = avg_30day,
      lty = "30 day rolling avg"
    ),
    color = "red",
    size = 1
  )

# demographic pyramid 
azdhs_mortality_extract %>%
  drop_na(age_group_5yr, sex) %>%
  tabyl(age_group_5yr, sex, show_missing_levels = FALSE) 

library(apyramid)

age_pyramid( # this isn't working yet
  data = table_age_pyramid,
  age_group = age_group_5yr,
  split_by = sex
)  

# risk ratios #### 
install.packages("epitools")
library(epitools)

rrtable <- azdhs_mortality_extract %>%
  filter(military_service != "unknown") %>%
  drop_na(military_service, suicide) %>%
  tabyl(
    suicide, military_service
  ) %>%
  as.matrix()

rrtable <- table(
  azdhs_mortality_extract$suicide,
  azdhs_mortality_extract$military_service
)

riskratio.wald(
  rrtable
)

# hypothesis testing #### 
# males with no high school education and suicide risk 
# males with no high school diploma are at a higher risk of suicide than males with a high school diploma
suicide_male_hs <- azdhs_mortality_extract %>%
  drop_na(sex, hs_grad, suicide) %>%
  filter(sex == "Male") %>%
  transmute(
    male = if_else(
      sex == "Male", 1, 0
    ),
    no_hs = if_else(
      hs_grad == "No", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) %>%
  mutate(
    exposure = if_else(
      male == 1 & no_hs == 1, 1, 0
    )
  )

suicide_male_hs %>%
  tabyl(
    exposure,
    suicide
  )

chisq.test(table(
  suicide_male_hs$exposure,
  suicide_male_hs$suicide
))

fisher.test(
  table(
    suicide_male_hs$exposure,
    suicide_male_hs$suicide
  )
) # fail to reject 

# military service and suicide risk 
# death by suicide is more likely to occur with military service
suicide_military <- azdhs_mortality_extract %>%
  drop_na(military_service, suicide) %>%
  transmute(
    military = if_else(
      military_service == "yes", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_military %>%
  tabyl(
    military,
    suicide
  )

chisq.test(table(
  suicide_military$military,
  suicide_military$suicide
))

fisher.test(
  table(
    suicide_military$military,
    suicide_military$suicide
  )
) # reject the null; therefore this indicates military service may be protective agains death by suicide 

# male sex and suicide risk 
# male are more likely to die by suicide compared to female
suicide_male <- azdhs_mortality_extract %>%
  drop_na(sex, suicide) %>%
  transmute(
    male = if_else(
      sex == "Male", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_male %>%
  tabyl(
    male,
    suicide
  )

chisq.test(table(
  suicide_male$male,
  suicide_male$suicide
))

fisher.test(
  table(
    suicide_male$male,
    suicide_male$suicide
  )
) # reject the null; male are more likely to die by suicide than female 

# county resident and suicide risk 
# suicide are more likely to be resident than non-resident
suicide_resident <- azdhs_mortality_extract %>%
  drop_na(county_resident, suicide) %>%
  transmute(
    county_resident = if_else(
      county_resident == "Resident", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_resident %>%
  tabyl(
    county_resident,
    suicide
  )

chisq.test(table(
  suicide_resident$county_resident,
  suicide_resident$suicide
))

fisher.test(
  table(
    suicide_resident$county_resident,
    suicide_resident$suicide
  )
) # fail to reject 

# native american and suicide risk 
# native american are more likely to die from suicide than non native american
suicide_native <- azdhs_mortality_extract %>%
  drop_na(race_code_lump, suicide) %>%
  transmute(
    native_american = if_else(
      race_code_lump == "American Indian and Alaska Native", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_native %>%
  tabyl(
    native_american,
    suicide
  )

chisq.test(table(
  suicide_native$native_american,
  suicide_native$suicide
))

fisher.test(
  table(
    suicide_native$native_american,
    suicide_native$suicide
  )
) # reject the null; native american race & ethnicity are less likely to die by suicide than others 

# college grad and suicide risk 
# college grad are more likely to die from suicide than non college grad
suicide_hs <- azdhs_mortality_extract %>%
  drop_na(college_grad, suicide) %>%
  transmute(
    college_grad = if_else(
      college_grad == "yes", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_hs %>%
  tabyl(
    college_grad,
    suicide
  )

chisq.test(table(
  suicide_hs$college_grad,
  suicide_hs$suicide
))

fisher.test(
  table(
    suicide_hs$college_grad,
    suicide_hs$suicide
  )
) # fail to reject 

# married and suicide risk 
# married are less likely to die from suicide than unmarried
suicide_married <- azdhs_mortality_extract %>%
  drop_na(marital_code, suicide) %>%
  transmute(
    married = if_else(
      marital_code == "Married", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_married %>%
  tabyl(
    married,
    suicide
  )

chisq.test(table(
  suicide_married$married,
  suicide_married$suicide
))

fisher.test(
  table(
    suicide_married$married,
    suicide_married$suicide
  )
) # reject, people who are currently married are less likely to die by suicide 

# unmarried male and suicide risk 
# unmarried males are more likely to die from suicide than unmarried
suicide_unmarried_male <- azdhs_mortality_extract %>%
  filter(sex == "Male") %>%
  drop_na(sex, marital_code, suicide) %>%
  transmute(
    unmarried = if_else(
      marital_code == "Married", 0, 1
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_unmarried_male %>%
  tabyl(
    unmarried,
    suicide
  )

chisq.test(table(
  suicide_unmarried_male$unmarried,
  suicide_unmarried_male$suicide
))

fisher.test(
  table(
    suicide_unmarried_male$unmarried,
    suicide_unmarried_male$suicide
  )
) # reject, unmarried males are more likely to die by suicide than married males  

# young adult male and suicide risk 
# young adult males are more likely to die from suicide than males of over ages
suicide_ya_male <- azdhs_mortality_extract %>%
  filter(sex == "Male") %>%
  drop_na(sex, age_group_6cat, suicide) %>%
  transmute(
    ya_male = if_else(
      age_group_6cat == "18 to 25 years", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_ya_male %>%
  tabyl(
    ya_male,
    suicide
  )

chisq.test(table(
  suicide_ya_male$ya_male,
  suicide_ya_male$suicide
))

fisher.test(
  table(
    suicide_ya_male$ya_male,
    suicide_ya_male$suicide
  )
) # reject, young males age 18-25 are more likely to die by suicide than males in other age groups  

# teen girls and suicide risk 
# teenage girls are more likely to die from suicide than teenage boys
suicide_teen_girl <- azdhs_mortality_extract %>%
  filter(age_group_6cat == "14 to 17 years") %>%
  drop_na(sex, age_group_6cat, suicide) %>%
  transmute(
    teen_girl = if_else(
      sex == "Female", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_teen_girl %>%
  tabyl(
    teen_girl,
    suicide
  )

chisq.test(table(
  suicide_teen_girl$teen_girl,
  suicide_teen_girl$suicide
))

fisher.test(
  table(
    suicide_teen_girl$teen_girl,
    suicide_teen_girl$suicide
  )
) # fail to reject 

# military women and suicide risk 
# women with military service are more likely to die from suicide than non-military service women
suicide_military_female <- azdhs_mortality_extract %>%
  filter(sex == "Female") %>%
  drop_na(sex, military_service, suicide) %>%
  transmute(
    military_service = if_else(
      military_service == "yes", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_military_female %>%
  tabyl(
    military_service,
    suicide
  )

chisq.test(table(
  suicide_military_female$military_service,
  suicide_military_female$suicide
))

fisher.test(
  table(
    suicide_military_female$military_service,
    suicide_military_female$suicide
  )
) # fail to reject 

# military men and suicide risk 
# men with military service are more likely to die from suicide than non-military service men
suicide_military_male <- azdhs_mortality_extract %>%
  filter(sex == "Male") %>%
  drop_na(sex, military_service, suicide) %>%
  transmute(
    military_service = if_else(
      military_service == "yes", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_military_male %>%
  tabyl(
    military_service,
    suicide
  )

chisq.test(table(
  suicide_military_male$military_service,
  suicide_military_male$suicide
))

fisher.test(
  table(
    suicide_military_male$military_service,
    suicide_military_male$suicide
  )
) # reject, men with history of military service are less likely to die by suicide than men without 

glimpse(azdhs_mortality_extract)
levels(azdhs_mortality_extract$age_group_6cat)

# adults 45-64 and suicide risk 
# adults 45-64 are more likely to die from suicide than adults of over ages
suicide_45_to_64 <- azdhs_mortality_extract %>%
  filter(age_group_6cat != "Under 14 years" & age_group_6cat != "14 to 17 years") %>%
  drop_na(age_group_6cat, suicide) %>%
  transmute(
    age_45_to_64 = if_else(
      age_group_6cat == "45 to 64 years", 1, 0
    ),
    suicide = if_else(
      suicide == "yes", 1, 0
    )
  ) 

suicide_45_to_64 %>%
  tabyl(
    age_45_to_64,
    suicide
  )

chisq.test(table(
  suicide_45_to_64$age_45_to_64,
  suicide_45_to_64$suicide
))

fisher.test(
  table(
    suicide_45_to_64$age_45_to_64,
    suicide_45_to_64$suicide
  )
) # fail to reject 


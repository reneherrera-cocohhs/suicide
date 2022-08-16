# README


This project is intended to print a report detailing recent trends in death by suicide in Coconino County and for Coconino County residents by querying the AZ Department of Health mortality extract data. Aside from yearly counts, the variables of interest relate to:


- comparison to state and national rates
- comparison of count between deaths in Coconino County and from Coconino Residents
- Count and rate of city of residence
- rate of zip code
- age groups
- race and ethnicity
- sex
- method of injury


## Workflow


1. Setup the pin board
2. Setup the package libraries and colors needed.
3. Import and read mortality data provided by AZDHS
4. Import and read population denominators:
    - US Standard population
    - AZDHS population denominator
    - US Census population denominator
5. Import and read age adjusted suicide death rates for USA & AZ from CDC Wonder
6. Import and read list of deaths reported by the medical examiner
7. Tidy the mortality extract
    - join all AZDHS mortality data
    - select variables of interest
8. Transform the mortality extract
    - code new variables
9. Calculate age specific adjusted rates
10. Calculate race specific rates
11. Analyze quantitative
12. Analyze qualitative and categorical
13. Analyze geospatial
14. Geocode injury locations
15. Analysis
16. Prepare RMarkdown Report 


## Analytic Procedures


1. Where appropriate, provide rates or percentages.
2. Compute risk scores
3. Rank individual risk scores and overall risk index


## Guiding Questions


1. Where does the county show more risk?
2. Where does the county show more protection?
3. Where do you see trends for success and any trends of concern?
4. Who is your audience and what platforms will you use for presentation? Does that change the story?


## Demographic Information


1. Total population
2. population age 17 and younger
3. racial and ethnic composition



## Risk Indicators  


- Displayed as a column chart with positive and negative values for each risk indicator deviating from 0



## Predictive modeling


- how multiple variables work together to influence substance use and mental health outcomes
- population / geographic level predictors and individual level predictors
-

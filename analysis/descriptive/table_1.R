######################################

# This script:
# - Produces counts of patients prescribed antipsychotic by demographic characteristics between prior to April 2021.
# - saves data summaries (as table)

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
library('gt')
library('gtsummary')

## Create output directory
dir.create(here::here("output", "tables"), showWarnings = FALSE, recursive=TRUE)

## Custom functions
source(here("analysis", "lib", "custom_functions.R"))

## Read in data
data_cohort <- arrow::read_feather(here::here("output", "data", "input_2021-04-01.feather"))


## Table 1 ----
counts_table1 <- data_cohort %>% 
  mutate(
    
    # Sex
    sex = as.character(sex),
    sex = ifelse(sex %in% c("F", "M"), sex, "Other/Unknown"),
    
    sex = fct_case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      sex == "Other/Unknown" ~ "Other/Unknown",
      TRUE ~ NA_character_
    ),
    
    # Ethnicity
    ethnicity = as.character(eth2001),
    ethnicity = ifelse(is.na(eth2001), "Missing", ethnicity),
    ethnicity = fct_case_when(
      ethnicity == "1" ~ "White",
      ethnicity == "2" ~ "Mixed",
      ethnicity == "3" ~ "Asian or Asian British",
      ethnicity == "4" ~ "Black or Black British",
      ethnicity == "5" ~ "Other ethnic groups",
      ethnicity == "Missing" ~ "Missing",
      #TRUE ~ "Unknown"
      TRUE ~ NA_character_),
    
    # IMD
    imd = na_if(imd, "0"),
    imd = fct_case_when(
      imd == 1 ~ "1 most deprived",
      imd == 2 ~ "2",
      imd == 3 ~ "3",
      imd == 4 ~ "4",
      imd == 5 ~ "5 least deprived",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # Region
    region = fct_case_when(
      region == "London" ~ "London",
      region == "East" ~ "East of England",
      region == "East Midlands" ~ "East Midlands",
      region == "North East" ~ "North East",
      region == "North West" ~ "North West",
      region == "South East" ~ "South East",
      region == "South West" ~ "South West",
      region == "West Midlands" ~ "West Midlands",
      region == "Yorkshire and The Humber" ~ "Yorkshire and the Humber",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_),
    
    # Age
    ageband = cut(age,
                  breaks = c(0, 17, 24, 34, 44, 54, 69, 79, Inf),
                  labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-69", "70-79", "80+"),
                  right = FALSE)) %>%
  filter(age >= 0,
         age < 110,
         sex %in% c("M", "F")) %>%
  select(antipsychotic = antipsychotic_any,
         ageband, 
         sex,
         region,
         imd,
         ethnicity) %>%
  tbl_summary(by = antipsychotic) %>%
  add_overall()


# Save table 1 ----
write_csv(counts_table1$table_body, here::here("output",  "tables", "table1.csv"))


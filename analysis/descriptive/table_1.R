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

## Create output directory
dir.create(here::here("output", "tables"), showWarnings = FALSE, recursive=TRUE)

## Custom functions
source(here("analysis", "lib", "custom_functions.R"))


# Read in and format data ----
data_cohort <- read_rds(here::here("output", "data", "data_cohort.rds"))

## Counts
counts_table1 <- data_cohort %>% 
  mutate(ethnicity = ifelse(is.na(ethnicity), 6, ethnicity),
         ethnicity = fct_case_when(
           ethnicity == "1" ~ "White",
           ethnicity == "2" ~ "Mixed",
           ethnicity == "3" ~ "Asian or Asian British",
           ethnicity == "4" ~ "Black or Black British",
           ethnicity == "5" ~ "Other ethnic groups",
           ethnicity == "6" ~ "Unknown",
           #TRUE ~ "Unknown"
           TRUE ~ NA_character_),
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
           TRUE ~ NA_character_)) %>%
  mutate(ageband = cut(age,
                       breaks = c(0, 17, 24, 34, 44, 54, 69, 79, Inf),
                       labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-69", "70-79", "80+"),
                       right = FALSE)) %>%
  select(antipsychotic,
         ageband, 
         sex,
         region,
         imd,
         ethnicity) %>%
  tbl_summary(by = antipsychotic) %>%
  add_overall()


# Save table 1 ----
write_csv(counts_table1$table_body, here::here("output",  "tables", "table1.csv"))

  
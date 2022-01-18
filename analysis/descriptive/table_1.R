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
december <- arrow::read_feather(here::here("output", "data", "input_2021-12-01.feather"))

november <- arrow::read_feather(here::here("output", "data", "input_2021-11-01.feather")) %>%
  filter(!patient_id %in% december$patient_id)

october <- data_extract <- arrow::read_feather(here::here("output", "data", "input_2021-10-01.feather")) %>%
  filter(!patient_id %in% c(november$patient_id, december$patient_id))


# Process data ----
data_cohort <- rbind(december, november, october)

## Table 1 ----
counts_table1 <- data_cohort %>% 
  mutate(
    
    # Sex
    sex = as.character(sex),
    sex = fct_case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      TRUE ~ NA_character_
    ),
    
    # Ethnicity
    ethnicity = as.character(eth2001),
    ethnicity = ifelse(is.na(eth2001), "17", ethnicity),
    ethnicity = fct_case_when(
      ethnicity == "1" ~ "British or Mixed British",
      ethnicity == "2" ~ "Irish",
      ethnicity == "3" ~ "Other White",
      ethnicity == "4" ~ "White + Black Caribbean",
      ethnicity == "5" ~ "White + Black African",
      ethnicity == "6" ~ "White + Asian",
      ethnicity == "7" ~ "Other mixed",
      ethnicity == "8" ~ "Indian or British Indian",
      ethnicity == "9" ~ "Pakistani or British Pakistani",
      ethnicity == "10" ~ "Bangladeshi or British Bangladeshi",
      ethnicity == "11" ~ "Other Asian",
      ethnicity == "12" ~ "Caribbean",
      ethnicity == "13" ~ "African",
      ethnicity == "14" ~ "Other Black",
      ethnicity == "15" ~ "Chinese",
      ethnicity == "16" ~ "Other",
      ethnicity == "17" ~ "Unknown",
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
  select(antipsychotic = antipsychotic_any,
         ageband, 
         sex,
         region,
         imd,
         ethnicity) %>%
  tbl_summary(by = antipsychotic) %>%
  add_overall()


# Redaction ----

## Suppress cells that are less than a threshold (tbd as well as the next smallest value in a group if there is only one)
threshold = 8

table1_redacted <- counts_table1$table_body %>%
  select(group = variable, variable = label, total = stat_0, nonantipsychotic = stat_1,	antipsychotic =stat_2) %>%
  separate(total, c("total","perc"), sep = "([(])") %>%
  separate(nonantipsychotic, c("nonantipsychotic","perc2"), sep = "([(])") %>%
  separate(antipsychotic, c("antipsychotic","perc3"), sep = "([(])") %>%
  mutate(total = as.numeric(gsub(",", "", total)),
         nonantipsychotic = as.numeric(gsub(",", "", nonantipsychotic)),
         antipsychotic = as.numeric(gsub(",", "", antipsychotic))) %>%
  filter(!(is.na(total))) %>%
  select(-perc, -perc2, -perc3) %>%
  mutate(total = ifelse(total < threshold, NA, total),
         nonantipsychotic = ifelse(nonantipsychotic < threshold | is.na(total), NA, nonantipsychotic),
         antipsychotic = ifelse(antipsychotic < threshold | is.na(nonantipsychotic) | is.na(total), NA, antipsychotic))

## Round to nearest 5
table1_redacted <- table1_redacted %>%
  mutate(total = plyr::round_any(total, 5),
         nonantipsychotic = plyr::round_any(nonantipsychotic, 5),
         antipsychotic = plyr::round_any(antipsychotic, 5))

## Replace na with [REDACTED]
table1_redacted <- table1_redacted %>%
  mutate(total = ifelse(is.na(total), "[REDACTED]", total),
         nonantipsychotic = ifelse(is.na(nonantipsychotic), "[REDACTED]", nonantipsychotic),
         antipsychotic = ifelse(is.na(antipsychotic), "[REDACTED]", antipsychotic))


# Save table 1 ----
write_csv(table1_redacted, here::here("output",  "tables", "table1_redacted.csv"))


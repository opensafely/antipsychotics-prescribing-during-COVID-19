######################################

# This script:
# - Produces counts of patients prescribed antipsychotic by cohrt and demographic characteristics prior to April 2021.
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
data_cohort <- read_rds(here::here("output", "data", "data_processed.rds"))


# Table 2 ----

## All
table_2_all <- data_cohort %>% 
  mutate(ethnicity = as.character(ethnicity),
         ethnicity = ifelse(is.na(ethnicity), "Missing", ethnicity),
         ethnicity = fct_case_when(
           ethnicity == "White" ~ "White",
           ethnicity == "Mixed" ~ "Mixed",
           ethnicity == "Asian or Asian British" ~ "Asian or Asian British",
           ethnicity == "Black or Black British" ~ "Black or Black British",
           ethnicity == "Other ethnic groups" ~ "Other ethnic groups",
           ethnicity == "Unknown" ~ "Unknown",
           ethnicity == "Missing" ~ "Missing",
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

table_2_all <- table_2_all$table_body %>%
  select(group = variable, variable = label, population = stat_0, antipsychotic = stat_2) %>%
  separate(population, c("population","perc"), sep = "([(])") %>%
  separate(antipsychotic, c("antipsychotic","perc2"), sep = "([(])") %>%
  mutate(population = gsub(" ", "", population),
         population = as.numeric(gsub(",", "", population)),
         antipsychotic = gsub(" ", "", antipsychotic),
         antipsychotic = as.numeric(gsub(",", "", antipsychotic))) %>%
  filter(!(is.na(population)),
         !(is.na(antipsychotic))) %>%
  select(-perc, - perc2) %>%
  mutate(rate = antipsychotic/population*100000)

## Sub groups
table2_autism <- calculate_table2(population = "autism", Y = 10000)
table2_care_home <- calculate_table2(population = "care_home", Y = 10000)
table2_dementia <- calculate_table2(population = "dementia", Y = 10000)
table2_ld <- calculate_table2(population = "learning_disability", Y = 10000)
table2_smi <- calculate_table2(population = "serious_mental_illness", Y = 10000)


# Save tables ----
write_csv(table_2_all, here::here("output",  "tables", "table_2_all.csv"))
write_csv(table2_autism, here::here("output",  "tables", "table2_autism.csv"))
write_csv(table2_dementia, here::here("output",  "tables", "table2_dementia.csv"))
write_csv(table2_care_home, here::here("output",  "tables", "table2_care_home.csv"))
write_csv(table2_ld, here::here("output",  "tables", "table2_ld.csv"))
write_csv(table2_smi, here::here("output",  "tables", "table2_smi.csv"))
######################################

# This script:
# - imports data extracted by the cohort extractor
# - combines ethnicity columns
# - calculates absolute number of antipsychotics issued each group
# - standardises some variables (eg convert to factor) and derives some new ones
# - saves processed dataset(s)

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('arrow')
library('here')

## Custom functions
source(here("analysis", "custom_functions.R"))

## Output processed data to rds
dir.create(here::here("output", "data", "processed"), showWarnings = FALSE, recursive=TRUE)


# Process data ----
filenames <- list.files(path = here::here("output", "data"), pattern = "input_2")

# Totals dataset
data_totals <- rbind(lapply(filenames, cohort = "all", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "learning_disability", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "autism", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "serious_mental_illness", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "care_home", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "dementia", calculate_totals) %>% bind_rows())

# Totals (incident) dataset(s)
data_incident_1y <- rbind(lapply(filenames, cohort = "all", calculate_incident_1y) %>% bind_rows(),
                     lapply(filenames, cohort = "learning_disability", calculate_incident_1y) %>% bind_rows(),
                     lapply(filenames, cohort = "autism", calculate_incident_1y) %>% bind_rows(),
                     lapply(filenames, cohort = "serious_mental_illness", calculate_incident_1y) %>% bind_rows(),
                     lapply(filenames, cohort = "care_home", calculate_incident_1y) %>% bind_rows(),
                     lapply(filenames, cohort = "dementia", calculate_incident_1y) %>% bind_rows())

data_incident_2y <- rbind(lapply(filenames, cohort = "all", calculate_incident_2y) %>% bind_rows(),
                          lapply(filenames, cohort = "learning_disability", calculate_incident_2y) %>% bind_rows(),
                          lapply(filenames, cohort = "autism", calculate_incident_2y) %>% bind_rows(),
                          lapply(filenames, cohort = "serious_mental_illness", calculate_incident_2y) %>% bind_rows(),
                          lapply(filenames, cohort = "care_home", calculate_incident_2y) %>% bind_rows(),
                          lapply(filenames, cohort = "dementia", calculate_incident_2y) %>% bind_rows())

# Measures datasets
measures_all <- lapply(filenames, cohort = "all", calculate_measures) %>% bind_rows()
measures_learning_disability <- lapply(filenames, cohort = "learning_disability", calculate_measures) %>% bind_rows()
measures_autism <- lapply(filenames, cohort = "autism", calculate_measures) %>% bind_rows()
measures_serious_mental_illness <- lapply(filenames, cohort = "serious_mental_illness", calculate_measures) %>% bind_rows()
measures_care_home <- lapply(filenames, cohort = "care_home", calculate_measures) %>% bind_rows()
measures_dementia <- lapply(filenames, cohort = "dementia", calculate_measures) %>% bind_rows()


# Save datasets ----

## Totals data as .rds files
write_rds(data_totals, here::here("output", "data", "data_totals.rds"), compress="gz")

## Totals (incident) data as .rds files
write_rds(data_incident_1y, here::here("output", "data", "data_incident_1y.rds"), compress="gz")
write_rds(data_incident_2y, here::here("output", "data", "data_incident_2y.rds"), compress="gz")

## Measures data as csvs
write_csv(measures_all, here::here("output", "data", "custom_measures_all.csv"))
write_csv(measures_learning_disability, here::here("output", "data", "custom_measures_learning_disability.csv"))
write_csv(measures_autism , here::here("output", "data", "custom_measures_autism.csv"))
write_csv(measures_serious_mental_illness, here::here("output", "data", "custom_measures_serious_mental_illness.csv"))
write_csv(measures_care_home, here::here("output", "data", "custom_measures_care_home.csv"))
write_csv(measures_dementia, here::here("output", "data", "custom_measures_dementia.csv"))



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
source(here("analysis", "lib", "custom_functions.R"))

## Output processed data to rds
dir.create(here::here("output", "data", "processed"), showWarnings = FALSE, recursive=TRUE)


# Process data ----
filenames <- list.files(path = here::here("output", "data"), pattern = "input_2")

## Totals dataset by groups
data_totals <- rbind(lapply(filenames, cohort = "all", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "learning_disability", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "autism", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "serious_mental_illness", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "care_home", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "dementia", calculate_totals) %>% bind_rows())

## Totals dataset by demographic
sex <- lapply(filenames, cohort = "sex", calculate_totals) %>% bind_rows()
imd <- lapply(filenames, cohort = "imd", calculate_totals) %>% bind_rows()
ethnicity <- lapply(filenames, cohort = "ethnicity", calculate_totals) %>% bind_rows()
region <- lapply(filenames, cohort = "region", calculate_totals) %>% bind_rows()
age <- lapply(filenames, cohort = "age", calculate_totals) %>% bind_rows()


## Totals (incident) dataset(s) by group
data_incident_1yr <- rbind(lapply(filenames, cohort = "all", calculate_incident_1yr) %>% bind_rows(),
                     lapply(filenames, cohort = "learning_disability", calculate_incident_1yr) %>% bind_rows(),
                     lapply(filenames, cohort = "autism", calculate_incident_1yr) %>% bind_rows(),
                     lapply(filenames, cohort = "serious_mental_illness", calculate_incident_1yr) %>% bind_rows(),
                     lapply(filenames, cohort = "care_home", calculate_incident_1yr) %>% bind_rows(),
                     lapply(filenames, cohort = "dementia", calculate_incident_1yr) %>% bind_rows())

data_incident_2yr <- rbind(lapply(filenames, cohort = "all", calculate_incident_2yr) %>% bind_rows(),
                          lapply(filenames, cohort = "learning_disability", calculate_incident_2yr) %>% bind_rows(),
                          lapply(filenames, cohort = "autism", calculate_incident_2yr) %>% bind_rows(),
                          lapply(filenames, cohort = "serious_mental_illness", calculate_incident_2yr) %>% bind_rows(),
                          lapply(filenames, cohort = "care_home", calculate_incident_2yr) %>% bind_rows(),
                          lapply(filenames, cohort = "dementia", calculate_incident_2yr) %>% bind_rows())

## Whole cohort
data_cohort <- calculate_data_cohort(filenames)
data_processed <- data_cohort %>%
  filter(age < 110,
         sex %in% c("M", "F")) 


# Save datasets ----

## Totals data as .rds files
write_rds(data_totals, here::here("output", "data", "data_totals_groups.rds"), compress="gz")
saveRDS(list(sex = sex, 
             imd = imd, 
             ethnicity = ethnicity, 
             region = region, 
             age = age),
        here::here("output", "data", "data_totals_demographics.rds"))

## Totals (incident) data as .rds files
saveRDS(list(data_incident_1yr, data_incident_2yr), here::here("output", "data", "data_incident_groups.rds"))

## Whole cohort
write_rds(data_processed, here::here("output", "data", "data_processed.rds"), compress="gz")

## Flow chart data
write_rds(data_cohort, here::here("output", "data", "data_flow_chart.rds"), compress="gz")

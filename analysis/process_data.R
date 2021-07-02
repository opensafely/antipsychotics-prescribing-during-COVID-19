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

# Totals dataset by groups
data_totals <- rbind(lapply(filenames, cohort = "all", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "learning_disability", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "autism", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "serious_mental_illness", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "care_home", calculate_totals) %>% bind_rows(),
                         lapply(filenames, cohort = "dementia", calculate_totals) %>% bind_rows())

# Totals dataset by demographic
sex <- lapply(filenames, cohort = "sex", calculate_totals) %>% bind_rows()
imd <- lapply(filenames, cohort = "imd", calculate_totals) %>% bind_rows()
ethnicity <- lapply(filenames, cohort = "ethnicity", calculate_totals) %>% bind_rows()
stp <- lapply(filenames, cohort = "stp", calculate_totals) %>% bind_rows()
region <- lapply(filenames, cohort = "region", calculate_totals) %>% bind_rows()
age <- lapply(filenames, cohort = "age", calculate_totals) %>% bind_rows()


# Totals (incident) dataset(s) by group
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

# # Measures datasets
# measures_all <- lapply(filenames, cohort = "all", calculate_measures) %>% bind_rows()
# measures_learning_disability <- lapply(filenames, cohort = "learning_disability", calculate_measures) %>% bind_rows()
# measures_autism <- lapply(filenames, cohort = "autism", calculate_measures) %>% bind_rows()
# measures_serious_mental_illness <- lapply(filenames, cohort = "serious_mental_illness", calculate_measures) %>% bind_rows()
# measures_care_home <- lapply(filenames, cohort = "care_home", calculate_measures) %>% bind_rows()
# measures_dementia <- lapply(filenames, cohort = "dementia", calculate_measures) %>% bind_rows()

# Flow chart
filenames_flow_chart <- list.files(path = here::here("output", "data"), pattern = "input_flow_chart_")[-1]

flow_chart_cohort <- arrow::read_feather(here::here("output", "data", "input_flow_chart_2019-02-01.feather"))  %>%
  mutate(date = as.Date("2019-01-01", format = "%Y-%m-%d"))

for (i in 1:length(filenames_flow_chart)){
  
  data_extract <- arrow::read_feather(
    here::here("output", "data", filenames_flow_chart[i])) %>%
    mutate(date = as.Date(substr(filenames_flow_chart[i], 18, 27), format = "%Y-%m-%d")) %>%
    filter(!patient_id %in% flow_chart_cohort$patient_id)
  
  flow_chart_cohort <- rbind(flow_chart_cohort, data_extract)
  
}


# Save datasets ----

## Totals data as .rds files
write_rds(data_totals, here::here("output", "data", "data_totals_groups.rds"), compress="gz")
saveRDS(list(sex = sex, 
             imd = imd, 
             ethnicity = ethnicity, 
             stp = stp, 
             region = region, 
             age =age),
        here::here("output", "data", "data_totals_demographics.rds"))

## Totals (incident) data as .rds files
saveRDS(list(data_incident_1yr, data_incident_2yr), here::here("output", "data", "data_incident_groups.rds"))

# ## Measures data as csvs
# write_csv(measures_all, here::here("output", "data", "custom_measures_all.csv"))
# write_csv(measures_learning_disability, here::here("output", "data", "custom_measures_learning_disability.csv"))
# write_csv(measures_autism , here::here("output", "data", "custom_measures_autism.csv"))
# write_csv(measures_serious_mental_illness, here::here("output", "data", "custom_measures_serious_mental_illness.csv"))
# write_csv(measures_care_home, here::here("output", "data", "custom_measures_care_home.csv"))
# write_csv(measures_dementia, here::here("output", "data", "custom_measures_dementia.csv"))


## Flow chart data as .rds file
write_rds(flow_chart_cohort, here::here("output", "data", "data_flow_chart.rds"), compress="gz")



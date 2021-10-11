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


# Process data ----

## Prevalence datasets
data_prevalence <- full_join(read.csv(here::here("output", "data", "measure_antipsychotic_all_any.csv"))[,c("antipsychotic_any", "population", "value", "date")],
                               read.csv(here::here("output", "data", "measure_antipsychotic_all_first_gen.csv"))[,c("antipsychotics_first_gen", "date")], 
                               by = "date") %>%
  full_join(read.csv(here::here("output", "data", "measure_antipsychotic_all_second_gen.csv"))[,c("antipsychotics_second_gen", "date")],  
            by = "date") %>%
  full_join(read.csv(here::here("output", "data", "measure_antipsychotic_all_injectable_and_depot.csv"))[,c("antipsychotics_injectable_and_depot", "date")],  
            by = "date") %>%
  full_join(read.csv(here::here("output", "data", "measure_antipsychotic_all_prochlorperazine.csv"))[,c("prochlorperazine", "date")],  
            by = "date") %>%
  select(date, antipsychotic_any, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot,
         prochlorperazine, population) %>%
  mutate(group = "All",
         date = as.Date(as.character(date), format = "%Y-%m-%d")) %>%
  rbind(combine_measures(group = "dementia", incident = FALSE),
        combine_measures(group = "care_home", incident = FALSE),
        combine_measures(group = "learning_disability", incident = FALSE),
        combine_measures(group = "autism", incident = FALSE),
        combine_measures(group = "serious_mental_illness", incident = FALSE))

## Incident datasets
data_incident <- full_join(read.csv(here::here("output", "data", "measure_antipsychotic_all_any_incident.csv"))[,c("antipsychotic_any_incident", "population", "value", "date")],
                                 read.csv(here::here("output", "data", "measure_antipsychotic_all_first_gen_incident.csv"))[,c("antipsychotics_first_gen_incident", "date")], 
                                 by = "date") %>%
  full_join(read.csv(here::here("output", "data", "measure_antipsychotic_all_second_gen_incident.csv"))[,c("antipsychotics_second_gen_incident", "date")],  
            by = "date") %>%
  full_join(read.csv(here::here("output", "data", "measure_antipsychotic_all_injectable_and_depot_incident.csv"))[,c("antipsychotics_injectable_and_depot_incident", "date")],  
            by = "date") %>%
  full_join(read.csv(here::here("output", "data", "measure_antipsychotic_all_prochlorperazine_incident.csv"))[,c("prochlorperazine_incident", "date")],  
            by = "date") %>%
  select(date, antipsychotic_any_incident, antipsychotics_first_gen_incident, antipsychotics_second_gen_incident, antipsychotics_injectable_and_depot_incident,
         prochlorperazine_incident, population) %>%
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
         group = "All") %>%
  rbind(combine_measures(group = "dementia", incident = TRUE),
        combine_measures(group = "care_home", incident = TRUE),
        combine_measures(group = "learning_disability", incident = TRUE),
        combine_measures(group = "autism", incident = TRUE),
        combine_measures(group = "serious_mental_illness", incident = TRUE))


# Redaction ----

## Redact values < 8
threshold = 8

data_prevalence_redacted <- data_prevalence %>%
  mutate(antipsychotic_any = ifelse(antipsychotic_any < threshold, NA, as.numeric(antipsychotic_any)),
         antipsychotics_first_gen = ifelse(antipsychotics_first_gen < threshold, NA, as.numeric(antipsychotics_first_gen)),
         antipsychotics_second_gen = ifelse(antipsychotics_second_gen < threshold, NA, as.numeric(antipsychotics_second_gen)),
         antipsychotics_injectable_and_depot = ifelse(antipsychotics_injectable_and_depot < threshold, NA, as.numeric(antipsychotics_injectable_and_depot)),
         prochlorperazine = ifelse(prochlorperazine < threshold, NA, as.numeric(prochlorperazine)))

data_incident_redacted <- data_incident %>%
  mutate(antipsychotic_any_incident = ifelse(antipsychotic_any_incident < threshold, NA, as.numeric(antipsychotic_any_incident)),
         antipsychotics_first_gen_incident = ifelse(antipsychotics_first_gen_incident < threshold, NA, as.numeric(antipsychotics_first_gen_incident)),
         antipsychotics_second_gen_incident = ifelse(antipsychotics_second_gen_incident < threshold, NA, as.numeric(antipsychotics_second_gen_incident)),
         antipsychotics_injectable_and_depot_incident = ifelse(antipsychotics_injectable_and_depot_incident < threshold, NA, as.numeric(antipsychotics_injectable_and_depot_incident)),
         prochlorperazine_incident = ifelse(prochlorperazine_incident < threshold, NA, as.numeric(prochlorperazine_incident)))
         
## Recalculate totals
data_prevalence_redacted$antipsychotic_any = rowSums(data_prevalence_redacted[,c("antipsychotics_first_gen",
                                                                                "antipsychotics_second_gen",
                                                                                "antipsychotics_injectable_and_depot",
                                                                                "prochlorperazine")], 
                                                     na.rm = T)

data_incident_redacted$antipsychotic_any = rowSums(data_incident_redacted[,c("antipsychotics_first_gen_incident",
                                                                                 "antipsychotics_second_gen_incident",
                                                                                 "antipsychotics_injectable_and_depot_incident",
                                                                                 "prochlorperazine_incident")], 
                                                     na.rm = T)


# Save datasets ----

## Save as .csv
write.csv(data_prevalence_redacted, file = here::here("output", "data", "data_prevalence_redacted.csv"))
write.csv(data_incident_redacted, file = here::here("output", "data", "data_incident_redacted.csv"))


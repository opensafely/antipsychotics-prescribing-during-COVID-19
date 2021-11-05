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
library('reshape2')

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

## Sensitivity analysis
data_prevalence_SA <- read.csv(here::here("output", "data", "measure_antipsychotic_all_any_alive_2weeks_post_antipsychotic.csv")) %>%
  mutate(group = "All",
         date = as.Date(as.character(date), format = "%Y-%m-%d")) %>%
  select(date, group, alive_2weeks_post_antipsychotic, population) %>%
  rbind((read.csv(here::here("output", "data", "measure_antipsychotic_groups_any_alive_2weeks_post_antipsychotic.csv")) %>%
              select(-value) %>%
              melt(id.var = c("alive_2weeks_post_antipsychotic", "population", "date")) %>%
              filter(value == "True") %>%
              group_by(date, variable) %>%
              summarise(alive_2weeks_post_antipsychotic = sum(alive_2weeks_post_antipsychotic, na.rm = T),
                        population = sum(population, na.rm = T)) %>%
              mutate(date = as.Date(as.character(date), format = "%Y-%m-%d")) %>%
              select(date, group = variable, alive_2weeks_post_antipsychotic, population))) %>%
  arrange(group, date)

data_incidence_SA <- read.csv(here::here("output", "data", "measure_antipsychotic_all_any_alive_2weeks_post_new_antipsychotic.csv")) %>%
  mutate(group = "All",
         date = as.Date(as.character(date), format = "%Y-%m-%d")) %>%
  select(date, group, alive_2weeks_post_new_antipsychotic, population) %>%
  rbind((read.csv(here::here("output", "data", "measure_antipsychotic_groups_any_alive_2weeks_post_new_antipsychotic.csv")) %>%
           select(-value) %>%
           melt(id.var = c("alive_2weeks_post_new_antipsychotic", "population", "date")) %>%
           filter(value == "True") %>%
           group_by(date, variable) %>%
           summarise(alive_2weeks_post_new_antipsychotic = sum(alive_2weeks_post_new_antipsychotic, na.rm = T),
                     population = sum(population, na.rm = T)) %>%
           mutate(date = as.Date(as.character(date), format = "%Y-%m-%d")) %>%
           select(date, group = variable, alive_2weeks_post_new_antipsychotic, population))) %>%
  arrange(group, date)


# Redaction ----

## Redact values < 8
threshold = 8

data_prevalence_redacted <- data_prevalence %>%
  left_join(data_prevalence_SA, by = c("date", "group", "population")) %>%
  mutate(antipsychotic_any = ifelse(antipsychotic_any < threshold, NA, as.numeric(antipsychotic_any)),
         antipsychotics_first_gen = ifelse(antipsychotics_first_gen < threshold, NA, as.numeric(antipsychotics_first_gen)),
         antipsychotics_second_gen = ifelse(antipsychotics_second_gen < threshold, NA, as.numeric(antipsychotics_second_gen)),
         antipsychotics_injectable_and_depot = ifelse(antipsychotics_injectable_and_depot < threshold, NA, as.numeric(antipsychotics_injectable_and_depot)),
         prochlorperazine = ifelse(prochlorperazine < threshold, NA, as.numeric(prochlorperazine)),
         alive_2weeks_post_antipsychotic = ifelse(alive_2weeks_post_antipsychotic < threshold, NA, as.numeric(alive_2weeks_post_antipsychotic)))

data_incident_redacted <- data_incident %>%
  left_join(data_incidence_SA, by = c("date", "group", "population")) %>%
  mutate(antipsychotic_any_incident = ifelse(antipsychotic_any_incident < threshold, NA, as.numeric(antipsychotic_any_incident)),
         antipsychotics_first_gen_incident = ifelse(antipsychotics_first_gen_incident < threshold, NA, as.numeric(antipsychotics_first_gen_incident)),
         antipsychotics_second_gen_incident = ifelse(antipsychotics_second_gen_incident < threshold, NA, as.numeric(antipsychotics_second_gen_incident)),
         antipsychotics_injectable_and_depot_incident = ifelse(antipsychotics_injectable_and_depot_incident < threshold, NA, as.numeric(antipsychotics_injectable_and_depot_incident)),
         prochlorperazine_incident = ifelse(prochlorperazine_incident < threshold, NA, as.numeric(prochlorperazine_incident)),
         alive_2weeks_post_new_antipsychotic = ifelse(alive_2weeks_post_new_antipsychotic < threshold, NA, as.numeric(alive_2weeks_post_new_antipsychotic)))

## Redact values where difference between SA value is < threshold
data_prevalence_redacted <- data_prevalence_redacted %>%
  mutate(diff = antipsychotic_any - alive_2weeks_post_antipsychotic,
         alive_2weeks_post_antipsychotic = ifelse(diff < threshold, antipsychotic_any, alive_2weeks_post_antipsychotic)) %>%
  select(-diff)

data_incident_redacted <- data_incident_redacted %>%
  mutate(diff = antipsychotic_any_incident - alive_2weeks_post_new_antipsychotic,
         alive_2weeks_post_new_antipsychotic = ifelse(diff < threshold, antipsychotic_any_incident, alive_2weeks_post_new_antipsychotic)) %>%
  select(-diff)

## Round to nearest 5
data_prevalence_redacted <- data_prevalence_redacted %>%
  mutate(population = plyr::round_any(population, 5),
         antipsychotic_any = plyr::round_any(antipsychotic_any, 5),
         antipsychotics_first_gen = plyr::round_any(antipsychotics_first_gen, 5),
         antipsychotics_second_gen = plyr::round_any(antipsychotics_second_gen, 5),
         antipsychotics_injectable_and_depot = plyr::round_any(antipsychotics_injectable_and_depot, 5),
         prochlorperazine = plyr::round_any(prochlorperazine, 5),
         alive_2weeks_post_antipsychotic = plyr::round_any(alive_2weeks_post_antipsychotic, 5))

data_incident_redacted <- data_incident_redacted %>%
  mutate(population = plyr::round_any(population, 5),
         antipsychotic_any_incident = plyr::round_any(antipsychotic_any_incident, 5),
         antipsychotics_first_gen_incident = plyr::round_any(antipsychotics_first_gen_incident, 5),
         antipsychotics_second_gen_incident = plyr::round_any(antipsychotics_second_gen_incident, 5),
         antipsychotics_injectable_and_depot_incident = plyr::round_any(antipsychotics_injectable_and_depot_incident, 5),
         prochlorperazine_incident = plyr::round_any(prochlorperazine_incident, 5),
         alive_2weeks_post_new_antipsychotic = plyr::round_any(alive_2weeks_post_new_antipsychotic, 5))


# Save datasets ----

## Save as .csv
write.csv(data_prevalence_redacted, file = here::here("output", "data", "data_prevalence_redacted.csv"))
write.csv(data_incident_redacted, file = here::here("output", "data", "data_incident_redacted.csv"))


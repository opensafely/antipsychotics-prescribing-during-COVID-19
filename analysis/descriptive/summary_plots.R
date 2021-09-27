######################################

# This script:
# - imports data
# - plots : 1. total number of patients issued antipsychotics, by clinical and dempogrpahic groups
#           2. total new number of patients issued antipsychotics, by clinical groups

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
library('patchwork')

## Create output directory
dir.create(here::here("output", "figures"), showWarnings = FALSE, recursive=TRUE)

## Custom functions
source(here("analysis", "lib", "custom_functions.R"))

## Import processed data
data_totals_groups <- readRDS(here::here("output", "data", "data_totals_groups.rds"))
data_totals_demographics <- readRDS(here::here("output", "data", "data_totals_demographics.rds"))
data_incident_groups <- readRDS(here::here("output", "data", "data_incident_groups.rds"))

## Combine data
data_processed_1yr <- data_incident_groups[[1]] %>%
  mutate(`Number of patients with first prescriptions` = "None in previous year")
data_processed_2yr <- data_incident_groups[[2]]  %>%
  mutate(`Number of patients with first prescriptions` = "None in previous two years")

data_incident_groups <- rbind(data_processed_1yr, data_processed_2yr)


# Figures ----

## Total number of patients issued antipsychotics by group
groups <- unique(data_totals_groups$group)
lapply(groups, FUN = plot_prevalent_antipsychotics_by_group)

## Total number of patients issued antipsychotics by demographic
demographic <- names(data_totals_demographics)
lapply(demographic, FUN = plot_antipsychotics_by_demographic)

## Total number of patients with newly issued antipsychotics, by group
lapply(groups, FUN = plot_incident_antipsychotics_by_group)


# Redaction ----
data_totals_groups_redacted <- redact_table(table = data_totals_groups, threshold = 8)
data_incident_groups_redacted <- redact_table(table = data_processed_1yr, threshold = 8)
data_totals_demographics_redacted <- rbind(redact_table(table = data_totals_demographics$age, threshold = 8),
                                           redact_table(table = data_totals_demographics$sex, threshold = 8),
                                           redact_table(table = data_totals_demographics$region, threshold = 8),
                                           redact_table(table = data_totals_demographics$imd, threshold = 8),
                                           redact_table(table = data_totals_demographics$ethnicity, threshold = 8))


# Save redacted tables ----
write_csv(data_totals_groups_redacted, here::here("output","data", "data_totals_groups_redacted.csv"))
write_csv(data_incident_groups_redacted, here::here("output","data", "data_incident_groups_redacted_redacted.csv"))
write_csv(data_totals_demographics_redacted, here::here("output","data", "data_totals_demographics_redacted.csv"))

          



######################################

# This script:
# - Produces counts of patients prescribed antipsychotic by demographic characteristics between January 2021 and April 2021.
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
source(here("analysis", "custom_functions.R"))

## Import data
feb <- arrow::read_feather(here::here("output", "data", "input_2021-02-01.feather"))

march <- arrow::read_feather(here::here("output", "data", "input_2021-03-01.feather")) %>%
  filter(!patient_id %in% feb$patient_id)

april <- data_extract <- arrow::read_feather(here::here("output", "data", "input_2021-04-01.feather")) %>%
  filter(!patient_id %in% c(feb$patient_id, march$patient_id))


# Process data ----
data_table1 <- rbind(feb, march, april) %>% 
  group_by(patient_id) %>% 
  filter(row_number()==1) %>%
  mutate(ethnicity = ifelse(is.na(ethnicity_6), ethnicity_sus, ethnicity_6),
         ethnicity = ifelse(is.na(ethnicity), 6, ethnicity),
         
         ethnicity = fct_case_when(
           ethnicity == "1" ~ "White",
           ethnicity == "2" ~ "Mixed",
           ethnicity == "3" ~ "Asian or Asian British",
           ethnicity == "4" ~ "Black or Black British",
           ethnicity == "5" ~ "Other ethnic groups",
           ethnicity == "6" ~ "Unknown",
           #TRUE ~ "Unknown"
           TRUE ~ NA_character_))

all <- table_1(data_table1 %>% filter(antipsychotics_first_gen == 1 |
                                        antipsychotics_second_gen ==1 |
                                        antipsychotics_injectable_and_depot == 1 |
                                        prochlorperazine == 1))

antipsychotics_first_gen <- table_1(data_table1 %>% filter(antipsychotics_first_gen == 1))
antipsychotics_second_gen <- table_1(data_table1 %>% filter(antipsychotics_second_gen == 1))
antipsychotics_injectable_and_depot <- table_1(data_table1 %>% filter(antipsychotics_injectable_and_depot == 1))
prochlorperazine <- table_1(data_table1 %>% filter(prochlorperazine == 1))

# Table 1 ----
resuts_table_1 <- left_join(all, antipsychotics_first_gen, by = c("Characteristic")) %>%
  left_join(antipsychotics_second_gen, by = c("Characteristic")) %>%
  left_join(antipsychotics_injectable_and_depot, by = c("Characteristic")) %>%
  left_join(prochlorperazine, by = c("Characteristic"))

colnames(resuts_table_1) <- c("Characteristic", "All", "First gen", "Second gen", "Injectable and depot", "Prochlorperazine")
  
write_csv(resuts_table_1, here::here("output",  "tables", "table1.csv"))
gt::gtsave(gt(resuts_table_1), here::here("output","tables", "table1.html"))

  
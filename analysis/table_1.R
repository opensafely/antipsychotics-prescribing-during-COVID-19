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
  mutate(ethnicity_long =  ifelse(is.na(eth) & ethnicity_other == 1, 17, 
                                  ifelse(is.na(eth) & ethnicity_not_given == 1, 18,
                                         ifelse(is.na(eth) & ethnicity_not_stated == 1, 19,
                                                ifelse(is.na(eth) & ethnicity_no_record == 1, 20,
                                                       eth)))),
         ethnicity_long = ifelse(is.na(ethnicity_long), 20, ethnicity_long),
         ethnicity_short = ifelse(ethnicity_long %in% c(1,2,3), 1, ethnicity_long),
         ethnicity_short = ifelse(ethnicity_long %in% c(4,5,6,7), 2, ethnicity_short),
         ethnicity_short = ifelse(ethnicity_long %in% c(8,9,10,11), 3, ethnicity_short),
         ethnicity_short = ifelse(ethnicity_long %in% c(12,13,14), 4, ethnicity_short),
         ethnicity_short = ifelse(ethnicity_long %in% c(15,16), 5, ethnicity_short),
         ethnicity_short = ifelse(ethnicity_short %in% c(1:16), ethnicity_short, 6)) %>%
  rename(ethnicity = ethnicity_short)

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

  
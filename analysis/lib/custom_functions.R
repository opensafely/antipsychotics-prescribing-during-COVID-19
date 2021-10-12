######################################

# This script contains custom functions  for:
# - data processing
# - plotting

######################################


# Factorise ----
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# Read in feather data ----
my_read_feather <- function(x) {
  
  data_extract <- arrow::read_feather(
    here::here("output", "data", x)) %>%
    mutate(date = as.Date(substr(x, 7, 16), format = "%Y-%m-%d"))

}

# Combine measures ----
combine_measures <- function(group = "all", incident = TRUE) {
  
  if(incident == TRUE){
    
    data_antipsychotic_any_incident <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_any_incident.csv")) %>%
    select(cohort = group, date, antipsychotic_any_incident, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(antipsychotic_any_incident = sum(antipsychotic_any_incident, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_antipsychotic_first_gen_incident <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_first_gen_incident.csv")) %>%
    select(cohort = group, date, antipsychotics_first_gen_incident, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(antipsychotics_first_gen_incident = sum(antipsychotics_first_gen_incident, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_antipsychotic_second_gen_incident <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_second_gen_incident.csv")) %>%
    select(cohort = group, date, antipsychotics_second_gen_incident, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(antipsychotics_second_gen_incident = sum(antipsychotics_second_gen_incident, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_antipsychotic_injectable_and_depot_incident <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_injectable_and_depot_incident.csv")) %>%
    select(cohort = group, date, antipsychotics_injectable_and_depot_incident, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(antipsychotics_injectable_and_depot_incident = sum(antipsychotics_injectable_and_depot_incident, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_prochlorperazine_incident <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_prochlorperazine_incident.csv")) %>%
    select(cohort = group, date, prochlorperazine_incident, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(prochlorperazine_incident = sum(prochlorperazine_incident, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_measures <- left_join(data_antipsychotic_any_incident, data_antipsychotic_first_gen_incident, by = c("date", "population")) %>%
    left_join(data_antipsychotic_second_gen_incident, by = c("date", "population")) %>%
    left_join(data_antipsychotic_injectable_and_depot_incident, by = c("date", "population")) %>%
    left_join(data_prochlorperazine_incident, by = c("date", "population")) %>%
    select(date, antipsychotic_any_incident, antipsychotics_first_gen_incident, antipsychotics_second_gen_incident, antipsychotics_injectable_and_depot_incident,
           prochlorperazine_incident, population) %>%
    mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
           group = group)
  
  data_measures
  
  }else{
    
    data_antipsychotic_any <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_any.csv")) %>%
    select(cohort = group, date, antipsychotic_any, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(antipsychotic_any = sum(antipsychotic_any, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_antipsychotic_first_gen <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_first_gen.csv")) %>%
    select(cohort = group, date, antipsychotics_first_gen, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_antipsychotic_second_gen <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_second_gen.csv")) %>%
    select(cohort = group, date, antipsychotics_second_gen, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_antipsychotic_injectable_and_depot <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_injectable_and_depot.csv")) %>%
    select(cohort = group, date, antipsychotics_injectable_and_depot, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_prochlorperazine <- read.csv(here::here("output", "data", "measure_antipsychotic_groups_prochlorperazine.csv")) %>%
    select(cohort = group, date, prochlorperazine, population) %>%
    filter(cohort == "True") %>%
    group_by(date) %>%
    summarise(prochlorperazine = sum(prochlorperazine, na.rm = T),
              population = sum(population, na.rm = T))
  
  data_measures <- left_join(data_antipsychotic_any, data_antipsychotic_first_gen, by = c("date", "population")) %>%
    left_join(data_antipsychotic_second_gen, by = c("date", "population")) %>%
    left_join(data_antipsychotic_injectable_and_depot, by = c("date", "population")) %>%
    left_join(data_prochlorperazine, by = c("date", "population")) %>%
    select(date, antipsychotic_any, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot,
           prochlorperazine, population) %>%
    mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"),
           group = group)
  
  data_measures
  
  }

}


## Table 2
calculate_table2 <-function(population = "autism", Y = 10000){
  
  table_2_all <- data_cohort %>% 
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
      ethnicity = ifelse(is.na(eth2001), "Missing", ethnicity),
      ethnicity = fct_case_when(
        ethnicity == "1" ~ "White",
        ethnicity == "2" ~ "Mixed",
        ethnicity == "3" ~ "Asian or Asian British",
        ethnicity == "4" ~ "Black or Black British",
        ethnicity == "5" ~ "Other ethnic groups",
        ethnicity == "Missing" ~ "Missing",
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
    select(group = paste0(population),
           antipsychotic = antipsychotic_any,
           ageband, 
           sex,
           region,
           imd,
           ethnicity) %>%
    filter(group == 1) %>%
    select(-group) %>%
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
    mutate(rate = antipsychotic/population*Y)
}

# Redaction
redact_table <- function(table = data_totals_groups, threshold = 8){
  
  ## Redact values < threshold
  threshold = threshold
  
  table_redacted <- table %>%
    mutate(population = ifelse(population < threshold, NA, population),
           antipsychotic = ifelse(antipsychotic < threshold | is.na(population), NA, antipsychotic),
           diff = (population - antipsychotic),
           antipsychotic = ifelse(diff < threshold, NA, antipsychotic)) %>%
    select(-diff, - rate)
  
  ## Round to nearest 5
  table_redacted <- table_redacted %>%
    mutate(population = plyr::round_any(population, 5),
           antipsychotic = plyr::round_any(antipsychotic, 5))
  
  ## Replace na with [REDACTED]
  table_redacted <- table_redacted %>%
    mutate(population = ifelse(is.na(population), "[REDACTED]", population),
           antipsychotic = ifelse(is.na(antipsychotic), "[REDACTED]", antipsychotic))
}


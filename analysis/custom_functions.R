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

# Calculate measures dataset ----
calculate_measures <- function(x, cohort = "learning_disability") {
  
  if(cohort %in% c("learning_disability", "autism", "serious_mental_illness", "care_home", "dementia")){
    
    ## Read in data
    measures_data <- data_processed %>%
      select(date, practice, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine, paste0(cohort)) %>%
      rename(cohort = paste0(cohort)) %>%
      filter(cohort == 1) %>%
      select(-cohort) %>%
      group_by(date, practice) %>%
      summarise(antipsychotics_first_gen_event = sum(antipsychotics_first_gen == 1),
                antipsychotics_first_gen_population = sum(antipsychotics_first_gen %in% c(0,1)),
                antipsychotics_second_gen_event = sum(antipsychotics_second_gen == 1),
                antipsychotics_second_gen_population = sum(antipsychotics_second_gen %in% c(0,1)),
                antipsychotics_injectable_and_depot_event = sum(antipsychotics_injectable_and_depot == 1),
                antipsychotics_injectable_and_depot_population = sum(antipsychotics_injectable_and_depot %in% c(0,1)),
                prochlorperazine_event = sum(prochlorperazine == 1),
                prochlorperazine_population = sum(prochlorperazine %in% c(0,1))) %>%
      mutate(antipsychotics_first_gen = antipsychotics_first_gen_event/antipsychotics_first_gen_population*1000,
             antipsychotics_second_gen = antipsychotics_second_gen_event/antipsychotics_second_gen_population*1000,
             antipsychotics_injectable_and_depot = antipsychotics_injectable_and_depot_event/antipsychotics_injectable_and_depot_population*1000,
             prochlorperazine = prochlorperazine_event/prochlorperazine_population*1000) %>%
      select(date, practice, antipsychotics_first_gen, antipsychotics_second_gen,
             antipsychotics_injectable_and_depot, prochlorperazine)
    
  } else {
    
    ## Read in data
    measures_data <- data_processed %>%
      select(date, practice, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine) %>%
      group_by(date, practice) %>%
      summarise(antipsychotics_first_gen_event = sum(antipsychotics_first_gen == 1),
                antipsychotics_first_gen_population = sum(antipsychotics_first_gen %in% c(0,1)),
                antipsychotics_second_gen_event = sum(antipsychotics_second_gen == 1),
                antipsychotics_second_gen_population = sum(antipsychotics_second_gen %in% c(0,1)),
                antipsychotics_injectable_and_depot_event = sum(antipsychotics_injectable_and_depot == 1),
                antipsychotics_injectable_and_depot_population = sum(antipsychotics_injectable_and_depot %in% c(0,1)),
                prochlorperazine_event = sum(prochlorperazine == 1),
                prochlorperazine_population = sum(prochlorperazine %in% c(0,1))) %>%
      mutate(antipsychotics_first_gen = antipsychotics_first_gen_event/antipsychotics_first_gen_population*1000,
             antipsychotics_second_gen = antipsychotics_second_gen_event/antipsychotics_second_gen_population*1000,
             antipsychotics_injectable_and_depot = antipsychotics_injectable_and_depot_event/antipsychotics_injectable_and_depot_population*1000,
             prochlorperazine = prochlorperazine_event/prochlorperazine_population*1000) %>%
      select(date, practice, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine)
    
  }
  
}

# Totals data set ----
calculate_totals <- function(x, cohort = "learning_disability") {
  
  if(cohort %in% c("learning_disability", "autism", "serious_mental_illness", "care_home", "dementia")){
    
    data_processed <-  data_processed_all %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine, paste0(cohort)) %>%
      rename(cohort = paste0(cohort)) %>%
      filter(cohort == 1) %>%
      select(-cohort) %>%
      group_by(date) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = paste0(cohort))
    
  }else {
    
    data_processed <- data_processed_all %>%
        select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine) %>%
        group_by(date) %>%
        summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
            antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
            antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
            prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
        mutate(group = "all")
  }
}
      
      
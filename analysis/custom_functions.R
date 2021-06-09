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

# Totals data set ----
calculate_totals <- function(x, cohort = "learning_disability") {
  
  data_extract <- arrow::read_feather(
    here::here("output", "data", x)) %>%
    mutate(date = as.Date(substr(x, 7, 16), format = "%Y-%m-%d"))
  
  if(cohort %in% c("learning_disability", "autism", "serious_mental_illness", "care_home", "dementia")){
    
    data_processed <-  data_extract %>%
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
    
    data_processed <- data_extract %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine) %>%
      group_by(date) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = "all")
  }
}

# Calculate measures dataset ----
calculate_measures <- function(x, cohort = "learning_disability") {
  
  data_extract <- arrow::read_feather(
    here::here("output", "data", x)) %>%
    mutate(date = as.Date(substr(x, 7, 16), format = "%Y-%m-%d"))
  
  if(cohort %in% c("learning_disability", "autism", "serious_mental_illness", "care_home", "dementia")){
    
    ## Read in data
    measures_data <- data_extract %>%
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
    measures_data <- data_extract %>%
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



# data_extract_ethnicity <- arrow::read_feather(
#   here::here("output", "data", "input_ethnicity.feather")) %>%
#   mutate(ethnicity_long =  ifelse(is.na(eth) & ethnicity_other == 1, 17, 
#                                   ifelse(is.na(eth) & ethnicity_not_given == 1, 18,
#                                          ifelse(is.na(eth) & ethnicity_not_stated == 1, 19,
#                                                 ifelse(is.na(eth) & ethnicity_no_record == 1, 20,
#                                                        eth)))),
#          ethnicity_long = ifelse(is.na(ethnicity_long), 20, ethnicity_long),
#          ethnicity_short = ifelse(ethnicity_long %in% c(1,2,3), 1, ethnicity_long),
#          ethnicity_short = ifelse(ethnicity_long %in% c(4,5,6,7), 2, ethnicity_short),
#          ethnicity_short = ifelse(ethnicity_long %in% c(8,9,10,11), 3, ethnicity_short),
#          ethnicity_short = ifelse(ethnicity_long %in% c(12,13,14), 4, ethnicity_short),
#          ethnicity_short = ifelse(ethnicity_long %in% c(15,16), 5, ethnicity_short),
#          ethnicity_short = ifelse(ethnicity_short %in% c(1:16), ethnicity_short, 6)) %>%
#   select(patient_id, ethnicity_long, ethnicity_short)
# 
# ## Format columns (i.e, set factor levels)
# data_processed_all <- left_join(data_extract, data_extract_ethnicity, by = "patient_id") %>%
#   mutate(
#     
#     # Age
#     ageband = cut(
#       age,
#       breaks = c(-Inf, 18, 30, 40, 50, 60, 65, Inf),
#       labels = c("under 18", "18-30", "30s", "40s", "50s", "60-64", "65+"),
#       right = FALSE
#     ),
#     
#     # Ethnicity
#     ethnicity_long = fct_case_when(
#       ethnicity_long == "1" ~ "White - British",
#       ethnicity_long == "2" ~ "White - Irish",
#       ethnicity_long == "3" ~ "White - Any other White background",
#       ethnicity_long == "4" ~ "Mixed - White and Black Caribbean",
#       ethnicity_long == "5" ~ "Mixed - White and Black African",
#       ethnicity_long == "6" ~ "Mixed - White and Asian",
#       ethnicity_long == "7" ~ "Mixed - Any other mixed background",
#       ethnicity_long == "8" ~ "Asian or Asian British - Indian",
#       ethnicity_long == "9" ~ "Asian or Asian British - Pakistani",
#       ethnicity_long == "10" ~ "Asian or Asian British - Bangladeshi",
#       ethnicity_long == "11" ~ "Asian or Asian British - Any other Asian background",
#       ethnicity_long == "12" ~ "Black or Black British - Caribbean",
#       ethnicity_long == "13" ~ "Black or Black British - African",
#       ethnicity_long == "14" ~ "Black or Black British - Any other Black background",
#       ethnicity_long == "15" ~ "Other ethnic groups - Chinese",
#       ethnicity_long == "16" ~ "Other ethnic groups - Any other ethnic group",
#       ethnicity_long == "17" ~ "Patients with any other ethnicity code",
#       ethnicity_long == "18" ~ "Ethnicity not given - patient refused",
#       ethnicity_long == "19" ~ "Ethnicity not stated",
#       ethnicity_long == "20" ~ "Ethnicity not recorded",
#       #TRUE ~ "Unknown",
#       TRUE ~ NA_character_
#     ),
#     
#     # Ethnicity
#     ethnicity_short = fct_case_when(
#       ethnicity_short == "1" ~ "White",
#       ethnicity_short == "2" ~ "Mixed",
#       ethnicity_short == "3" ~ "Asian or Asian British",
#       ethnicity_short == "4" ~ "Black or Black British",
#       ethnicity_short == "5" ~ "Other ethnic groups",
#       ethnicity_short == "6" ~ "Unknown",
#       #TRUE ~ "Unknown"
#       TRUE ~ NA_character_
#     ),
#     
#     # IMD
#     imd = na_if(imd, "0"),
#     imd = as.integer(imd),
#     imd = fct_case_when(
#       (imd >=1) & (imd < 32844*1/5) ~ "1 most deprived",
#       (imd >= 32844*1/5) & (imd < 32844*2/5) ~ "2",
#       (imd >= 32844*2/5) & (imd < 32844*3/5) ~ "3",
#       (imd >= 32844*3/5) & (imd < 32844*4/5) ~ "4",
#       (imd >= 32844*4/5) ~ "5 least deprived",
#       TRUE ~ NA_character_
#     ),
#     
#     # Sex
#     sex = fct_case_when(
#       sex == "F" ~ "Female",
#       sex == "M" ~ "Male",
#       #sex == "I" ~ "Inter-sex",
#       #sex == "U" ~ "Unknown",
#       TRUE ~ NA_character_
#     ),
#     
#     # Region
#     region = fct_case_when(
#       region == "London" ~ "London",
#       region == "East" ~ "East of England",
#       region == "East Midlands" ~ "East Midlands",
#       region == "North East" ~ "North East",
#       region == "North West" ~ "North West",
#       region == "South East" ~ "South East",
#       region == "South West" ~ "South West",
#       region == "West Midlands" ~ "West Midlands",
#       region == "Yorkshire and The Humber" ~ "Yorkshire and the Humber",
#       #TRUE ~ "Unknown",
#       TRUE ~ NA_character_
#     ),
#     
#     ## STP
#     stp = as.factor(stp)
#     
#   ) %>%
#   droplevels() %>%
#   mutate(
#     across(
#       where(is.logical),
#       ~.x*1L
#     )
#   ) %>%
#   droplevels() %>%
#   arrange(date, patient_id, practice)



      
      
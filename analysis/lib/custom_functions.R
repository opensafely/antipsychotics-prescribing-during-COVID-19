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

# Totals datasets ----
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
    
  } else if(cohort == "sex"){
      
      data_processed <-  data_extract %>%
        select(date, antipsychotics_first_gen, antipsychotics_second_gen, 
               antipsychotics_injectable_and_depot, prochlorperazine, sex) %>%
        filter(sex %in% c("M", "F")) %>%
        mutate(sex = fct_case_when(sex == "F" ~ "Female",
                                   sex == "M" ~ "Male",
                                   #sex == "I" ~ "Inter-sex",
                                   #sex == "U" ~ "Unknown",
                                   TRUE ~ NA_character_)) %>%
        group_by(date, sex) %>%
        summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                  antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                  antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                  prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
        mutate(group = paste0(cohort))
      
  } else if(cohort == "imd"){
    
    data_processed <-  data_extract %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine, imd) %>%
      mutate(imd = na_if(imd, "0"),
             imd = as.numeric(as.character(imd)),
             imd = fct_case_when( imd ==1 ~ "1 most deprived",
                                  imd ==2 ~ "2",
                                  imd ==3 ~ "3",
                                  imd == 4 ~ "4",
                                  imd == 5 ~ "5 least deprived",
                                  TRUE ~ NA_character_)) %>%
      drop_na(imd) %>%
      group_by(date, imd) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = paste0(cohort))
    
  } else if(cohort == "region"){
    
    data_processed <-  data_extract %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine, region) %>%
      mutate(region = fct_case_when(
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
        TRUE ~ NA_character_)) %>%
      drop_na(region) %>%
      group_by(date, region) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = paste0(cohort))
    
  } else if(cohort == "stp"){
    
    data_processed <-  data_extract %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine, stp) %>%
      mutate(stp = as.factor(stp)) %>%
      drop_na(stp) %>%
      group_by(date, stp) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = paste0(cohort))
    
  } else if(cohort == "age"){
    
    data_processed <-  data_extract %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine, age) %>%
      mutate(ageband = cut(age, 
                           breaks = c(-Inf, 18, 30, 40, 50, 60, 65, Inf),
                           labels = c("under 18", "18-30", "30s", "40s", "50s", "60-64", "65+"),
                           right = FALSE
                           )) %>%
      drop_na(ageband) %>%
      group_by(date, ageband) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = paste0(cohort)) %>%
      rename(age = ageband)
    
  } else if(cohort == "ethnicity"){
    
    data_processed <-  data_extract %>%
      mutate(ethnicity = ifelse(is.na(ethnicity), 6, ethnicity)) %>%
      select(patient_id, date, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine, ethnicity) %>%
      mutate(ethnicity = fct_case_when(
        ethnicity == "1" ~ "White",
        ethnicity == "2" ~ "Mixed",
        ethnicity == "3" ~ "Asian or Asian British",
        ethnicity == "4" ~ "Black or Black British",
        ethnicity == "5" ~ "Other ethnic groups",
        ethnicity == "6" ~ "Unknown",
        #TRUE ~ "Unknown"
        TRUE ~ NA_character_)) %>%
      group_by(date, ethnicity) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = paste0(cohort))
    
  } else {
    
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

# Totals (incident) datasets ----
calculate_incident_1yr <- function(x, cohort = "learning_disability") {
  
  data_extract <- arrow::read_feather(
    here::here("output", "data", x)) %>%
    mutate(date = as.Date(substr(x, 7, 16), format = "%Y-%m-%d"))
  
  if(cohort %in% c("learning_disability", "autism", "serious_mental_illness", "care_home", "dementia")){
    
    data_processed_1yr <-  data_extract %>%
      filter(antipsychotics_first_gen_incident_1yr == 1) %>%
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
    
    data_processed_1y <- data_extract %>%
      filter(antipsychotics_first_gen_incident_1yr == 1) %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine) %>%
      group_by(date) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = "all")
  }
}

calculate_incident_2yr <- function(x, cohort = "learning_disability") {
  
  data_extract <- arrow::read_feather(
    here::here("output", "data", x)) %>%
    mutate(date = as.Date(substr(x, 7, 16), format = "%Y-%m-%d"))
  
  if(cohort %in% c("learning_disability", "autism", "serious_mental_illness", "care_home", "dementia")){
    
    data_processed_2yr <-  data_extract %>%
      filter(antipsychotics_first_gen_incident_2yr == 1) %>%
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
    
    data_processed_2y <- data_extract %>%
      filter(antipsychotics_first_gen_incident_2yr == 1) %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine) %>%
      group_by(date) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
      mutate(group = "all")
  }
}

# Calculate measures datasets ----
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

# Plot totals by group ----
plot_prevalent_antipsychotics_by_group <- function(group = "all"){
  total_antipsychotics <- data_totals_groups %>%
    filter(group == group) %>%
    select(-group) %>%
    melt(id.vars = c("date")) %>%
    mutate(variable = factor(variable, labels = c("First generation antipsychotics (excluding long acting depots)",
                                                  "Second generation antipsychotics, excluding long acting depots",
                                                  "Long acting injectable and depot antipsychotics",
                                                  "Prochlorperazine")),
           value = ifelse(value < 8, 7, value)) %>%
    ggplot(aes(x = date, y = value, colour = variable)) +
    geom_line() +
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    theme(legend.position = "none") +
    ylab("Total number of patients issued antipsychotics, per month") +
    xlab("") +
    scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  ggsave(filename = here::here(paste("output/figures/total_antipsychotics_group_", group, ".png", sep = "")),
         total_antipsychotics,
         units = "cm", width = 40, height = 20
  )
}

plot_incident_antipsychotics_by_group <- function(group = "all"){
  total_antipsychotics <- data_processed_1yr %>%
    filter(group == group) %>%
    select(-group, -`Number of patients with first prescriptions`) %>%
    melt(id.vars = c("date")) %>%
    mutate(variable = factor(variable, labels = c("First generation antipsychotics (excluding long acting depots)",
                                                  "Second generation antipsychotics, excluding long acting depots",
                                                  "Long acting injectable and depot antipsychotics",
                                                  "Prochlorperazine")),
           value = ifelse(value < 8, 7, value)) %>%
    ggplot(aes(x = date, y = value, colour = variable)) +
    geom_line() +
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    theme(legend.position = "none") +
    ylab("Total number of patients with newly issued antipsychotics, per month") +
    xlab("") +
    scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  ggsave(filename = here::here(paste("output/figures/new_antipsychotics_group_", group, ".png", sep = "")),
         total_antipsychotics,
         units = "cm", width = 40, height = 20
  )
}

# Plot totals by demographic ----
plot_antipsychotics_by_demographic <- function(group = "age"){
  total_antipsychotics <- data_totals_demographics[[group]] %>%
    select(-group, groups = paste0(group)) %>%
    melt(id.vars = c("date", "groups")) %>%
    mutate(variable = factor(variable, labels = c("First generation antipsychotics (excluding long acting depots)",
                                                  "Second generation antipsychotics, excluding long acting depots",
                                                  "Long acting injectable and depot antipsychotics",
                                                  "Prochlorperazine")),
           value = ifelse(value < 8, 7, value)) %>%
    ggplot(aes(x = date, y = value, colour = groups)) +
    geom_line() +
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(colour=guide_legend(title=" ")) +
    ylab("Total number of patients issued antipsychotics, per month") +
    xlab("") +
    scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  ggsave(filename = here::here(paste("output/figures/total_antipsychotics_demographic_", group, ".png", sep = "")),
         total_antipsychotics,
         units = "cm", width = 40, height = 20
  )
}

## Global labels ----
add_global_label <- function(pwobj, Xlab = NULL, Ylab = NULL, Xgap = 0.03, Ygap = 0.03, ...) {
  ylabgrob <- patchwork::plot_spacer()
  if (!is.null(Ylab)) {
    ylabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Ylab, angle = 90, ...) +
      theme_void()
  }
  if (!is.null(Xlab)) {
    xlabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Xlab, ...) +
      theme_void()
  }
  if (!is.null(Ylab) & is.null(Xlab)) {
    return((ylabgrob + patchworkGrob(pwobj)) + 
             patchwork::plot_layout(widths = 100 * c(Ygap, 1 - Ygap)))
  }
  if (is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = c(0, 100),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  if (!is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = 100 * c(Ygap, 1 - Ygap),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  return(pwobj)
}

## Total number of patients issued antipsychotics by group
plot_antipsychotics_by_group <- function(group = "all"){
  data_totals_groups %>%
    filter(group == group) %>%
    select(-group) %>%
    melt(id.vars = c("date")) %>%
    mutate(variable = factor(variable, labels = c("First generation antipsychotics (excluding long acting depots)",
                                                  "Second generation antipsychotics, excluding long acting depots",
                                                  "Long acting injectable and depot antipsychotics",
                                                  "Prochlorperazine"))) %>%
    ggplot(aes(x = date, y = value, colour = variable)) +
    geom_line() +
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    theme(legend.position = "none") +
    ylab("Total number of patients issued antipsychotics, per month") +
    xlab("") +
    scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ggtitle()
}

## Whole cohort
calculate_data_cohort <- function(x) {
  
  # Read in and format data
  data_base <- arrow::read_feather(
    here::here("output", "data", x[1])) %>%
    mutate(date = as.Date(substr(x[1], 7, 16), format = "%Y-%m-%d"),
           ethnicity = ifelse(is.na(ethnicity), 6, ethnicity),
           ethnicity = fct_case_when(
             ethnicity == "1" ~ "White",
             ethnicity == "2" ~ "Mixed",
             ethnicity == "3" ~ "Asian or Asian British",
             ethnicity == "4" ~ "Black or Black British",
             ethnicity == "5" ~ "Other ethnic groups",
             ethnicity == "6" ~ "Unknown",
             #TRUE ~ "Unknown"
             TRUE ~ NA_character_),
           antipsychotic = ifelse((antipsychotics_first_gen == 1 |
                                     antipsychotics_second_gen ==1 |
                                     antipsychotics_injectable_and_depot == 1 |
                                     prochlorperazine == 1), 1, 0)) %>%
    select(patient_id, date, antipsychotic, 
           autism, care_home, dementia, learning_disability, serious_mental_illness,
           age, sex, region, imd, ethnicity)
  
  for(i in 2:length(filenames)) {
    
    data_add <- arrow::read_feather(
      here::here("output", "data", x[i])) %>%
      filter(!(patient_id %in% data_base$patient_id)) %>%
      mutate(date = as.Date(substr(x[i], 7, 16), format = "%Y-%m-%d"),
             ethnicity = ifelse(is.na(ethnicity), 6, ethnicity),
             ethnicity = fct_case_when(
               ethnicity == "1" ~ "White",
               ethnicity == "2" ~ "Mixed",
               ethnicity == "3" ~ "Asian or Asian British",
               ethnicity == "4" ~ "Black or Black British",
               ethnicity == "5" ~ "Other ethnic groups",
               ethnicity == "6" ~ "Unknown",
               #TRUE ~ "Unknown"
               TRUE ~ NA_character_),
             antipsychotic = ifelse((antipsychotics_first_gen == 1 |
                                       antipsychotics_second_gen ==1 |
                                       antipsychotics_injectable_and_depot == 1 |
                                       prochlorperazine == 1), 1, 0)) %>%
      select(patient_id, date, antipsychotic, 
             autism, care_home, dementia, learning_disability, serious_mental_illness,
             age, sex, region, imd, ethnicity)
    
    data_base <- rbind(data_base, data_add)
    
  }
  
  data_base

}
  




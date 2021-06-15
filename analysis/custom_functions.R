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
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
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
                  prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
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
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
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
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
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
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
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
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
      mutate(group = paste0(cohort)) %>%
      rename(age = ageband)
    
  } else if(cohort == "ethnicity"){
    
    data_extract_ethnicity <- arrow::read_feather(
      here::here("output", "data", "input_ethnicity.feather")) %>%
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
      select(patient_id, ethnicity_long, ethnicity_short)
    
    data_processed <-  data_extract %>%
      select(patient_id, date, antipsychotics_first_gen, antipsychotics_second_gen, 
             antipsychotics_injectable_and_depot, prochlorperazine, age) %>%
      left_join(data_extract_ethnicity, by = "patient_id") %>%
      mutate(
        # 16 categories
        ethnicity_long = fct_case_when(
        ethnicity_long == "1" ~ "White - British",
        ethnicity_long == "2" ~ "White - Irish",
        ethnicity_long == "3" ~ "White - Any other White background",
        ethnicity_long == "4" ~ "Mixed - White and Black Caribbean",
        ethnicity_long == "5" ~ "Mixed - White and Black African",
        ethnicity_long == "6" ~ "Mixed - White and Asian",
        ethnicity_long == "7" ~ "Mixed - Any other mixed background",
        ethnicity_long == "8" ~ "Asian or Asian British - Indian",
        ethnicity_long == "9" ~ "Asian or Asian British - Pakistani",
        ethnicity_long == "10" ~ "Asian or Asian British - Bangladeshi",
        ethnicity_long == "11" ~ "Asian or Asian British - Any other Asian background",
        ethnicity_long == "12" ~ "Black or Black British - Caribbean",
        ethnicity_long == "13" ~ "Black or Black British - African",
        ethnicity_long == "14" ~ "Black or Black British - Any other Black background",
        ethnicity_long == "15" ~ "Other ethnic groups - Chinese",
        ethnicity_long == "16" ~ "Other ethnic groups - Any other ethnic group",
        ethnicity_long == "17" ~ "Patients with any other ethnicity code",
        ethnicity_long == "18" ~ "Ethnicity not given - patient refused",
        ethnicity_long == "19" ~ "Ethnicity not stated",
        ethnicity_long == "20" ~ "Ethnicity not recorded",
        #TRUE ~ "Unknown",
        TRUE ~ NA_character_
      ),
      
      # 6 categories
      ethnicity_short = fct_case_when(
        ethnicity_short == "1" ~ "White",
        ethnicity_short == "2" ~ "Mixed",
        ethnicity_short == "3" ~ "Asian or Asian British",
        ethnicity_short == "4" ~ "Black or Black British",
        ethnicity_short == "5" ~ "Other ethnic groups",
        ethnicity_short == "6" ~ "Unknown",
        #TRUE ~ "Unknown"
        TRUE ~ NA_character_)) %>%
      group_by(date, ethnicity_short) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
      rename(ethnicity = ethnicity_short) %>%
      mutate(group = paste0(cohort))
    
  } else {
    
    data_processed <- data_extract %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine) %>%
      group_by(date) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
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
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
      mutate(group = paste0(cohort))
    
  }else {
    
    data_processed_1y <- data_extract %>%
      filter(antipsychotics_first_gen_incident_1yr == 1) %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine) %>%
      group_by(date) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
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
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
      mutate(group = paste0(cohort))
    
  }else {
    
    data_processed_2y <- data_extract %>%
      filter(antipsychotics_first_gen_incident_2yr == 1) %>%
      select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine) %>%
      group_by(date) %>%
      summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
                antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
                antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
                prochlorperazine = sum(prochlorperazine, na.rm = T), .groups = 'drop') %>%
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
                prochlorperazine_population = sum(prochlorperazine %in% c(0,1)), .groups = 'drop') %>%
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
                prochlorperazine_population = sum(prochlorperazine %in% c(0,1)), .groups = 'drop') %>%
      mutate(antipsychotics_first_gen = antipsychotics_first_gen_event/antipsychotics_first_gen_population*1000,
             antipsychotics_second_gen = antipsychotics_second_gen_event/antipsychotics_second_gen_population*1000,
             antipsychotics_injectable_and_depot = antipsychotics_injectable_and_depot_event/antipsychotics_injectable_and_depot_population*1000,
             prochlorperazine = prochlorperazine_event/prochlorperazine_population*1000) %>%
      select(date, practice, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine)
    
  }
  
}

# Plot totals by demographic ----
antipsychotic_plot_by_demographic <- function(antipsychotic = "antipsychotics_first_gen", cohort = "sex") {
  
  data_plot <- data_totals_demographics[[cohort]] %>%
    rename(y = paste0(antipsychotic),
           colour = paste0(cohort))
  
   ggplot(data_plot, aes(x = date, y = y, colour = colour)) +
    geom_line() + facet_wrap(~group, scales = "free") +
    theme_bw() +
    theme(legend.position = "right",
          legend.box.background = element_rect(color = "black"),
          legend.key.size = unit(0.5, 'cm'),
          legend.key.height = unit(0.5, 'cm'),
          legend.key.width = unit(0.2, 'cm'),
          legend.text = element_text(size=6),
          legend.title = element_blank()) +
    guides(colour = guide_legend(ncol = 1)) +
    ylab("") +
     xlab("") +
     scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
     theme(axis.text.x = element_text(angle = 60, hjust = 1))
   
}

## Global labels
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





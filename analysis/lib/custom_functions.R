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
    mutate(date = as.Date(as.character(date), format = "%Y-%M-%d"),
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
    mutate(date = as.Date(as.character(date), format = "%Y-%M-%d"),
           group = group)
  
  data_measures
  
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
           ethnicity = as.character(ethnicity),
           ethnicity = ifelse(is.na(ethnicity), "Missing", ethnicity),
           ethnicity = fct_case_when(
             ethnicity == "White" ~ "White",
             ethnicity == "Mixed" ~ "Mixed",
             ethnicity == "South Asian" ~ "Asian or Asian British",
             ethnicity == "Black" ~ "Black or Black British",
             ethnicity == "Other" ~ "Other ethnic groups",
             ethnicity == "Unknown" ~ "Unknown",
             ethnicity == "Missing" ~ "Missing",
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
             ethnicity = as.character(ethnicity),
             ethnicity = ifelse(is.na(ethnicity), "Missing", ethnicity),
             ethnicity = fct_case_when(
               ethnicity == "White" ~ "White",
               ethnicity == "Mixed" ~ "Mixed",
               ethnicity == "South Asian" ~ "Asian or Asian British",
               ethnicity == "Black" ~ "Black or Black British",
               ethnicity == "Other" ~ "Other ethnic groups",
               ethnicity == "Unknown" ~ "Unknown",
               ethnicity == "Missing" ~ "Missing",
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

## Table 2
calculate_table2 <-function(population = "autism", Y = 10000){
  
  table_2_all <- data_cohort %>% 
    mutate(ethnicity = fct_case_when(
             ethnicity == "White" ~ "White",
             ethnicity == "Mixed" ~ "Mixed",
             ethnicity == "South Asian" ~ "Asian or Asian British",
             ethnicity == "Black" ~ "Black or Black British",
             ethnicity == "Other" ~ "Other ethnic groups",
             ethnicity == "Unknown" ~ "Unknown",
             ethnicity == "Missing" ~ "Missing",
             #TRUE ~ "Unknown"
             TRUE ~ NA_character_),
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
             TRUE ~ NA_character_)) %>%
    mutate(ageband = cut(age,
                         breaks = c(0, 17, 24, 34, 44, 54, 69, 79, Inf),
                         labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-69", "70-79", "80+"),
                         right = FALSE)) %>%
    select(group = paste0(population),
           antipsychotic,
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
    mutate(antipsychotics_first_gen = ifelse(antipsychotics_first_gen < threshold, NA, antipsychotics_first_gen),
           antipsychotics_second_gen = ifelse(antipsychotics_second_gen < threshold, NA, antipsychotics_second_gen),
           antipsychotics_injectable_and_depot = ifelse(antipsychotics_injectable_and_depot < threshold, NA, antipsychotics_injectable_and_depot),
           prochlorperazine = ifelse(prochlorperazine < threshold, NA, prochlorperazine))
  
  ## Round to nearest 5
  table_redacted <- table_redacted %>%
    mutate(antipsychotics_first_gen = plyr::round_any(antipsychotics_first_gen, 5),
           antipsychotics_second_gen = plyr::round_any(antipsychotics_second_gen, 5),
           antipsychotics_injectable_and_depot = plyr::round_any(antipsychotics_injectable_and_depot, 5),
           prochlorperazine = plyr::round_any(prochlorperazine, 5))
  
  ## Replace na with [REDACTED]
  table_redacted <- table_redacted %>%
    replace(is.na(.), "[REDACTED]")
}


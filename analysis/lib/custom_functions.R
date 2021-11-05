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
        ethnicity == "Missing" ~ "Unknown",
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
      
      # Age
      ageband = cut(age,
                    breaks = c(0, 17, 24, 34, 44, 54, 69, 79, Inf),
                    labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-69", "70-79", "80+"),
                    right = FALSE)) %>%
    select(group = paste0(population),
           antipsychotic = antipsychotic_any,
           ageband, 
           sex,
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

# Plot totals and rates by group ----
plot_antipsychotics_by_group <- function(Group = "All", 
                                         data = data_prevalence_TPP, 
                                         type = "total",
                                         Y = 1000,
                                         folder = "TPP"){
  
  if (type == "total"){
    
    total_antipsychotics <- data %>%
      filter(group == Group) %>%
      select(-antipsychotic_any, -group, -population) %>%
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
      geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
      geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
    
    ggsave(filename = here::here(paste("released_outputs/", folder, "/figures/total_antipsychotics_group_", Group, ".png", sep = "")),
           total_antipsychotics,
           units = "cm", width = 40, height = 20
    )
    
  } else if(type == "rate"){
    
    rate_antipsychotics <- data %>%
      filter(group == Group) %>%
      select(-group) %>%
      melt(id.vars = c("date", "population")) %>%
      mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                                      .[,2], .[,4]),
             se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                                          .[,2], .[,4]),
             rate = exp(est)*Y,
             lci = exp(est - se)*Y,
             uci = exp(est + se)*Y) %>%
      mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                    "First generation antipsychotics (excluding long acting depots)",
                                                    "Second generation antipsychotics, excluding long acting depots",
                                                    "Long acting injectable and depot antipsychotics",
                                                    "Prochlorperazine"))) %>%
      filter(variable != "Any antipsychotic") %>%
      ggplot(aes(x = date, y = rate, colour = variable, group = 1)) +
      geom_line() +
      geom_ribbon(aes(ymin = lci, ymax = uci, fill = variable), alpha=0.2, colour = "transparent", show.legend = F) +
      facet_wrap(~variable, scales = "free") +
      theme_bw() +
      theme(legend.position = "none") +
      guides(colour = guide_legend(ncol=2)) +
      scale_colour_discrete(name = "") +
      ylab("Rate of patients issued antipsychotics, per 1000 registered patients") +
      xlab("") +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
      geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
    
    ggsave(filename = here::here(paste("released_outputs/", folder, "/figures/rate_antipsychotics_group_",Group, ".png", sep = "")),
           rate_antipsychotics,
           units = "cm", width = 40, height = 20)
    
  } else if (type == "total new") {
    
    total_antipsychotics <- data %>%
      filter(group == Group) %>%
      select(-antipsychotic_any, -group, -population) %>%
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
      ylab("Total number of patients newly prescribed antipsychotics, per month") +
      xlab("") +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
      geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
    
    ggsave(filename = here::here(paste("released_outputs/", folder, "/figures/new_total_antipsychotics_group_", Group, ".png", sep = "")),
           total_antipsychotics,
           units = "cm", width = 40, height = 20
    )
    
  } else {
    
    rate_antipsychotics <- data %>%
      filter(group == Group) %>%
      select(-group) %>%
      melt(id.vars = c("date", "population")) %>%
      mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                          .[,2], .[,4]),
             se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                         .[,2], .[,4]),
             rate = exp(est)*Y,
             lci = exp(est - se)*Y,
             uci = exp(est + se)*Y) %>%
      mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                    "First generation antipsychotics (excluding long acting depots)",
                                                    "Second generation antipsychotics, excluding long acting depots",
                                                    "Long acting injectable and depot antipsychotics",
                                                    "Prochlorperazine"))) %>%
      filter(variable != "Any antipsychotic") %>%
      ggplot(aes(x = date, y = rate, colour = variable, group = 1)) +
      geom_line() +
      geom_ribbon(aes(ymin = lci, ymax = uci, fill = variable), alpha=0.2, colour = "transparent", show.legend = F) +
      facet_wrap(~variable, scales = "free") +
      theme_bw() +
      theme(legend.position = "none") +
      guides(colour = guide_legend(ncol=2)) +
      scale_colour_discrete(name = "") +
      ylab("Rate of patients newly issued antipsychotics, per 1000 registered patients") +
      xlab("") +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
      geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
    
    ggsave(filename = here::here(paste("released_outputs/", folder, "/figures/new_rate_antipsychotics_group_",Group, ".png", sep = "")),
           rate_antipsychotics,
           units = "cm", width = 40, height = 20)
    
  }
  
}


## Combined TPP and EMIS plots
plot_antipsychotic_combined <- function(Group = "Dementia", 
                                        data_TPP = data_prevalence_TPP,
                                        data_EMIS = data_prevalence_EMIS,
                                        type = "rate",
                                        Y = 1000,
                                        folder = "Combined"){
  
  if(type == "rate"){
    
    rate_antipsychotics_TPP <- data_TPP %>%
      filter(group == Group) %>%
      select(-group) %>%
      melt(id.vars = c("date", "population")) %>%
      mutate(value = ifelse(is.na(value), 8, value)) %>%
      mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                          .[,2], .[,4]),
             se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                         .[,2], .[,4]),
             rate = exp(est)*Y,
             lci = exp(est - se)*Y,
             uci = exp(est + se)*Y) %>%
      mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                    "First generation antipsychotics (excluding long acting depots)",
                                                    "Second generation antipsychotics, excluding long acting depots",
                                                    "Long acting injectable and depot antipsychotics",
                                                    "Prochlorperazine"))) %>%
      mutate(backend = "TPP")
    
    rate_antipsychotics_EMIS <- data_EMIS %>%
      filter(group == Group) %>%
      select(-group) %>%
      melt(id.vars = c("date", "population")) %>%
      mutate(value = ifelse(is.na(value), 8, value)) %>%
      mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                          .[,2], .[,4]),
             se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                         .[,2], .[,4]),
             rate = exp(est)*Y,
             lci = exp(est - se)*Y,
             uci = exp(est + se)*Y) %>%
      mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                    "First generation antipsychotics (excluding long acting depots)",
                                                    "Second generation antipsychotics, excluding long acting depots",
                                                    "Long acting injectable and depot antipsychotics",
                                                    "Prochlorperazine"))) %>%
      mutate(backend = "EMIS")
    
    plot_all <- rbind(rate_antipsychotics_TPP, rate_antipsychotics_EMIS) %>%
      filter(variable == "Any antipsychotic") %>%
      ggplot(aes(x = date, y = rate, colour = backend, group = backend)) +
      geom_line() +
      geom_ribbon(aes(ymin = lci, ymax = uci, fill = backend), alpha=0.2, colour = "transparent", show.legend = F) +
      theme_bw() +
      theme(legend.position = "none") +
      facet_wrap(~variable, scales = "free") +
      guides(colour = guide_legend(ncol=2)) +
      scale_colour_discrete(name = "") +
      ylab("") +
      xlab("") +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
      geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
    
    plot_groups <- rbind(rate_antipsychotics_TPP, rate_antipsychotics_EMIS) %>%
      filter(variable != "Any antipsychotic") %>%
      ggplot(aes(x = date, y = rate, colour = backend, group = backend)) +
      geom_line() +
      geom_ribbon(aes(ymin = lci, ymax = uci, fill = backend), alpha=0.2, colour = "transparent", show.legend = F) +
      facet_wrap(~variable, scales = "free") +
      theme_bw() +
      theme(legend.position = "none") +
      guides(colour = guide_legend(ncol=2)) +
      scale_colour_discrete(name = "") +
      ylab("") +
      xlab("") +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
      geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
    
    rate_antipsychotics = plot_grid(plot_all, plot_groups, 
                                    nrow = 2, 
                                    rel_heights = c(2/5, 3/5), 
                                    labels = c("(a)", "(b)"),
                                    label_size = 12) 
    
    y.grob <- grid::textGrob("Rate of patients issued antipsychotics, per 1000 registered patients", 
                       gp=grid::gpar(col="black", fontsize=12), rot=90)
    
    rate_antipsychotics_plot <- grid.arrange(arrangeGrob(rate_antipsychotics, left = y.grob))
    
    ggsave(filename = here::here(paste("released_outputs/", folder, "/figures/rate_antipsychotics_combined_",Group, ".png", sep = "")),
           rate_antipsychotics_plot,
           units = "cm", width = 30, height = 40)
    
  } else {
    
    rate_antipsychotics_TPP <- data_TPP %>%
      filter(group == Group) %>%
      select(-group) %>%
      melt(id.vars = c("date", "population")) %>%
      mutate(value = ifelse(is.na(value), 8, value)) %>%
      mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                          .[,2], .[,4]),
             se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                         .[,2], .[,4]),
             rate = exp(est)*Y,
             lci = exp(est - se)*Y,
             uci = exp(est + se)*Y) %>%
      mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                    "First generation antipsychotics (excluding long acting depots)",
                                                    "Second generation antipsychotics, excluding long acting depots",
                                                    "Long acting injectable and depot antipsychotics",
                                                    "Prochlorperazine"))) %>%
      mutate(backend = "TPP")
    
    rate_antipsychotics_EMIS <- data_EMIS %>%
      filter(group == Group) %>%
      select(-group) %>%
      melt(id.vars = c("date", "population")) %>%
      mutate(value = ifelse(is.na(value), 8, value)) %>%
      mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                          .[,2], .[,4]),
             se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                         .[,2], .[,4]),
             rate = exp(est)*Y,
             lci = exp(est - se)*Y,
             uci = exp(est + se)*Y) %>%
      mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                    "First generation antipsychotics (excluding long acting depots)",
                                                    "Second generation antipsychotics, excluding long acting depots",
                                                    "Long acting injectable and depot antipsychotics",
                                                    "Prochlorperazine"))) %>%
      mutate(backend = "EMIS")
    
    plot_all <- rbind(rate_antipsychotics_TPP, rate_antipsychotics_EMIS) %>%
      filter(variable == "Any antipsychotic") %>%
      ggplot(aes(x = date, y = rate, colour = backend, group = backend)) +
      geom_line() +
      geom_ribbon(aes(ymin = lci, ymax = uci, fill = backend), alpha=0.2, colour = "transparent", show.legend = F) +
      theme_bw() +
      theme(legend.position = "none") +
      facet_wrap(~variable, scales = "free") +
      guides(colour = guide_legend(ncol=2)) +
      scale_colour_discrete(name = "") +
      ylab("") +
      xlab("") +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
      geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
    
    plot_groups <- rbind(rate_antipsychotics_TPP, rate_antipsychotics_EMIS) %>%
      filter(variable != "Any antipsychotic") %>%
      ggplot(aes(x = date, y = rate, colour = backend, group = backend)) +
      geom_line() +
      geom_ribbon(aes(ymin = lci, ymax = uci, fill = backend), alpha=0.2, colour = "transparent", show.legend = F) +
      facet_wrap(~variable, scales = "free") +
      theme_bw() +
      theme(legend.position = "none") +
      guides(colour = guide_legend(ncol=2)) +
      scale_colour_discrete(name = "") +
      ylab("") +
      xlab("") +
      scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
      geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
    
    rate_antipsychotics = plot_grid(plot_all, plot_groups, 
                                    nrow = 2, 
                                    rel_heights = c(2/5, 3/5), 
                                    labels = c("(a)", "(b)"),
                                    label_size = 12) 
    
    y.grob <- grid::textGrob("Rate of patients newly issued antipsychotics, per 1000 registered patients", 
                             gp=grid::gpar(col="black", fontsize=12), rot=90)
    
    rate_antipsychotics_plot <- grid.arrange(arrangeGrob(rate_antipsychotics, left = y.grob))
    
    ggsave(filename = here::here(paste("released_outputs/", folder, "/figures/new_rate_antipsychotics_combined_",Group, ".png", sep = "")),
           rate_antipsychotics_plot,
           units = "cm", width = 30, height = 40)
    
  }
  
}

## Combine TPP and EMIS table 1
combine_table1 <- function(TPP_Table = table1_TPP, EMIS_Table = table1_EMIS){
  
  ## TPP
  ## Calculate group totals
  table1_TPP_totals <- TPP_Table %>%
    group_by(group) %>%
    summarise(g_total = sum(total, na.rm = T),
              g_nonantipsychotic = sum(nonantipsychotic, na.rm = T),
              g_antipsychotic = sum(antipsychotic, na.rm = T)) 
  
  ## Add overall total
  table1_TPP_totals <- table1_TPP_totals %>%
    add_row(group = "All", 
            g_total = table1_TPP_totals$g_total[1],
            g_nonantipsychotic = table1_TPP_totals$g_nonantipsychotic[1],
            g_antipsychotic = table1_TPP_totals$g_antipsychotic[1], .before = 1)
  
  ## Format TPP table
  table1_TPP_formatted <- TPP_Table %>%
    add_row(group = "All", variable = "", total = table1_TPP_totals$g_total[1],
            nonantipsychotic = table1_TPP_totals$g_nonantipsychotic[1],
            antipsychotic = table1_TPP_totals$g_antipsychotic[1], .before = 1) %>%
    left_join(table1_TPP_totals) %>%
    mutate(total_TPP = paste(format(total, big.mark = ",", scientific = FALSE), " (", round(total/g_total*100, digits = 0), ")", sep = ""),
           nonantipsychotic_TPP = paste(format(nonantipsychotic, big.mark = ",", scientific = FALSE), " (", round(nonantipsychotic/g_nonantipsychotic*100, digits = 0), ")", sep = ""),
           antipsychotic_TPP = paste(format(antipsychotic, big.mark = ",", scientific = FALSE), " (", round(antipsychotic/g_antipsychotic*100, digits = 0), ")", sep = "")) %>%
    select(group, variable, total_TPP, nonantipsychotic_TPP, antipsychotic_TPP)
  
  ## EMIS
  ## Calculate group totals
  table1_EMIS_totals <- EMIS_Table %>%
    group_by(group) %>%
    summarise(g_total = sum(total, na.rm = T),
              g_nonantipsychotic = sum(nonantipsychotic, na.rm = T),
              g_antipsychotic = sum(antipsychotic, na.rm = T)) 
  
  ## Add overall total
  table1_EMIS_totals <- table1_EMIS_totals %>%
    add_row(group = "All", 
            g_total = table1_EMIS_totals$g_total[1],
            g_nonantipsychotic = table1_EMIS_totals$g_nonantipsychotic[1],
            g_antipsychotic = table1_EMIS_totals$g_antipsychotic[1], .before = 1)
  
  ## Format EMIS table
  table1_EMIS_formatted <- EMIS_Table %>%
    add_row(group = "All", variable = "", total = table1_EMIS_totals$g_total[1],
            nonantipsychotic = table1_EMIS_totals$g_nonantipsychotic[1],
            antipsychotic = table1_EMIS_totals$g_antipsychotic[1], .before = 1) %>%
    left_join(table1_EMIS_totals) %>%
    mutate(total_EMIS = paste(format(total, big.mark = ",", scientific = FALSE), " (", round(total/g_total*100, digits = 0), ")", sep = ""),
           nonantipsychotic_EMIS = paste(format(nonantipsychotic, big.mark = ",", scientific = FALSE), " (", round(nonantipsychotic/g_nonantipsychotic*100, digits = 0), ")", sep = ""),
           antipsychotic_EMIS = paste(format(antipsychotic, big.mark = ",", scientific = FALSE), " (", round(antipsychotic/g_antipsychotic*100, digits = 0), ")", sep = "")) %>%
    select(group, variable, total_EMIS, nonantipsychotic_EMIS, antipsychotic_EMIS)
  
  ## Combined table
  table1_combined <- left_join(table1_TPP, table1_EMIS, by = c("group", "variable")) %>%
    rowwise() %>%
    mutate(total = sum(total.x, total.y, na.rm = T),
           nonantipsychotic = sum(nonantipsychotic.x, nonantipsychotic.y, na.rm = T),
           antipsychotic = sum(antipsychotic.x, antipsychotic.y, na.rm = T))
  
  table1_combined_totals <- table1_combined %>%
    group_by(group) %>%
    summarise(g_total = sum(total, na.rm = T),
              g_nonantipsychotic = sum(nonantipsychotic, na.rm = T),
              g_antipsychotic = sum(antipsychotic, na.rm = T))
  
  table1_combined_formatted <- table1_combined %>%
    left_join(table1_combined_totals) %>%
    mutate(total = paste(format(total, big.mark = ",", scientific = FALSE), " (", round(total/g_total*100, digits = 0), ")", sep = ""),
           nonantipsychotic = paste(format(nonantipsychotic, big.mark = ",", scientific = FALSE), " (", round(nonantipsychotic/g_nonantipsychotic*100, digits = 0), ")", sep = ""),
           antipsychotic = paste(format(antipsychotic, big.mark = ",", scientific = FALSE), " (", round(antipsychotic/g_antipsychotic*100, digits = 0), ")", sep = "")) %>%
    select(group, variable, total, nonantipsychotic, antipsychotic)
  
  ## Combine all three tables
  table1_final <- left_join(table1_TPP_formatted, table1_EMIS_formatted, by = c("group", "variable"))  %>%
    left_join(table1_combined_formatted, by = c("group", "variable")) 
  
  table1_final[1,9] <- paste(table1_TPP_totals[1,2:4] + table1_EMIS_totals[1,2:4], " (100)", sep = "")[1]
  table1_final[1,10] <- paste(table1_TPP_totals[1,2:4] + table1_EMIS_totals[1,2:4], " (100)", sep = "")[2]
  table1_final[1,11] <- paste(table1_TPP_totals[1,2:4] + table1_EMIS_totals[1,2:4], " (100)", sep = "")[3]
  
  table1_final
}

## Combine TPP and EMIS table 2
combine_table2 <- function(TPP_Table = table2_autism_TPP, EMIS_Table = table2_autism_EMIS){
  
  # TPP
  ## Calculate group totals
  table2_TPP_totals <- TPP_Table %>%
    mutate(population = as.numeric(population),
           antipsychotic = as.numeric(antipsychotic)) %>%
    group_by(group) %>%
    summarise(g_population = sum(population, na.rm = T),
              g_antipsychotic = sum(antipsychotic, na.rm = T)) 
  
  ## Add overall total
  table2_TPP_totals <- table2_TPP_totals %>%
    add_row(group = "All", 
            g_population = table2_TPP_totals$g_population[1],
            g_antipsychotic = table2_TPP_totals$g_antipsychotic[1], .before = 1)
  
  ## Format TPP table
  table2_TPP_formatted <- TPP_Table %>%
    mutate(population = as.numeric(population),
           antipsychotic = as.numeric(antipsychotic)) %>%
    add_row(group = "All", variable = "", population = table2_TPP_totals$g_population[1],
            antipsychotic = table2_TPP_totals$g_antipsychotic[1], .before = 1) %>%
    mutate(total_TPP = paste(format(antipsychotic, big.mark = ",", scientific = FALSE), " (", round(antipsychotic/population*100, digits = 0), ")", sep = ""),
           rate = round(antipsychotic/population*1000, digits = 2)) %>%
    select(group, variable, total_TPP, rate)
  
  # EMIS
  ## Calculate group totals
  table2_EMIS_totals <- EMIS_Table %>%
    mutate(population = as.numeric(population),
           antipsychotic = as.numeric(antipsychotic)) %>%
    group_by(group) %>%
    summarise(g_population = sum(population, na.rm = T),
              g_antipsychotic = sum(antipsychotic, na.rm = T)) 
  
  ## Add overall total
  table2_EMIS_totals <- table2_EMIS_totals %>%
    add_row(group = "All", 
            g_population = table2_EMIS_totals$g_population[2],
            g_antipsychotic = table2_EMIS_totals$g_antipsychotic[2], .before = 1)
  
  ## Format EMIS table
  table2_EMIS_formatted <- EMIS_Table %>%
    mutate(population = as.numeric(population),
           antipsychotic = as.numeric(antipsychotic)) %>%
    add_row(group = "All", variable = "", population = table2_EMIS_totals$g_population[1],
            antipsychotic = table2_EMIS_totals$g_antipsychotic[1], .before = 1) %>%
    mutate(total_EMIS = paste(format(antipsychotic, big.mark = ",", scientific = FALSE), " (", round(antipsychotic/population*100, digits = 0), ")", sep = ""),
           rate = round(antipsychotic/population*1000, digits = 2)) %>%
    select(group, variable, total_EMIS, rate)
  
  ## Combined table
  table2_combined <- left_join(TPP_Table, EMIS_Table, by = c("group", "variable")) %>%
    mutate(population.x = as.numeric(population.x),
           antipsychotic.x = as.numeric(antipsychotic.x),
           population.y = as.numeric(population.y),
           antipsychotic.y = as.numeric(antipsychotic.y)) %>%
    rowwise() %>%
    mutate(population = sum(population.x, population.y, na.rm = T),
           antipsychotic = sum(antipsychotic.x, antipsychotic.y, na.rm = T))
  
  table2_combined_totals <- table2_combined %>%
    group_by(group) %>%
    summarise(g_population = sum(population, na.rm = T),
              g_antipsychotic = sum(antipsychotic, na.rm = T))
  
  table2_combined_formatted <- table2_combined %>%
    left_join(table2_combined_totals) %>%
    mutate(total = paste(format(antipsychotic, big.mark = ",", scientific = FALSE), " (", round(antipsychotic/population*100, digits = 0), ")", sep = ""),
           rate = round(antipsychotic/population*1000, digits = 2),
           lower = round((ifelse(antipsychotic/population - qnorm(0.975)*(sqrt(antipsychotic/(population^2))) < 0, 0, 
                          antipsychotic/population - qnorm(0.975)*(sqrt(antipsychotic/(population^2)))))*1000, digits = 2),
           upper = round((ifelse(antipsychotic/population + qnorm(0.975)*(sqrt(antipsychotic/(population^2))) < 0, 0, 
                                 antipsychotic/population + qnorm(0.975)*(sqrt(antipsychotic/(population^2)))))*1000, digits = 2)) %>%
    select(group, variable, total, rate, lower, upper)
  
  ## Combine all three tables
  table2_final <- left_join(table2_TPP_formatted, table2_EMIS_formatted, by = c("group", "variable"))  %>%
    left_join(table2_combined_formatted, by = c("group", "variable")) 
  
  all <- table2_TPP_totals[1,2:3] + table2_EMIS_totals[1,2:3]
  
  table2_final[1,7] <- paste(format(all$g_antipsychotic, big.mark = ",", scientific = FALSE), " (", round(all$g_antipsychotic/all$g_population*100, digits = 0), ")", sep = "")
  table2_final[1,8] <- round(all$g_antipsychotic/all$g_population*1000, digits = 2)
  table2_final[1,9] <- round((all$g_antipsychotic/all$g_population - qnorm(0.975)*(sqrt(all$g_antipsychotic/all$g_population^2)))*1000, digits = 2)
  table2_final[1,10] <- round((all$g_antipsychotic/all$g_population + qnorm(0.975)*(sqrt(all$g_antipsychotic/all$g_population^2)))*1000, digits = 2)
  
  table2_final
}


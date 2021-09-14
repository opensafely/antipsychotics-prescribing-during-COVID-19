######################################

# This script:
# - imports data
# - saves data summaries (tables and figures)

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
#library('egg')
library('patchwork')

## Create output directory
dir.create(here::here("output", "figures"), showWarnings = FALSE, recursive=TRUE)

## Custom functions
source(here("analysis", "custom_functions.R"))

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


# Redacted figures ----

## Total number of patients issued antipsychotics by demographic

### First generation antipsychotics, excluding long acting depots
antipsychotics_first_gen_sex <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "sex")
antipsychotics_first_gen_eth <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "ethnicity")
antipsychotics_first_gen_re <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "region")
antipsychotics_first_gen_age <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "age")
antipsychotics_first_gen_imd <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "imd")
antipsychotics_first_gen_stp <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "stp")

antipsychotics_first_gen <- add_global_label((aantipsychotics_first_gen <- antipsychotics_first_gen_sex + antipsychotics_first_gen_eth + 
                                                antipsychotics_first_gen_re + antipsychotics_first_gen_age + 
                                                antipsychotics_first_gen_imd + antipsychotics_first_gen_stp + plot_layout(ncol = 3)),
                                             Ylab = "Total number of patients issued first generation antipsychotics \n (excluding long acting depots), per month")

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_first_gen_by_demographic.svg"),
       antipsychotics_first_gen,
       units = "cm", width = 40, height = 20
)

### Second generation antipsychotics, excluding long acting depots
antipsychotics_second_gen_sex <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "sex")
antipsychotics_second_gen_eth <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "ethnicity")
antipsychotics_second_gen_re <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "region")
antipsychotics_second_gen_age <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "age")
antipsychotics_second_gen_imd <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "imd")
antipsychotics_second_gen_stp <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "stp")

antipsychotics_second_gen <- add_global_label((aantipsychotics_second_gen <- antipsychotics_second_gen_sex + antipsychotics_second_gen_eth + 
                                                 antipsychotics_second_gen_re + antipsychotics_second_gen_age + 
                                                 antipsychotics_second_gen_imd + antipsychotics_second_gen_stp + plot_layout(ncol = 3)),
                                              Ylab = "Total number of patients issued first generation antipsychotics \n (excluding long acting depots), per month")

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_second_gen_by_demographic.svg"),
       antipsychotics_second_gen,
       units = "cm", width = 40, height = 20
)

### Long acting injectable and depot antipsychotics
antipsychotics_injectable_and_depot_sex <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "sex")
antipsychotics_injectable_and_depot_eth <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "ethnicity")
antipsychotics_injectable_and_depot_re <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "region")
antipsychotics_injectable_and_depot_age <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "age")
antipsychotics_injectable_and_depot_imd <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "imd")
antipsychotics_injectable_and_depot_stp <- antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "stp")

antipsychotics_injectable_and_depot <- add_global_label((aantipsychotics_injectable_and_depot <- antipsychotics_injectable_and_depot_sex + antipsychotics_injectable_and_depot_eth + 
                                                           antipsychotics_injectable_and_depot_re + antipsychotics_injectable_and_depot_age + 
                                                           antipsychotics_injectable_and_depot_imd + antipsychotics_injectable_and_depot_stp + plot_layout(ncol = 3)),
                                                        Ylab = "Total number of patients issued first generation antipsychotics \n (excluding long acting depots), per month")

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_injectable_and_depot_by_demographic.svg"),
       antipsychotics_injectable_and_depot,
       units = "cm", width = 40, height = 20
)

### Prochlorperazine
prochlorperazine_sex <- antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "sex")
prochlorperazine_eth <- antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "ethnicity")
prochlorperazine_re <- antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "region")
prochlorperazine_age <- antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "age")
prochlorperazine_imd <- antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "imd")
prochlorperazine_stp <- antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "stp")

prochlorperazine <- add_global_label((aprochlorperazine <- prochlorperazine_sex + prochlorperazine_eth + 
                                        prochlorperazine_re + prochlorperazine_age + 
                                        prochlorperazine_imd + prochlorperazine_stp + plot_layout(ncol = 3)),
                                     Ylab = "Total number of patients issued first generation antipsychotics \n (excluding long acting depots), per month")

ggsave(filename=here::here("output", "figures", "plot_total_prochlorperazine_by_demographic.svg"),
       prochlorperazine,
       units = "cm", width = 40, height = 20
)


## Total number of patients with newly issued antipsychotics 

### First generation antipsychotics, excluding long acting depots
antipsychotics_first_gen <- ggplot(data_incident_groups %>% filter(!group %in% c("autism", "learning_disability")), 
                                   aes(x = date, y = antipsychotics_first_gen, colour = group, linetype = `Number of patients with first prescriptions`)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour=FALSE, linetype = guide_legend(nrow = 2)) +
  ylab("Total number of patients issued first prescription of \n first generation antipsychotics (excluding long acting depots), per month") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename=here::here("output", "figures", "plot_first_antipsychotics_first_gen.svg"),
       antipsychotics_first_gen,
       units = "cm", width = 40, height = 20
)

### Second generation antipsychotics, excluding long acting depots
antipsychotics_second_gen <-  ggplot(data_incident_groups %>% filter(!group %in% c("autism", "learning_disability")),
                                     aes(x = date, y = antipsychotics_second_gen, colour = group, linetype = `Number of patients with first prescriptions`)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour=FALSE, linetype = guide_legend(nrow = 2)) +
  ylab("Total number of patients issued first prescription of \n second generation antipsychotics (excluding long acting depots), per month") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename=here::here("output", "figures", "plot_first_antipsychotics_second_gen.svg"),
       antipsychotics_second_gen,
       units = "cm", width = 40, height = 20
)

### Long acting injectable and depot antipsychotics
antipsychotics_injectable_and_depot <-  ggplot(data_incident_groups %>% filter(group == "all"),
                                               aes(x = date, y = antipsychotics_injectable_and_depot, colour = group, linetype = `Number of patients with first prescriptions`)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour=FALSE, linetype = guide_legend(nrow = 2)) +
  ylab("Total number of patients issued first prescription of \n injectable and depot antipsychotics, per month") +
  xlab("date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename=here::here("output", "figures", "plot_first_antipsychotics_injectable_and_depot.svg"),
       antipsychotics_injectable_and_depot,
       units = "cm", width = 40, height = 20
)

### Prochlorperazine
prochlorperazine <-  ggplot(data_incident_groups %>% filter(!group %in% c("autism", "learning_disability")),
                            aes(x = date, y = prochlorperazine, colour = group, linetype = `Number of patients with first prescriptions`)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour=FALSE, linetype = guide_legend(nrow = 2)) +
  ylab("Total number of patients issued first prescription of \n prochlorperazine antipsychotics, per month") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename=here::here("output", "figures", "plot_first_prochlorperazine.svg"),
       prochlorperazine,
       units = "cm", width = 40, height = 20
)

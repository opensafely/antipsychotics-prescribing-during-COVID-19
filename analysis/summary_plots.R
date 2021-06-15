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
library('egg')

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


# Figures ----

## Total number of patients issued antipsychotics by group

### First generation antipsychotics, excluding long acting depots
antipsychotics_first_gen <- ggplot(data_totals_groups, aes(x = date, y = antipsychotics_first_gen, colour = group)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Total number of patients issued \n first generation antipsychotics (excluding long acting depots), per month") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_first_gen_by_group.svg"),
       antipsychotics_first_gen,
       units = "cm", width = 40, height = 20
)

### Second generation antipsychotics, excluding long acting depots
antipsychotics_second_gen <-  ggplot(data_totals_groups, aes(x = date, y = antipsychotics_second_gen, colour = group)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Total number of patients issued \n second generation antipsychotics (excluding long acting depots), per month") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_second_gen_by_group.svg"),
       antipsychotics_second_gen,
       units = "cm", width = 40, height = 20
)

### Long acting injectable and depot antipsychotics
antipsychotics_injectable_and_depot <-  ggplot(data_totals_groups, aes(x = date, y = antipsychotics_injectable_and_depot, colour = group)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Total number of patients issued \n injectable and depot antipsychotics, per month") +
  xlab("date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_injectable_and_depot_by_group.svg"),
       antipsychotics_injectable_and_depot,
       units = "cm", width = 40, height = 20
)

### Prochlorperazine
prochlorperazine <-  ggplot(data_totals_groups, aes(x = date, y = prochlorperazine, colour = group)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Total number of patients issued \n prochlorperazine antipsychotics, per month") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename=here::here("output", "figures", "plot_total_prochlorperazine_by_group.svg"),
       prochlorperazine,
       units = "cm", width = 40, height = 20
)


## Total number of patients issued antipsychotics by demographic

### First generation antipsychotics, excluding long acting depots
antipsychotics_first_gen <- egg::ggarrange(antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "sex"),
                                           antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "ethnicity"),
                                           antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "region"),
                                           antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "age"),
                                           antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "imd"),
                                           antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_first_gen", cohort = "stp"),
                                           ncol=3,
                                           left = "Total number of patients issued first generation antipsychotics \n (excluding long acting depots), per month")

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_first_gen_by_demographic.svg"),
       antipsychotics_first_gen,
       units = "cm", width = 40, height = 20
)

### Second generation antipsychotics, excluding long acting depots
antipsychotics_second_gen <- egg::ggarrange(antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "sex"),
                                            antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "ethnicity"),
                                            antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "region"),
                                            antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "age"),
                                            antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "imd"),
                                            antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_second_gen", cohort = "stp"),
                                            ncol=3,
                                            left = "Total number of patients issued second generation antipsychotics \n (excluding long acting depots), per month")

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_second_gen_by_demographic.svg"),
       antipsychotics_second_gen,
       units = "cm", width = 40, height = 20
)

### Long acting injectable and depot antipsychotics
antipsychotics_injectable_and_depot <- egg::ggarrange(antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "sex"),
                                                      antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "ethnicity"),
                                                      antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "region"),
                                                      antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "age"),
                                                      antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "imd"),
                                                      antipsychotic_plot_by_demographic(antipsychotic = "antipsychotics_injectable_and_depot", cohort = "stp"),
                                                      ncol=3,
                                                      left = "Total number of patients issued injectable and depot antipsychotics, per month")

ggsave(filename=here::here("output", "figures", "plot_total_antipsychotics_injectable_and_depot_by_demographic.svg"),
       antipsychotics_second_gen,
       units = "cm", width = 40, height = 20
)

### Prochlorperazine
prochlorperazine <- egg::ggarrange(antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "sex"),
                                   antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "ethnicity"),
                                   antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "region"),
                                   antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "age"),
                                   antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "imd"),
                                   antipsychotic_plot_by_demographic(antipsychotic = "prochlorperazine", cohort = "stp"),
                                   ncol=3,
                                   left = "Total number of patients issued prochlorperazine antipsychotics, per month")

ggsave(filename=here::here("output", "figures", "plot_total_prochlorperazine_by_demographic.svg"),
       antipsychotics_second_gen,
       units = "cm", width = 40, height = 20
)


## Total number of patients with newly issued antipsychotics 

### First generation antipsychotics, excluding long acting depots
antipsychotics_first_gen <- ggplot(data_process_incident, aes(x = date, y = antipsychotics_first_gen, colour = group, linetype = `Number of patients with first prescriptions`)) +
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
antipsychotics_second_gen <-  ggplot(data_processed, aes(x = date, y = antipsychotics_second_gen, colour = group, linetype = `Number of patients with first prescriptions`)) +
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
antipsychotics_injectable_and_depot <-  ggplot(data_processed, aes(x = date, y = antipsychotics_injectable_and_depot, colour = group, linetype = `Number of patients with first prescriptions`)) +
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
prochlorperazine <-  ggplot(data_processed, aes(x = date, y = prochlorperazine, colour = group, linetype = `Number of patients with first prescriptions`)) +
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
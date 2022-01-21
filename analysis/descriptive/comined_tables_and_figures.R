######################################

# This script:
# - imports data
# - combines TPP and EMIS tables and saves as new table
# - plots : 1. total number and rate of patients issued antipsychotics, by clinical and demographic groups
#           2. total number and rate of new patients issued antipsychotics, by clinical groups

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
library('patchwork')
library('cowplot')
library('gridExtra')

## Custom functions
source(here("analysis", "lib", "custom_functions.R"))

## Create output directories
dir.create(here::here("released_outputs", "TPP", "figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("released_outputs", "EMIS", "figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("released_outputs", "Combined", "figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("released_outputs", "Combined", "tables"), showWarnings = FALSE, recursive=TRUE)

## Import processed data

### TPP
data_prevalence_TPP <- read_csv(file = here::here("released_outputs", "TPP", "data", "data_prevalence_redacted.csv"))[,-1]
data_incidence_TPP <- read_csv(file = here::here("released_outputs", "TPP", "data", "data_incident_redacted.csv"))[,-1]

table1_TPP <- read_csv(file = here::here("released_outputs", "TPP",  "tables", "table1_redacted.csv"))
table2_autism_TPP <- read_csv(file = here::here("released_outputs", "TPP",  "tables", "table2_autism_redacted.csv"))
table2_care_home_TPP <- read_csv(file = here::here("released_outputs", "TPP",  "tables", "table2_care_home_redacted.csv"))
table2_dementia_TPP <- read_csv(file = here::here("released_outputs", "TPP",  "tables", "table2_dementia_redacted.csv"))
table2_ld_TPP <- read_csv(file = here::here("released_outputs", "TPP",  "tables", "table2_ld_redacted.csv"))
table2_smi_TPP <- read_csv(file = here::here("released_outputs", "TPP",  "tables", "table2_smi_redacted.csv"))

### EMIS
data_prevalence_EMIS <- read_csv(file = here::here("released_outputs", "EMIS", "data", "data_prevalence_redacted.csv"))[,-1]
data_incidence_EMIS <- read_csv(file = here::here("released_outputs", "EMIS", "data", "data_incident_redacted.csv"))[,-1]

table1_EMIS <- read_csv(file = here::here("released_outputs", "EMIS",  "tables", "table1_redacted.csv"))
table2_autism_EMIS <- read_csv(file = here::here("released_outputs", "EMIS",  "tables", "table2_autism_redacted.csv"))
table2_care_home_EMIS <- read_csv(file = here::here("released_outputs", "EMIS",  "tables", "table2_care_home_redacted.csv"))
table2_dementia_EMIS <- read_csv(file = here::here("released_outputs", "EMIS",  "tables", "table2_dementia_redacted.csv"))
table2_ld_EMIS <- read_csv(file = here::here("released_outputs", "EMIS",  "tables", "table2_ld_redacted.csv"))
table2_smi_EMIS <- read_csv(file = here::here("released_outputs", "EMIS",  "tables", "table2_smi_redacted.csv"))

## Format data
colnames(data_incidence_TPP) <- colnames(data_prevalence_TPP)
colnames(data_incidence_EMIS) <- colnames(data_prevalence_EMIS)

data_prevalence_TPP <- data_prevalence_TPP %>%
  mutate(group = factor(group, labels = c("All", "Autism", "Care Home", 
                                          "Dementia", "Learning Disability", 
                                          "Serious Mental Illness")))
data_incidence_TPP <- data_incidence_TPP %>%
  mutate(group = factor(group, labels = c("All", "Autism", "Care Home", 
                                          "Dementia", "Learning Disability", 
                                          "Serious Mental Illness")))

data_prevalence_EMIS <- data_prevalence_EMIS %>%
  mutate(group = factor(group, labels = c("All", "Autism", "Care Home", 
                                          "Dementia", "Learning Disability", 
                                          "Serious Mental Illness")))
data_incidence_EMIS <- data_incidence_EMIS %>%
  mutate(group = factor(group, labels = c("All", "Autism", "Care Home", 
                                          "Dementia", "Learning Disability", 
                                          "Serious Mental Illness")))

## Combine data
data_prevalence <- rbind(data_prevalence_TPP, data_prevalence_EMIS) %>%
  group_by(date, group) %>%
  summarise(antipsychotic_any = sum(antipsychotic_any, na.rm = T),
            antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
            antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
            antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
            prochlorperazine = sum(prochlorperazine, na.rm = T),
            alive_2weeks_post_antipsychotic = sum(alive_2weeks_post_antipsychotic, na.rm = T),
            antipsychotic_without_midazolam = sum(antipsychotic_without_midazolam, na.rm = T),
            population = sum(population, na.rm = T))

data_incidence <- rbind(data_incidence_TPP, data_incidence_EMIS) %>%
  group_by(date, group) %>%
  summarise(antipsychotic_any = sum(antipsychotic_any, na.rm = T),
            antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
            antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
            antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
            prochlorperazine = sum(prochlorperazine, na.rm = T),
            alive_2weeks_post_antipsychotic = sum(alive_2weeks_post_antipsychotic, na.rm = T),
            antipsychotic_without_midazolam = sum(antipsychotic_without_midazolam, na.rm = T),
            population = sum(population, na.rm = T))



# Flow chart ----
TPP <- rbind(table2_autism_TPP,
             table2_care_home_TPP,
             table2_dementia_TPP,
             table2_ld_TPP,
             table2_smi_TPP) %>%
  mutate(cohort = rep(c("autism", "care-home", "demetia", "ld", "smi"), each = 43)) %>%
  filter(group == "sex") %>%
  group_by(cohort) %>%
  mutate(antipsychotic = as.numeric(antipsychotic),
         population = as.numeric(population)) %>%
  summarise(antipsychotic = sum(antipsychotic),
            population = sum(population))

EMIS <- rbind(table2_autism_EMIS,
              table2_care_home_EMIS,
              table2_dementia_EMIS,
              table2_ld_EMIS,
              table2_smi_EMIS) %>%
  mutate(cohort = rep(c("autism", "care-home", "demetia", "ld", "smi"), each = 40)) %>%
  filter(group == "sex") %>%
  group_by(cohort) %>%
  mutate(antipsychotic = as.numeric(antipsychotic),
         population = as.numeric(population)) %>%
  summarise(antipsychotic = sum(antipsychotic),
            population = sum(population))


# Tables ----

## Table 1
table1 <- combine_table1(TPP_Table = table1_TPP, EMIS_Table = table1_EMIS)
write_csv(table1, here::here("released_outputs", "Combined",  "tables", "table1.csv"))

## Table 2
table2 <- combine_table2(TPP_Table = table1_TPP %>% select(group, variable, population = total, antipsychotic), 
                         EMIS_Table = table1_EMIS %>% select(group, variable, population = total, antipsychotic))
table2_autism <- combine_table2(TPP_Table = table2_autism_TPP, EMIS_Table = table2_autism_EMIS)
table2_care_home <- combine_table2(TPP_Table = table2_care_home_TPP, EMIS_Table = table2_care_home_EMIS)
table2_dementia <- combine_table2(TPP_Table = table2_dementia_TPP, EMIS_Table = table2_dementia_EMIS)
table2_ld<- combine_table2(TPP_Table = table2_ld_TPP, EMIS_Table = table2_ld_EMIS)
table2_smi<- combine_table2(TPP_Table = table2_smi_TPP, EMIS_Table = table2_smi_EMIS)

write_csv(table2, here::here("released_outputs", "Combined",  "tables", "table2.csv"))
write_csv(table2_autism, here::here("released_outputs", "Combined",  "tables", "table2_autism.csv"))
write_csv(table2_care_home, here::here("released_outputs", "Combined",  "tables", "table2_care_home.csv"))
write_csv(table2_dementia, here::here("released_outputs", "Combined",  "tables", "table2_dementia.csv"))
write_csv(table2_ld, here::here("released_outputs", "Combined",  "tables", "table2_ld.csv"))
write_csv(table2_smi, here::here("released_outputs", "Combined",  "tables", "table2_smi.csv"))


# Figures ----
groups <- c("All", "Autism", "Care Home", "Dementia", "Learning Disability", "Serious Mental Illness")

## Total number of patients issued antipsychotics, by group
lapply(groups, data = data_prevalence_TPP, type = "total", folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence_EMIS, type = "total", folder = "EMIS",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence, type = "total", folder = "Combined",
       FUN = plot_antipsychotics_by_group)

lapply(groups, FUN = population_plots)

## Rate of patients issued antipsychotics, by group
lapply(groups, data = data_prevalence_TPP, type = "rate", Y = 1000, folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence_EMIS, type = "rate", Y = 1000, folder = "EMIS",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence, type = "rate", Y = 1000, folder = "Combined",
       FUN = plot_antipsychotics_by_group)

lapply(groups, data_TPP = data_prevalence_TPP, data_EMIS = data_prevalence_EMIS, type = "rate", Y = 1000, 
       folder = "Combined", FUN = plot_antipsychotic_combined)

## All-in-one rate plot
### TPP
plot_rates_all_TPP <- data_prevalence_TPP %>%
  select(-alive_2weeks_post_antipsychotic, -antipsychotic_without_midazolam) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "First generation antipsychotics (excluding long acting depots)",
                                                "Second generation antipsychotics, excluding long acting depots",
                                                "Long acting injectable and depot antipsychotics",
                                                "Prochlorperazine"))) %>%
  filter(variable == "Any antipsychotic") %>%
  ggplot(aes(x = date, y = rate, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, scales = "free", ncol = 2) +
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

ggsave(filename = here::here("released_outputs/TPP/figures/rate_antipsychotics_all.png"),
       plot_rates_all_TPP,
       units = "cm", width = 35, height = 40)

### EMIS
plot_rates_all_EMIS <- data_prevalence_EMIS %>%
  select(-alive_2weeks_post_antipsychotic, -antipsychotic_without_midazolam) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "First generation antipsychotics (excluding long acting depots)",
                                                "Second generation antipsychotics, excluding long acting depots",
                                                "Long acting injectable and depot antipsychotics",
                                                "Prochlorperazine"))) %>%
  filter(variable == "Any antipsychotic") %>%
  ggplot(aes(x = date, y = rate, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, scales = "free", ncol = 2) +
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

ggsave(filename = here::here("released_outputs/EMIS/figures/rate_antipsychotics_all.png"),
       plot_rates_all_EMIS,
       units = "cm", width = 35, height = 40)

### Combined
plot_rates_all <- data_prevalence %>%
  select(-alive_2weeks_post_antipsychotic, -antipsychotic_without_midazolam) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "First generation antipsychotics (excluding long acting depots)",
                                                "Second generation antipsychotics, excluding long acting depots",
                                                "Long acting injectable and depot antipsychotics",
                                                "Prochlorperazine"))) %>%
  filter(variable == "Any antipsychotic") %>%
  ggplot(aes(x = date, y = rate, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, scales = "free", ncol = 2) +
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

ggsave(filename = here::here("released_outputs/Combined/figures/rate_antipsychotics_all.png"),
       plot_rates_all,
       units = "cm", width = 35, height = 40)


## Total number of patients with newly issued antipsychotics, by group
lapply(groups, data = data_incidence_TPP, type = "total new", folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_incidence_EMIS, type = "total new", folder = "EMIS",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_incidence, type = "total new", folder = "Combined",
       FUN = plot_antipsychotics_by_group)

## Rate of patients newly issued antipsychotics, by group
lapply(groups, data = data_incidence_TPP, type = "rate new", Y = 1000, folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_incidence_EMIS, type = "rate new", Y = 1000, folder = "EMIS",
       FUN = plot_antipsychotics_by_group)
lapply(groups[1:6], data = data_incidence, type = "rate new", Y = 1000, folder = "Combined",
       FUN = plot_antipsychotics_by_group)

lapply(groups, data_TPP = data_incidence_TPP, data_EMIS = data_incidence_EMIS, type = "rate new", Y = 1000, 
       folder = "Combined", FUN = plot_antipsychotic_combined)


## All-in-one new rate plot
### TPP
plot_rates_all_TPP <- data_incidence_TPP %>%
  select(-alive_2weeks_post_antipsychotic, -antipsychotic_without_midazolam) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  filter(! is.na(value)) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "First generation antipsychotics (excluding long acting depots)",
                                                "Second generation antipsychotics, excluding long acting depots",
                                                "Long acting injectable and depot antipsychotics",
                                                "Prochlorperazine"))) %>%
  filter(variable == "Any antipsychotic") %>%
  ggplot(aes(x = date, y = rate, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, scales = "free", ncol = 2) +
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
  #geom_text(aes(x = as.Date("2020-03-26"), label="Lockdown one", y=5), colour="grey", angle=90, text=element_text(size=5))
  

ggsave(filename = here::here("released_outputs/TPP/figures/new_rate_antipsychotics_all.png"),
       plot_rates_all_TPP,
       units = "cm", width = 35, height = 40)

### EMIS
plot_rates_all_EMIS <- data_incidence_EMIS %>%
  select(-alive_2weeks_post_antipsychotic, -antipsychotic_without_midazolam) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  filter(! is.na(value)) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "First generation antipsychotics (excluding long acting depots)",
                                                "Second generation antipsychotics, excluding long acting depots",
                                                "Long acting injectable and depot antipsychotics",
                                                "Prochlorperazine"))) %>%
  filter(variable == "Any antipsychotic") %>%
  ggplot(aes(x = date, y = rate, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, scales = "free", ncol = 2) +
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
#geom_text(aes(x = as.Date("2020-03-26"), label="Lockdown one", y=5), colour="grey", angle=90, text=element_text(size=5))


ggsave(filename = here::here("released_outputs/EMIS/figures/new_rate_antipsychotics_all.png"),
       plot_rates_all_EMIS,
       units = "cm", width = 35, height = 40)

### Combined
plot_rates_all <- data_incidence %>%
  select(-alive_2weeks_post_antipsychotic, -antipsychotic_without_midazolam) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  filter(! is.na(value)) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "First generation antipsychotics (excluding long acting depots)",
                                                "Second generation antipsychotics, excluding long acting depots",
                                                "Long acting injectable and depot antipsychotics",
                                                "Prochlorperazine"))) %>%
  filter(variable == "Any antipsychotic") %>%
  ggplot(aes(x = date, y = rate, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = group), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, scales = "free", ncol = 2) +
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
#geom_text(aes(x = as.Date("2020-03-26"), label="Lockdown one", y=5), colour="grey", angle=90, text=element_text(size=5))


ggsave(filename = here::here("released_outputs/Combined/figures/new_rate_antipsychotics_all.png"),
       plot_rates_all,
       units = "cm", width = 35, height = 40)


# Sensitivity analysis ----

## TPP
plot_SA_TPP <- data_incidence_TPP %>%
  select(date, group, antipsychotic_any, alive_2weeks_post_antipsychotic, antipsychotic_without_midazolam, population) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  filter(! is.na(value)) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "Alive two weeks post any antipsychotic prescription",
                                                "Not co-prescribed midazolam with an antipsychotic"))) %>%
  filter(group %in% c("Care Home", "Dementia")) %>%
  ggplot(aes(x = date, y = rate, colour = variable)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = variable), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, ncol = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol=3)) +
  scale_colour_discrete(name = "") +
  ylab("Rate of patients newly issued antipsychotics, per 1000 registered patients") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
  geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)

ggsave(filename = here::here("released_outputs/TPP/figures/SA_new_rate_antipsychotics.png"),
       plot_SA_TPP,
       units = "cm", width = 35, height = 20)

## EMIS
plot_SA_EMIS <- data_incidence_EMIS %>%
  select(date, group, antipsychotic_any, alive_2weeks_post_antipsychotic, antipsychotic_without_midazolam, population) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  filter(! is.na(value)) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "Alive two weeks post any antipsychotic prescription",
                                                "Not co-prescribed midazolam with an antipsychotic"))) %>%
  filter(group %in% c("Care Home", "Dementia")) %>%
  ggplot(aes(x = date, y = rate, colour = variable)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = variable), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, ncol = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol=3)) +
  scale_colour_discrete(name = "") +
  ylab("Rate of patients newly issued antipsychotics, per 1000 registered patients") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
  geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)
#geom_text(aes(x = as.Date("2020-03-26"), label="Lockdown one", y=5), colour="grey", angle=90, text=element_text(size=5))


ggsave(filename = here::here("released_outputs/EMIS/figures/SA_new_rate_antipsychotics.png"),
       plot_SA_EMIS,
       units = "cm", width = 35, height = 20)

## Combined
plot_SA_Combined <- data_incidence %>%
  select(date, group, antipsychotic_any, alive_2weeks_post_antipsychotic, antipsychotic_without_midazolam, population) %>%
  melt(id.vars = c("date", "population", "group")) %>%
  filter(! is.na(value)) %>%
  mutate(est = mapply(function(x,y) glm(y ~ 1 + offset(log(x)), family = "poisson")$coefficients, 
                      .[,2], .[,5]),
         se = mapply(function(x,y) coef(summary(glm(y ~ 1 + offset(log(x)), family = "poisson")))[, "Std. Error"], 
                     .[,2], .[,5]),
         rate = exp(est)*1000,
         lci = exp(est - se)*1000,
         uci = exp(est + se)*1000) %>%
  mutate(variable = factor(variable, labels = c("Any antipsychotic", 
                                                "Alive two weeks post any antipsychotic prescription",
                                                "Not co-prescribed midazolam with an antipsychotic"))) %>%
  filter(group %in% c("Care Home", "Dementia")) %>%
  ggplot(aes(x = date, y = rate, colour = variable)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = variable), alpha=0.2, colour = "transparent", show.legend = F) +
  facet_wrap(~group, ncol = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol=3)) +
  scale_colour_discrete(name = "") +
  ylab("Rate of patients newly issued antipsychotics, per 1000 registered patients") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_vline(xintercept = as.Date("2020-03-16"), colour="grey", type = 2) +
  geom_vline(xintercept = as.Date("2020-12-02"), colour="grey", type = 2)

ggsave(filename = here::here("released_outputs/Combined/figures/SA_new_rate_antipsychotics.png"),
       plot_SA_Combined,
       units = "cm", width = 35, height = 20)

######################################

# This script:
# - imports data
# - combines TPP and EMIS tables and saves as new table
# - plots : 1. total number and rate of patients issued antipsychotics, by clinical and dempogrpahic groups
#           2. total number and rate of new patients issued antipsychotics, by clinical groups

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
sfsaf
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

###EMIS
data_prevalence_EMIS <- read_csv(file = here::here("released_outputs", "EMIS", "data", "data_prevalence_redacted.csv"))[,-1]
data_incidence_EMIS <- read_csv(file = here::here("released_outputs", "EMIS", "data", "data_incident_redacted.csv"))[,-1]

table1_EMIS <- read_csv(file = here::here("released_outputs", "EMIS",  "tables", "table1.csv"))
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

table1_EMIS <- table1_EMIS %>%
  select(group = variable, variable = label, total = stat_0, nonantipsychotic = stat_1,	antipsychotic =stat_2) %>%
  separate(total, c("total","perc"), sep = "([(])") %>%
  separate(nonantipsychotic, c("nonantipsychotic","perc2"), sep = "([(])") %>%
  separate(antipsychotic, c("antipsychotic","perc3"), sep = "([(])") %>%
  mutate(total = as.numeric(gsub(",", "", total)),
         nonantipsychotic = as.numeric(gsub(",", "", nonantipsychotic)),
         antipsychotic = as.numeric(gsub(",", "", antipsychotic))) %>%
  filter(!(is.na(total))) %>%
  select(-perc, -perc2, -perc3) %>%
  mutate(total = ifelse(total < 8, NA, total),
         nonantipsychotic = ifelse(nonantipsychotic < 8 | is.na(total), NA, nonantipsychotic),
         antipsychotic = ifelse(antipsychotic < 8 | is.na(nonantipsychotic) | is.na(total), NA, antipsychotic)) %>%
  mutate(total = plyr::round_any(total, 5),
         nonantipsychotic = plyr::round_any(nonantipsychotic, 5),
         antipsychotic = plyr::round_any(antipsychotic, 5))

## Combine data
data_prevalence <- rbind(data_prevalence_TPP, data_prevalence_EMIS) %>%
  group_by(date, group) %>%
  summarise(antipsychotic_any = sum(antipsychotic_any, na.rm = T),
            antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
            antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
            antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
            prochlorperazine = sum(prochlorperazine, na.rm = T),
            population = sum(population, na.rm = T))

data_incidence <- rbind(data_incidence_TPP, data_incidence_EMIS) %>%
  group_by(date, group) %>%
  summarise(antipsychotic_any = sum(antipsychotic_any, na.rm = T),
            antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
            antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
            antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
            prochlorperazine = sum(prochlorperazine, na.rm = T),
            population = sum(population, na.rm = T))


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

lapply(groups[-3], data = data_incidence_EMIS, type = "total new", folder = "EMIS",
       FUN = plot_antipsychotics_by_group)

total_antipsychotics <- data_incidence_EMIS %>%
  filter(group == "Care Home") %>%
  select(-antipsychotic_any, -group, -population) %>%
  melt(id.vars = c("date")) %>%
  filter(variable != "antipsychotics_injectable_and_depot") %>%
  mutate(variable = factor(variable, labels = c("First generation antipsychotics (excluding long acting depots)",
                                                "Second generation antipsychotics, excluding long acting depots",
                                                "Prochlorperazine"))) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Total number of patients newly prescribed antipsychotics, per month") +
  xlab("") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(filename = here::here(paste("released_outputs/", "EMIS", "/figures/new_total_antipsychotics_group_", "Care Home", ".png", sep = "")),
       total_antipsychotics,
       units = "cm", width = 20, height = 40
)

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



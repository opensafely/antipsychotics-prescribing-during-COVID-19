######################################

# This script:
# - imports data
# - plots : 1. total number of patients issued antipsychotics, by clinical and dempogrpahic groups
#           2. total new number of patients issued antipsychotics, by clinical groups

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
library('patchwork')

## Custom functions
source(here("analysis", "lib", "custom_functions.R"))

## Create output directories
dir.create(here::here("released_outputs", "TPP", "figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("released_outputs", "EMIS", "figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here::here("released_outputs", "Combined", "figures"), showWarnings = FALSE, recursive=TRUE)

## Import processed data
data_prevalence_TPP <- read_csv(file = here::here("released_outputs", "TPP", "data", "data_prevalence_redacted.csv"))[,-1]
data_incidence_TPP <- read_csv(file = here::here("released_outputs", "TPP", "data", "data_incident_redacted.csv"))[,-1]
colnames(data_incidence_TPP) <- colnames(data_prevalence_TPP)

data_prevalence_EMIS <- read_csv(file = here::here("released_outputs", "EMIS", "data", "data_prevalence_redacted.csv"))[,-1]
data_incidence_EMIS <- read_csv(file = here::here("released_outputs", "EMIS", "data", "data_incident_redacted.csv"))[,-1]

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
  summarise(antipsychotic_any_incident = sum(antipsychotic_any_incident, na.rm = T),
            antipsychotics_first_gen_incident = sum(antipsychotics_first_gen_incident, na.rm = T),
            antipsychotics_second_gen_incident = sum(antipsychotics_second_gen_incident, na.rm = T),
            antipsychotics_injectable_and_depot_incident = sum(antipsychotics_injectable_and_depot_incident, na.rm = T),
            prochlorperazine_incident = sum(prochlorperazine_incident, na.rm = T),
            population = sum(population, na.rm = T))


# Figures ----
groups <- c("All", "dementia", "care_home", "learning_disability", "autism", "serious_mental_illness")

## Total number of patients issued antipsychotics, by group
lapply(groups, data = data_prevalence_TPP, type = "total", folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence_EMIS, type = "total", folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence, type = "total", folder = "Combined",
       FUN = plot_antipsychotics_by_group)

## Rate of patients issued antipsychotics, by group
lapply(groups, data = data_prevalence_TPP, type = "rate", Y = 1000, folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence_EMIS, type = "rate", Y = 1000, folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence, type = "rate", Y = 1000, folder = "Combined",
       FUN = plot_antipsychotics_by_group)

## All-in-one rate plot
plot_rates_all <- data_prevalence_TPP %>%
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
  facet_wrap(~group, scales = "free") +
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
       plot_rates_all,
       units = "cm", width = 40, height = 20)


## Total number of patients with newly issued antipsychotics, by group
lapply(groups, data = data_incidence_TPP, type = "total new", folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_incidence_EMIS, type = "total new", folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_incidence_Combined, type = "total new", folder = "Combined",
       FUN = plot_antipsychotics_by_group)

## Rate of patients newly issued antipsychotics, by group
lapply(groups, data = data_prevalence_TPP, type = "rate new", Y = 1000, folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence_EMIS, type = "rate new", Y = 1000, folder = "TPP",
       FUN = plot_antipsychotics_by_group)
lapply(groups, data = data_prevalence, type = "rate new", Y = 1000, folder = "Combined",
       FUN = plot_antipsychotics_by_group)

## All-in-one new rate plot
plot_rates_all <- data_incidence_TPP %>%
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
  facet_wrap(~group, scales = "free") +
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
       plot_rates_all,
       units = "cm", width = 40, height = 20)





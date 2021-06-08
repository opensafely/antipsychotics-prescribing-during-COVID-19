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

## Create output directory
dir.create(here::here("output", "figures"), showWarnings = FALSE, recursive=TRUE)

## Import processed data
data_processed <- readRDS(here::here("output", "data", "data_processed_all.rds"))
data_processed_ld <- readRDS(here::here("output", "data", "data_processed_learning_disability.rds"))
data_processed_autism <- readRDS(here::here("output", "data", "data_processed_autism.rds"))
data_processed_serious_mental_illness <- readRDS(here::here("output", "data", "data_processed_serious_mental_illness.rds"))
data_processed_care_home <- readRDS(here::here("output", "data", "data_processed_care_home.rds"))
data_processed_dementia <- readRDS(here::here("output", "data", "data_processed_dementia.rds"))

# Custom functions ----
source(here("analysis", "custom_functions.R"))


# Figures ----

## Total number of antipsychotics issued

### First generation antipsychotics, excluding long acting depots
antipsychotics_first_gen <-  ggplot(data_processed, aes(x = date, y = antipsychotics_first_gen, colour = group)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Total number of first generation antipsychotics (excluding long acting depots) issued per month") +
  xlab("date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(
  here::here("output", "figures", "plot_total_antipsychotics_first_gen.svg"),
  antipsychotics_first_gen,
  units = "cm", width = 40, height = 20
)

### Second generation antipsychotics, excluding long acting depots
antipsychotics_second_gen <-  ggplot(data_processed, aes(x = date, y = antipsychotics_second_gen, colour = group)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Total number of second generation antipsychotics (excluding long acting depots) issued per month") +
  xlab("date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(
  here::here("output", "figures", "plot_total_antipsychotics_second_gen.svg"),
  antipsychotics_second_gen,
  units = "cm", width = 40, height = 20
)

### Long acting injectable and depot antipsychotics
antipsychotics_injectable_and_depot <-  ggplot(data_processed, aes(x = date, y = antipsychotics_injectable_and_depot, colour = group)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Total number of injectable and depot antipsychotics issued per month") +
  xlab("date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(
  here::here("output", "figures", "plot_total_antipsychotics_injectable_and_depot.svg"),
  antipsychotics_injectable_and_depot,
  units = "cm", width = 40, height = 20
)

### Prochlorperazine
prochlorperazine <-  ggplot(data_processed, aes(x = date, y = prochlorperazine, colour = group)) +
  geom_line() +
  facet_wrap(~group, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Absolute number of prochlorperazine antipsychotics issued per month") +
  xlab("date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(
  here::here("output", "figures", "plot_total_prochlorperazine.svg"),
  prochlorperazine,
  units = "cm", width = 40, height = 20
)

## Learning disability inequalities

### Sex
data_ld_by_sex <- data_processed_learning_disability %>%
  select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine,
         sex) %>%
  group_by(date, sex) %>%
  summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
            antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
            antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
            prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
  melt(id.vars = c("date", "sex")) %>%
  ggplot(aes(x = date, y = value, colour = sex)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  ylab("Absolute number of antipsychotics issued to those with learning disabilities per month, by sex") +
  xlab("date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(
  here::here("output", "figures", "plot_ld_sex.svg"),
  data_ld_by_sex,
  units = "cm", width = 40, height = 20
)

### IMD
data_ld_by_imd <- data_processed_learning_disability %>%
  select(date, antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, prochlorperazine,
         imd) %>%
  filter(imd != 0) %>%
  group_by(date, imd) %>%
  summarise(antipsychotics_first_gen = sum(antipsychotics_first_gen, na.rm = T),
            antipsychotics_second_gen = sum(antipsychotics_second_gen, na.rm = T),
            antipsychotics_injectable_and_depot = sum(antipsychotics_injectable_and_depot, na.rm = T),
            prochlorperazine = sum(prochlorperazine, na.rm = T)) %>%
  melt(id.vars = c("date", "imd")) %>%
  ggplot(aes(x = date, y = value, colour = imd)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  ylab("Absolute number of antipsychotics issued to those with learning disabilities per month, by imd") +
  xlab("date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(
  here::here("output", "figures", "plot_ld_imd.svg"),
  data_ld_by_imd,
  units = "cm", width = 40, height = 20
)


  


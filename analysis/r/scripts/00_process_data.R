######################################

# This script:
# - imports data extracted by the cohort extractor
# - combines ethnicity columns
# - calculates absolute number of antipsychotics issued each group
# - standardises some variables (eg convert to factor) and derives some new ones
# - saves processed dataset(s)

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('arrow')
library('here')

## Custom functions

### Factorise
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

## Read in data and format
my_read_csv <- function(x) {
  
  ## Read in data (don't rely on defaults)
  data_extract <- read_csv(
    here::here("output", "data", x),
    col_types = cols_only(
      
      # Identifier
      patient_id = col_integer(),
      
      # Outcome(s)
      antipsychotics_first_gen = col_logical(),
      antipsychotics_second_gen = col_logical(),
      antipsychotics_injectable_and_depot = col_logical(),
      Prochlorperazine = col_logical(),
      
      # Groups
      learning_disability = col_logical(),
      autism = col_logical(),
      serious_mental_illness = col_logical(),
      care_home = col_logical(),
      dementia = col_logical(),

      # Demographic
      age = col_integer(),
      sex = col_character(),
      ethnicity = col_character(),
      ethnicity_other = col_date(format="%Y-%m-%d"),
      ethnicity_not_given = col_date(format="%Y-%m-%d"),
      ethnicity_not_stated = col_date(format="%Y-%m-%d"),
      ethnicity_no_record = col_date(format="%Y-%m-%d"),
      
      # Geographical
      practice = col_integer(),
      imd = col_character(),
      region = col_character(),
      stp = col_character()
      
    ), na = character()) 
  
  # Add date column and parse NAs
  data_extract <- data_extract %>%
    mutate(date = substr(x, 7, 16)) %>%
    mutate(across(
      .cols = where(is.character),
      .fns = ~na_if(.x, "")
    )) %>%
    mutate(across(
      .cols = c(where(is.numeric), -ends_with("_id")), #convert numeric+integer but not id variables
      .fns = ~na_if(.x, 0)
    )) %>%
    arrange(patient_id)
  
  ## Format columns (i.e, set factor levels)
  data_extract %>%
    mutate(

      # Age band
      ageband = cut(
        age,
        breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
        labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
        right = FALSE
      ),
      
      # Sex
      sex = fct_case_when(
        sex == "F" ~ "Female",
        sex == "M" ~ "Male",
        #sex == "I" ~ "Inter-sex",
        #sex == "U" ~ "Unknown",
        TRUE ~ NA_character_
      ),
      
      # Ethnicity
      ethnicity =  ifelse(is.na(ethnicity) & !is.na(ethnicity_other), 17, 
                          ifelse(is.na(ethnicity) & !is.na(ethnicity_not_given), 18,
                                 ifelse(is.na(ethnicity) & !is.na(ethnicity_not_stated), 19,
                                        ifelse(is.na(ethnicity) & !is.na(ethnicity_no_record), 20,
                                               ethnicity)))),
      
      ethnicity = ifelse(is.na(ethnicity), 20, ethnicity),
      
      ethnicity = fct_case_when(
        ethnicity == "1" ~ "White - British",
        ethnicity == "2" ~ "White - Irish",
        ethnicity == "3" ~ "White - Any other White background",
        ethnicity == "4" ~ "Mixed - White and Black Caribbean",
        ethnicity == "5" ~ "Mixed - White and Black African",
        ethnicity == "6" ~ "Mixed - White and Asian",
        ethnicity == "7" ~ "Mixed - Any other mixed background",
        ethnicity == "8" ~ "Asian or Asian British - Indian",
        ethnicity == "9" ~ "Asian or Asian British - Pakistani",
        ethnicity == "10" ~ "Asian or Asian British - Bangladeshi",
        ethnicity == "11" ~ "Asian or Asian British - Any other Asian background",
        ethnicity == "12" ~ "Black or Black British - Caribbean",
        ethnicity == "13" ~ "Black or Black British - African",
        ethnicity == "14" ~ "Black or Black British - Any other Black background",
        ethnicity == "15" ~ "Other ethnic groups - Chinese",
        ethnicity == "16" ~ "Other ethnic groups - Any other ethnic group",
        ethnicity == "17" ~ "Patients with any other ethnicity code",
        ethnicity == "18" ~ "Ethnicity not given - patient refused",
        ethnicity == "19" ~ "Ethnicity not stated",
        ethnicity == "20" ~ "Ethnicity not recorded",
        #TRUE ~ "Unknown",
        TRUE ~ NA_character_
      ),
      
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
      
      # Region
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
        TRUE ~ NA_character_
      ),

      ## STP
      stp = as.factor(stp)

    ) %>%
    droplevels() %>%
    mutate(
      across(
        where(is.logical),
        ~.x*1L
      )
    ) %>%
    filter(!is.na(practice)) %>%
    select(date, practice, 
           antipsychotics_first_gen, antipsychotics_second_gen, antipsychotics_injectable_and_depot, Prochlorperazine,
           autism, care_home, dementia, learning_disability, serious_mental_illness,
           age, ethnicity, imd, region, sex, stp)
}

## Read feather data
my_read_feather <- function(x) {
  
  ## Read in data
  data_extract <- arrow::read_feather(
    here::here("output", "data", x)) %>%
    mutate(date = as.Date(substr(x, 10, 19), format = "%Y-%m-%d"))
}

## Output processed data to rds
dir.create(here::here("output", "data", "Processed"), showWarnings = FALSE, recursive=TRUE)


# Process data ----

## Read in, format and combine data
filenames <- list.files(path = here::here("output", "data"))

#tbl <- lapply(filenames, my_read_csv) %>% bind_rows()
#tbl <- lapply(filenames, my_read_feather) %>% bind_rows()


# Save dataset as .rds files ----
#write_rds(tbl, here::here("output", "data", "data_processed.rds"), compress="gz")


# Quick summaries ----
print(filenames)
data_extract <- arrow::read_feather(here::here("output", "data", "input_ld_2019-01-01.feather"))

print(names(data_extract))
# dim(data_extract)
# summary(data_extract)
write_rds(data_extract, here::here("output", "data", "data_processed.rds"), compress="gz")

# print(names(tbl))
# print(summary(tbl))
# print(table(tbl$antipsychotics_first_gen))
# print(table(tbl$antipsychotics_first_gen_event_code))
# print(table(tbl$antipsychotics_second_gen_event_code))
# print(table(tbl$learning_disability))
# 
#tbl2 <- read_csv(here::here("output", "data", "measure_ld_antipsychotics_first_gen.csv"))
#head(tbl2)
#table(tbl2$antipsychotics_first_gen_event_code)
#sum(tbl2$antipsychotics_first_gen, na.rm = T)

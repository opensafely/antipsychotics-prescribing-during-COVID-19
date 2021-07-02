######################################

# This script extracts all data relating to the study population variables so that inclusion/exclusion
# numbers can be calculated in respect to the study population

######################################

# IMPORT STATEMENTS ----

## Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
)

## Import codelists
from codelists import *
  
  
  # DEFINE STUDY POPULATION ----

## Define study time variables
from datetime import datetime
end_date = datetime.today().strftime('%Y-%m-%d')

## Define study population and variables
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "1970-01-01", "latest": end_date},
    "rate": "uniform",
    "incidence": 0.2,
  },
  
  # Set index date
  index_date = "2019-01-01",
  
  # Define the study population
  population = patients.all(),
  
  # Inclusion/exclusion variables
  
  ## Alive
  has_died = patients.died_from_any_cause(
    on_or_before = "index_date",
    returning = "binary_flag",
  ),
  
  ## Registered
  registered = patients.satisfying(
    "registered_at_start",
    registered_at_start = patients.registered_as_of("index_date"),
  ),
  
  ## Age
  age = patients.age_as_of(
    "index_date",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.001
    },
  ),
  
  ## At least one year of follow-up
  has_follow_up_previous_year = patients.registered_with_one_practice_between(
    start_date = "index_date - 1 year",
    end_date = "index_date",
    return_expectations = {"incidence": 0.95},
  ),
  
  ## Sex
  sex = patients.sex(
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
    }
  ),
  
  
  # Outcome
  
  ## Antipsychotic
  antipsychotic = patients.with_these_clinical_events(
    combine_codelists(
      antipsychotics_first_gen_codes,
      antipsychotics_second_gen_codes,
      antipsychotics_injectable_and_depot_codes,
      prochlorperazine_codes
    ),
    returning = "binary_flag",
    on_or_before = "index_date",
    find_first_match_in_period = True,
    return_expectations = {"rate": "exponential_increase"},
  ),
  
  
  # Groups
  
  ## Learning disabilities
  learning_disability = patients.with_these_clinical_events(
    learning_disability_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  ## Autism
  autism = patients.with_these_clinical_events(
    autism_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  ## Serious Mental Illness
  serious_mental_illness = patients.with_these_clinical_events(
    serious_mental_illness_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  ## Care home
  care_home = patients.with_these_clinical_events(
    carehome_primis_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  ## Dementia
  dementia = patients.with_these_clinical_events(
    dementia_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
)



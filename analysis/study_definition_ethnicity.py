######################################

# Rather than extarct ethnicity records for every month, as per input data, (as it takes a long time), this script extracts
# a single ethnicity records for each patient which will be joined to input data late (using `join_ethnicity` action. 

######################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist,
  codelist_from_csv,
  Measure,
)

## Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *


# --- DEFINE STUDY POPULATION ---

## Define study time variables
from datetime import date

start_date = "2019-01-01"
end_date = "2021-04-01"

## Define study population and variables
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "1900-01-01", "latest": "today"},
    "rate": "uniform",
  },
  
  # Set index date to end date
  index_date = end_date,
  
  # Define the study population
  population = patients.all(),
  
  # Define ethnicity variables
  
  ## Ethnicity
  eth = patients.with_these_clinical_events(
    ethnicity_codes,
    returning = "category",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    return_expectations = {
      "category": {"ratios": {
        "1": 0.25, 
        "2": 0.05, 
        "3": 0.05, 
        "4": 0.05, 
        "5": 0.05,
        "6": 0.05, 
        "7": 0.05, 
        "8": 0.05, 
        "9": 0.05, 
        "10": 0.05, 
        "11": 0.05,
        "12": 0.05, 
        "13": 0.05, 
        "14": 0.05, 
        "15": 0.05, 
        "16": 0.05}},
      "incidence": 0.75,},
  ),
  
  ethnicity_6 = patients.with_these_clinical_events(
    ethnicity_6_codes,
    returning = "category",
    find_last_match_in_period = True,
    include_date_of_match = False,
    return_expectations = {
      "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
      "incidence": 0.75,
    },
  ),
  
  ## Any other ethnicity code
  ethnicity_other = patients.with_these_clinical_events(
    ethnicity_other_codes,
    returning = "binary_flag",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    return_expectations = {"incidence": 0.1}
  ),
  
  ## Ethnicity not given - patient refused
  ethnicity_not_given = patients.with_these_clinical_events(
    ethnicity_not_given_codes,
    returning = "binary_flag",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    return_expectations = {"incidence": 0.1}
  ),
  
  ## Ethnicity not stated
  ethnicity_not_stated = patients.with_these_clinical_events(
    ethnicity_not_stated_codes,
    returning = "binary_flag",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    return_expectations = {"incidence": 0.1}
  ),
  
  ## Ethnicity no record
  ethnicity_no_record = patients.with_these_clinical_events(
    ethnicity_no_record_codes,
    returning = "binary_flag",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    return_expectations = {"incidence": 0.1}
  ), 
  
  ## Ethnicity from SUS
    ethnicity_sus = patients.with_ethnicity_from_sus(
        returning = "group_6",  
        use_most_frequent_code = True,
        return_expectations = {
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.4,
            },
    ),
  
)

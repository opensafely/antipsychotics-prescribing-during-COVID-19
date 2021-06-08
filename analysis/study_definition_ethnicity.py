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
end_date = date.today().isoformat()

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
  population = patients.registered_with_one_practice_between(
    "index_date", "index_date"
  ),
  
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
  
  
  # # Combine into single ethnicity variable
  # ethnicity = patients.categorised_as(
  #   {"0": "DEFAULT",
  #     "1": "eth='1'", 
  #     "2": "eth='2'", 
  #     "3": "eth='3'", 
  #     "4": "eth='4'", 
  #     "5": "eth='5'", 
  #     "6": "eth='6'", 
  #     "7": "eth='7'", 
  #     "8": "eth='8'", 
  #     "9": "eth='9'", 
  #     "10": "eth='10'", 
  #     "11": "eth='11'", 
  #     "12": "eth='12'", 
  #     "13": "eth='13'", 
  #     "14": "eth='14'", 
  #     "15": "eth='15'", 
  #     "16": "eth='16'", 
  #     "17": "NOT eth AND ethnicity_other", 
  #     "18": "NOT eth AND ethnicity_not_given",  
  #     "19": "NOT eth AND ethnicity_not_stated",  
  #     "20": "NOT eth AND ethnicity_no_record",  
  #   }, 
  #   return_expectations = {
  #     "category": {"ratios": {
  #       "0": 0.01,
  #       "1": 0.24,
  #       "2": 0.05,
  #       "3": 0.05,
  #       "4": 0.05,
  #       "5": 0.05,
  #       "6": 0.05,
  #       "7": 0.05,
  #       "8": 0.05,
  #       "9": 0.05,
  #       "10": 0.05,
  #       "11": 0.05,
  #       "12": 0.05,
  #       "13": 0.05,
  #       "14": 0.05,
  #       "15": 0.05,
  #       "16": 0.01,
  #       "17": 0.01,
  #       "18": 0.01,
  #       "19": 0.01,
  #       "20": 0.01,}},
  #     "incidence": 0.4,
  #   },
  # ),
  
)

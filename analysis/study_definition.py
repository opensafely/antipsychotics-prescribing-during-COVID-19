######################################

# This script provides the formal specification of the study data that will be extracted from 
# the OpenSAFELY database.

######################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  Measure
)

## Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *
  
  
# --- DEFINE STUDY POPULATION ---
  
## Define study time variables
from datetime import datetime

start_date = "2019-01-01"
end_date = datetime.today().strftime('%Y-%m-%d')

## Define study population and variables
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": start_date, "latest": end_date},
    "rate": "uniform",
    "incidence": 0.1,
  },
  
  # Set index date to start date
  index_date = start_date,
  
  # Define the study population
  population = patients.satisfying(
    """
        NOT has_died
        AND
        registered
        AND
        age
        AND
        has_follow_up_previous_year
        AND
        (sex = "M" OR sex = "F")
        """,
    
    has_died = patients.died_from_any_cause(
      on_or_before = "index_date",
      returning = "binary_flag",
    ),
    
    registered = patients.satisfying(
      "registered_at_start",
      registered_at_start = patients.registered_as_of("index_date"),
    ),
    
    has_follow_up_previous_year = patients.registered_with_one_practice_between(
      start_date = "index_date - 1 year",
      end_date = "index_date",
      return_expectations = {"incidence": 0.95},
    ),
    
  ),
  
  
  ## Medication DM&D
  
  ## First generation antipsychotics, excluding long acting depots
  antipsychotics_first_gen = patients.with_these_medications(
    antipsychotics_first_gen_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  antipsychotics_first_gen_event_code = patients.with_these_clinical_events(
    codelist = antipsychotics_first_gen_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "code",
    return_expectations = {"category": {
      "ratios": {str(36152011000001103): 0.8, str(321393004): 0.2}}, }
  ),
  
  ## Second generation antipsychotics excluding long acting injections
  antipsychotics_second_gen = patients.with_these_medications(
    antipsychotics_second_gen_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  antipsychotics_second_gen_event_code = patients.with_these_medications(
    antipsychotics_second_gen_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "code",
    return_expectations = {"category": {
      "ratios": {str(321589009): 0.8, str(321590000): 0.2}}, }
  ),
  
  ## Long acting injectable and depot antipsychotics
  antipsychotics_injectable_and_depot = patients.with_these_medications(
    antipsychotics_injectable_and_depot_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  antipsychotics_injectable_and_depot_event_code = patients.with_these_medications(
    antipsychotics_injectable_and_depot_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "code",
    return_expectations = {"category": {
      "ratios": {str(4559111000001109): 0.8, str(4177011000001109): 0.2}}, }
  ),
  
  ## Prochlorperazine
  prochlorperazine = patients.with_these_medications(
    prochlorperazine_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  prochlorperazine_event_code = patients.with_these_medications(
    prochlorperazine_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "code",
    return_expectations = {"category": {
      "ratios": {str(322149009): 0.8, str(322144004): 0.2}}, }
  ),
  
  
  ## Groups
  
  ### Learning disabilities
  learning_disability = patients.with_these_clinical_events(
    learning_disability_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  ### Autism
  autism = patients.with_these_clinical_events(
    autism_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  ### Serious Mental Illness
  serious_mental_illness = patients.with_these_clinical_events(
    serious_mental_illness_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  ### Care home
  care_home = patients.with_these_clinical_events(
    carehome_primis_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  ### Dementia
  dementia = patients.with_these_clinical_events(
    dementia_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  
  ## Variables
  
  ### Practice
  practice = patients.registered_practice_as_of(
    "index_date",
    returning="pseudo_id",
    return_expectations = {"int" : {"distribution": "normal", "mean": 25, "stddev": 5}, "incidence" : 0.5}
  ),
  
  ### Sex
  sex = patients.sex(
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
    }
  ),
  
  ### Index of multiple deprivation
  imd = patients.categorised_as(
    {"0": "DEFAULT",
      "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
      "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
      "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
      "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
      "5": """index_of_multiple_deprivation >= 32844*4/5 """,
    },
    index_of_multiple_deprivation = patients.address_as_of(
      "index_date",
      returning = "index_of_multiple_deprivation",
      round_to_nearest = 100,
    ),
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "0": 0.01,
          "1": 0.20,
          "2": 0.20,
          "3": 0.20,
          "4": 0.20,
          "5": 0.19,
        }},
    },
  ),
  
  ### Age
  age = patients.age_as_of(
    "index_date",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.001
    },
  ),
  
  ### Region - NHS England 9 regions
  region = patients.registered_practice_as_of(
    "index_date",
    returning = "nuts1_region_name",
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "North East": 0.1,
          "North West": 0.1,
          "Yorkshire and The Humber": 0.1,
          "East Midlands": 0.1,
          "West Midlands": 0.1,
          "East": 0.1,
          "London": 0.2,
          "South West": 0.1,
          "South East": 0.1,},},
    },
  ),
  
  ### STP (regional grouping of practices)
  stp = patients.registered_practice_as_of("index_date",
                                           returning = "stp_code",
                                           return_expectations = {
                                             "rate": "universal",
                                             "category": {
                                               "ratios": {
                                                 "STP1": 0.1,
                                                 "STP2": 0.1,
                                                 "STP3": 0.1,
                                                 "STP4": 0.1,
                                                 "STP5": 0.1,
                                                 "STP6": 0.1,
                                                 "STP7": 0.1,
                                                 "STP8": 0.1,
                                                 "STP9": 0.1,
                                                 "STP10": 0.1,}},
                                           },
  ),
  
  ### Depression (Codelists TBC)
  
  ### People without a diagnosis of clinical groups above  LD, SMI
  
)


# --- DEFINE MEASURES ---

measures = [
  
  ### First generation antipsychotics, excluding long acting depots
  Measure(
    id = "antipsychotics_first_gen",
    numerator = "antipsychotics_first_gen",
    denominator = "population",
    group_by = ["practice", "antipsychotics_first_gen_event_code"]
  ),
  
  ### Second generation antipsychotics excluding long acting injections
  Measure(
    id = "antipsychotics_second_gen",
    numerator = "antipsychotics_second_gen",
    denominator = "population",
    group_by = ["practice", "antipsychotics_second_gen_event_code"]
  ),
  
  ## Long acting injectable and depot antipsychotics
  Measure(
    id = "antipsychotics_injectable_and_depot",
    numerator = "antipsychotics_injectable_and_depot",
    denominator = "population",
    group_by = ["practice", "antipsychotics_injectable_and_depot_event_code"]
  ),
  
  ## Prochlorperazine
  Measure(
    id = "prochlorperazine",
    numerator = "prochlorperazine",
    denominator = "population",
    group_by = ["practice", "prochlorperazine_event_code"]
  ),
  
]





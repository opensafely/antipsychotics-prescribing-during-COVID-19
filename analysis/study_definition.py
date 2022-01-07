######################################

# This script provides the formal specification of the study data that will be extracted from 
# the OpenSAFELY database.

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
  Measure
)

## Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *
  
  
# DEFINE STUDY POPULATION ----

## Define study time variables
from datetime import datetime

## Define study population and variables
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "2019-01-01", "latest": "2021-04-30"},
    "rate": "uniform",
    "incidence": 0.1,
  },
  
  # Set index date
  index_date = "2019-01-01",
  
  # Define the study population
  population = patients.satisfying(
    """
        NOT has_died
        AND
        registered
        AND 
        (sex = "M" OR sex = "F")
        AND
        (age >=0 AND age < 110)
        """,
    
    has_died = patients.died_from_any_cause(
      on_or_before = "index_date",
      returning = "binary_flag",
    ),
    
    registered = patients.satisfying(
      "registered_at_start",
      registered_at_start = patients.registered_as_of("index_date"),
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
  
  antipsychotics_first_gen_incident = patients.satisfying(
    
    """
    antipsychotics_first_gen_current_date
    AND 
    NOT antipsychotics_first_gen_last_date
    """, 
    
    return_expectations = {
      "incidence": 0.01,
    },
    
    antipsychotics_first_gen_current_date = patients.with_these_medications(
      antipsychotics_first_gen_codes,
      returning = "date",
      find_last_match_in_period = True,
      between = ["index_date", "last_day_of_month(index_date)"],
      date_format = "YYYY-MM-DD",
      return_expectations = {"incidence": 0.1}
    ),
    
    antipsychotics_first_gen_last_date = patients.with_these_medications(
      antipsychotics_first_gen_codes,
      returning = "date",
      find_first_match_in_period = True,
      between = ["antipsychotics_first_gen_current_date - 2 year", "antipsychotics_first_gen_current_date - 1 day"],
      date_format = "YYYY-MM-DD",
      return_expectations = {"incidence": 0.5}
    ),
  ),
  
  ## Second generation antipsychotics excluding long acting injections
  antipsychotics_second_gen = patients.with_these_medications(
    antipsychotics_second_gen_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  antipsychotics_second_gen_incident = patients.satisfying(
    
    """
    antipsychotics_second_gen_current_date
    AND 
    NOT antipsychotics_second_gen_last_date
    """, 
    
    return_expectations = {
      "incidence": 0.01,
    },
    
    antipsychotics_second_gen_current_date = patients.with_these_medications(
      antipsychotics_second_gen_codes,
      returning = "date",
      find_last_match_in_period = True,
      between = ["index_date", "last_day_of_month(index_date)"],
      date_format = "YYYY-MM-DD",
      return_expectations = {"incidence": 0.1}
    ),
    
    antipsychotics_second_gen_last_date = patients.with_these_medications(
      antipsychotics_second_gen_codes,
      returning = "date",
      find_first_match_in_period = True,
      between = ["antipsychotics_second_gen_current_date - 2 years", "antipsychotics_second_gen_current_date - 1 day"],
      date_format = "YYYY-MM-DD",
      return_expectations = {"incidence": 0.5}
    ),
  ),
  
  ## Long acting injectable and depot antipsychotics
  antipsychotics_injectable_and_depot = patients.with_these_medications(
    antipsychotics_injectable_and_depot_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  antipsychotics_injectable_and_depot_incident = patients.satisfying(
    
    """
    antipsychotics_injectable_and_depot_current_date
    AND 
    NOT antipsychotics_injectable_and_depot_last_date
    """, 
    
    return_expectations = {
      "incidence": 0.01,
    },
    
    antipsychotics_injectable_and_depot_current_date = patients.with_these_medications(
      antipsychotics_injectable_and_depot_codes,
      returning = "date",
      find_last_match_in_period = True,
      between = ["index_date", "last_day_of_month(index_date)"],
      date_format = "YYYY-MM-DD",
      return_expectations = {"incidence": 0.1}
    ),
    
    antipsychotics_injectable_and_depot_last_date = patients.with_these_medications(
      antipsychotics_injectable_and_depot_codes,
      returning = "date",
      find_first_match_in_period = True,
      between = ["antipsychotics_injectable_and_depot_current_date - 2 years", "antipsychotics_injectable_and_depot_current_date - 1 day"],
      date_format = "YYYY-MM-DD",
      return_expectations = {"incidence": 0.5}
    ),
  ),
  
  ## Prochlorperazine
  prochlorperazine = patients.with_these_medications(
    prochlorperazine_codes,
    between = ["index_date", "last_day_of_month(index_date)"],
    returning = "binary_flag",
    return_expectations = {"incidence": 0.5}
  ),
  
  prochlorperazine_incident = patients.satisfying(
    
    """
    prochlorperazine_current_date
    AND 
    NOT prochlorperazine_last_date
    """, 
    
    return_expectations = {
      "incidence": 0.01,
    },
    
    prochlorperazine_current_date = patients.with_these_medications(
      prochlorperazine_codes,
      returning = "date",
      find_last_match_in_period = True,
      between = ["index_date", "last_day_of_month(index_date)"],
      date_format = "YYYY-MM-DD",
      return_expectations = {"incidence": 0.1}
    ),
    
    prochlorperazine_last_date = patients.with_these_medications(
      prochlorperazine_codes,
      returning = "date",
      find_first_match_in_period = True,
      between = ["prochlorperazine_current_date - 2 years", "prochlorperazine_current_date - 1 day"],
      date_format = "YYYY-MM-DD",
      return_expectations = {"incidence": 0.5}
    ),
  ),
  
  ### Any antipsychotic
  antipsychotic_any = patients.satisfying(
    
    """
    antipsychotics_first_gen
    OR 
    antipsychotics_second_gen
    OR
    antipsychotics_injectable_and_depot
    OR
    prochlorperazine
    """, 
    
    return_expectations = {
      "incidence": 0.4,
    },
  ),
  
  antipsychotic_any_incident = patients.satisfying(
    
    """
    antipsychotics_first_gen_incident
    OR 
    antipsychotics_second_gen_incident
    OR
    antipsychotics_injectable_and_depot_incident
    OR
    prochlorperazine_incident
    """, 
    
    return_expectations = {
      "incidence": 0.05,
    },
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
  
  ### Region
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
          "South East": 0.1,
          "South West": 0.1,
        },
      },
    },
  ),
  
  ### MSOA
  msoa = patients.registered_practice_as_of(
    "index_date",
    returning = "msoa_code",
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"E02000001": 0.0625, "E02000002": 0.0625, "E02000003": 0.0625, "E02000004": 0.0625,
        "E02000005": 0.0625, "E02000007": 0.0625, "E02000008": 0.0625, "E02000009": 0.0625, 
        "E02000010": 0.0625, "E02000011": 0.0625, "E02000012": 0.0625, "E02000013": 0.0625, 
        "E02000014": 0.0625, "E02000015": 0.0625, "E02000016": 0.0625, "E02000017": 0.0625}},
    },
  ),
  
  
  
  ## Groups
  
  ### Learning disabilities
  learning_disability = patients.with_these_clinical_events(
    learning_disability_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.2}
  ),
  
  ### Autism
  autism = patients.with_these_clinical_events(
    autism_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.3}
  ),
  
  ### Serious Mental Illness
  serious_mental_illness = patients.with_these_clinical_events(
    serious_mental_illness_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.1}
  ),
  
  ### Care home
  care_home = patients.with_these_clinical_events(
    carehome_primis_codes,
    on_or_before = "index_date",
    returning = "binary_flag",
    return_expectations = {"incidence": 0.2}
  ),
  
  ### Dementia
  dementia = patients.satisfying(
    
    """
    dementia_all
    AND
    age > 39
    """, 
    
    return_expectations = {
      "incidence": 0.01,
    },
    
    dementia_all = patients.with_these_clinical_events(
      dementia_codes,
      on_or_before = "index_date",
      returning = "binary_flag",
      return_expectations = {"incidence": 0.05}
    ),
    
  ),
  
  
  ## Sensitivity anlysis variables
  
  ### Antipsychotic date
  antipsychotics_date = patients.with_these_medications(
    codelist = combine_codelists(antipsychotics_first_gen_codes, antipsychotics_second_gen_codes, 
                                 antipsychotics_injectable_and_depot_codes, prochlorperazine_codes),
    returning = "date",
    find_last_match_in_period = True,
    between = ["index_date", "last_day_of_month(index_date)"],
    date_format = "YYYY-MM-DD",
    return_expectations = {"incidence": 0.6}
  ),
  
  ### Death date
  death_date = patients.died_from_any_cause(
    returning = "date_of_death",
    date_format = "YYYY-MM-DD",
    between = ["index_date", "last_day_of_month(index_date)"],
    return_expectations = {"incidence": 0.6}
  ),
  
  ### Midazolam date
  midazolam_date = patients.with_these_medications(
    midazolam_codes,
    returning = "date",
    find_last_match_in_period = True,
    between = ["index_date", "last_day_of_month(index_date)"],
    date_format = "YYYY-MM-DD",
    return_expectations = {"incidence": 0.01}
  ),
  
  
  # ### Flag for individuals who didn't die two weeks after recieving an antipsychotic
  # alive_2weeks_post_antipsychotic = patients.satisfying(
  #   
  #   """
  #   antipsychotics_date
  #   AND
  #   NOT died_2weeks_post_antipsychotic
  #   """, 
  #   
  #   return_expectations = {
  #     "incidence": 0.35,
  #   },
  #   
  #   antipsychotics_date = patients.with_these_medications(
  #     codelist = combine_codelists(antipsychotics_first_gen_codes, antipsychotics_second_gen_codes, 
  #                                  antipsychotics_injectable_and_depot_codes, prochlorperazine_codes),
  #     returning = "date",
  #     find_last_match_in_period = True,
  #     between = ["index_date", "last_day_of_month(index_date)"],
  #     date_format = "YYYY-MM-DD",
  #     return_expectations = {"incidence": 0.1}
  #   ),
  #   
  #   died_2weeks_post_antipsychotic = patients.died_from_any_cause(
  #     between = ["antipsychotics_date", "antipsychotics_date + 14 days"],
  #     returning = "binary_flag",
  #   ),
  # ),
  # 
  # ### Flag for individuals who didn't die two weeks after recieving a new antipsychotic
  # alive_2weeks_post_new_antipsychotic = patients.satisfying(
  #   
  #   """
  #   antipsychotic_any_incident
  #   AND
  #   alive_2weeks_post_antipsychotic
  #   """, 
  #   
  #   return_expectations = {
  #     "incidence": 0.005,
  #   },
  #   
  # ),
  # 
  # ### Flag for individuals who recieved a prescription for midazolam at the same time as their AP
  # midazolam_with_antipsychotic = patients.satisfying(
  #   
  #   """
  #   antipsychotic_date
  #   AND
  #   midazolam_date
  #   """, 
  #   
  #   return_expectations = {
  #     "incidence": 0.05,
  #   },
  #   
  #   antipsychotic_date = patients.with_these_medications(
  #     codelist = combine_codelists(antipsychotics_first_gen_codes, antipsychotics_second_gen_codes, 
  #                                  antipsychotics_injectable_and_depot_codes, prochlorperazine_codes),
  #     returning = "date",
  #     find_last_match_in_period = True,
  #     between = ["index_date", "last_day_of_month(index_date)"],
  #     date_format = "YYYY-MM-DD",
  #     return_expectations = {"incidence": 0.01}
  #   ),
  #   
  #   midazolam_date = patients.with_these_medications(
  #     midazolam_codes,
  #     returning = "date",
  #     find_last_match_in_period = True,
  #     between = ["antipsychotic_date - 1 day", "antipsychotic_date + 1 day"],
  #     date_format = "YYYY-MM-DD",
  #     return_expectations = {"incidence": 0.01}
  #   ),
  # ),
  # 
  # ### Flag for individuals recieved a prescription for midazolam at the same time as a new AP
  # midazolam_with_new_antipsychotic = patients.satisfying(
  #   
  #   """
  #   antipsychotic_any_incident
  #   AND
  #   midazolam
  #   """, 
  #   
  #   return_expectations = {
  #     "incidence": 0.05,
  #   },
  #   
  #   antipsychotic = patients.with_these_medications(
  #     codelist = combine_codelists(antipsychotics_first_gen_codes, antipsychotics_second_gen_codes, 
  #                                  antipsychotics_injectable_and_depot_codes, prochlorperazine_codes),
  #     returning = "date",
  #     find_last_match_in_period = True,
  #     between = ["index_date", "last_day_of_month(index_date)"],
  #     date_format = "YYYY-MM-DD",
  #     return_expectations = {"incidence": 0.01}
  #   ),
  #   
  #   midazolam = patients.with_these_medications(
  #     midazolam_codes,
  #     returning = "date",
  #     find_last_match_in_period = True,
  #     between = ["antipsychotic - 1 day", "antipsychotic + 1 day"],
  #     date_format = "YYYY-MM-DD",
  #     return_expectations = {"incidence": 0.01}
  #   ),
  # ),
  
)



# --- DEFINE MEASURES ---

measures = [
  
  # Monthly rates - whole population
  ## Any antipsychotic
  Measure(
    id = "antipsychotic_all_any",
    numerator = "antipsychotic_any",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  ## First generation
  Measure(
    id = "antipsychotic_all_first_gen",
    numerator = "antipsychotics_first_gen",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  ## Second generation
  Measure(
    id = "antipsychotic_all_second_gen",
    numerator = "antipsychotics_second_gen",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  ## Injectable and depot
  Measure(
    id = "antipsychotic_all_injectable_and_depot",
    numerator = "antipsychotics_injectable_and_depot",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  ## Prochlorperazine
  Measure(
    id = "antipsychotic_all_prochlorperazine",
    numerator = "prochlorperazine",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  
  # Monthly rates - sub populations
  ## Populations
  Measure(
    id = "populations",
    numerator = "population",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## Any antipsychotic
  Measure(
    id = "antipsychotic_groups_any",
    numerator = "antipsychotic_any",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## First generation
  Measure(
    id = "antipsychotic_groups_first_gen",
    numerator = "antipsychotics_first_gen",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## Second generation
  Measure(
    id = "antipsychotic_groups_second_gen",
    numerator = "antipsychotics_second_gen",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## Injectable and depot
  Measure(
    id = "antipsychotic_groups_injectable_and_depot",
    numerator = "antipsychotics_injectable_and_depot",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## Prochlorperazine
  Measure(
    id = "antipsychotic_groups_prochlorperazine",
    numerator = "prochlorperazine",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  
  # Incidence - whole population
  ## Any antipsychotic
  Measure(
    id = "antipsychotic_all_any_incident",
    numerator = "antipsychotic_any_incident",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  ## First generation
  Measure(
    id = "antipsychotic_all_first_gen_incident",
    numerator = "antipsychotics_first_gen_incident",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  ## Second generation
  Measure(
    id = "antipsychotic_all_second_gen_incident",
    numerator = "antipsychotics_second_gen_incident",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  ## Injectable and depot
  Measure(
    id = "antipsychotic_all_injectable_and_depot_incident",
    numerator = "antipsychotics_injectable_and_depot_incident",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  ## Prochlorperazine
  Measure(
    id = "antipsychotic_all_prochlorperazine_incident",
    numerator = "prochlorperazine_incident",
    denominator = "population",
    group_by = ["population"],
    small_number_suppression = True,
  ),
  
  
  # Incidence - sub population
  ## Any antipsychotic
  Measure(
    id = "antipsychotic_groups_any_incident",
    numerator = "antipsychotic_any_incident",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## First generation
  Measure(
    id = "antipsychotic_groups_first_gen_incident",
    numerator = "antipsychotics_first_gen_incident",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## Second generation
  Measure(
    id = "antipsychotic_groups_second_gen_incident",
    numerator = "antipsychotics_second_gen_incident",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## Injectable and depot
  Measure(
    id = "antipsychotic_groups_injectable_and_depot_incident",
    numerator = "antipsychotics_injectable_and_depot_incident",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  ## Prochlorperazine
  Measure(
    id = "antipsychotic_groups_prochlorperazine_incident",
    numerator = "prochlorperazine_incident",
    denominator = "population",
    group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  ),
  
  # 
  # # Sensitivity anlysis
  # 
  # ## Individuals who didn't die two weeks after recieving an antipsychotic
  # ### Whole population
  # Measure(
  #   id = "antipsychotic_all_any_alive_2weeks_post_antipsychotic",
  #   numerator = "alive_2weeks_post_antipsychotic",
  #   denominator = "population",
  #   group_by = ["population"],
  #   small_number_suppression = True,
  # ),
  # 
  # ### Sub populations
  # Measure(
  #   id = "antipsychotic_groups_any_alive_2weeks_post_antipsychotic",
  #   numerator = "alive_2weeks_post_antipsychotic",
  #   denominator = "population",
  #   group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  # ),
  # 
  # ## Individuals who didn't die two weeks after recieving a new antipsychotic
  # ### Whole population
  # Measure(
  #   id = "antipsychotic_all_any_alive_2weeks_post_new_antipsychotic",
  #   numerator = "alive_2weeks_post_new_antipsychotic",
  #   denominator = "population",
  #   group_by = ["population"],
  #   small_number_suppression = True,
  # ),
  # 
  # ### Sub populations
  # Measure(
  #   id = "antipsychotic_groups_any_alive_2weeks_post_new_antipsychotic",
  #   numerator = "alive_2weeks_post_new_antipsychotic",
  #   denominator = "population",
  #   group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  # ),
  # 
  # ## Individuals who recieved a prescription for midazolam at the same time as their AP
  # ### Whole population
  # Measure(
  #   id = "midazolam_with_antipsychotic_all",
  #   numerator = "midazolam_with_antipsychotic",
  #   denominator = "population",
  #   group_by = ["population"],
  #   small_number_suppression = True,
  # ),
  # 
  # ### Sub populations
  # Measure(
  #   id = "midazolam_with_antipsychotic_groups",
  #   numerator = "midazolam_with_antipsychotic",
  #   denominator = "population",
  #   group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  # ),
  # 
  # ## Individuals who recieved a prescription for midazolam at the same time as a new AP
  # ### Whole population
  # Measure(
  #   id = "midazolam_with_new_antipsychotic_all",
  #   numerator = "midazolam_with_new_antipsychotic",
  #   denominator = "population",
  #   group_by = ["population"],
  #   small_number_suppression = True,
  # ),
  # 
  # ### Sub populations
  # Measure(
  #   id = "midazolam_with_new_antipsychotic_groups",
  #   numerator = "midazolam_with_new_antipsychotic",
  #   denominator = "population",
  #   group_by = ["dementia", "care_home", "learning_disability", "autism", "serious_mental_illness"]
  # ),
  
]





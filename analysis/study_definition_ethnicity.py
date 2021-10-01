from cohortextractor import (
  StudyDefinition,
  patients,
  codelist,
  codelist_from_csv,
)

from datetime import date

start_date = "2019-01-01"
end_date = "2021-04-01"

from codelists import *
  
study = StudyDefinition(
    
    default_expectations={
      "date": {"earliest": "1900-01-01", "latest": "today"},
      "rate": "uniform",
      "incidence": 0.5,
    },
    
    index_date = end_date,
    
    population = patients.all(),
    
    eth2001 = patients.with_these_clinical_events(
      ethnicity_codes,
      returning = "category",
      find_last_match_in_period = True,
      on_or_before = "last_day_of_month(index_date)",
      return_expectations = {
        "category": {"ratios": {"1": 0.8, "5": 0.1, "3": 0.1}},
        "incidence": 0.75,
      },
    ),
    
    # Any other ethnicity code
    non_eth2001_dat = patients.with_these_clinical_events(
      ethnicity_other_codes,
      returning = "date",
      find_last_match_in_period = True,
      on_or_before = "last_day_of_month(index_date)",
      date_format = "YYYY-MM-DD",
    ),
    
    # Ethnicity not given - patient refused
    eth_notgiptref_dat = patients.with_these_clinical_events(
      ethnicity_not_given_codes,
      returning = "date",
      find_last_match_in_period = True,
      on_or_before = "last_day_of_month(index_date)",
      date_format = "YYYY-MM-DD",
    ),
    
    # Ethnicity not stated
    eth_notstated_dat = patients.with_these_clinical_events(
      ethnicity_not_stated_codes,
      returning = "date",
      find_last_match_in_period = True,
      on_or_before = "last_day_of_month(index_date)",
      date_format = "YYYY-MM-DD",
    ),
    
    # Ethnicity no record
    eth_norecord_dat = patients.with_these_clinical_events(
      ethnicity_no_record_codes,
      returning = "date",
      find_last_match_in_period = True,
      on_or_before = "last_day_of_month(index_date)",
      date_format = "YYYY-MM-DD",
    ),

)

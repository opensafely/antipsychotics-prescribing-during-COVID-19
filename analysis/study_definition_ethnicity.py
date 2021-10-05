from cohortextractor import (
  StudyDefinition,
  patients,
  codelist,
  codelist_from_csv,
  Measure,
)

from datetime import date

end_date = "2021-04-01"

from codelists import *
  
study = StudyDefinition(
    default_expectations={
      "date": {"earliest": "1900-01-01", "latest": "today"},
      "rate": "uniform",
    },
    
    index_date=end_date,
    
    population = patients.satisfying(
    """
        NOT has_died
        AND
        registered
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
    
    # Ethnicity
    eth2001=patients.with_these_clinical_events(
      ethnicity_6_codes,
      returning="category",
      find_last_match_in_period=True,
      on_or_before="index_date",
      return_expectations={
        "category": {
          "ratios": {
            "1": 0.6,
            "2": 0.1,
            "3": 0.1,
            "4": 0.1,
            "5": 0.1,
          }
        },
        "rate": "universal",
      },
    ),
    
    
)

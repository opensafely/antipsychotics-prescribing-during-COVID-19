from cohortextractor import (
  StudyDefinition,
  patients,
  codelist,
  codelist_from_csv,
  Measure,
)

from datetime import date

end_date = "2021-12-01"

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
    eth2001 = patients.with_these_clinical_events(
        ethnicity_codes_16,
        returning = "category",
        find_last_match_in_period = True,
        include_date_of_match = False,
        return_expectations = {
            "category": {
                "ratios": {
                    "1": 0.0625,
                    "2": 0.0625,
                    "3": 0.0625,
                    "4": 0.0625,
                    "5": 0.0625,
                    "6": 0.0625,
                    "7": 0.0625,
                    "8": 0.0625,
                    "9": 0.0625,
                    "10": 0.0625,
                    "11": 0.0625,
                    "12": 0.0625,
                    "13": 0.0625,
                    "14": 0.0625,
                    "15": 0.0625,
                    "16": 0.0625,
                }
            },
            "incidence": 0.75,
        },
    ),
    
    
)

from cohortextractor import (
    StudyDefinition,
    patients,
    codelist,
    codelist_from_csv,
    Measure,
)

from datetime import date

start_date = "2019-01-01"
end_date = "2021-04-01"

from codelists import ethnicity_codes

study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
    },
    index_date=end_date,
    population=patients.all(),

    # ETHNICITY IN 6 CATEGORIES
    eth=patients.with_these_clinical_events(
        ethnicity_codes,
        returning="category",
        find_last_match_in_period=True,
        include_date_of_match=False,
        return_expectations={
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.75,
        },
    ),

    # fill missing ethnicity from SUS
    ethnicity_sus = patients.with_ethnicity_from_sus(
        returning="group_6",  
        use_most_frequent_code=True,
        return_expectations={
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.4,
            },
    ),
    

    ethnicity = patients.categorised_as(
            {"0": "DEFAULT",
            "1": "eth='1' OR (NOT eth AND ethnicity_sus='1')", 
            "2": "eth='2' OR (NOT eth AND ethnicity_sus='2')", 
            "3": "eth='3' OR (NOT eth AND ethnicity_sus='3')", 
            "4": "eth='4' OR (NOT eth AND ethnicity_sus='4')",  
            "5": "eth='5' OR (NOT eth AND ethnicity_sus='5')",
            }, 
            return_expectations={
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.4,
            },
    ),


)

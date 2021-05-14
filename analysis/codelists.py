######################################

# Some covariates used in the study are created from codelists of clinical conditions or 
# numerical values available on a patient's records.
# This script fetches all of the codelists identified in codelists.txt from OpenCodelists.

######################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (codelist, codelist_from_csv, combine_codelists)


# --- CODELISTS ---

## Learning disabilities
learning_disability_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-ld_cod.csv",
  system = "snomed",
  column = "code",
)

## Autism
autism_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-autism_cod.csv",
  system = "snomed",
  column = "code",
)

## Serious Mental Illness
serious_mental_illness_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-mh_cod.csv",
  system = "snomed",
  column = "code",
)

## Care homes
carehome_primis_codes = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-longres.csv", 
    system = "snomed", 
    column = "code",
)

## Dementia
dementia_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-dem_cod.csv", 
    system="snomed", 
    column="code",
)

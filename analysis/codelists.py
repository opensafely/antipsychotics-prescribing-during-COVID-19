######################################

# Some covariates used in the study are created from codelists of clinical conditions or 
# numerical values available on a patient's records.
# This script fetches all of the codelists identified in codelists.txt from OpenCodelists.

######################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (codelist, codelist_from_csv, combine_codelists)


# --- CODELISTS ---


## Medication DM&D

### First generation antipsychotics, excluding long acting depots
antipsychotics_first_gen_codes = codelist_from_csv(
  "codelists/opensafely-first-generation-antipsychotics-excluding-long-acting-depots-dmd.csv",
  system = "snomed",
  column = "dmd_id",
)

### Second generation antipsychotics excluding long acting injections
antipsychotics_second_gen_codes = codelist_from_csv(
  "codelists/opensafely-second-generation-antipsychotics-excluding-long-acting-injections.csv",
  system = "snomed",
  column = "dmd_id",
)

### Long acting injectable and depot antipsychotics
antipsychotics_injectable_and_depot_codes = codelist_from_csv(
  "codelists/opensafely-long-acting-injectable-and-depot-antipsychotics-dmd.csv",
  system = "snomed",
  column = "dmd_id",
)

### Prochlorperazine
prochlorperazine_codes = codelist_from_csv(
  "codelists/opensafely-prochlorperazine-dmd.csv",
  system = "snomed",
  column = "dmd_id",
)


## Groups

### Learning disabilities
learning_disability_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-ld_cod.csv",
  system = "snomed",
  column = "code",
)

### Autism
autism_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-autism_cod.csv",
  system = "snomed",
  column = "code",
)

### Serious Mental Illness
serious_mental_illness_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-mh_cod.csv",
  system = "snomed",
  column = "code",
)

### Care homes
carehome_primis_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-longres.csv", 
  system = "snomed", 
  column = "code",
)

### Dementia
dementia_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-dem_cod.csv", 
  system="snomed", 
  column="code",
)


## Variables

## Ethnicity
ethnicity_codes = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-eth2001.csv",
    system="snomed",
    column="code",
    category_column="grouping_16_id",
)

ethnicity_6_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-eth2001.csv",
  system = "snomed",
  column = "code",
  category_column="grouping_6_id",
)

## Any other ethnicity code
ethnicity_other_codes = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-non_eth2001.csv",
    system = "snomed",
    column = "code",
)

## Ethnicity not given - patient refused
ethnicity_not_given_codes = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-eth_notgiptref.csv",
    system = "snomed",
    column = "code",
)

## Ethnicity not stated
ethnicity_not_stated_codes = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-eth_notstated.csv",
    system = "snomed",
    column = "code",
)

# Ethnicity no record
ethnicity_no_record_codes = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-eth_norecord.csv",
    system = "snomed",
    column = "code",
)

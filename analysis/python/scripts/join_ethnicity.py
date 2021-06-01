######################################

# This script adds the ethnicity variable to the main input data

######################################

# --- IMPORT STATEMENTS ---

## Import packages
import pandas as pd
import os
from pathlib import Path


## Import data
BASE_DIR = Path(__file__).parents[3]
OUTPUT_DIR = BASE_DIR / "output/data"
ethnicity_df = pd.read_feather(OUTPUT_DIR / 'input_ethnicity.feather')


# --- ADD ETHNICITY ---

for file in os.listdir('output/data'):
  if file.startswith('input'):
    #exclude ethnicity
    if file.split('_')[1] not in ['ethnicity.csv', 'practice']:
      file_path = os.path.join('output/data', file)
      df = pd.read_feather(file_path)
      merged_df = df.merge(ethnicity_df, how = 'left', on = 'patient_id')
      
      merged_df.to_feather(file_path) 

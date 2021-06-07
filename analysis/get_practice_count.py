import pandas as pd
import numpy as np
import json

practice_df = pd.read_feather('output/data/input_practice_count.feather')

def get_number_practices(df):
    practices = df['practice']
    total_num = len(np.unique(practices))
    return total_num

num_practices = get_number_practices(practice_df)

with open('output/data/practice_count.json', 'w') as f:
    json.dump({"num_practices": num_practices}, f)




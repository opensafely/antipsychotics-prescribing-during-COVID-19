import pandas as pd
import numpy as np
import json

practice_df = pd.read_feather('output/data/input_ld_2019-01-01.feather')

data_top = practice_df.head() 
    
data_top 

def get_number_practices(df):
    practices = df['practice']
    total_num = len(np.unique(practices))
    return total_num


num_practices = get_number_practices(practice_df)


with open('output/practice_count.json', 'w') as f:
    json.dump({"num_practices": num_practices}, f)




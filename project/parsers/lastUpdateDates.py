import pandas as pd
import numpy as np

df = pd.read_csv('../dataset/googleplaystore.csv')

df['new'] = pd.to_datetime(df['Last Updated'])
df['lastupdate'] = (df['new'] -  df['new'].max()).dt.days

df.to_csv('../dataset/pre-processed/lastUpdated.csv')
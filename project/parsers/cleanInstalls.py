import pandas as pd
import numpy as np

df = pd.read_csv('../dataset/googleplaystore.csv')

df['Installs'] = df['Installs'].apply(lambda x: x.replace('+', '') if '+' in str(x) else x)
df['Installs'] = df['Installs'].apply(lambda x: x.replace(',', '') if ',' in str(x) else x)
df['Installs'] = df['Installs'].apply(lambda x: int(x))

df.to_csv('../dataset/googleplaystore.csv')
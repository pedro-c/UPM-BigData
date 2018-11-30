import pandas as pd
import numpy as np

def returnInt(x):
	x=x.replace('M', '')
	return float(x)*1000

#---------------------------------------------------------------------------------------------------------------------#
df = pd.read_csv('../dataset/googleplaystore.csv')

df['Installs'] = df['Installs'].apply(lambda x: x.replace('+', '') if '+' in str(x) else x)
df['Installs'] = df['Installs'].apply(lambda x: x.replace(',', '') if ',' in str(x) else x)
df['Installs'] = df['Installs'].apply(lambda x: int(x))


df['Size'] = df['Size'].apply(lambda x: returnInt(x) if 'M' in str(x) else x)
df['Size'] = df['Size'].apply(lambda x: x.replace('k', '') if 'k' in str(x) else x)
df['Size'] = df['Size'].apply(lambda x: x.replace('Varies with device', '') if 'Varies with device' in str(x) else x)

df.to_csv('../dataset/pre-processed/cleanSize.csv')



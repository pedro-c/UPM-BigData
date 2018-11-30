import pandas as pd
import math

def selectData(df, newData):
	for i, rowAux in newData.iterrows():
		#Initialize counts for each column in each Category
		countRating=0
		countSize=0
		countInstalls=0
		for j, row in df.iterrows():
			if rowAux['Category'] == row['Category']:
				if math.isnan(row['Rating'])==False:									
					rowAux['Rating']+=float(row['Rating'])
					countRating += 1
				if math.isnan(row['Installs'])==False:
					rowAux['Installs'] += int(row['Installs'])
					countInstalls += 1
				if math.isnan(row['Size'])==False:
					rowAux['Size'] += float(row['Size'])
					countSize += 1
		
		#Calculate the mean for each variable of a given Category
		newData.iloc[i, newData.columns.get_loc('Rating')]=rowAux['Rating']/countRating
		newData.iloc[i, newData.columns.get_loc('Installs')]=rowAux['Installs']/countInstalls
		newData.iloc[i, newData.columns.get_loc('Size')]=rowAux['Size']/countSize
	
	return newData




#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
df = pd.read_csv('../dataset/pre-processed/cleanSize.csv')

d = {'Category': ['ART_AND_DESIGN', 'AUTO_AND_VEHICLES', 'BEAUTY', 'BOOKS_AND_REFERENCE', 'BUSINESS', 'COMICS', 'COMMUNICATION', 'DATING', 'EDUCATION', 'ENTERTAINMENT', 'EVENTS', 						'FINANCE', 'FOOD_AND_DRINK', 'HEALTH_AND_FITNESS', 'HOUSE_AND_HOME', 'LIBRARIES_AND_DEMO', 'LIFESTYLE', 'GAME', 'FAMILY', 'MEDICAL', 'SOCIAL', 'SHOPPING', 						'PHOTOGRAPHY', 'SPORTS', 'TRAVEL_AND_LOCAL', 'TOOLS', 'PERSONALIZATION', 'PRODUCTIVITY', 'PARENTING', 'WEATHER', 'MAPS_AND_NAVIGATION', 'NEWS_AND_MAGAZINES', 						'VIDEO_PLAYERS'], 'Installs':0, 'Rating':0, 'Size':0}
newData = pd.DataFrame(data=d)



wData = selectData(df, newData)
wData.to_csv('../dataset/pre-processed/dataBubbleChart.csv')

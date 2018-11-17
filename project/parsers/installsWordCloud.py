import csv
from decimal import *

# ex: averages = {'game_name': [count, installs_average]}
averages = {}

fh = open("../dataset/pre-processed/parsedInstalls.csv", 'rt') 
reader = csv.reader(fh, delimiter=',')
next(reader, None)  # skip the headers
data = list(reader) 

# game = ['game_name', installs]
for i, game in enumerate(data):
  # games without numeric installs are not considered
  gameTitle = game[1]
  gameInstalls = game[6]
  installs = float(gameInstalls)
  # go through each word on the game title
  for x, word in enumerate(gameTitle.split()):
    #convert all words to lower case
    lowerCaseWord = word.lower()
    if lowerCaseWord in averages:
      count = averages[lowerCaseWord][0]
      average = averages[lowerCaseWord][1]
      # calculation for continuous average (https://stackoverflow.com/a/22999488)
      averages[lowerCaseWord][1] = ((count * average + installs) / (count + 1))
      averages[lowerCaseWord][0] = count + 1
    else:
      averages[lowerCaseWord] = [1,installs]

# Save data to new file
with open('../dataset/pre-processed/wordInstalls.csv', 'w') as csvfile:
  parsedCSV = csv.writer(csvfile, delimiter=';')
  for key, value in averages.items():
    if (value[0] > 15): 
      parsedCSV.writerow([key, value[1], value[0]])


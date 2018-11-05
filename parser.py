import csv

# ex: averages = {'game_name': [count, rating_average]}
averages = {}

fh = open("worddata.csv", 'rt') 
reader = csv.reader(fh, delimiter=';') 
data = list(reader) 

# game = ['game_name', rating]
for i, game in enumerate(data):
  # games without numeric rating are not considered
  gameTitle = game[0]
  gameRating = game[1]
  if gameRating != 'NaN':
    rating = float(gameRating)
    # go through each word on the game title
    for x, word in enumerate(gameTitle.split()):
      #convert all words to lower case
      lowerCaseWord = word.lower()
      if lowerCaseWord in averages:
        count = averages[lowerCaseWord][0]
        average = averages[lowerCaseWord][1]
        # calculation for continuous average (https://stackoverflow.com/a/22999488)
        averages[lowerCaseWord][1] = ((count * average + rating) / (count + 1))
        averages[lowerCaseWord][0] = count + 1
      else:
        averages[lowerCaseWord] = [1,rating]

# Save data to new file
with open('parsed.csv', 'w') as csvfile:
  parsedCSV = csv.writer(csvfile, delimiter=';')
  for key, value in averages.items():
    if value[0] > 20:
      parsedCSV.writerow([key, value[0], value[1]])


import csv

fh = open("../dataset/pre-processed/parsedInstalls.csv", 'rt') 
reader = csv.reader(fh, delimiter=',')
next(reader, None)  # skip the headers
data = list(reader) 



    ggplot(data=df, aes(df$Rating, df$Installs)) +
                  geom_point()

# game = ['game_name', rating]
for i, game in enumerate(data):
  # games without numeric rating are not considered
  gameTitle = game[0]
  gameRating = game[2]
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
with open('../dataset/pre-processed/avgRatings.csv', 'w') as csvfile:
  parsedCSV = csv.writer(csvfile, delimiter=';')
  for key, value in averages.items():
    if (value[0] > 15): 
      parsedCSV.writerow([key, value[1], value[0]])



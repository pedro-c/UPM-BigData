##Load Required Library
library(shiny)
library(tm)
library(wordcloud)
library(memoise)

datasets<<- list("googleplaystore")
ratings <- read.csv("./dataset/pre-processed/ratings150.csv", header=TRUE, sep=";")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(data) {
  
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(data %in% datasets))
    stop("Unknown book")
  
  text<-read.csv(sprintf("./dataset/%s.csv", data))
  text<-data.frame(text)
  
  myCorpus = Corpus(VectorSource(text$App))
  myCorpus = tm_map(myCorpus, PlainTextDocument)
  myCorpus = tm_map(myCorpus,tolower)
  myCorpus = tm_map(myCorpus,removeNumbers)
  myCorpus = tm_map(myCorpus,removeWords,stopwords("english"))
  myCorpus = tm_map(myCorpus,removePunctuation)
  myCorpus = tm_map(myCorpus,stripWhitespace)
  myCorpus = tm_map(myCorpus,stemDocument)
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Big Data - Visualization Project"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a wordcloud:",
                  choices = datasets),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 20),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  # wordcloud
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  # ratings bar plot
  output$distPlot <- renderPlot({
    # draw the histogram with the specified number of bins
    df <- ratings[order(ratings$Rating,decreasing = TRUE),]
    
    barplot(df$Rating, names.arg = ratings$App, las=2, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


##Load Required Library
library(shiny)
library(tm)
library(wordcloud)
library(memoise)

datasets<<- list("wordRatings", "wordInstalls")
ratings <- read.csv("./dataset/pre-processed/wordRatings.csv", header=TRUE, sep=";")
installs <- read.csv("./dataset/pre-processed/wordInstalls.csv", header=TRUE, sep=";")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(data) {
  
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(data %in% datasets))
    stop("Unknown book")
  
  text<-read.csv(sprintf("./dataset/pre-processed/%s.csv", data), header=TRUE, sep=";")
  text<-data.frame(text)
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
                  min = 1,  max = 200, value = 50),
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
    v <- subset(terms(), Count > input$freq)
    wordcloud_rep(words = v$Word, freq = v$Value, min.freq = input$freq, max.words=input$max, colors=brewer.pal(8, "Dark2"))
  })
  
  # ratings bar plot
  output$distPlot <- renderPlot({
    # draw the histogram with the specified number of bins
    df <- ratings[order(ratings$Value,decreasing = TRUE),]
    
    barplot(df$Value, names.arg = ratings$Word, las=2, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


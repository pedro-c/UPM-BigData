##Load Required Library
library(shiny)
library(tm)
library(wordcloud)
library(memoise)

datasets<<- list("wordInstalls", "wordRatings")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(data) {
  
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(data %in% datasets))
    stop("Unknown book")
  
  text<-read.csv(sprintf("./dataset/pre-processed/%s.csv", data), header=TRUE, sep=";")
  text<-data.frame(text)
})

getHistogramData <-memoise(function(data) {
  if (!(data %in% datasets))
    stop("Unknown book")
  
  data<-read.csv(sprintf("./dataset/pre-processed/%s.csv", data), header=TRUE, sep=";")
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
                  min = 1,  max = 300,  value = 100)
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
  
  df <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Updating histogram...")
        getHistogramData(input$selection)
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
  
 
  
  output$distPlot <- renderPlot({
    x <- subset(df(), Count > input$freq)
    values<-x[order(x$Value,decreasing = TRUE),]
    values<-head(values,n=input$max)
    words <-x$Word
    words <-head(words,n=input$max)
    
    barplot(values$Value, names.arg = words, las=2, col = 'darkgray', border = 'white')
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


##Load Required Library
library(shiny)
library(tm)
library(wordcloud)
library(memoise)

datasets<<- list("googleplaystore")

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
  titlePanel("Word Cloud"),
  
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
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
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
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


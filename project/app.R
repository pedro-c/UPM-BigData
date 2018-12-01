##Load Required Library
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(ggplot2)

datasets <- list("wordInstalls", "wordRatings")
records <- read.csv(file="./dataset/pre-processed/dataBubbleChart.csv", header=TRUE, sep=",")
category = records[,2]

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

getLastUpdateData <-memoise(function() {
  data<-read.csv("./dataset/pre-processed/lastUpdated.csv", header=TRUE, sep=",")
  data<-data.frame(data)
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
                  min = 15,  max = 200, value = 50),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100),
      sliderInput("days",
                  "Number of days:",
                  min = 1,  max = 3000,  value = 3000),
      list(h3("Multicolumn checkboxGroupInput"), 
           tags$div(align = 'left', 
                    class = 'multicol', 
                    checkboxGroupInput(inputId  = 'checkBoxCat', 
                                       label    = NULL, 
                                       choices  = category,
                                       selected = category,
                                       inline   = FALSE))) 
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("wordcloud"),
      plotOutput("distPlot"),
      plotOutput("scatterplot"),
      plotOutput("bubbleChartPlot")
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
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
  output$wordcloud <- renderPlot({
    v <- subset(terms(), Count > input$freq)
    wordcloud_rep(words = v$Word, freq = v$Value, min.freq = input$freq, max.words=input$max, colors=brewer.pal(8, "Dark2"))
  })
  
 
  # Histogram
  output$distPlot <- renderPlot({
    x <- subset(df(), Count > input$freq)
    values<-x[order(x$Value,decreasing = TRUE),]
    values<-head(values,n=input$max)
    words <-x$Word
    words <-head(words,n=input$max)
    
    barplot(values$Value, names.arg = words, las=2, col = 'darkgray', border = 'white')
    
  })
  
  # scatterplot
  output$scatterplot <- renderPlot({
    data <- subset(getLastUpdateData(), lastupdate > -(input$days))
    
    plot(data$lastupdate, data$Rating, main="Rating  VS Last Update( days ago )", xlab="Last Update (days) ", ylab="Rating", pch=19,  col  = "aquamarine")
    
    abline(lm(data$Rating~data$lastupdate), col="red") # regression line (ypl~x) 
    
  })

  # Bubblechart
  output$bubbleChartPlot <- renderPlot({
    df<-records
    
    x <- input$checkBoxCat
    
    #new df with values for chosen categories
    count = 0
    for (row in 1:nrow(df)) {
      for (i in 1:length(x)){
        value <- df[row, "Category"]
        
        if(count==0){
          aux <- data.frame(matrix(ncol = 4, nrow = 0))
          c <- c("Category", "Installs", "Rating", "Size")
          colnames(aux) <- c
          count = count + 1
        }
        else if(value==x[i]){
          newRow <- data.frame(Category=value, Installs=df[row,"Installs"], Rating=df[row,"Rating"], Size=df[row, "Size"])
          aux <- rbind(aux, newRow)
        }
      }
    }
    
    updateCheckboxGroupInput(session, "checkBoxCat",
                             label = paste("Checkboxgroup label", length(x)),
                             choices = category,
                             selected = x
    )
    
    Categories <- aux[,1]
    Size <- aux[,4]
    ggplot(data=aux, aes(aux[, 3], aux[, 2], color=Categories, size=Size)) + geom_point() + scale_size_continuous(range = c(3, 20)) +
      labs(x = ~Rating, y = ~Installations) + theme(legend.position="bottom")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


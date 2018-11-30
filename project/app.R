##Load Required Library
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(ggplot2)

datasets <- list("wordInstalls", "wordRatings")
records <- read.csv(file="./dataset/pre-processed/dataBubbleChart.csv", header=TRUE, sep=",")
category = records[,2]
installs = records[,3]
ratings = records[,4]
sizeApp = records[,5]

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
      plotOutput("plot")
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

  # bubblechart
  output$plot <- renderPlot({
    df2<-records
    
    y <- input$checkBoxCat
    
    updateCheckboxGroupInput(session, "checkBoxCat",
                             label = paste("Checkboxgroup label", length(y)),
                             choices = category,
                             selected = y
    )
    
    dih_col <- which(df2$Category %in% y)
    if (y  == category){
      ggplot(data=df2, aes(ratings, installs, color=category, size=sizeApp)) + geom_point() + scale_size_continuous(range = c(3, 20)) +
        labs(x = ~Rating, y = ~Installations)
    }else {
      print(y)
      rowNum <- which(df2$Category==y)
      
      print(rowNum)
      print(df2[rowNum,2])
      ggplot(data=df2, aes(ratings, installs, color=records[rowNum, 2], size=df2[rowNum, 5])) + geom_point() + scale_size_continuous(range = c(3, 20)) +
        labs(x = ~Rating, y = ~Installations)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


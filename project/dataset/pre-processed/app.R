##Load Required Library
library(shiny)
library(ggplot2)

records <- read.csv(file="dataBubbleChart.csv", header=TRUE, sep=",")
category = records[,2]
installs = records[,3]
ratings = records[,4]
sizeApp = records[,5]

#list object to set up multicols for checkboxGroupInput
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))

controls <-
  list(h3("Multicolumn checkboxGroupInput"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'checkBoxCat', 
                                   label    = NULL, 
                                   choices  = category,
                                   selected = category,
                                   inline   = FALSE))) 


# --------------------------------------------------------------Define UI---------------------------------------------------------
ui <- fluidPage(
  titlePanel("Categories worth exploring"),
  br(),
  
  mainPanel(
    plotOutput("plot")
  ),
  
  tweaks,
  fluidRow(
    column(width = 10, controls)
  )
)	

# ---------------------------------------------------------Define server logic---------------------------------------------------------
server <- function(input, output, session) {
  
  # bubblechart
  output$plot <- renderPlot({
    df<-records
    
    x <- input$checkBoxCat
    
    updateCheckboxGroupInput(session, "checkBoxCat",
                             label = paste("Checkboxgroup label", length(x)),
                             choices = category,
                             selected = x
    )
    
    dih_col <- which(df$Category %in% x)
    if (x == category){
      ggplot(data=df, aes(ratings, installs, color=category, size=sizeApp)) + geom_point() + scale_size_continuous(range = c(3, 20)) +
        labs(x = ~Rating, y = ~Installations)
    }else {
      print(x)
      rowNum <- which(df$Category==x)
      
      print(rowNum)
      print(df[rowNum,2])
      ggplot(data=df, aes(ratings, installs, color=records[rowNum, 2], size=df[rowNum, 5])) + geom_point() + scale_size_continuous(range = c(3, 20)) +
        labs(x = ~Rating, y = ~Installations)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




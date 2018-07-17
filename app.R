library(shiny)
library(DT)

source("dfs.R")

#Define UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("Downloading Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("dataset", "Which format do you want your data in?",
                  choices = c("Individual", "Dyad", "Pairwise")),
      
      #Render UI in the server will display a selectInput() and let user choose variables 
      #based on the format of data they
      uiOutput("varControls"),
      
      # Button
      downloadButton("downloadData", "Download"),
      
      width = 3
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("View Data", hr(),
                           dataTableOutput("table")),
                  tabPanel("Explore Variables", 
                           column(3, hr(),
                           uiOutput("varExplore")), 
                           column(9, hr(),
                           plotOutput("plot"))),
                  tabPanel("Data Formats")
      )
      
    )
    
  )
  
)

server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Individual" = df_indiv,
           "Dyad" = df_dyad,
           "Pairwise" = df_pair)
  })
  
  # Table of selected dataset ----
  output$table <- renderDataTable({
    datasetInput() %>%
      select(varInput())
  }, options = list(scrollX = TRUE))
  
  output$varControls <- renderUI({
    whichone <- datasetInput()
    selectInput("vars", "Which variables do you want included?",
                choices = names(whichone), multiple = TRUE, selected = "DYADID")
  })
  
  varInput <- reactive({
    input$vars
  })
  
  output$varExplore <- renderUI({
    whichone <- datasetInput()
    selectInput("vars2", "Which variable do you want to explore?",
                choices = names(whichone), selected = "DYADID")
  })
  
  varInput2 <- reactive({
    input$vars2
  })
  
  #Plot for data exploration

  output$plot <- renderPlot({
    ggplot(datasetInput(), aes(varInput2())) +
      geom_histogram(stat = "count")
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput() %>%
                  select(varInput()), file, row.names = FALSE)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
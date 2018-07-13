library(shiny)

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
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("View Data", tableOutput("table")),
                  tabPanel("Explore Variables"),
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
  
  varInput <- reactive({
   input$vars
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    head(datasetInput() %>%
      select(varInput()))
  })
  
  output$varControls <- renderUI({
    whichone <- datasetInput()
    selectInput("vars", "Which variables do you want included?",
                choices = names(whichone), multiple = TRUE, selected = "DYADID")
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
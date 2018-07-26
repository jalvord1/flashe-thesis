library(shiny)
library(DT)

source("dfs.R")
source("codebook.R")

#Define UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("Downloading FLASHE Data"),
      
      tabsetPanel(type = "tabs",
                  tabPanel("Download/View Data",
                           
                           column(3, br(),
                                  # Input: Choose dataset ----
                                  selectInput("dataset", "Which format do you want your data in?",
                                              choices = c("Individual", "Dyad", "Pairwise")),
                                  
                                  #Render UI in the server will display a selectInput() and let user choose variables 
                                  #based on the format of data they
                                  uiOutput("varControls"),
                                  
                                  # Button
                                  downloadButton("downloadData", "Download")), 
                            column(9, br(), dataTableOutput("table"))),
                  tabPanel("Explore Variables", 
                           column(3, br(),
                          selectInput("vars2", h5("Which variable do you want to explore?"),
                                              choices = codebook_df_full$cleaned_var, selected = "DYADID"),
                          span(textOutput("warning_var_exp"), style="color:grey")), 
                           column(9,br(),
                           h4("The question/identifier from the codebook is:"), textOutput("question"),
                           br(),
                           h4("The valid values include:"), tableOutput("valid"))),
                  tabPanel("Download Codebook", br(), h5("Download the Codebook"), 
                           downloadButton("downloadCodebook", "Download"))
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
  
  # pulling out the vars chosen
  varInput <- reactive({
    input$vars
  })
  
  # Table of selected dataset ----
  output$table <- renderDataTable({
    datasetInput() %>%
      select(varInput())
  }, options = list(scrollX = TRUE))

  # selectInput() function allowing users to choose variables of selected df
  output$varControls <- renderUI({
    whichone <- datasetInput()
    selectInput("vars", "Which variables do you want included?",
                choices = names(whichone), multiple = TRUE, selected = "DYADID")
  })
  
  # selectInput() function allowing users to choose 1 variable to explore in second tab
  # output$varExplore <- renderUI({
  #   whichone <- datasetInput()
  #   selectInput("vars2", "Which variable do you want to explore?",
  #               choices = names(whichone), selected = "DYADID")
  # })
  
  # pulling out that 1 var
  varInput2 <- reactive({
    input$vars2
  })
  
  output$warning_var_exp <- renderText({
    "Please be aware that these choices come from the individual format dataframe. 
    These variables and their respective partner variables have essentially the same codebook questions, valid values, and exploratory statistics so
    it is unnecessary to include both in this exploration tab."
  })
  
  # df for plot
  
  #querying df to get out the question
  output$question <- renderText({
    question1 <- codebook_df_full %>%
      mutate(yes_cleaned_var = ifelse(cleaned_var == varInput2(), 1, 0)) %>%
      arrange(desc(yes_cleaned_var)) %>%
      select(codebook_question) %>%
      head(1)
    
    c(t(question1))
  })
  
  #valid values df
  output$valid <- renderTable({
    valid1 <- codebook_df_full %>%
      mutate(yes_cleaned_var = ifelse(cleaned_var == varInput2(), 1, 0)) %>%
      arrange(desc(yes_cleaned_var)) %>%
      select(valid_value1:valid_value12) %>%
      head(1)
  }, colnames = FALSE)


  # Plot for data exploration
  output$plot <- renderPlot({
    ggplot(datasetInput(), aes(varInput2())) +
      geom_bar()
  })
  
  # creating a summary table
  # output$tableExplore <- renderDataTable({
  #   datasetInput() %>%
  #     select(var = varInput2()) %>%
  #     summarise(mean = mean(var, na.rm = TRUE))
  # })
  
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
  
  output$downloadCodebook <- downloadHandler(
    filename = "codebook.csv",
    content = function(file) { 
      write.csv(codebook_df_full, file, row.names = FALSE)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
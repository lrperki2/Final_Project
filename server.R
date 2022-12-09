###
# Purpose: To create the back-end of the app for exploring wine data
# Date: 07DEC2022
# Author: Luke Perkins
###

# Read in packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)

# Read in raw data
white <- read_delim("winequality-white.csv", delim = ";") %>%
  mutate(type = 0)
red <- read_delim("winequality-red.csv", delim = ";") %>%
  mutate(type = 1)
# Combine data sets and rename columns; add type variable based on data source
wine <- bind_rows(white, red) %>%
  rename("fixed_acidity" = "fixed acidity",
         "volatile_acidity" = "volatile acidity",
         "citric_acid" = "citric acid",
         "residual_sugar" = "residual sugar",
         "free_sulfur_dioxide" = "free sulfur dioxide",
         "total_sulfur_dioxide" = "total sulfur dioxide")
wine$type <- factor(wine$type, labels = c("white", "red"))

# Define server for app
shinyServer(function(input, output, session) {
  
  # Subset data based on user inputs
  data_input <- reactive({
    wine <- wine %>%
      # Filter based on min range of input values
      filter((fixed_acidity >= input$fixed_acidity_min) &
             (volatile_acidity >= input$volatile_acidity_min) &
             (citric_acid >= input$citric_acid_min) &
             (residual_sugar >= input$residual_sugar_min) &
             (chlorides >= input$chlorides_min) &
             (free_sulfur_dioxide >= input$free_sulfur_dioxide_min) &
             (total_sulfur_dioxide >= input$total_sulfur_dioxide_min) &
             (density >= input$density_min) &
             (pH >= input$pH_min) &
             (sulphates >= input$sulphates_min) &
             (alcohol >= input$alcohol_min) &
             (quality >= input$quality_min) &
             # Filter based on max range of input values
             (fixed_acidity <= input$fixed_acidity_max) &
             (volatile_acidity <= input$volatile_acidity_max) &
             (citric_acid <= input$citric_acid_max) &
             (residual_sugar <= input$residual_sugar_max) &
             (chlorides <= input$chlorides_max) &
             (free_sulfur_dioxide <= input$free_sulfur_dioxide_max) &
             (total_sulfur_dioxide <= input$total_sulfur_dioxide_max) &
             (density <= input$density_max) &
             (pH <= input$pH_max) &
             (sulphates <= input$sulphates_max) &
             (alcohol <= input$alcohol_max) &
             (quality <= input$quality_max) &
             # Filter based on type of wine selected
             (type %in% input$type_box)
             ) %>%
      # Select input variables after filtering to avoid conflicts
      select(input$var_select)
  })
  
  # Render data table and add scroll bars
  output$table <- renderDataTable({
    datatable(data_input(), options = list(scrollX = TRUE, scrollY = 400))
  })
  
  #Save a vector of numeric variable names to use in value ranges
  numvars <- names(wine)[!names(wine) == "type"]
  
  # Set min input value for numeric variables
  output$filter_min <- renderUI({
    lapply(numvars, function(var){
      numericInput(
        inputId = paste0(var, "_min"),
        label = paste0("Min ", var),
        value = min(wine[ ,var]),
        step = 0.01,
        min = min(wine[ ,var]))
    })
  })
  
  # Set max input value for numeric variables
  output$filter_max <- renderUI({
    lapply(numvars, function(var){
      numericInput(
        inputId = paste0(var, "_max"),
        label = paste0("Max ", var),
        value = max(wine[ ,var]),
        step = 0.01,
        max = max(wine[ ,var]))
    })
  })
  
  # Function to download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("wine_data.csv")
    },
    content = function(file) {
      write.csv(data_input(), file, row.names = FALSE)
    }
  )
  
})

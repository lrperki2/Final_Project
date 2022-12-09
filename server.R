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

# Read in raw data; add type variable based on data source
white <- read_delim("winequality-white.csv", delim = ";") %>%
  mutate(type = 0)
red <- read_delim("winequality-red.csv", delim = ";") %>%
  mutate(type = 1)
wine <- bind_rows(white, red)
wine$type <- factor(wine$type, labels = c("white", "red"))

# Define server for app
shinyServer(function(input, output, session) {
  
  # Render subset table
  output$table <- renderDataTable({
    wine %>%
      select(input$varselect)
  })
  
  # Set min input variable value
  # FIX INPUTID
  output$filter_min <- renderUI({
    lapply(input$varselect, function(var){
      numericInput(
        inputId = paste0(var, "min_val"),
        label = paste0("Min ", var),
        value = NULL,
        step = 0.01,
        min = 0)
    })
  })
  
  # Set max input variable value
  # FIX INPUTID
  output$filter_max <- renderUI({
    lapply(input$varselect, function(var){
      numericInput(
        inputId = paste0(var, "max_val"),
        label = paste0("Max ", var),
        value = NULL,
        step = 0.01)
    })
  })
})

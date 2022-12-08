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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

})

###
# Purpose: To create the front-end of the app for exploring wine data
# Date: 07DEC2022
# Author: Luke Perkins
###

# Read in packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(caret)

# Read in raw data; add type variable based on data source
white <- read_delim("winequality-white.csv", delim = ";") %>%
  mutate(type = 0)
red <- read_delim("winequality-red.csv", delim = ";") %>%
  mutate(type = 1)
wine <- bind_rows(white, red)
wine$type <- factor(wine$type, labels = c("white", "red"))

# Define UI for app
dashboardPage(skin = "red",
              
              # Define header
              dashboardHeader(title = "Exploring Portugese Vinho Verde Wine", titleWidth = 1000),
              
              #Define sidebar items
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("circle-info")),
                menuItem("Data", tabName = "data", icon = icon("table")),
                menuItem("Data Exploration", tabName = "explore", icon = icon("chart-simple")),
                menuItem("Modeling", tabName = "model", icon = icon("magnifying-glass-chart"))
              )),
              
              # Define body and about tab
              dashboardBody(
                tabItems(
                  tabItem(tabName = "about",
                          fluidRow(
                            h1("PLACEHOLDER1")
                            )
                          ),
                  
                  # Define data tab
                  tabItem(tabName = "data",
                          fluidRow(
                            column(width = 3,
                                   box(width = 12, 
                                       background ="red",
                                       # Widget to select input vars for table
                                       selectizeInput(inputId = "varselect",
                                                      label = "Select variables to view",
                                                      choices = names(wine),
                                                      multiple = TRUE,
                                                      selected = names(wine))
                                       ),
                                     # Widget to filter min values
                                     fluidRow(
                                       column(width = 12, 
                                              box(width = 6,
                                                  background ="red",
                                                  uiOutput("filter_min")
                                                  ),
                                              
                                              # Widget to filter max values
                                              box(width = 6,
                                                  background ="red",
                                                  uiOutput("filter_max")
                                                  )
                                              )
                                       )
                                   ),
                            # Output table
                            column(width = 9,
                                   dataTableOutput("table"),
                                   )
                            )
                          ),
                  
                  # Define exploration tab
                  tabItem(tabName = "explore",
                          fluidRow(
                            h1("PLACEHOLDER3")
                            )
                          ),
                  
                  # Define modeling tab
                  tabItem(tabName = "model",
                          fluidRow(
                            h1("PLACEHOLDER4"),
                            # Define sub-tabs
                            tabsetPanel(
                              tabPanel("Model Info", "PLACEHOLDER5"),
                              tabPanel("Model Fitting", "PLACEHOLDER6"),
                              tabPanel("Model Predictions", "PLACEHOLDER7")
                            )
                          )
                  )
                )
              )
)

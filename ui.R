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
library(plotly)

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

#Save a vector of numeric variable names
numvars <- names(wine)[!names(wine) == "type"]

# Define UI for app
dashboardPage(skin = "yellow",
              
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
                                       background ="yellow",
                                       # Widget to select input vars for table
                                       selectizeInput(inputId = "var_select",
                                                      label = h4("Select Variables"),
                                                      choices = names(wine),
                                                      multiple = TRUE,
                                                      selected = names(wine))
                                       ),
                                   
                                   # Row for subsetting based on wine type
                                   fluidRow(
                                     box(width = 12,
                                         background = "yellow",
                                         checkboxGroupInput(inputId = "type_box",
                                                            label = h4("Wine Type"),
                                                            choices = c("white", "red"),
                                                            selected = c("white", "red"),
                                                            inline = TRUE)
                                     )
                                   ),
                                   
                                   # Row for value range header
                                   fluidRow(
                                     column(width = 6, 
                                            background = "yellow",
                                            h4("Set Value Ranges")
                                            ),
                                     
                                     # UI download button
                                     column(width = 6,
                                            downloadButton("downloadData", "Download")
                                            ),
                                     # Line break
                                     br(),
                                   ),
                                   
                                     # Widget to filter min values
                                   fluidRow(
                                     box(width = 12,
                                         background = "yellow",
                                         column(width = 6,
                                                uiOutput("filter_min")
                                                ),
                                              
                                            # Widget to filter max values
                                          column(width = 6,
                                                 uiOutput("filter_max")
                                                 )
                                         )
                                     )
                                   ),
                            
                            # Output table
                            column(width = 9,
                                   dataTableOutput("table")
                                   )
                            )
                          ),
                  
                  # Define exploration tab
                  tabItem(tabName = "explore",
                          fluidRow(
                            column(width = 3,
                                   box(width = 12, 
                                       background ="yellow",
                                       # Widget to select plot type
                                       radioButtons(inputId = "plot_rad",
                                                    label = h4("Plot Type"),
                                                    choices = c("Scatter Plot",
                                                                "Histogram"),
                                                    inline = FALSE
                                                    ),
                                       
                                       # Widget to select x var for plot
                                       selectizeInput(inputId = "x_var",
                                                      label = h4("X Variable"),
                                                      choices = numvars,
                                                      multiple = FALSE
                                                      ),
                                       
                                       # Conditional panel for y var selection
                                       conditionalPanel(condition = "input.plot_rad == 'Scatter Plot'",
                                         selectizeInput(inputId = "y_var",
                                                        label = h4("Y Variable"),
                                                        choices = numvars,
                                                        multiple = FALSE
                                                        )
                                         )
                                       ),
                                   
                                   # Subset summaries based on wine type
                                   box(width = 12,
                                       background = "yellow",
                                       checkboxGroupInput(inputId = "type_sum",
                                                          label = h4("Wine Type"),
                                                          choices = c("white", "red"),
                                                          selected = c("white", "red"),
                                                          inline = TRUE
                                                          )
                                       ),
                                   
                                   # Widget to select summary type
                                   box(width = 12,
                                       background = "yellow",
                                       radioButtons(inputId = "sum_rad",
                                                    label = h4("Summary Type"),
                                                    choices = c("Five-Number Summary and Mean",
                                                                "Correlation Matrix"),
                                                    inline = FALSE
                                                    )
                                       )
                            ),
                            
                            # Display graphical and numeric summaries
                            column(width = 9,
                                   h3("Graphical Summaries"),
                                   # Show plots
                                   plotlyOutput("wine_plot"),
                                   h3("Numeric Summaries"),
                                   # Dynamic title
                                   h4(textOutput("sum_title")),
                                   # Show numeric summaries
                                   dataTableOutput("sum_tbl")
                                   )
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

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
library(rpart)
library(randomForest)
library(rattle)

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

#Save a vector of variable names other than the response
predvars <- names(wine)[!names(wine) == "quality"]

# Define UI for app
dashboardPage(skin = "yellow",
              
              # Define header
              dashboardHeader(title = "Portugese Vinho Verde Wine Data", titleWidth = 1000),
              
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
                                   fluidRow(
                                     box(width = 12, 
                                         background ="yellow",
                                         # Widget to select input vars for table
                                         selectizeInput(inputId = "var_select",
                                                        label = h4("Select Variables"),
                                                        choices = names(wine),
                                                        multiple = TRUE,
                                                        selected = names(wine)
                                                        )
                                         )
                                       ),
                                   
                                   # Row for subsetting based on wine type
                                   fluidRow(
                                     box(width = 12,
                                         background = "yellow",
                                         checkboxGroupInput(inputId = "type_box",
                                                            label = h4("Wine Type"),
                                                            choices = c("white", "red"),
                                                            selected = c("white", "red"),
                                                            inline = TRUE
                                                            )
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
                                            downloadButton("downloadData", 
                                                           "Download", 
                                                           "btn btn-danger")
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
                                       background = "yellow",
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
                                                      selected = "density",
                                                      multiple = FALSE
                                                      ),
                                       
                                       # Conditional panel for y var selection
                                       conditionalPanel(condition = "input.plot_rad == 'Scatter Plot'",
                                         selectizeInput(inputId = "y_var",
                                                        label = h4("Y Variable"),
                                                        choices = numvars,
                                                        selected = "fixed_acidity",
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
                                                    selected = "Correlation Matrix",
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
                            # Define sub-tabs
                            tabsetPanel(
                              # Define model info sub-tab
                              tabPanel("Model Info", "PLACEHOLDER5"),
                              
                              
                              #Define model fitting sub-tab
                              tabPanel("Model Fitting", 
                                       fluidRow(
                                         column(width = 3,
                                                # Linear Regression inputs
                                                h3("Linear Regression"),
                                                box(width = 12, 
                                                    background = "yellow",
                                                    # Widget for lm variable selection
                                                    selectizeInput(inputId = "var_lm_inp",
                                                                   label = h4("Select Variables"),
                                                                   choices = predvars,
                                                                   multiple = TRUE,
                                                                   selected = predvars
                                                                   ),
                                                    
                                                    # Widget for including interaction terms
                                                    checkboxInput(inputId = "inter_box",
                                                                  label = h4("Include all interaction terms")
                                                                  )
                                                ),
                                                
                                                # Regression Tree inputs
                                                h3("Regression Tree"),
                                                box(width = 12,
                                                    background = "yellow",
                                                    # Widget for tree variable selection
                                                    selectizeInput(inputId = "var_tree_inp",
                                                                   label = h4("Select Variables"),
                                                                   choices = predvars,
                                                                   multiple = TRUE,
                                                                   selected = predvars
                                                                   ),
                                                    
                                                    # Sub-column for complexity parameters
                                                    h4("Complexity Parameters"),
                                                    column(width = 4,
                                                           # Widget for min cp parameter
                                                           numericInput(inputId = "cp_from",
                                                                        label = "Min",
                                                                        value = 0,
                                                                        min = 0,
                                                                        max = 1,
                                                                        step = 0.01
                                                                        )
                                                           ),
                                                    
                                                    # Widget for max cp parameter
                                                    column(width = 4,
                                                           numericInput(inputId = "cp_to",
                                                                        label = "Max",
                                                                        value = 0.1,
                                                                        min = 0.01,
                                                                        max = 1,
                                                                        step = 0.01
                                                                        )
                                                           ),
                                                    
                                                    # Widget for cp grid increment
                                                    column(width = 4,
                                                           numericInput(inputId = "cp_by",
                                                                        label = "By interval",
                                                                        value = 0.001,
                                                                        min = 0.0001,
                                                                        max = 1,
                                                                        step = 0.0001
                                                                        )
                                                           )
                                                    ),
                                                    
                                                # Random Forest inputs
                                                h3("Random Forest"),
                                                box(width = 12, 
                                                    background = "yellow",
                                                    # Widget for rf variable selection
                                                    selectizeInput(inputId = "var_rf_inp",
                                                                   label = h4("Select Variables"),
                                                                   choices = predvars,
                                                                   multiple = TRUE,
                                                                   selected = predvars
                                                                   ),
                                                    
                                                    # Widget to set max number of "try" variables
                                                    numericInput(inputId = "m_try",
                                                                 label = h4("Max random variables to try"),
                                                                 value = 4,
                                                                 min = 1,
                                                                 max = 5,
                                                                 step = 1
                                                                 )
                                                    ),
                                                
                                                # Training parameter inputs
                                                h3("Training Parameters"),
                                                box(width = 12,
                                                    background = "yellow",
                                                    # Widget for selecting proportion of training data
                                                    numericInput(inputId = "p_train",
                                                                 label = h4("Proportion of data in training set"),
                                                                 value = 0.7,
                                                                 min = 0.05,
                                                                 max = 1,
                                                                 step = 0.05
                                                                 ),
                                                    
                                                    # Widget for selecting number of cross-validation folds
                                                    numericInput(inputId = "k_folds",
                                                                 label = h4("Cross-validation folds"),
                                                                 value = 4,
                                                                 min = 2,
                                                                 max = 5,
                                                                 step = 1
                                                                 ),
                                                    
                                                    # Widget for centering and scaling data
                                                    checkboxInput(inputId = "preproc_box",
                                                                  label = h4("Center and scale data"),
                                                                  value = TRUE
                                                                  ),
                                                    
                                                    # Widget for setting random seed
                                                    numericInput(inputId = "rng_inp",
                                                                 label = h4("Seed"),
                                                                 value = 0,
                                                                 step = 1
                                                                 ),
                                                    
                                                    # Widget to fit all models
                                                    actionButton(inputId = "fit_button",
                                                                 label = "Fit All Models",
                                                                 class = "btn btn-danger btn-lg"
                                                                 ),
                                                    )
                                                ),
                                         # Body of modeling page
                                         column(width = 9,
                                                
                                                # Output training fit stats
                                                h3("Training Fit Statistics(Best Tunes)"),
                                                dataTableOutput("train_tbl"),
                                                
                                                # Output coefficient summaries
                                                h3("Linear Model Coefficients Summary"),
                                                dataTableOutput("mlr_sum_tbl"),
                                                
                                                # Output tree plot
                                                h3("Tree Plot(Best Tune)"),
                                                plotOutput("tree_plot"),
                                                
                                                # Output variable importance for random forest
                                                h3("Random Forest Variable Importance"),
                                                plotOutput("rf_imp_plot"),
                                                
                                                # Output table of fit stats on test data
                                                h3("Fit Statistics on Test Set"),
                                                dataTableOutput("fit_test_tbl"),
                                                
                                                h3("Conclusion"),
                                                h4(textOutput("fit_statement"))
                                                )
                                         )
                                       ),
                              
                              # Define predictions tab
                              tabPanel("Model Predictions", "PLACEHOLDER7")
                              )
                            )
                          )
                  )
                )
              )
              


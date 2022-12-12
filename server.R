###
# Purpose: To create the back-end of the app for exploring wine data
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

# Define server for app
shinyServer(function(input, output, session) {
  
  # Subset data based on user inputs
  data_input <- reactive({
    wine_data <- wine %>%
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
    datatable(data_input(), options = list(scrollX = TRUE, 
                                           scrollY = 800,
                                           pageLength = 20))
  })
  
  # Set min input value for each numeric variable
  output$filter_min <- renderUI({
    # Apply numericInput function to each var and generate inputs
    lapply(numvars, function(var){
      numericInput(
        inputId = paste0(var, "_min"),
        label = paste0("Min ", var),
        value = min(wine[ ,var]),
        step = 0.01,
        min = min(wine[ ,var]))
    })
  })
  
  # Set max input value for each numeric variable
  output$filter_max <- renderUI({
    # Apply numericInput function to each var and generate inputs
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
  
  # Subset summary data based on user input
  data_sum <- reactive({
    wine_sum <- wine %>%
      # Filter based on type of wine selected
      filter(type %in% input$type_sum)
    })
  
  # Render plots
  output$wine_plot <- renderPlotly({
    # Conditionally add scatter plot
    if(input$plot_rad == "Scatter Plot"){
      # Base plotting object mapping x, y, and color to user inputs
      g <- ggplot(data_sum(), aes_string(x = input$x_var, 
                                         y = input$y_var,
                                         col = data_sum()$type))
      # Add scatter plot layer and regression lines
      g <- g + geom_point() +
        geom_smooth(method = "lm") +
        # Overwrite default colors and label
        scale_colour_manual(name = "type", 
                            values = c("white" = "gold", "red" = "maroon")) +
        # Add title based on input variables
        labs(title = paste0("Scatter plot of ", input$y_var, 
                            " by ", input$x_var))
      # Convert ggplot object to plotly object
      ggplotly(g)
      
        # Conditionally add histogram
    } else if(input$plot_rad == "Histogram"){
      # Base plotting object mapping x and fills to user inputs
      g <- ggplot(data_sum(), aes_string(x = input$x_var, 
                                         fill = data_sum()$type))
      # Add histogram layer
      g <- g + geom_histogram(color = "black", 
                              alpha = 0.7, 
                              position = "identity") + 
        # Overwrite default fills and label
        scale_fill_manual(name = "type",
                          values = c("white" = "gold", "red" = "maroon")) +
        # Add title based on input variable
        labs(title = paste0("Histogram of ", input$x_var))
      # Convert ggplot object to plotly object
      ggplotly(g)
    } else {}
  })
  
  # Output numeric summary title based on summary type
  output$sum_title <- renderText({
    if(input$sum_rad == "Five-Number Summary and Mean"){
      "Five-Number Summary and Mean"
    } else if(input$sum_rad == "Correlation Matrix"){
      "Correlation Matrix"
    } else{}
  })
  
  # Render summary statistics tables based on user input
  output$sum_tbl <- renderDataTable({
    # Conditionally output five number summary and mean
    if(input$sum_rad == "Five-Number Summary and Mean"){
      # Make list where each element is var's sum stats, then pass all lists 
      # as args to cbind function
      fmt_sum <- do.call(cbind, lapply(select(data_sum(), -(type)), summary))
      # Output as table with scroll bars
      datatable(fmt_sum, options = list(scrollX = TRUE, 
                                        scrollY = 250,
                                        pageLength = 20))
      # Conditionally output correlation matrix of numeric variables
    } else if(input$sum_rad == "Correlation Matrix"){
      cor_mat <- cor(select(data_sum(), -(type)))
      datatable(cor_mat, options = list(scrollX = TRUE, 
                                        scrollY = 250,
                                        pageLength = 20))
    } else{}
  })
  
  # Fit models on action button click
  observeEvent(input$fit_button, {
    
    # Create a Progress object
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    # Set progress bar
    progress$set(message = "Fitting Models:", value = 0)
    
    # Create selected variable vectors from inputs
    var_lm <- unlist(input$var_lm_inp)
    var_tree <- unlist(input$var_tree_inp)
    var_rf <- unlist(input$var_rf_inp)
    
    # Create complexity parameter sequence from inputs
    cp_seq <- seq(from = input$cp_from, to = input$cp_to, by = input$cp_by)
    
    # Create mtry parameter sequence from input
    m_try_seq <- seq(1:input$m_try)
    
    # Create preprocess argument for training
    if(input$preproc_box){
      preproc <- c("center", "scale")
    } else {
      preproc <- NULL
    }
    
    # Set RNG seed for partitioning
    set.seed(input$rng_inp)
    
    # Separate data into train and test sets at input proportion
    dataIndex <- createDataPartition(wine$quality, p = input$p_train, list = FALSE)
    dataTrain <- wine[dataIndex, ]
    dataTest <- wine[-dataIndex, ]
    
    # Set training parameters
    trainControl <- trainControl(method = "cv", number = input$k_folds)
    
    # Update progress bar
    progress$inc(0.2, detail = "Linear")
    
    # Reset seed for training linear model
    set.seed(input$rng_inp)
    # Fit regression model; conditionally include interaction terms from user input
    if(input$inter_box){
      mlr_fit <- train(quality ~ .^2, 
                       data = dataTrain[ , c("quality", var_lm)],
                       method = "lm",
                       preProcess = preproc,
                       trControl = trainControl
                       )
    } else {
      mlr_fit <- train(quality ~ ., 
                       data = dataTrain[ , c("quality", var_lm)],
                       method = "lm",
                       preProcess = preproc,
                       trControl = trainControl
                       )
    }
    
    # Update progress bar
    progress$inc(0.2, detail = "Tree")
    
    # Reset seed for training tree model
    set.seed(input$rng_inp)
    # Fit tree model
    tree_fit <- train(quality ~ ., 
                      data = dataTrain[ , c("quality", var_tree)],
                      method = "rpart",
                      preProcess = preproc,
                      trControl = trainControl,
                      tuneGrid = data.frame(cp = cp_seq)
                      )
    
    # Update progress bar
    progress$inc(0.2, detail = "Random Forest")
    
    # Reset seed for training random forest model
    set.seed(input$rng_inp)
    # Fit random forest model
    rf_fit <- train(quality ~ ., 
                    data = dataTrain[ , c("quality", var_tree)],
                    method = "rf",
                    preProcess = preproc,
                    trControl = trainControl,
                    tuneGrid = data.frame(mtry = m_try_seq)
                    )
    
    # Get fit stats from tree and rf models
    tree_trn_stats <- filter(tree_fit$results, cp == tree_fit$bestTune$cp)
    rf_trn_stats <- filter(rf_fit$results, mtry == rf_fit$bestTune$mtry)
    
    # Combine training fit stats into a dataframe
    trn_stats <- bind_rows(mlr_fit$results, tree_trn_stats, rf_trn_stats) %>%
      mutate(Model = c("linear", "tree", "random forest")) %>%
      select(Model, everything())
    
    # Render table of training fit stats
    output$train_tbl <- renderDataTable({
      datatable(trn_stats, options = list(scrollX = TRUE, pageLength = 20))
    })
    
    # Create data frame of coefficient summaries and set coefficients column
    mlr_sum_df <- summary(mlr_fit)$coefficients %>%
      as.data.frame()
    mlr_sum_df <- bind_cols(Coefficients = rownames(mlr_sum_df), mlr_sum_df)
    rownames(mlr_sum_df) <- NULL
    
    # Render table of linear model coefficients summary
    output$mlr_sum_tbl <- renderDataTable({
      datatable(mlr_sum_df, options = list(scrollX = TRUE, pageLength = 20))
    })
    
    # Render tree plot of final model
    output$tree_plot <- renderPlot(
      fancyRpartPlot(tree_fit$finalModel)
    )
    
    # Render plot of variable importance for random forest
    output$rf_imp_plot <- renderPlot(
      plot(varImp(rf_fit))
    )
    
    # Predict on each model using test set
    preds_lm <- predict(mlr_fit, newdata = dataTest)
    preds_tree <- predict(tree_fit, newdata = dataTest)
    preds_rf <- predict(rf_fit, newdata = dataTest)
    
    # Evaluate fit statistics on each model
    fit_stat_tst_lm <- postResample(preds_lm, obs = dataTest$quality)
    fit_stat_tst_tree <- postResample(preds_tree, obs = dataTest$quality)
    fit_stat_tst_rf <- postResample(preds_rf, obs = dataTest$quality)
    
    # Create tibble of fit stats on test data by model
    fit_test <- bind_cols(
      Model = c("linear", "tree", "random forest"),
      bind_rows(fit_stat_tst_lm, fit_stat_tst_tree, fit_stat_tst_rf)
    )
    
    # Find best model based on minimum RMSE
    best_model <- fit_test %>%
      filter(RMSE == min(RMSE))
    
    # Render table of fit stats on test data
    output$fit_test_tbl <- renderDataTable({
      datatable(fit_test)
    })
    
    # Render statement of best model based on lowest RMSE
    output$fit_statement <- renderText({
      paste("The 'best' model evaluated by lowest RMSE is the ", 
            best_model[, 1], " model with an RMSE of ", best_model[, 2], ".")
    })
  })
  
  
})

## Purpose

The purpose of this app is to explore data collected on Portuguese "Vinho Verde" wine, 
through graphical summaries, numerical summaries, and predictions.

## Requirements

The following packages are needed for this app:

  + `shiny`: building web apps
  + `shinydashboard`: dashboard features
  + `tidyverse`: data manipulation and piping
  + `DT`: displaying tables
  + `plotly`: displaying plots
  + `caret`: training models
  + `rpart`: tree fits
  + `randomForest`: random forest fits
  + `rattle`: tree plot

To install the packages, run the below code chunk:

```{r}
install.packages(c("shiny", "shinydashboard", "tidyverse", "DT", "plotly", "caret", "rpart", "randomForest", "rattle"))
```

## Running the app

To run the app, run the below code chunk:

```{r}
shiny::runGitHub("lrperki2/Final_Project")
```

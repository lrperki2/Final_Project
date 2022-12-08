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

# Define UI for application that draws a histogram
dashboardPage(skin = "red",
              
              # Define header
              dashboardHeader(title = "Exploring Portugese Vinho Verde wine", titleWidth = 1000),
              
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
                            h1("PLACEHOLDER2")
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

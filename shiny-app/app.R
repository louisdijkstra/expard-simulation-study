#' Shiny App for the KiKme dataset 

rm(list = ls())

# Load the libraries ---
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(readr)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(gtable)
library(gridExtra)
library(grid)

# Get all the parameter settings from the files in the data/ folder ---
cat("Getting parameter settings and loading the data from the data/ folder\n")
source("loadData.R")

# Load the individual tabs ---
source("tabMain.R")
source("tabConfusionMatrix.R")

# Define UI ----
ui <- fluidPage(
  tagList(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  )),
  uiOutput("UIafterLogin")
)

# Define server logic ----
server <- function(input, output, session) {
  
  # Define the user --- 
  USER <- reactiveValues(Logged = TRUE , session = session$user) 

  # Set-up general structure of the tabs ---
  output$UIafterLogin <- renderUI({
    if (USER$Logged) {
      navbarPage("Exposure Models",
             tabMain, 
             tabConfusionMatrix)
    }
  })
  
  # Create confusion matrix plot --------------------------
  
  output$confusionMatrixPlot <- renderPlot({
    
    p <- parameter_settings %>% dplyr::filter(
      prob_exposed == input$prob_exposed, 
      min_chance == input$min_chance,
      max_chance == input$max_chance
    )
    
    p$plots[[1]]
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
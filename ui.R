library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")  # Ajoutez cette ligne
  ),
  
  
  
  titlePanel(
    div(
      img(src = "insep.png", height = 130), # Spécifie uniquement la hauteur
      "Optojump Tapping et CMJ", 
      windowTitle = "Optojump visualisation"
    )
  ),
  
  tabsetPanel(
    tabPanel(tags$span("CMJ"), CMJ_UI("CMJ_module")),
    tabPanel(tags$span("Visualisation"), Visualisation_UI("Visualisation_module")),
    id = "myTabsetPanel" # Add an id to the tabsetPanel for CSS targeting
  )
  
)
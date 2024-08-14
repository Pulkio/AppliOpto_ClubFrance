# Chargement des bibliothèques nécessaires
library(shiny)
library(dplyr)
library(plotly)

# Chargement des bibliothèques nécessaires
library(shiny)
library(dplyr)
library(plotly)

# Définition de l'interface utilisateur (UI) avec la fonction Visualisation_UI()
Visualisation_UI <- function(id) {
  # Création d'un espace de noms pour cette fonction UI avec la fonction NS()
  ns <- NS(id)
  
  # Utilisation de tagList() pour lister les éléments de l'interface utilisateur
  tagList(
    # Ajout d'une balise CSS externe pour le style
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    
    # Création d'une ligne fluide (fluidRow) avec deux colonnes (column)
    fluidRow(
      # Première colonne : largeur 6, alignement centré, titre de niveau 4 et saut de ligne
      column(width = 6, align = "center",
             br(),
             h4("TEST CMJ"),
             # Deuxième colonne :
             # - Champ pour uploader un fichier CSV (fileInput)
             # - Saut de ligne (br())
             # - Sélecteur multiple pour les catégories (selectInput)
             # - Bouton d'action pour soumettre les informations (actionButton)
             fileInput(ns("csv_file"), "Uploader un fichier CSV"),
             br(),
             selectInput(ns("categorie_select"), "Sélectionner les catégories", choices = c("EcoleAthle", "Poussin", "Benjamins", "Minimes", "Cadets", "Juniors", "Espoirs", "Seniors", "Masters"), multiple = TRUE),
             br(),
             actionButton(ns("submit_button"), label = "Soumettre")
      ),
      # Deuxième colonne : largeur 6, alignement centré, espace réservé pour afficher les résultats du test (htmlOutput)
      column(width = 6, align = "center",
             br(),
             htmlOutput(ns("results_text")),
             textOutput(ns("record_text")),
             verbatimTextOutput(ns("average_performance_text")),  # Utilisation de verbatimTextOutput pour afficher le texte brut
             verbatimTextOutput(ns("average_performance_group_text"))
      )
    ),
    # Ajout de tabsetPanel pour créer les onglets
    tabsetPanel(
      id = ns("tabs"),
      tabPanel("Homme", plotlyOutput(ns("bar_chart_homme"), width = "100%", height = "600px")),
      tabPanel("Femme", plotlyOutput(ns("bar_chart_femme"), width = "100%", height = "600px")),
      tabPanel("Non binaire", plotlyOutput(ns("bar_chart_nb"), width = "100%", height = "600px"))
    )
  )
}




# Server module
Visualisation_Server <- function(input, output, session) {
  ns <- session$ns
  isDataProcessed <- reactiveVal(NULL)
  record_visiteur <- reactiveVal(NULL)
  deux_visiteur <- reactiveVal(NULL)
  trois_visiteur <- reactiveVal(NULL)
  
  observeEvent(input$submit_button, {
    req(input$csv_file, input$categorie_select)
    
    # Lire les données du fichier CSV
    csv_file_path <- input$csv_file$datapath
    data <- read.csv(csv_file_path, stringsAsFactors = FALSE)
    
    # Filtrer les données en fonction des catégories sélectionnées
    data <- data %>% filter(Categorie %in% input$categorie_select)
    
    # Récupérer le "Nom" de la dernière ligne
    dernier_nom <- data[nrow(data), "Nom"]
    
    # Sélectionner la ligne avec la hauteur maximale pour chaque Nom
    result <- data %>%
      group_by(Nom) %>%
      slice(which.max(Hauteur))
    
    # Afficher le résultat
    print(result)
    
    # Mettre à jour les graphiques pour chaque onglet
    output$bar_chart_homme <- renderPlotly({
      # Filtrer les données pour les hommes
      data_homme <- result %>% filter(Sexe == "Homme")
      swarm_plot_colored_visualisation(data_homme, dernier_nom)
    })
    
    output$bar_chart_femme <- renderPlotly({
      # Filtrer les données pour les femmes
      data_femme <- result %>% filter(Sexe == "Femme")
      swarm_plot_colored_visualisation(data_femme, dernier_nom)
    })
    
    output$bar_chart_nb <- renderPlotly({
      # Filtrer les données pour les personnes non-binaires
      data_nb <- result %>% filter(Sexe == "Non binaire")
      swarm_plot_colored_visualisation(data_nb, dernier_nom)
    })
    
    observeEvent(input$tabs, {
      req(input$csv_file)
      # Lire les données du fichier CSV
      csv_file_path <- input$csv_file$datapath
      data <- read.csv(csv_file_path, stringsAsFactors = FALSE)
      
      # Filtrer les données en fonction des catégories sélectionnées
      data <- data %>% filter(Categorie %in% input$categorie_select)
      
      # Sélectionner la ligne avec la hauteur maximale pour chaque Nom
      result <- data
      
      selected_sexe <- switch(input$tabs,
                              "Homme" = "Homme",
                              "Femme" = "Femme",
                              "Non binaire" = "Non binaire")
      
      record <- result %>% filter(Sexe == selected_sexe) %>% summarise(Record = max(Hauteur, na.rm = TRUE))
      
      output$record_text <- renderText({
        paste("Record pour", selected_sexe, ":", record$Record, "cm")
      })
    })
    
    
    # Calculer et afficher le classement de la moyenne des performances par groupe de disciplines
    # Calculer et afficher le classement de la moyenne des performances par groupe de disciplines
    output$average_performance_text <- renderText({
      if (!is.null(input$csv_file) && !is.null(input$categorie_select)) {
        csv_file_path <- input$csv_file$datapath
        data <- read.csv(csv_file_path, stringsAsFactors = FALSE)
        
        # Filtrer les données en fonction des catégories sélectionnées
        data <- data %>% filter(Categorie %in% input$categorie_select)
        
        selected_sexe <- switch(input$tabs,
                                "Homme" = "Homme",
                                "Femme" = "Femme",
                                "Non binaire" = "Non binaire")
        
        # Filtrer les données par sexe sélectionné
        data <- data %>% filter(Sexe == selected_sexe)
        
        # Sélectionner la meilleure performance pour chaque athlète
        best_performances <- data %>%
          group_by(Nom, Categorie) %>%
          summarise(Best_Hauteur = max(Hauteur, na.rm = TRUE), .groups = 'drop')
        
        # Calculer la moyenne des meilleures performances par catégorie
        avg_performances <- best_performances %>%
          group_by(Categorie) %>%
          summarise(Average = mean(Best_Hauteur, na.rm = TRUE), .groups = 'drop')
        
        # Trier par ordre décroissant de moyenne
        avg_performances <- avg_performances[order(-avg_performances$Average), ]
        
        # Concaténer chaque paire catégorie et moyenne de performance dans une variable
        avg_performance_text <- paste(avg_performances$Categorie, ": ", round(avg_performances$Average, 2), " cm")
        
        # Retourner le texte concaténé
        avg_performance_text <- paste(avg_performance_text, collapse = "\n")
        
        return(avg_performance_text)
      }
    })
    
    
    
    
    
    # Calculer et afficher le classement de la moyenne des performances par groupe de disciplines
    output$average_performance_group_text <- renderText({
      if (!is.null(input$csv_file) && !is.null(input$categorie_select)) {
        csv_file_path <- input$csv_file$datapath
        data <- read.csv(csv_file_path, stringsAsFactors = FALSE)
        
        # Filtrer les données en fonction des catégories sélectionnées
        data <- data %>% filter(Categorie %in% input$categorie_select)
        
        selected_sexe <- switch(input$tabs,
                                "Homme" = "Homme",
                                "Femme" = "Femme",
                                "Non binaire" = "Non binaire")
        
        # Filtrer les données par sexe sélectionné
        data <- data %>% filter(Sexe == selected_sexe)
        
        # Sélectionner la meilleure performance pour chaque athlète
        best_performances <- data %>%
          group_by(Nom, Discipline) %>%
          summarise(Best_Hauteur = max(Hauteur, na.rm = TRUE), .groups = 'drop')
        
        # Calculer la moyenne des meilleures performances par catégorie
        avg_performances <- best_performances %>%
          group_by(Discipline) %>%
          summarise(Average = mean(Best_Hauteur, na.rm = TRUE), .groups = 'drop')
        
        # Trier par ordre décroissant de moyenne
        avg_performances <- avg_performances[order(-avg_performances$Average), ]
        
        # Concaténer chaque paire catégorie et moyenne de performance dans une variable
        average_performance_group_text <- paste(avg_performances$Discipline, ": ", round(avg_performances$Average, 2), " cm")
        
        # Retourner le texte concaténé
        average_performance_group_text <- paste(average_performance_group_text, collapse = "\n")
        
        return(average_performance_group_text)
      }
    })
    
    
    
  })
}




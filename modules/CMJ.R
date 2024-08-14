# Chargement des bibliothèques nécessaires
library(shiny)
library(dplyr)
library(plotly)

# Définition de l'interface utilisateur (UI) avec la fonction CMJ_UI()
CMJ_UI <- function(id) {
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
             # - Champ pour uploader un fichier XML (fileInput)
             # - Saut de ligne (br())
             # - Champ de saisie de texte pour le nom de l'athlète (textInput)
             # - Liste déroulante pour sélectionner le genre (selectInput)
             # - Bouton d'action pour soumettre les informations (actionButton)
             fileInput(ns("xml_file"), "Uploader un fichier XML"),
             br(),
             textInput(ns("athlete_name"), label = "Nom de l'athlète"),
             selectInput(ns("gender_select"), label = "Genre", choices = c("Homme", "Femme", "Non binaire")),
             textInput(ns("sport"), label = "Sport"), 
             selectInput(ns("Niveau_sportif"), label = "Niveau_sportif", choices = c("Occasionnel(1-2h)", "Régulier(3-5h)", "Intensif(6-12h)", "Compétitif(11-15h)", "Élite(+15h)")),
             selectInput(ns("categorie_select"), label = "Catégorie", choices = c("EcoleAthle(2015+)", "Poussin(2013)", "Benjamins(2011)", "Minimes(2009)", "Cadets(2007)", "Juniors(2005)", "Espoirs(2002)", "Seniors(1990)", "Masters(1989-)")),
             actionButton(ns("submit_button"), label = "Soumettre")
      ),
      # Deuxième colonne : largeur 6, alignement centré, espace réservé pour afficher les résultats du test (htmlOutput)
      column(width = 6, align = "center",
             br(),
             htmlOutput(ns("results_text")),
             textOutput(ns("record_text"))
      )
    ),
    # Création d'une deuxième ligne fluide (fluidRow) avec une colonne (column)
    fluidRow(
      # Colonne : largeur 12, alignement centré, espace réservé pour le graphique en barres (plotlyOutput)
      column(width = 12, align = "center",
             br(),
             plotlyOutput(ns("bar_chart"), width = "100%", height = "500px")
      )
    )
  )
}


# Server module
CMJ_Server <- function(input, output, session) {
  ns <- session$ns
  isDataProcessed <- reactiveVal(NULL)
  record_visiteur <- reactiveVal(NULL)
  deux_visiteur <- reactiveVal(NULL)
  trois_visiteur <- reactiveVal(NULL)
  
  observeEvent(input$submit_button, {
    req(input$xml_file, input$athlete_name, input$gender_select)
    
    # Exemple d'utilisation
    xml_file <- input$xml_file$datapath
    columns_to_extract <- c("Elevation")
    
    # Résultats en colonnes
    result <- parse_xml_file(xml_file, columns_to_extract)
    
    if (!is.null(result) && length(result) > 0) {
      result <- as.data.frame(t(result))
      # Définir les noms de colonnes avec la première ligne
      names(result) <- unlist(result[1, ])
      # Supprimer la première ligne car elle est maintenant utilisée comme noms de colonnes
      result <- result[-1, ]
      result <- as.numeric(result)
      
      Nom <- input$athlete_name
      Sexe <- input$gender_select
      Sport <- input$sport
      Niveau <- input$Niveau_sportif
      Categorie <- input$categorie_select
      
      # Trier les valeurs de result en ordre décroissant
      sorted_result <- sort(result, decreasing = TRUE)
      
      # Obtenir les 3 plus grandes valeurs si il y a 3 valeurs ou plus,
      # les 2 plus grandes valeurs si il y a 2 valeurs,
      # et la valeur maximale si il y a une seule valeur
      top_values <- head(sorted_result, min(length(sorted_result), 3))
      
      # Créer un dataframe avec les données
      nouvelle_ligne <- data.frame(Nom = rep(Nom, length(top_values)),
                                   Hauteur = top_values,
                                   Sexe = rep(Sexe, length(top_values)),
                                   Sport = rep(Sport, length(top_values)),
                                   Niveau = rep(Niveau, length(top_values)),
                                   Categorie = rep(Categorie, length(top_values)))
      
      # Filtrer les lignes avec une hauteur inférieure à 150
      nouvelle_ligne <- nouvelle_ligne %>% filter(Hauteur < 150)
    }
    
    
    # Vérification si le fichier TXT existe
    file_path <- "results_globaux_CMJ.txt"
    if (!file.exists(file_path)) {
      # Si le fichier n'existe pas, créer une ligne de texte
      ligne_texte <- apply(nouvelle_ligne, 1, function(row) paste(row, collapse = ","))
      writeLines(ligne_texte, con = file_path)
    } else {
      # Si le fichier existe, lire les données actuelles
      donnees_existantes <- readLines(file_path)
      
      # Vérifier si le nom de l'athlète existe déjà
      noms_existants <- sapply(donnees_existantes, function(line) strsplit(line, ",")[[1]][2])
      if (Nom %in% noms_existants) {
        # Trouver un nom unique en ajoutant un numéro
        num_suffix <- 1
        while (paste0(Nom, num_suffix) %in% noms_existants) {
          num_suffix <- num_suffix + 1
        }
        Nom <- paste0(Nom, num_suffix)
      }
      
      # Ajouter la nouvelle ligne avec le nom mis à jour
      nouvelle_ligne$Nom <- Nom
      # Créer une nouvelle ligne de texte
      ligne_texte <- apply(nouvelle_ligne, 1, function(row) paste(row, collapse = ","))
      
      # Ajouter la nouvelle ligne au fichier existant
      writeLines(c(donnees_existantes, ligne_texte), con = file_path)
      isDataProcessed(TRUE)
    }
  })
  
  
  
  output$bar_chart <- renderPlotly({
    req(input$xml_file, input$athlete_name, input$gender_select, isDataProcessed())
    
    file_path <- "results_globaux_CMJ.txt"
    if (file.exists(file_path)) {
      # Lire les données du fichier texte avec des colonnes spécifiées
      donnees <- read.table(file_path, sep = ",", header = FALSE, stringsAsFactors = FALSE,
                            col.names = c("Nom", "Hauteur", "Sexe", "Sport", "Niveau", "Categorie"))
      
      print(donnees)  # Affiche les données pour le débogage
      
      if (!is.null(donnees) && nrow(donnees) > 0) {
        # Filtrer les données pour l'athlète actuel
        n <- nrow(donnees)
        last_name <- donnees[n, "Nom"]
        athlete_data <- NULL
        for (i in seq(n, 1, -1)) {
          if (donnees[i, "Nom"] == last_name) {
            athlete_data <- rbind(athlete_data, donnees[i, ])
          } else {
            break
          }
        }
        
        if (nrow(athlete_data) > 0) {
          sorted_heights <- sort(as.numeric(athlete_data$Hauteur), decreasing = TRUE)
          record_visiteur(sorted_heights[1])
          
          if (length(sorted_heights) >= 2) {
            deux_visiteur(sorted_heights[2])
          }
          
          if (length(sorted_heights) >= 3) {
            trois_visiteur(sorted_heights[3])
          }
          
          p <- swarm_plot_colored(donnees, input$athlete_name, "Hauteur")
          return(p)
        }
      } else {
        print("Le fichier TXT est vide.")
      }
    } else {
      print("Le fichier TXT n'existe pas.")
    }
  })
  
  
}


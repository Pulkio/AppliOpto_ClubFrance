# Chargement des bibliothèques nécessaires
library(shiny)
library(dplyr)
library(plotly)
library(uuid)

CMJ_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(includeCSS("www/style.css")),
    
    fluidRow(
      column(width = 6, align = "center",
             br(),
             fileInput(ns("xml_file"), "Uploader un fichier XML"),
             br(),
             
             # Première ligne : Sport et Niveau sportif
             fluidRow(
               column(width = 6, textInput(ns("sport"), label = "Sport")),
               column(width = 6, selectInput(ns("Niveau_sportif"), label = "Niveau sportif", 
                                             choices = c("Occasionnel(1-2h)", "Régulier(3-5h)", "Intensif(6-12h)", 
                                                         "Compétitif(11-15h)", "Élite(+15h)")))
             ),
             
             # Deuxième ligne : Âge et Sexe
             fluidRow(
               column(width = 6, numericInput(ns("age"), label = "Âge", value = 20, min = 0, max = 120)),
               column(width = 6, selectInput(ns("sexe"), label = "Sexe", choices = c("Homme", "Femme", "Non binaire")))
             ),
             
             # Ligne pour le bouton Soumettre
             fluidRow(
               column(width = 12, align = "center",
                      actionButton(ns("submit_button"), label = "Soumettre")
               )
             )
      ),
      
      # Ajout d'une colonne pour afficher le record et les moyennes
      column(width = 6, align = "center",
             br(),
             htmlOutput(ns("results_text")),
             # Affichage du record du participant avec une classe pour le texte en gras
             div(textOutput(ns("votre_record")), class = "performance"),
             
             # Affichage du record Homme
             div(textOutput(ns("record_homme")), class = "record-homme"),
             
             # Affichage du record Femme
             div(textOutput(ns("record_femme")), class = "record-femme"),
             
             # Affichage de la moyenne pour les hommes
             div(textOutput(ns("mean_male")), class = "mean-homme"),
             
             # Affichage de la moyenne pour les femmes
             div(textOutput(ns("mean_female")), class = "mean-femme")
      )
    ),
    
    fluidRow(
      column(width = 12, align = "center",
             br(),
             plotlyOutput(ns("bar_chart"), width = "100%", height = "500px")
      )
    )
  )
}




# Fonction pour déterminer la catégorie en fonction de l'année de naissance
get_category_from_year <- function(year_of_birth) {
  current_year <- 2024
  age <- current_year - year_of_birth
  
  if (age >= 85) {
    return("Master10")
  } else if (age >= 80) {
    return("Master9")
  } else if (age >= 75) {
    return("Master8")
  } else if (age >= 70) {
    return("Master7")
  } else if (age >= 65) {
    return("Master6")
  } else if (age >= 60) {
    return("Master5")
  } else if (age >= 55) {
    return("Master4")
  } else if (age >= 50) {
    return("Master3")
  } else if (age >= 45) {
    return("Master2")
  } else if (age >= 40) {
    return("Master1")
  } else if (age >= 23) {
    return("Seniors")
  } else if (age >= 20) {
    return("Espoirs")
  } else if (age >= 18) {
    return("Juniors")
  } else if (age >= 16) {
    return("Cadets")
  } else if (age >= 14) {
    return("Minimes")
  } else if (age >= 12) {
    return("Benjamins")
  } else if (age >= 10) {
    return("Poussin")
  } else {
    return("EcoleAthle")
  }
}

# Server module
CMJ_Server <- function(input, output, session) {
  ns <- session$ns
  isDataProcessed <- reactiveVal(NULL)
  record_visiteur <- reactiveVal(NULL)
  deux_visiteur <- reactiveVal(NULL)
  trois_visiteur <- reactiveVal(NULL)
  
  observeEvent(input$submit_button, {
    req(input$xml_file, input$age)
    
    xml_file <- input$xml_file$datapath
    columns_to_extract <- c("Elevation")
    
    result <- parse_xml_file(xml_file, columns_to_extract)
    
    if (!is.null(result) && length(result) > 0) {
      result <- as.data.frame(t(result))
      names(result) <- unlist(result[1, ])
      result <- result[-1, ]
      result <- as.numeric(result)
      
      AthleteID <- UUIDgenerate()
      Sport <- input$sport
      Niveau <- input$Niveau_sportif
      Sexe <- input$sexe
      YearOfBirth <- 2024 - input$age
      Categorie <- get_category_from_year(YearOfBirth)
      
      sorted_result <- sort(result, decreasing = TRUE)
      top_values <- head(sorted_result, min(length(sorted_result), 3))
      
      nouvelle_ligne <- data.frame(AthleteID = rep(AthleteID, length(top_values)),
                                   Hauteur = top_values,
                                   Sport = rep(Sport, length(top_values)),
                                   Niveau = rep(Niveau, length(top_values)),
                                   Sexe = rep(Sexe, length(top_values)),
                                   YearOfBirth = rep(YearOfBirth, length(top_values)),
                                   Categorie = rep(Categorie, length(top_values)))
      
      
      print(nouvelle_ligne)
      
      nouvelle_ligne <- nouvelle_ligne %>% filter(Hauteur < 150)
    }
    
    file_path <- "results_globaux_CMJ.txt"
    if (!file.exists(file_path)) {
      ligne_texte <- apply(nouvelle_ligne, 1, function(row) paste(row, collapse = ","))
      writeLines(ligne_texte, con = file_path)
    } else {
      donnees_existantes <- readLines(file_path)
      
      athlete_ids <- sapply(donnees_existantes, function(line) strsplit(line, ",")[[1]][1])
      while (AthleteID %in% athlete_ids) {
        AthleteID <- UUIDgenerate()
      }
      
      nouvelle_ligne$AthleteID <- AthleteID
      ligne_texte <- apply(nouvelle_ligne, 1, function(row) paste(row, collapse = ","))
      
      writeLines(c(donnees_existantes, ligne_texte), con = file_path)
      isDataProcessed(TRUE)
    }
  })
  
  output$bar_chart <- renderPlotly({
    req(input$xml_file, input$age, isDataProcessed())
    
    file_path <- "results_globaux_CMJ.txt"
    if (file.exists(file_path)) {
      donnees <- read.table(file_path, sep = ",", header = FALSE, stringsAsFactors = FALSE,
                            col.names = c("AthleteID", "Hauteur", "Sport", "Niveau", "Sexe", "YearOfBirth", "Categorie"))
      
      if (!is.null(donnees) && nrow(donnees) > 0) {
        n <- nrow(donnees)
        last_id <- donnees[n, "AthleteID"]
        
        
        
        
        # Record maximum
        max_record <- max(as.numeric(donnees$Hauteur), na.rm = TRUE)
        
        
        # Sélectionner la meilleure hauteur pour chaque AthleteID
        meilleures_hauteurs <- donnees %>%
          group_by(AthleteID, Sexe) %>%
          summarise(Hauteur = max(as.numeric(Hauteur), na.rm = TRUE))
        
        print(meilleures_hauteurs)
        
        # Moyenne pour les hommes
        mean_male <- mean(as.numeric(meilleures_hauteurs$Hauteur[meilleures_hauteurs$Sexe == "Homme"]), na.rm = TRUE)
        
        # Moyenne pour les femmes
        mean_female <- mean(as.numeric(meilleures_hauteurs$Hauteur[meilleures_hauteurs$Sexe == "Femme"]), na.rm = TRUE)
        
        
        # Moyennes
        # mean_male <- mean(as.numeric(donnees$Hauteur[donnees$Sexe == "Homme"]), na.rm = TRUE)
        # mean_female <- mean(as.numeric(donnees$Hauteur[donnees$Sexe == "Femme"]), na.rm = TRUE)
        
        output$record_homme <- renderText({
          paste("Record Homme :", max(as.numeric(donnees$Hauteur[donnees$Sexe == "Homme"]), na.rm = TRUE))
        })
        
        output$record_femme <- renderText({
          paste("Record Femme :", max(as.numeric(donnees$Hauteur[donnees$Sexe == "Femme"]), na.rm = TRUE))
        })
        
        output$mean_male <- renderText({
          paste("Moyenne Homme:", round(mean_male, 2))
        })
        
        output$mean_female <- renderText({
          paste("Moyenne Femme:", round(mean_female, 2))
        })
        
        
        
        
        
        athlete_data <- NULL
        for (i in seq(n, 1, -1)) {
          if (donnees[i, "AthleteID"] == last_id) {
            athlete_data <- rbind(athlete_data, donnees[i, ])
          } else {
            break
          }
        }
        
        if (nrow(athlete_data) > 0) {
          sorted_heights <- sort(as.numeric(athlete_data$Hauteur), decreasing = TRUE)
          record_visiteur(sorted_heights[1])
          
          output$votre_record <- renderText({
            paste("Votre performance:", record_visiteur())
          })
          
          if (length(sorted_heights) >= 2) {
            deux_visiteur(sorted_heights[2])
          }
          
          if (length(sorted_heights) >= 3) {
            trois_visiteur(sorted_heights[3])
          }
          
          p <- swarm_plot_colored(donnees, last_id)
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

# Chargement des bibliothèques nécessaires
library(shiny)
library(dplyr)
library(plotly)


# Définition de l'interface utilisateur (UI) avec la fonction Visualisation_UI()
Visualisation_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    
    fluidRow(
      column(width = 6, align = "center",
             br(),
             h4("TEST CMJ"),
             fileInput(ns("txt_file"), "Uploader un fichier txt"),
             br(),
             selectInput(ns("categorie_select"), "Sélectionner les catégories", choices = c("EcoleAthle", "Poussin", "Benjamins", "Minimes", "Cadets", "Juniors", "Espoirs", "Seniors", "Master1", "Master2", "Master3", "Master4", "Master5", "Master6", "Master7", "Master8", "Master9", "Master10"), multiple = TRUE),
             br(),
             selectInput(ns("Niveau_sportif"), "Niveau sportif", choices = c("Occasionnel(1-2h)", "Régulier(3-5h)", "Intensif(6-12h)", "Compétitif(11-15h)", "Élite(+15h)"), multiple = TRUE),
             br(),
             actionButton(ns("submit_button"), label = "Soumettre")
      ),
      column(width = 6, align = "center",
             br(),
             h4("Records"),
             div(style = "font-size: 24px;",
                 textOutput(ns("record_homme")),
                 textOutput(ns("record_femme")),
                 textOutput(ns("record_non_binaire"))
             )
      )
    ),
    
    fluidRow(
      column(width = 12,
             plotlyOutput(ns("swarm_plot_colored"))
      )
    )
  )
}





Visualisation_Server <- function(input, output, session) {
  ns <- session$ns
  
  # Réactive pour stocker les données filtrées après clic sur le bouton
  filtered_data <- reactiveVal(NULL)
  
  observeEvent(input$submit_button, {
    req(input$txt_file, input$categorie_select, input$Niveau_sportif)
    
    # Lire les données du fichier texte
    donnees <- read.table(input$txt_file$datapath, sep = ",", header = FALSE, stringsAsFactors = FALSE,
                          col.names = c("AthleteID", "Hauteur", "Sport", "Niveau", "Sexe", "YearOfBirth", "Categorie"))
  
    
    
    donnees$Nom <- trimws(donnees$AthleteID)
    donnees$Hauteur <- trimws(donnees$Hauteur)
    donnees$Sexe <- trimws(donnees$Sexe)
    donnees$Sport <- trimws(donnees$Sport)
    donnees$Niveau <- trimws(donnees$Niveau)
    donnees$Categorie <- trimws(donnees$Categorie)
    
    # # Nettoyer les données en supprimant les espaces
    # donnees$Sexe <- trimws(donnees$Sexe)
    
    # Filtrer les données en fonction des sélections
    donnees_filtrees <- donnees %>%
      filter(Categorie %in% input$categorie_select, Niveau %in% input$Niveau_sportif)
    
    # Mettre à jour les données filtrées
    filtered_data(donnees_filtrees)
  })
  
  
  output$record_homme <- renderText({
    req(filtered_data())  # S'assure que filtered_data() est disponible
    dataframe <- filtered_data()
    
    # Extraire la hauteur maximale pour les hommes
    record_homme <- max(dataframe$Hauteur[dataframe$Sexe == "Homme"], na.rm = TRUE)
    paste("Record Homme ️:", record_homme)
  })
  
  output$record_femme <- renderText({
    req(filtered_data())  # S'assure que filtered_data() est disponible
    dataframe <- filtered_data()
    
    # Extraire la hauteur maximale pour les femmes
    record_femme <- max(dataframe$Hauteur[dataframe$Sexe == "Femme"], na.rm = TRUE)
    paste("Record Femme :", record_femme)
  })
  
  output$record_non_binaire <- renderText({
    req(filtered_data())  # S'assure que filtered_data() est disponible
    dataframe <- filtered_data()
    
    # Extraire la hauteur maximale pour les personnes non-binaires
    record_non_binaire <- max(dataframe$Hauteur[dataframe$Sexe == "Non binaire"], na.rm = TRUE)
    paste("Record Non Binaire :", record_non_binaire)
  })
  
  
  
  output$swarm_plot_colored <- renderPlotly({
    
    req(filtered_data())  # S'assure que filtered_data() est disponible
    
    dataframe <- filtered_data()
    
    colonne <- "Hauteur"
    
    
    # Regroupement des données par Nom et Sexe, et garde la valeur maximale de la colonne spécifiée
    dataframe <- dataframe %>%
      group_by(Nom, Sexe) %>%
      slice_max(order_by = !!rlang::sym(colonne), n = 1) %>%
      ungroup() %>%
      arrange(Nom)
  
    
    dataframe <- dataframe %>%
      mutate(couleur = case_when(
        Sexe == "Homme" ~ "Homme",
        Sexe == "Femme" ~ "Femme",
        Sexe == "Non binaire" ~ "Non binaire",
        TRUE ~ NA_character_  # Valeur par défaut si aucune condition n'est remplie
      ))
    
    # Vérifier et convertir la colonne spécifiée en numérique si nécessaire
    if (!is.numeric(dataframe[[colonne]])) {
      dataframe[[colonne]] <- as.numeric(dataframe[[colonne]])
    }
    
    # Arrondi des valeurs de hauteur à la première décimale
    dataframe[[colonne]] <- round(dataframe[[colonne]], 1)
    
    # Filtrage des valeurs et calcul des moyennes
    moy_hauteur_femme <- round(mean(dataframe$Hauteur[dataframe$Sexe == "Femme"], na.rm = TRUE), 1)
    moy_hauteur_homme <- round(mean(dataframe$Hauteur[dataframe$Sexe == "Homme"], na.rm = TRUE), 1)
    moy_hauteur_non_binaire <- round(mean(dataframe$Hauteur[dataframe$Sexe == "Non binaire"], na.rm = TRUE), 1)
    
    # Création du swarm plot avec ggplot
    p <- ggplot() +
      geom_point(
        data = dataframe,
        aes(
          x = Sexe,
          y = !!rlang::sym(colonne),
          fill = couleur,
          text = paste(colonne, ": ", round(!!rlang::sym(colonne), 1))
        ),
        alpha = 0.8,
        size = 3,
        shape = 21,
        color = "black",
        position = position_jitter(height = 0)
      ) +
      scale_fill_manual(
        values = c(
          "Homme" = "#2C2F65",
          "Femme" = "#C5243D",
          "Non binaire" = "green"
        ),
        labels = c(
          "Homme" = "Hommes",
          "Femme" = "Femmes",
          "Non binaire" = "Non binaires"
        )
      ) +
      
      geom_hline(
        aes(
          yintercept = moy_hauteur_femme,
          linetype = "Moyenne Femme",
          text = paste("Moyenne Femme: ", moy_hauteur_femme)
        ),
        color = "#C5243D"
      ) +
      
      geom_hline(
        aes(
          yintercept = moy_hauteur_homme,
          linetype = "Moyenne Homme",
          text = paste("Moyenne Homme: ", moy_hauteur_homme)
        ),
        color = "#2C2F65"
      ) +
      
      geom_hline(
        aes(
          yintercept = moy_hauteur_non_binaire,
          linetype = "Moyenne Non binaire",
          text = paste("Moyenne Non binaire: ", moy_hauteur_non_binaire)
        ),
        color = "green"
      ) +
      
      scale_linetype_manual(values = c("Moyenne Femme" = "dashed", "Moyenne Homme" = "dashed", "Moyenne Non binaire" = "dashed")) +
      
      geom_text_repel(
        data = dataframe,
        aes(
          x = Sexe,
          y = !!rlang::sym(colonne),
          label = round(!!rlang::sym(colonne), 1)
        ),
        vjust = -1.5
      ) +
      
      labs(
        x = "Sexe",
        y = "Hauteur du saut",
        title = "Résultats sauts CMJ",
        caption = "Survolez la ligne pour voir la hauteur"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Convertir le graphique ggplot en plotly pour l'interactivité
    p <- ggplotly(p, tooltip = c("text"))
    
    # Modifier les étiquettes de la légende manuellement
    for (i in seq_along(p$x$data)) {
      if (p$x$data[[i]]$name == "(Homme,1)") {
        p$x$data[[i]]$name <- "Score Hommes"
      } else if (p$x$data[[i]]$name == "(Femme,1)") {
        p$x$data[[i]]$name <- "Score Femmes"
      } else if (p$x$data[[i]]$name == "(Non binaire,1)") {
        p$x$data[[i]]$name <- "Score Non binaires"
      } else if (p$x$data[[i]]$name == "(Moyenne Femme,1)") {
        p$x$data[[i]]$name <- "Moyenne Femme"
      } else if (p$x$data[[i]]$name == "(Moyenne Homme,1)") {
        p$x$data[[i]]$name <- "Moyenne Homme"
      } else if (p$x$data[[i]]$name == "(Moyenne Non binaire,1)") {
        p$x$data[[i]]$name <- "Moyenne Non binaire"
      }
    }
    
    # Appliquer la mise en page pour modifier le titre de la légende et centrer le titre du plot
    p <- p %>%
      layout(
        legend = list(
          title = list(text = "Légende",
                       font = list(size = 14)),
          y = 0.5,
          yanchor = "middle"
        ),
        title = list(
          text = "Résultats sauts CMJ",
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # Affichage du graphique
    print(p)
    
    
    
    
    
    
    
    
  })
}






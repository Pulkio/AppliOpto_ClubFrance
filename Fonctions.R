# Chargement des bibliothèques nécessaires
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(flexdashboard)
library(plotly)
library(DT)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(forcats)





# Fonction pour créer des dataframes à partir de fichiers XML
parse_xml_file <- function(xml_file, columns_to_extract) {
  # Lecture du fichier XML ligne par ligne
  xml_lines <- read_lines(xml_file)
  
  # Initialisation d'une liste pour stocker les lignes entre les balises Row
  rows_list <- list()
  
  # Initialisation d'un vecteur pour stocker les lignes entre les balises Row
  current_row <- c()
  
  # Boucle pour lire le fichier XML ligne par ligne
  for (line in xml_lines) {
    # Vérification si la ligne contient "<Row>"
    if (grepl("<Row>", line)) {
      # Si la ligne contient "<Row>", initialisation d'un nouveau vecteur pour stocker les lignes entre les balises Row
      current_row <- c()
    } else if (grepl("</Row>", line)) {
      # Si la ligne contient "</Row>", ajout du vecteur courant à la liste et réinitialisation du vecteur courant
      rows_list <- c(rows_list, list(current_row))
      current_row <- c()
    } else {
      # Si la ligne ne contient ni "<Row>" ni "</Row>", ajout de la ligne au vecteur courant
      current_row <- c(current_row, line)
    }
  }
  
  # Filtrage des lignes de chaque liste de liste
  filtered_rows_list <- lapply(rows_list[1], function(row) {
    row[!grepl("</Cell>|<Cell>", row)]
  })
  
  # Fonction pour trouver les numéros de lignes correspondant à chaque mot clé dans la première liste de la liste de liste
  find_row_numbers <- function(keywords, filt_rows_list) {
    # Initialisation d'une liste pour stocker les numéros de lignes correspondant à chaque mot clé
    row_numbers <- list()
    
    # Parcours de chaque mot clé
    for (keyword in keywords) {
      # Initialisation d'un vecteur pour stocker les numéros de lignes correspondant au mot clé courant
      current_row_numbers <- c()
      
      # Parcours de chaque ligne de la liste
      for (i in 1:length(filt_rows_list[[1]])) {
        # Vérification si le mot clé courant est un mot entier dans la ligne
        if (any(str_detect(filt_rows_list[[1]][i], fixed(keyword)))){
          valeur <-
            gsub(
              "<Data ss:Type=\"String\">|<Data ss:Type=\"Number\">|</Data>",
              "",
              filt_rows_list[[1]][i]
            )
          valeur <- gsub("\\s+", "", valeur) #Supprime les espaces
          keyword <- gsub("\\s+", "", keyword) #Supprime les espaces
          
          #Vérifie si le mot est vraiment identique
          if (nchar(valeur) == nchar(keyword)) {
            current_row_numbers <- c(current_row_numbers, i)
          }
        }
      }
      
      # Ajout des numéros de lignes correspondant au mot clé courant à la liste principale
      row_numbers <- c(row_numbers, current_row_numbers)
    }
    
    # Retourne une liste de listes contenant les numéros de lignes correspondant à chaque mot clé
    return(row_numbers)
  }
  
  # Exemple d'utilisation de la fonction
  row_numbers <-
    find_row_numbers(columns_to_extract, filtered_rows_list)
  get_values_at_indexes <- function(index_list, rows_list_complete) {
    dataframe <- data.frame()
    
    # Parcours de la liste de listes
    str(rows_list_complete)
    for (i in 2:length(rows_list_complete)) {
      # Initialisation du compteur
      compteur <- 0
      # Parcours de chaque élément de la liste de listes
      for (j in 1:length(rows_list_complete[[i]])) {
        # Vérification si l'élément contient "Data ss:Type"
        if (grepl("Data ss:Type", rows_list_complete[[i]][[j]])) {
          # Incrémentation du compteur
          compteur <- compteur + 1
          # Si la ligne contient le motif, extraction de la valeur entre les balises
          ligne <- rows_list_complete[[i]][j]
          # Utilisation de l'expression régulière pour extraire les valeurs entre les balises
          valeur <-
            gsub("<Data ss:Type=\"String\">|<Data ss:Type=\"Number\">|</Data>",
                 "",
                 ligne)
          dataframe <-
            rbind(dataframe,
                  data.frame(
                    Numero = compteur,
                    Valeur = valeur,
                    liste = (i - 1)
                  ))
        } else if (grepl("ss:Index", rows_list_complete[[i]][[j]])) {
          # Si la ligne contient le motif, extraction de la valeur entre les balises
          ligne <- rows_list_complete[[i]][j]
          # Utilisation de l'expression régulière pour extraire les nombres
          numbers <- gsub("[^0-9]", "", ligne)
          compteur <- (as.numeric(numbers) - 1)
        }
        
      }
      # Filtrer les lignes pour ne conserver que celles où le numero est dans index_list
      dataframe <- subset(dataframe, Numero %in% index_list)
    }
    # Retourne le compteur
    return(dataframe)
  }
  
  df_global <- get_values_at_indexes(unlist(row_numbers), rows_list)
  
  relation_mot_num <-
    data.frame(num_ligne = integer(), mot_cle = character())
  
  # Parcours de chaque mot clé
  for (i in 1:length(columns_to_extract)) {
    # Extraction des numéros de ligne correspondant au mot clé courant
    current_row_numbers <- unlist(row_numbers[i])
    
    # Ajout des numéros de ligne et du mot clé courant au dataframe principal
    relation_mot_num <-
      rbind(
        relation_mot_num,
        data.frame(num_ligne = current_row_numbers, mot_cle = columns_to_extract[i])
      )
  }
  
  df_global <-
    merge(
      relation_mot_num,
      df_global,
      by.x = "num_ligne",
      by.y = "Numero",
      all.x = TRUE
    )
  df_global <- df_global %>% arrange(liste)
  
  df_wide <- df_global %>%
    pivot_wider(names_from = liste,
                values_from = Valeur) %>%
    mutate(across(everything(), trimws))
  
  # Renommer les colonnes si nécessaire
  names(df_wide) <- c("mot_cle", seq_along(names(df_wide))[-1])
  
  # Décalage du contenu des colonnes
  for (i in 1:(ncol(df_wide) - 1)) {
    df_wide[, i] <- df_wide[, i + 1]
  }
  
  # Suppression de la dernière colonne
  df_wide <- df_wide[,-ncol(df_wide)]
  
  return(df_wide)
}


# Fonction pour créer un swarm plot en fonction de la colonne spécifiée
swarm_plot_colored <- function(dataframe, nom, colonne) {
  # Regroupement des données par Nom et garder la valeur maximum de la colonne et le sexe
  dataframe <- dataframe %>%
    group_by(Nom, Sexe) %>%
    summarise(across(all_of(colonne), max), .groups = 'drop')
  
  # Création d'une nouvelle colonne pour les couleurs
  dataframe$couleur <-
    ifelse(dataframe$Sexe == "Homme", "Homme", 
           ifelse(dataframe$Sexe == "Femme", "Femme", "Non binaire"))
  dataframe$couleur[dataframe$Nom == nom] <- "Votre performance"
  
  # Arrondi des valeurs de hauteur à la première décimale
  dataframe[[colonne]] <- round(dataframe[[colonne]], 1)
  
  # Filtrage des valeurs où le Sexe est égal à "Femme"
  filtre_femme <- dataframe[dataframe$Sexe == "Femme",]
  # Calcul de la moyenne de la colonne
  moy_hauteur_femme <- round(mean(filtre_femme[[colonne]], na.rm = TRUE), 1)
  
  if (is.nan(moy_hauteur_femme)) {
    moy_hauteur_femme = 0
  }
  print(moy_hauteur_femme)
  
  # Filtrage des valeurs où le Sexe est égal à "Homme"
  filtre_homme <- dataframe[dataframe$Sexe == "Homme",]
  # Calcul de la moyenne de la colonne
  moy_hauteur_homme <- round(mean(filtre_homme[[colonne]], na.rm = TRUE), 1)
  
  # Filtrage des valeurs où le Sexe est égal à "Non binaire"
  filtre_non_binaire <- dataframe[dataframe$Sexe == "Non binaire",]
  # Calcul de la moyenne de la colonne
  moy_hauteur_non_binaire <- round(mean(filtre_non_binaire[[colonne]], na.rm = TRUE), 1)
  
  # Filtrage des données pour "Votre performance"
  votre_performance <- dataframe[dataframe$couleur == "Votre performance",]
  
  # Création du swarm plot avec ggplot
  p <- ggplot() +
    geom_point(
      data = dataframe[dataframe$couleur != "Votre performance",],
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
    geom_point(
      data = votre_performance,
      aes(
        x = Sexe,
        y = !!rlang::sym(colonne),
        fill = couleur,
        text = paste(colonne, ": ", round(!!rlang::sym(colonne), 1))
      ),
      size = 4,
      shape = 21,
      color = "black"
    ) +
    scale_fill_manual(
      values = c(
        "Homme" = "#2C2F65",
        "Femme" = "#C5243D",
        "Non binaire" = "green",
        "Votre performance" = "gold"
      ),
      labels = c(
        "Homme" = "Hommes",
        "Femme" = "Femmes",
        "Non binaire" = "Non binaires",
        "Votre performance" = "Votre performance"
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
    
    geom_text_repel(data = dataframe,
                    aes(
                      x = Sexe,
                      y = !!rlang::sym(colonne),
                      label = round(!!rlang::sym(colonne), 1)
                    ),
                    vjust = -1.5) +
    
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
    } else if (p$x$data[[i]]$name == "(Votre performance,1)") {
      p$x$data[[i]]$name <- "Votre performance"
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
}


swarm_plot_colored_visualisation <- function(dataframe, nom) {
  
  colonne = "Hauteur"
  # Regroupement des données par Nom et garder la valeur maximum de la colonne et la discipline
  dataframe <- dataframe %>%
    group_by(Nom, Discipline) %>%
    summarise(across(all_of(colonne), max), .groups = 'drop')
  
  # Création d'une nouvelle colonne pour les couleurs
  dataframe$couleur <- dataframe$Discipline
  dataframe$couleur[dataframe$Nom == nom] <- "Votre performance"
  
  # Arrondi des valeurs de hauteur à la première décimale
  dataframe[[colonne]] <- round(dataframe[[colonne]], 1)
  
  # Filtrage des données pour "Votre performance"
  votre_performance <- dataframe[dataframe$couleur == "Votre performance",]
  
  # Création du swarm plot avec ggplot
  p <- ggplot() +
    geom_point(
      data = dataframe[dataframe$couleur != "Votre performance",],
      aes(
        x = Discipline,
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
    geom_point(
      data = votre_performance,
      aes(
        x = Discipline,
        y = !!rlang::sym(colonne),
        fill = couleur,
        text = paste(colonne, ": ", round(!!rlang::sym(colonne), 1))
      ),
      size = 4,
      shape = 21,
      color = "black"
    ) +
    scale_fill_manual(
      values = c(
        "Sprint" = "#1f78b4",
        "Demi-Fond" = "#33a02c",
        "Haies" = "#e31a1c",
        "Sauts" = "#ff7f00",
        "Lancers" = "#6a3d9a",
        "Marche" = "#b15928",
        "Votre performance" = "gold"
      ),
      labels = c(
        "Sprint" = "Sprint",
        "Demi-Fond" = "Demi-Fond",
        "Haies" = "Haies",
        "Sauts" = "Sauts",
        "Lancers" = "Lancers",
        "Marche" = "Marche",
        "Votre performance" = "Votre performance"
      )
    ) +
    
    geom_text_repel(data = dataframe,
                    aes(
                      x = Discipline,
                      y = !!rlang::sym(colonne),
                      label = round(!!rlang::sym(colonne), 1)
                    ),
                    vjust = -1.5) +
    
    labs(
      x = "Discipline",
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
    if (p$x$data[[i]]$name == "(Sprint,1)") {
      p$x$data[[i]]$name <- "Sprint"
    } else if (p$x$data[[i]]$name == "(Demi-Fond,1)") {
      p$x$data[[i]]$name <- "Demi-Fond"
    } else if (p$x$data[[i]]$name == "(Haies,1)") {
      p$x$data[[i]]$name <- "Haies"
    } else if (p$x$data[[i]]$name == "(Sauts,1)") {
      p$x$data[[i]]$name <- "Sauts"
    } else if (p$x$data[[i]]$name == "(Lancers,1)") {
      p$x$data[[i]]$name <- "Lancers"
    } else if (p$x$data[[i]]$name == "(Marche,1)") {
      p$x$data[[i]]$name <- "Marche"
    } else if (p$x$data[[i]]$name == "(Votre performance,1)") {
      p$x$data[[i]]$name <- "Votre performance"
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
}


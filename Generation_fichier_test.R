# Définir les valeurs possibles pour chaque colonne
sexes <- c("Homme", "Femme", "Non binaire")
disciplines <- c("Sprint", "Demi-Fond", "Haies", "Sauts", "Lancers", "Marche")
categories <- c("EcoleAthle", "Poussin", "Benjamins", "Minimes", "Cadets", "Juniors", "Espoirs", "Seniors", "Masters")

# Nombre total de lignes
n <- 10000
# Nombre de noms uniques
unique_names_count <- ceiling(n / 3)

# Générer les noms
noms <- paste0("test", 1:unique_names_count)

# Générer les autres colonnes avec des valeurs constantes pour chaque groupe de 3 lignes
set.seed(123) # Pour la reproductibilité

# Créer une liste pour stocker les données
data_list <- lapply(noms, function(name) {
  sexe <- sample(sexes, 1)
  discipline <- sample(disciplines, 1)
  categorie <- sample(categories, 1)
  data.frame(
    Test = rep("CMJ", 3),
    Nom = rep(name, 3),
    Hauteur = round(runif(3, 20, 60), 1),
    Sexe = rep(sexe, 3),
    Discipline = rep(discipline, 3),
    Categorie = rep(categorie, 3)
  )
})

# Combiner toutes les données en un seul DataFrame
data <- do.call(rbind, data_list)

# Si le nombre de lignes dépasse n, tronquer le DataFrame
if (nrow(data) > n) {
  data <- data[1:n, ]
}

# Écrire les données dans un fichier CSV
write.csv(data, "test_data_adjusted.csv", row.names = FALSE)

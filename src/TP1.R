# TP1 Data Mining
# FRANCK - TELLIER
# Marius - Baptiste
# 16/03/2026

library(ggplot2)
library(dplyr)

# Chargement du dataset
data.TP1 <- read.csv("data/data-TP1.csv")

# -----------------------------
# (a) Informations fondamentales
# -----------------------------

# Nombre d'exemples et de variables
dim(data.TP1)

nb_exemples <- nrow(data.TP1)
nb_variables <- ncol(data.TP1)

cat("Nombre d'exemples :", nb_exemples, "\n")
cat("Nombre de variables :", nb_variables, "\n\n")

# Noms des variables
names(data.TP1)

# Types des variables
str(data.TP1)

# Type de chaque variable de façon plus lisible
sapply(data.TP1, class)

# -----------------------------
# (b) Nettoyage des données
# -----------------------------

# Conversion des variables de type chr en factor
data.TP1 <- data.TP1 %>%
  mutate(across(where(is.character), factor))

# Vérification après conversion
str(data.TP1)

# Recherche des valeurs manquantes
nb_na_total <- sum(is.na(data.TP1))
cat("\nNombre total de valeurs manquantes :", nb_na_total, "\n\n")

# Nombre de valeurs manquantes par variable
na_par_colonne <- colSums(is.na(data.TP1))
print(na_par_colonne)

# Gestion des valeurs manquantes
if (nb_na_total > 0) {
  cat("\nDes valeurs manquantes ont été détectées.\n")
  
  # Suppression des lignes incomplètes
  data.TP1 <- na.omit(data.TP1)
  
  cat("Les lignes contenant des valeurs manquantes ont été supprimées.\n")
  cat("Nouvelle dimension du dataset :", dim(data.TP1), "\n")
} else {
  cat("Aucune valeur manquante détectée.\n")
}
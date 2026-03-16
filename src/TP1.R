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

# =========================================
# 3. Analyse univariée
# =========================================

# Distribution des âges
ggplot(data.TP1, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution des âges", x = "Age", y = "Nombre d'étudiants") +
  theme_minimal()

# Répartition des genres
gender_counts <- table(data.TP1$Gender)
pie(gender_counts, main = "Répartition hommes / femmes", col = c("lightpink","lightblue"))

# Plateformes les plus utilisées
platform_counts <- sort(table(data.TP1$Most_Used_Platform), decreasing = TRUE)
barplot(platform_counts, col="skyblue", main="Plateformes les plus utilisées")

# Répartition par pays
country_counts <- sort(table(data.TP1$Country), decreasing = TRUE)
barplot(country_counts, las=2, col="lightgreen", main="Répartition par pays")

# Distribution par niveau d'étude
academic_counts <- table(data.TP1$Academic_Level)
barplot(academic_counts, col="orange", main="Distribution par niveau d'étude")

# =========================================
# 4. Analyse multivariée
# =========================================

# Moyenne du score d'addiction par pays

addiction_country <- aggregate(Addicted_Score ~ Country, data=data.TP1, mean)

# Trier par ordre décroissant
addiction_country <- addiction_country[order(-addiction_country$Addicted_Score),]

# Affichage
addiction_country

# Graphique
ggplot(addiction_country, aes(x=reorder(Country, Addicted_Score), y=Addicted_Score)) +
  geom_bar(stat="identity", fill="red") +
  coord_flip() +
  labs(
    title="Score moyen d'addiction par pays",
    x="Pays",
    y="Score moyen d'addiction"
  ) +
  theme_minimal()

# Relation entre durée d’usage et sommeil

# Nuage de points
ggplot(data.TP1, aes(x=Avg_Daily_Usage_Hours, y=Sleep_Hours_Per_Night)) +
  geom_point(color="blue", alpha=0.6) +
  labs(
    title="Usage des réseaux sociaux vs durée de sommeil",
    x="Durée d'usage quotidien (heures)",
    y="Heures de sommeil"
  ) +
  theme_minimal()

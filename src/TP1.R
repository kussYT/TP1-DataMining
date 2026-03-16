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

# =========================================
# 4. Analyse multivariée
# =========================================


# -------------------------------------------------
# 1. Pays avec les scores d'addiction moyens les plus élevés
# -------------------------------------------------

addiction_country <- aggregate(Addicted_Score ~ Country, data = data.TP1, mean)

# Trier par ordre décroissant
addiction_country <- addiction_country[order(-addiction_country$Addicted_Score),]

# Affichage du tableau
print(addiction_country)

# Graphique
ggplot(addiction_country, aes(x = reorder(Country, Addicted_Score), y = Addicted_Score)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(
    title = "Score moyen d'addiction par pays",
    x = "Pays",
    y = "Score moyen d'addiction"
  ) +
  theme_minimal()



# -------------------------------------------------
# 2. Relation entre durée d'usage et durée de sommeil
# -------------------------------------------------

ggplot(data.TP1, aes(x = Avg_Daily_Usage_Hours, y = Sleep_Hours_Per_Night)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Durée d'usage des réseaux sociaux vs sommeil",
    x = "Durée d'usage quotidien (heures)",
    y = "Heures de sommeil"
  ) +
  theme_minimal()

# Régression linéaire
modele_sleep <- lm(Sleep_Hours_Per_Night ~ Avg_Daily_Usage_Hours, data = data.TP1)
summary(modele_sleep)

# Coefficient de corrélation
correlation_sleep_usage <- cor(data.TP1$Avg_Daily_Usage_Hours, data.TP1$Sleep_Hours_Per_Night)
print(correlation_sleep_usage)



# -------------------------------------------------
# 3. Différence hommes / femmes
# -------------------------------------------------

ggplot(data.TP1, aes(x = Avg_Daily_Usage_Hours, y = Sleep_Hours_Per_Night, color = Gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Usage des réseaux sociaux et sommeil selon le genre",
    x = "Durée d'usage quotidien",
    y = "Heures de sommeil"
  ) +
  theme_minimal()



# -------------------------------------------------
# 4. Corrélation entre addiction et santé mentale
# -------------------------------------------------

ggplot(data.TP1, aes(x = Addicted_Score, y = Mental_Health_Score)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Score d'addiction vs santé mentale",
    x = "Score d'addiction",
    y = "Score de santé mentale"
  ) +
  theme_minimal()

# Calcul de corrélation
correlation_addiction_mental <- cor(data.TP1$Addicted_Score, data.TP1$Mental_Health_Score)
print(correlation_addiction_mental)



# -------------------------------------------------
# 5. Impact du genre sur addiction et santé mentale
# -------------------------------------------------

ggplot(data.TP1, aes(x = Addicted_Score, y = Mental_Health_Score, color = Gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Addiction et santé mentale selon le genre",
    x = "Score d'addiction",
    y = "Score de santé mentale"
  ) +
  theme_minimal()



# -------------------------------------------------
# 6. Impact du niveau d'étude
# -------------------------------------------------

ggplot(data.TP1, aes(x = Addicted_Score, y = Mental_Health_Score, color = Academic_Level)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Addiction et santé mentale selon le niveau d'étude",
    x = "Score d'addiction",
    y = "Score de santé mentale"
  ) +
  theme_minimal()

# =========================================
# 5. Classification
# =========================================


library(OneR)
library(e1071)
library(rpart)
library(rpart.plot)



# -------------------------------------------------
# 1. Discrétisation de la durée de sommeil
# -------------------------------------------------

data.TP1$Sleep_Category <- cut(
  data.TP1$Sleep_Hours_Per_Night,
  breaks = c(-Inf,4,7,Inf),
  labels = c("Faible","Moyenne","Elevee")
)

table(data.TP1$Sleep_Category)



# -------------------------------------------------
# 2. Séparation train / test (70 / 30)
# -------------------------------------------------

set.seed(123)

n <- nrow(data.TP1)

train_index <- sample(1:n, size = 0.7*n)

train_data <- data.TP1[train_index,]
test_data <- data.TP1[-train_index,]

train_data$Sleep_Category <- droplevels(train_data$Sleep_Category)
test_data$Sleep_Category <- droplevels(test_data$Sleep_Category)



# -------------------------------------------------
# 3. Modèle One Rule
# -------------------------------------------------

model_oner <- OneR(Sleep_Category ~ ., data=train_data)

pred_oner <- predict(model_oner, test_data)

accuracy_oner <- mean(as.character(pred_oner) == as.character(test_data$Sleep_Category))

print(accuracy_oner)



# -------------------------------------------------
# 4. Naive Bayes
# -------------------------------------------------

model_nb <- naiveBayes(Sleep_Category ~ ., data=train_data)

pred_nb <- predict(model_nb, test_data)

accuracy_nb <- mean(as.character(pred_nb) == as.character(test_data$Sleep_Category))

print(accuracy_nb)


# -----------------------------
# Préparation des données pour l'arbre
# -----------------------------

train_tree <- train_data[, c(
  "Age",
  "Gender",
  "Avg_Daily_Usage_Hours",
  "Mental_Health_Score",
  "Addicted_Score",
  "Conflicts_Over_Social_Media",
  "Sleep_Category"
)]

test_tree <- test_data[, c(
  "Age",
  "Gender",
  "Avg_Daily_Usage_Hours",
  "Mental_Health_Score",
  "Addicted_Score",
  "Conflicts_Over_Social_Media",
  "Sleep_Category"
)]

train_tree <- droplevels(train_tree)
test_tree <- droplevels(test_tree)
# -------------------------------------------------
# 5. Arbre de décision
# -------------------------------------------------

library(rpart)
library(rpart.plot)

model_tree <- rpart(
  Sleep_Category ~ .,
  data = train_tree,
  method = "class"
)

pred_tree <- predict(model_tree, test_tree, type = "class")

accuracy_tree <- mean(as.character(pred_tree) == as.character(test_tree$Sleep_Category))

print(accuracy_tree)

rpart.plot(model_tree)

# -------------------------------------------------
# 6. Comparaison des résultats
# -------------------------------------------------

results <- data.frame(
  Modele = c("One Rule", "Naive Bayes", "Decision Tree"),
  Accuracy = c(accuracy_oner, accuracy_nb, accuracy_tree)
)

print(results)


############################ DEFINITION DES BIBLIOTHEQUES #######################################

library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(pROC)
library(xgboost)
library(leaflet)
library(leaflet.extras)

############################ MANIPULATION DES DATASETS #######################################

data1 <- read.csv("C:/Users/enzom/OneDrive/Documents/Cours B3/Data Analysis/Projet UBER/UberDatasets/uber-raw-data-apr14.csv", sep=",")
data2 <- read.csv("C:/Users/enzom/OneDrive/Documents/Cours B3/Data Analysis/Projet UBER/UberDatasets/uber-raw-data-may14.csv", sep=",")
data3 <- read.csv("C:/Users/enzom/OneDrive/Documents/Cours B3/Data Analysis/Projet UBER/UberDatasets/uber-raw-data-jun14.csv", sep=",")
data4 <- read.csv("C:/Users/enzom/OneDrive/Documents/Cours B3/Data Analysis/Projet UBER/UberDatasets/uber-raw-data-jul14.csv", sep=",")
data5 <- read.csv("C:/Users/enzom/OneDrive/Documents/Cours B3/Data Analysis/Projet UBER/UberDatasets/uber-raw-data-aug14.csv", sep=",")
data6 <- read.csv("C:/Users/enzom/OneDrive/Documents/Cours B3/Data Analysis/Projet UBER/UberDatasets/uber-raw-data-sep14.csv", sep=",")

all(c("Date.Time", "Lat", "Lon", "Base") %in% colnames(data1))

data_fus <- full_join(data1, data2, by = c("Date.Time", "Lat", "Lon", "Base"))
data_fus <- full_join(data_fus, data3, by = c("Date.Time", "Lat", "Lon", "Base"))
data_fus <- full_join(data_fus, data4, by = c("Date.Time", "Lat", "Lon", "Base"))
data_fus <- full_join(data_fus, data5, by = c("Date.Time", "Lat", "Lon", "Base"))
data_fus <- full_join(data_fus, data6, by = c("Date.Time", "Lat", "Lon", "Base"))

#prendre connaissance avec le dataset

summary(data_fus)

# Fonction pour vérifier le dataframe
check_df <- function(df, head = 5, tail = 5) {
  cat("=== SHAPE ===\n")
  cat("Observations ------->", nrow(df), "\n")
  cat("Features     ------->", ncol(df), "\n")
  
  cat("\n=== Types of Features ===\n")
  print(str(df))
  
  cat("\n=== Duplicate Values Analysis ===\n")
  cat("Nombre de valeurs dupliquées :", sum(duplicated(df)), "\n")
  cat(rep("=", 40), "\n")
  
  # Afficher les premières et dernières lignes du dataframe
  cat("\nPremières lignes du dataframe\n")
  print(head(df, head))
  
  cat("\nDernières lignes du dataframe\n")
  print(tail(df, tail))
}

# Fonction pour visualiser les valeurs manquantes
missing_plot <- function(dataset) {
  # Calculer le nombre de valeurs non manquantes et le pourcentage de valeurs manquantes
  null_feat <- colSums(!is.na(dataset))
  percentage_null <- colMeans(is.na(dataset)) * 100
  
  # Créer un dataframe pour la visualisation
  missing_data <- data.frame(
    Feature = names(null_feat),
    Count = null_feat,
    Percentage = round(percentage_null, 2)
  )
  
  # Plot des valeurs manquantes
  ggplot(missing_data, aes(x = reorder(Feature, -Count), y = Count)) +
    geom_bar(stat = "identity", fill = "#D84E5F", color = "#000000", width = 0.7) +
    geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, color = "black") +
    labs(title = "Missing Values Analysis",
         x = "Features",
         y = "Count of Non-Missing Values") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

check_df(data_fus)
missing_plot(data_fus)

# Utilisation de df pour ne pas entraver le dataset initial
df <- data_fus

############################################ EXTRACTIONS ET VISUALISATION DE DATA UTILES #######################################

# DATE

# Convertir la colonne 'Date.Time' en format date-heure
df$Date.Time <- as.POSIXct(df$Date.Time, format = "%m/%d/%Y %H:%M:%S")

# Extraire la date uniquement
df$Date <- as.Date(df$Date.Time)


df$Month <- month(df$Date.Time, label = TRUE, abbr = TRUE)


# Compter le nombre de trajets par jour
trips_per_day <- df %>%
  group_by(Date, Month) %>%
  summarise(Trips = n())

# Afficher les statistiques descriptives
stats <- trips_per_day %>%
  summarise(
    Mean = mean(Trips),
    Median = median(Trips),
    Min = min(Trips),
    Max = max(Trips)
  )

# Afficher les résultats
print(trips_per_day)
print(stats)

# HEURE

# Extraire l'heure uniquement
df$Hour <- hour(df$Date.Time)

# Compter le nombre de trajets par heure pour chaque jour
trips_per_hour <- df %>%
  group_by(Date = as.Date(Date.Time), Hour) %>%
  summarise(Trips = n()) %>%
  ungroup()

# Calculer la moyenne des trajets par heure
avg_trips_per_hour <- trips_per_hour %>%
  group_by(Hour) %>%
  summarise(Mean_Trips = mean(Trips))

# Créer un barplot des moyennes de trajets par heure
ggplot(avg_trips_per_hour, aes(x = factor(Hour), y = Mean_Trips)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +
  labs(title = "Moyenne des trajets par heure",
       x = "Heure de la journée",
       y = "Moyenne des trajets") +
  theme_minimal()

# MOIS

# Compter le nombre de trajets par mois pour chaque jour
trips_per_month <- df %>%
  group_by(Date = as.Date(Date.Time), Month) %>%
  summarise(Trips = n()) %>%
  ungroup()

# Calculer la moyenne des trajets par mois
avg_trips_per_month <- trips_per_month %>%
  group_by(Month) %>%
  summarise(Mean_Trips = mean(Trips))


# Créer un barplot des moyennes de trajets par mois
ggplot(avg_trips_per_month, aes(x = Month, y = Mean_Trips)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +
  labs(title = "Moyenne des trajets par mois",
       x = "Mois",
       y = "Moyenne des trajets") +
  theme_minimal()

# SEMAINE

# Extraire le jour de la semaine de la colonne 'Date'
trips_per_day$Day_of_Week <- wday(trips_per_day$Date, label = TRUE, abbr = TRUE)

# Calculer la moyenne des trajets pour chaque jour de la semaine
avg_trips_per_day_of_week <- trips_per_day %>%
  group_by(Day_of_Week) %>%
  summarise(Mean_Trips = mean(Trips))

# Créer un barplot des moyennes de trajets par jour de la semaine
ggplot(avg_trips_per_day_of_week, aes(x = Day_of_Week, y = Mean_Trips)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +
  labs(title = "Moyenne des trajets par jour de la semaine",
       x = "Jour de la semaine",
       y = "Moyenne des trajets") +
  theme_minimal()

# SEMAINES DANS L ANNEE

# JOUR MAX PAR MOIS

# Trouver le jour avec le plus de trajets pour chaque mois
max_trips_per_month <- trips_per_day %>%
  group_by(Month) %>%
  slice_max(order_by = Trips, n = 1)

# Afficher le résultat
print(max_trips_per_month)

############################ MODELE DE PREDICTION AVEC XGBOOST #######################################

df$Date.Time <- as.POSIXct(df$Date.Time, format = "%m/%d/%Y %H:%M:%S")
df$Date <- as.Date(df$Date.Time)
df$Month <- month(df$Date.Time, label = TRUE, abbr = TRUE)
df$Day_of_Week <- wday(df$Date.Time, label = TRUE, abbr = TRUE)
df$Hour <- hour(df$Date.Time)


# Agréger le nombre de trajets par date, jour de la semaine, mois et heure
trips_summary <- df %>%
  group_by(Date, Month, Day_of_Week, Hour) %>%
  summarise(Trips = n(), .groups = "drop")

# Définir la variable cible : "Surchargé" si le nombre de trajets est au-dessus du 90e percentile
threshold <- quantile(trips_summary$Trips, 0.9)
trips_summary$Surcharged <- ifelse(trips_summary$Trips > threshold, 1, 0)

# Sélection des caractéristiques
features <- trips_summary %>%
  select(Month, Day_of_Week, Hour, Surcharged)

# Convertir les variables catégorielles en facteurs
features$Month <- as.factor(features$Month)
features$Day_of_Week <- as.factor(features$Day_of_Week)
features$Hour <- as.factor(features$Hour)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)
trainIndex <- createDataPartition(features$Surcharged, p = 0.8, list = FALSE)
trainData <- features[trainIndex, ]
testData <- features[-trainIndex, ]

# Assurez-vous que vos données sont sous forme numérique
trainData$Surcharged <- as.factor(trainData$Surcharged)  # Assurez-vous que la variable cible est un facteur
testData$Surcharged <- as.factor(testData$Surcharged)

# Convertir les données en matrices XGBoost (matrices numériques)
train_matrix <- data.matrix(trainData[, -which(names(trainData) == "Surcharged")])  # Exclure la variable cible
test_matrix <- data.matrix(testData[, -which(names(testData) == "Surcharged")])

# Convertir la variable cible en numérique (0 et 1)
train_label <- as.numeric(trainData$Surcharged) - 1  # Convertir en 0 et 1
test_label <- as.numeric(testData$Surcharged) - 1

# Créer des objets DMatrix pour XGBoost
train_dmatrix <- xgb.DMatrix(data = train_matrix, label = train_label)
test_dmatrix <- xgb.DMatrix(data = test_matrix, label = test_label)

# Définir les paramètres pour XGBoost
params <- list(
  booster = "gbtree",      # Type de booster
  objective = "binary:logistic",  # Problème binaire
  eval_metric = "error",   # Métrique d'évaluation (erreur)
  max_depth = 6,           # Profondeur maximale des arbres
  eta = 0.3,               # Taux d'apprentissage
  nthread = 2              # Nombre de threads (peut être ajusté)
)

# Entraîner le modèle XGBoost
xgb_model <- xgb.train(params = params, 
                       data = train_dmatrix, 
                       nrounds = 100,   # Nombre d'itérations
                       watchlist = list(train = train_dmatrix, test = test_dmatrix),
                       early_stopping_rounds = 10,  # Arrêter si pas d'amélioration
                       print_every_n = 10)

# Faire des prédictions sur les données de test
predictions_prob <- predict(xgb_model, test_dmatrix)

# Convertir les probabilités en classes (0 ou 1)
predictions <- ifelse(predictions_prob > 0.5, 1, 0)

# Convertir les prédictions en facteur pour la matrice de confusion
predictions <- factor(predictions, levels = c(0, 1))

# Confusion Matrix
conf_matrix <- confusionMatrix(predictions, as.factor(test_label))
print(conf_matrix)

# Calcul d'autres métriques (précision, rappel, F1-score)
precision <- posPredValue(predictions, as.factor(test_label))
recall <- sensitivity(predictions, as.factor(test_label))
f1 <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1, "\n")

############################ VISUALISATION CARTE DE NY #######################################

# Créer la carte interactive avec une heatmap
leaflet(df) %>%
  addTiles() %>%  # Ajouter la couche de fond (OpenStreetMap)
  addHeatmap(lng = ~Lon, lat = ~Lat, intensity = ~1, blur = 20, max = 0.05, radius = 10) %>%
  setView(lng = -74.0060, lat = 40.7128, zoom = 9)  # Centrer la carte sur New York

############################ MODELE DE PREDICTION DE COORDONNEES DE TRAJETS #######################################

# Appliquer K-Means sur l'ensemble complet avant de diviser en train/test
set.seed(123)
coords <- df[, c("Lat", "Lon")]
kmeans_result <- kmeans(coords, centers = 10)

# Ajouter les clusters au dataset complet
df$Cluster <- as.factor(kmeans_result$cluster)

# Définir une palette de couleurs pour les clusters
palette <- colorFactor(topo.colors(10), levels = levels(df$Cluster))

# Créer la carte interactive avec Leaflet
leaflet(df) %>%
  addTiles() %>%  # Ajouter la couche de fond (OpenStreetMap)
  addCircleMarkers(
    lng = ~Lon, lat = ~Lat,
    color = ~palette(Cluster),
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.6
  ) %>%
  addLegend(
    "bottomright",
    pal = palette,
    values = ~Cluster,
    title = "Clusters",
    opacity = 1
  ) %>%
  setView(lng = -74.0060, lat = 40.7128, zoom = 9)  # Centrer la carte sur New York

# Créer une nouvelle colonne 'Trip_Count' en comptant les trajets par Cluster, Hour et Day_of_Week
df <- df %>%
  group_by(Cluster, Hour, Day_of_Week) %>%
  summarise(Trip_Count = n(), .groups = "drop")

trainIndex <- createDataPartition(df$Trip_Count, p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Convertir la colonne Cluster en numérique
trainData$Cluster <- as.numeric(as.factor(trainData$Cluster))
testData$Cluster <- as.numeric(as.factor(testData$Cluster))

# Créer la matrice pour XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(trainData[, c("Cluster", "Hour")]), label = trainData$Trip_Count)
test_matrix <- xgb.DMatrix(data = as.matrix(testData[, c("Cluster", "Hour")]), label = testData$Trip_Count)


# Entraîner le modèle XGBoost
xgb_model <- xgboost(data = train_matrix, nrounds = 100, objective = "reg:squarederror")

# Prédictions
predictions <- predict(xgb_model, test_matrix)

# Évaluation du modèle
correlation <- cor(predictions, testData$Trip_Count)
cat("Correlation entre les prédictions et les données réelles : ", round(correlation, 2), "\n")

testData$Predicted_Trip_Count <- predictions

ggplot(testData, aes(x = Hour, y = Predicted_Trip_Count, color = as.factor(Cluster))) +
  geom_line() +
  labs(title = "Prédictions des trajets par heure et par cluster",
       x = "Heure",
       y = "Nombre prédit de trajets") +
  theme_minimal()

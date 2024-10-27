require(ade4)
library(FactoMineR)

# Importer les données
data <- read.csv(file = "nba_draft_combine_all_years.csv", header = TRUE, sep = ',', dec = ',', stringsAsFactors = TRUE)

names(data)

# Vérifier le type de données pour la colonne "Year"
str(data$Year)

# Si "Year" n'est pas numérique, convertissez-le en numérique
data$Year <- as.numeric(as.character(data$Year))

# Filtrer les données pour conserver seulement les années 2009 à 2013
data <- subset(data, Year >= 2009 & Year <= 2013)
# Supprimer les colonnes qui contiennent peu de valeurs (par exemple, 'Hand (Length)' et 'Hand (Width)')
donnees <- subset(data, select = -c(`Hand..Length.`, `Hand..Width.`, `X`,`Draft.pick` , `Year`))
# Supprimer les lignes contenant des valeurs manquantes si nécessaire
donnees <- na.omit(donnees)
# Supprimer les colonnes qui sont des facteurs
donnees_num <- donnees[sapply(donnees, is.numeric)]

# Renommer les variables avec des noms appropriés
names(donnees_num)[names(donnees_num) == "Height..No.Shoes."] <- "hauteur_sans_chaussures"
names(donnees_num)[names(donnees_num) == "Height..With.Shoes."] <- "hauteur_avec_chaussures"
names(donnees_num)[names(donnees_num) == "Wingspan"] <- "envergure"
names(donnees_num)[names(donnees_num) == "Standing.reach"] <- "atteinte_debout"
names(donnees_num)[names(donnees_num) == "Vertical..Max."] <- "saut_vertical_max"
names(donnees_num)[names(donnees_num) == "Vertical..Max.Reach."] <- "atteinte_max_saut"
names(donnees_num)[names(donnees_num) == "Vertical..No.Step."] <- "saut_sans_elan"
names(donnees_num)[names(donnees_num) == "Vertical..No.Step.Reach."] <- "atteinte_sans_elan"
names(donnees_num)[names(donnees_num) == "Weight"] <- "poids"
names(donnees_num)[names(donnees_num) == "Body.Fat"] <- "pourcentage_graisse_corp"
names(donnees_num)[names(donnees_num) == "Bench"] <- "repetitions_developpe_couche"
names(donnees_num)[names(donnees_num) == "Agility"] <- "agilite"
names(donnees_num)[names(donnees_num) == "Sprint"] <- "temps_sprint"

# Utiliser la fonction summary pour obtenir un résumé statistique pour chaque colonne
summary(donnees_num)

statistiques <- data.frame(
  Moyenne = sapply(donnees_num, mean, na.rm = TRUE),
  EcartType = sapply(donnees_num, sd, na.rm = TRUE),
  Q1 = sapply(donnees_num, quantile, probs = 0.25, na.rm = TRUE),
  Median = sapply(donnees_num, median, na.rm = TRUE),
  Q3 = sapply(donnees_num, quantile, probs = 0.75, na.rm = TRUE)
)

# Afficher les statistiques calculées
print(statistiques)

# Créer des boîtes à moustaches pour chaque variable numérique
par(mfrow=c(4,4)) # Ajuster en fonction du nombre de variables
for(i in 1:ncol(donnees_num)) {
  boxplot(donnees_num[,i], main = names(donnees_num)[i])
}

# Charger factoextra
if(!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(factoextra)

# Réaliser une ACP
res.acp <- PCA(donnees_num, scale.unit = TRUE, graph = FALSE)

# Projection des individus - premier plan factoriel
fviz_pca_ind(res.acp, title = "Projection des individus (ACP)")

# Inertie totale expliquée
fviz_eig(res.acp, addlabels = TRUE, ylim = c(0, 50))

# Variance expliquée par chaque axe
barplot(res.acp$eig[,2], names.arg = 1:nrow(res.acp$eig), main = "Pourcentage de variance expliquée", xlab = "Axes", ylab = "Pourcentage de variance")

# Nombre de composantes principales
print(paste("Nombre de composantes principales sélectionnées : ", res.acp$ncp))

# Cercle des corrélations pour les deux premiers axes principaux
fviz_pca_var(res.acp, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Éviter le chevauchement du texte
             title = "Cercle des corrélations (ACP)")

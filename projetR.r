install.packages(c(
  "tidyverse",
  "FactoMineR",
  "factoextra",
  "cluster",
  "corrplot",
  "GGally"
))

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(corrplot)
library(GGally)

data <- read.csv("student_lifestyle_dataset.csv")
head(data)
str(data)
summary(data)


# Transformer le texte en numérique

data$Stress_Level_Numeric <- ifelse(
  data$Stress_Level == "Low", 1,
  ifelse(
    data$Stress_Level == "Moderate", 2, 3
  )
)


# 4. GESTION DES VALEURS MANQUANTES
# ============================================
missing_values <- colSums(is.na(data))
cat("Valeurs manquantes par variable :\n")
print(missing_values)

#CREATION NOUVELLE VARIABLE
#formule : Lifestyle_Score = (Sommeil × 2) + (Sport × 2) - (Stress × 1.5) - (Social)
data$Lifestyle_Score <-
  (data$Sleep_Hours_Per_Day * 2) +
  (data$Physical_Activity_Hours_Per_Day * 2) -
  (data$Stress_Level_Numeric * 1.5) -
  (data$Social_Hours_Per_Day)

# Supprimer Student_ID
# Supprimer Stress_Level texte

data <- data %>%
  select(-Student_ID, -Stress_Level)
data
colnames(data)

# Renommer pour des noms courts et clairs
colnames(data)[colnames(data) == "Sleep_Hours_Per_Day"] <- "Sommeil"
colnames(data)[colnames(data) == "Physical_Activity_Hours_Per_Day"] <- "Sport"
colnames(data)[colnames(data) == "Social_Hours_Per_Day"] <- "Social"
colnames(data)[colnames(data) == "Stress_Level_Numeric"] <- "Stress"
colnames(data)[colnames(data) == "Study_Hours_Per_Day"] <- "Study_Hours"
colnames(data)[colnames(data) == "Extracurricular_Hours_Per_Day"] <- "Extra"


# Vérifier les noms
colnames(data)
#DETECTION DES OUTLIERS

boxplot(data,
        main = "Détection des Outliers",
        col = "lightblue")

# 11. MATRICE DE CORRELATION
cor_matrix <- cor(data) # calculer corr entre var 

corrplot(cor_matrix,
         method = "color",#heatmap
         type = "upper", #triangle supérieur
         tl.cex = 0.8)  #"taille texte"


# VISUALISATION DES RELATIONS

ggpairs(data)
# NORMALISATION DES DONNEES

data_scaled <- scale(data)

head(data_scaled)

#ANALYSE EN COMPOSANTES PRINCIPALES
res.pca <- PCA(data_scaled)

summary(res.pca)

# VARIANCE EXPLIQUEE

fviz_eig(res.pca,
         addlabels = TRUE,
         ylim = c(0, 60))


#CERCLE DES CORRELATIONS
#fviz_pca_var(res.pca,
#             col.var = "contrib",
 #            gradient.cols = c("blue",
                   #            "yellow",
  #                             "red"),
   #          repel = TRUE)


#  VISUALISATION DES INDIVIDUS
#montre les individus projetés
fviz_pca_ind(res.pca,
             geom.ind = "point",
             col.ind = "lightblue")

#BIPLOT ACP: 👉 combine :
#individus
#variables


fviz_pca_biplot(res.pca,
                geom.ind = "point",
                col.var = "red",
                col.ind = "lightblue")

# CONTRIBUTION DES VARIABLES # elle monter quelle var influence le plus 
# Contribution Axe 1
fviz_contrib(res.pca,
             choice = "var",
             axes = 1,
             top = 10)

# Contribution Axe 2
fviz_contrib(res.pca,
             choice = "var",
             axes = 2,
             top = 10)

# Contribution Axe 3

#CHOIX NOMBRE CLUSTERS
# Méthode Elbow
fviz_nbclust(data_scaled,
             kmeans,
             method = "wss")

# Méthode Silhouette
fviz_nbclust(data_scaled,
             kmeans,
             method = "silhouette")

#APPLICATION KMEANS

set.seed(123) 

km <- kmeans(data_scaled,
             centers = 2,
             nstart = 25)# 25 essai 

km

# AJOUT DES CLUSTERS

data$Cluster <- as.factor(km$cluster)

head(data)

#  VISUALISATION DES CLUSTERS
#affiche séparation des groupes
fviz_cluster(km,
             data = data_scaled,
             palette = "jco",
             ellipse.type = "norm",
             repel = TRUE,
             ggtheme = theme_minimal())

# ANALYSE DES CLUSTERS

#👉 moyenne de chaque variable par cluster
aggregate(data,
          by = list(Cluster = data$Cluster),
          mean)


# CLASSIFICATION HIERARCHIQUE

dist_matrix <- dist(data_scaled) #👉 calcule distances

hc <- hclust(dist_matrix,
             method = "ward.D2") #minimise variance intra-cluster
#J’ai choisi ward.D2 parce qu’elle crée des groupes homogènes et bien séparés en minimisant la variance à l’intérieur des clusters.

# DENDROGRAMME
#arbre de regroupement
plot(hc,
     labels = FALSE,
     main = "Dendrogramme CAH")

#  DECOUPAGE GROUPES

groups <- cutree(hc,
                 k = 3)

table(groups)

# VISUALISATION CAH

fviz_dend(hc,
          k = 3,
          rect = TRUE,
          rect_fill = TRUE,
          rect_border = "jco")



#EXPORTATION RESULTATS


write.csv(data,
          "resultats_clusters.csv",
          row.names = FALSE)

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
dim(data)
summary(data)
colnames(data)

colSums(is.na(data))

# ============================================
# ENCODAGE STRESS (ordinal → numérique)
# ============================================
data$Stress_Level_Numeric <- ifelse(
  data$Stress_Level == "Low", 1,
  ifelse(data$Stress_Level == "Moderate", 2, 3)
)

# ============================================
# GESTION DES VALEURS MANQUANTES
# ============================================
missing_values <- colSums(is.na(data))
cat("Valeurs manquantes par variable :\n")
print(missing_values)

# ============================================
# RENOMMAGE DES COLONNES
# ============================================
colnames(data)[colnames(data) == "Sleep_Hours_Per_Day"]             <- "Sommeil"
colnames(data)[colnames(data) == "Physical_Activity_Hours_Per_Day"] <- "Sport"
colnames(data)[colnames(data) == "Social_Hours_Per_Day"]            <- "Social"
colnames(data)[colnames(data) == "Stress_Level_Numeric"]            <- "Stress"
colnames(data)[colnames(data) == "Study_Hours_Per_Day"]             <- "Study_Hours"
colnames(data)[colnames(data) == "Extracurricular_Hours_Per_Day"]   <- "Extra"

# ============================================
# SUPPRESSION DES COLONNES NON NUMERIQUES
# ============================================
data <- data %>% select(-Student_ID, -Stress_Level)

# ============================================
# FIX 1 : Lifestyle_Score APRES renommage,
#          calculé pour l'analyse descriptive SEULEMENT
#          → ne pas l'inclure dans le clustering
#            (c'est une combinaison linéaire des autres vars)
# ============================================
data$Lifestyle_Score <-
  (data$Sommeil * 2) +
  (data$Sport   * 2) -
  (data$Stress  * 1.5) -
  (data$Social)

# Variables PURES pour clustering (sans Lifestyle_Score pour éviter la redondance)
vars_clustering <- c("Study_Hours", "Extra", "Sommeil", "Social", "Sport", "GPA", "Stress")
data_cluster    <- data[, vars_clustering]

# ============================================
# DETECTION DES OUTLIERS
# ============================================
boxplot(data_cluster,
        main = "Détection des Outliers",
        las  = 2,
        col  = "lightblue")

# ============================================
# FIX 2 : Suppression des outliers extrêmes (Z-score > 3.5)
#          Les outliers "tirent" les centroïdes et gonflent le WCSS
# ============================================
z_scores   <- scale(data_cluster)
outliers   <- apply(abs(z_scores) > 3.5, 1, any)
cat("Nombre d'outliers supprimés :", sum(outliers), "\n")

data_clean   <- data[!outliers, ]
data_cluster_clean <- data_cluster[!outliers, ]

# ============================================
# MATRICE DE CORRELATION
# ============================================
cor_matrix <- cor(data_cluster_clean)
corrplot(cor_matrix,
         method = "color",
         type   = "upper",
         tl.cex = 0.8,
         title  = "Corrélations (variables de clustering)")

# ============================================
# VISUALISATION DES RELATIONS
# ============================================
ggpairs(data_cluster_clean)

# ============================================
# FIX 3 : NORMALISATION sur données propres
# ============================================
data_scaled <- scale(data_cluster_clean)
head(data_scaled)

# ============================================
# ANALYSE EN COMPOSANTES PRINCIPALES
# ============================================
res.pca <- PCA(data_scaled, graph = FALSE)
summary(res.pca)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))

fviz_pca_var(res.pca,
             col.var       = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             repel         = TRUE)

fviz_pca_ind(res.pca,
             geom.ind = "point",
             col.ind  = "blue",
             repel    = FALSE)   # repel=FALSE : plus rapide sur gros jeux

fviz_pca_biplot(res.pca,
                repel   = FALSE,
                col.var = "red",
                col.ind = "blue")

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

# ============================================
# FIX 4 : CHOIX DU NOMBRE DE CLUSTERS
#          Lire le coude sur wss ET silhouette
#          avant de choisir k=3 (ou autre)
# ============================================
fviz_nbclust(data_scaled, kmeans, method = "wss",
             k.max = 8, nstart = 25) +
  ggtitle("Méthode Elbow")

fviz_nbclust(data_scaled, kmeans, method = "silhouette",
             k.max = 8, nstart = 25) +
  ggtitle("Méthode Silhouette")

# Indice de Calinski-Harabasz (bonus : confirme le meilleur k)
ch_scores <- sapply(2:8, function(k) {
  km_tmp <- kmeans(data_scaled, centers = k, nstart = 25, iter.max = 50)
  between <- km_tmp$betweenss
  within  <- km_tmp$tot.withinss
  n       <- nrow(data_scaled)
  (between / (k - 1)) / (within / (n - k))
})
plot(2:8, ch_scores, type = "b", pch = 19,
     xlab = "Nombre de clusters k",
     ylab = "Indice Calinski-Harabasz",
     main = "Choix de k — Calinski-Harabasz (plus grand = meilleur)")

# ============================================
# FIX 5 : K-MEANS avec iter.max plus élevé
#          pour garantir la convergence
# ============================================
set.seed(123)
km <- kmeans(data_scaled,
             centers  = 3,
             nstart   = 50,      # plus de re-starts = meilleur optimum global
             iter.max = 100)     # évite les non-convergences

cat("\n=== RÉSULTATS K-MEANS ===\n")
cat("Between_SS / Total_SS :", round(km$betweenss / km$totss * 100, 1), "%\n")
print(km)

# ============================================
# FIX 6 : Score Silhouette du résultat final
#          Objectif : silhouette moyenne > 0.40
# ============================================
sil <- silhouette(km$cluster, dist(data_scaled))
cat("Silhouette moyenne :", round(mean(sil[, 3]), 3), "\n")
fviz_silhouette(sil) + ggtitle("Silhouette K-means k=3")

# ============================================
# AJOUT DES CLUSTERS AU DATASET PROPRE
# ============================================
data_clean$Cluster <- as.factor(km$cluster)
head(data_clean)

# ============================================
# VISUALISATION DES CLUSTERS (PCA space)
# ============================================
fviz_cluster(km,
             data         = data_scaled,
             palette      = "jco",
             ellipse.type = "norm",
             repel        = FALSE,
             ggtheme      = theme_minimal(),
             main         = "Clusters K-means (espace PCA)")

# ============================================
# ANALYSE DESCRIPTIVE PAR CLUSTER
# ============================================
cluster_summary <- data_clean %>%
  group_by(Cluster) %>%
  summarise(across(all_of(vars_clustering), mean, .names = "moy_{col}"),
            n = n())

cat("\n=== MOYENNES PAR CLUSTER ===\n")
print(cluster_summary)

# ============================================
# CLASSIFICATION HIERARCHIQUE (CAH)
# ============================================
# FIX 7 : CAH sur un échantillon si n > 2000 (dist() est O(n²))
n_cah <- min(nrow(data_scaled), 1500)
set.seed(42)
idx_cah     <- sample(nrow(data_scaled), n_cah)
dist_matrix <- dist(data_scaled[idx_cah, ])
hc          <- hclust(dist_matrix, method = "ward.D2")

plot(hc, labels = FALSE, main = "Dendrogramme CAH (Ward.D2)")
groups <- cutree(hc, k = 3)
table(groups)

fviz_dend(hc,
          k           = 3,
          rect        = TRUE,
          rect_fill   = TRUE,
          rect_border = "jco",
          show_labels = FALSE)

# Superposition ACP + Clusters K-means
fviz_cluster(km,
             data         = res.pca$ind$coord,
             geom         = "point",
             ellipse.type = "convex",
             ggtheme      = theme_minimal(),
             main         = "Clusters dans l'espace ACP")

# ============================================
# EXPORTATION
# ============================================
write.csv(data_clean, "resultats_clusters.csv", row.names = FALSE)
cat("\nFichier exporté : resultats_clusters.csv\n")
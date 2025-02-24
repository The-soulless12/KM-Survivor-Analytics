library(FactoMineR)
library(factoextra)
library(readxl)
library(dplyr)
library(tidyr)
library(dbscan)
library(kohonen)
library(openxlsx)

getwd()
# setwd("chemin absolu")

# Lecture du fichier excel
data <- read_excel("KM_data.xlsx")

# Traitement des cases vides dans le dataset
colSums(is.na(data))
data <- data %>% 
  mutate(across(everything(), ~replace(., is.na(.), "None")))

# Gestion des joueurs ayant participés plus d'une fois
data <- data %>%
  mutate(Joueur = ifelse(Already == 1, 
                         Nom,  # Si Already == 1, on garde juste le Nom
                         paste(Nom, "_S", Saison, sep = "")))  # Sinon, on concatène Nom avec "_S" + Saison

# --------------------------------------- Gestion de la colonne des avantages
# On récupère tous les avantages existants pour les étudier
liste_avantages <- data %>%
  separate_rows(Avantages, sep = ",") %>%  
  mutate(Avantages = trimws(Avantages)) %>% 
  filter(Avantages != "None") %>% # On retire la valeur "None"
  distinct(Avantages)  # On ne garde que les avantages uniques
print(liste_avantages) 

# Définition d'un Score pour chaque avantage à partir de la liste obtenue
Score_avantages <- c(
  "Verrouille vote" = 1, "Double vote" = 1, "Demi Collier" = 2, "Miroir" = 4.5,
  "Collier" = 4, "Steal vote" = 1.5, "Bloque vote" = 1, "Miroir Malefique" = 3.25,
  "De d'immunite" = 3, "L'epee du prince noir (Totem)" = 4, "Couronne (Collier)" = 4,
  "Fragment (3 votes)" = 2.5, "Demi diamant" = 3.75, "Exit" = 3, "Super Idol" = 5
)

# Séparer les avantages et calculer le Score pour chaque joueur
data_avantages <- data %>%
  separate_rows(Avantages, sep = ",") %>% 
  mutate(Avantages = trimws(Avantages)) %>%
  filter(Avantages != "None") %>% 
  mutate(Score_indiv = Score_avantages[Avantages]) %>% 
  group_by(Joueur, Saison) %>%
  summarise(Score_indiv = sum(Score_indiv, na.rm = TRUE), .groups = "drop")
print(data_avantages)

# Compter la somme des Scores par saison
avantages_par_saison <- data_avantages %>%
  group_by(Saison) %>%
  summarise(
    Total_Saison = sum(Score_indiv, na.rm = TRUE) 
  )
print(avantages_par_saison)

data <- data %>%
  left_join(data_avantages, by = c("Joueur", "Saison")) %>%  
  mutate(Score_indiv = replace_na(Score_indiv, 0))

data <- data %>%
  left_join(avantages_par_saison, by = "Saison") # %>% 

data <- data %>%
  mutate(Note_avantages = Score_indiv / Total_Saison)

# --------------------------------------- Gestion de la colonne du classement
# On associe chaque saison à son nombre de participants
nb_participants <- c(18, 18, 21, 24, 20, 21, 18, 16, 16)
participants_saison <- data.frame(
  Saison = 1:9,  
  Nb_Participants = nb_participants
)

# On normalise la colonne Classement
data <- data %>%
  mutate(Saison = as.integer(Saison)) %>%
  left_join(participants_saison, by = "Saison") %>%
  mutate(Classement = 1 - ((as.numeric(Classement) - 1) / as.numeric(Nb_Participants))) %>%
  select(-Nb_Participants)

# --------------------------------------- Gestion de la colonne Swap_Team
data <- data %>%
  mutate(Swap = ifelse(Swap_Team == "None", 0, 1))

# --------------------------------------- Gestion de la colonne Epreuves_solo
# Calcul du total des épreuves solo par saison
epreuves_solo_par_saison <- data %>%
  group_by(Saison) %>%
  summarise(Total_Epreuves_Solo = sum(as.numeric(epreuves_solo), na.rm = TRUE))

# Normalisation des épreuves solo par joueur
data <- data %>%
  left_join(epreuves_solo_par_saison, by = "Saison") %>%
  mutate(Nb_solo_epreuves = as.numeric(epreuves_solo) / Total_Epreuves_Solo)

# --------------------------------------- Nettoyage des colonnes inutilisables
data <- data %>%
  select(-Nom, -Already, -Avantages, -Score_indiv, -Total_Saison, -epreuves_solo,
         -Total_Epreuves_Solo)

View(data)

# --------------------------------------- Analyse en composantes principales
# On transforme les colonnes en valeurs quantitatives
data$Merge <- as.numeric(data$Merge)
data$epreuves_team <- as.numeric(data$epreuves_team)
data$Note_strategie <- as.numeric(data$Note_strategie)
data$Note_social <- as.numeric(data$Note_social)
data$Note_epreuves <- as.numeric(data$Note_epreuves)

ACP <- PCA(data %>% select(-Joueur, -Saison, -Team, -Swap_Team), scale.unit = TRUE, graph = FALSE)

# Les valeurs propres + Inerties 
val_propres <- ACP$eig
print(val_propres)
fviz_screeplot(ACP, addlabels = TRUE, ylim = c(0, 50))

# Projection des individus
p <- fviz_pca_ind(ACP, 
                  geom.ind = "point",     
                  col.ind = "black",       
                  label = "none",   
                  title = "Projection des joueurs"
)
print(p)
ggsave(paste0("./Graphiques/Résultats_ACP.png"), plot = p)

# Ajout du nom des joueurs dans les graphiques
d <- p + geom_text(aes(label = data$Joueur), size = 3, vjust = -0.5)
print(d)

# Projections des variables 
r <- fviz_pca_var(ACP, 
             col.var = "black", 
             axes = c(1, 2), 
             addlabels = TRUE, 
             legend.title = "¨Projections des variables"
)
print(r)
ggsave(paste0("./Graphiques/Cercle_corrélation.png"), plot = r)

# Valeurs des projections des variables
projections_variables <- data.frame(Variable = rownames(ACP$var$coord),
                                    Axe1 = ACP$var$coord[, 1],
                                    Axe2 = ACP$var$coord[, 2])
print(projections_variables)
write.xlsx(projections_variables, file = "./Tableaux/projections_variables.xlsx")

# Positionnement sur le nuage des individus
# Affichage des joueurs par Saisons
plots <- list()
for (season in 1:9) {
  s <- p + ggtitle(paste("Candidats de la saison ",season))
  players <- data %>% filter(Saison == season) %>% pull(Joueur)
  
  for (name in players) {
    index <- which(data$Joueur == name) 
    coord <- ACP$ind$coord[index, ] 
    
    s <- s + 
      annotate("point", x = coord[1], y = coord[2], color = "red", size = 2) +
      annotate("text", x = coord[1], y = coord[2], label = name, vjust = -1, color = "red", size = 3)
  }
  plots[[season]] <- s
  print(s)
  ggsave(paste0("./Graphiques/joueurs_saison_", season, ".png"), plot = s)
}

# Affichage des joueurs par Equipe
season_colors <- list(
  "1" = list("Macao" = "red", "Nayoco" = "blue"),
  "2" = list("Kayac" = "red", "Ekeloa" = "blue", "Tao" = "yellow"),
  "3" = list("Benjoh" = "red", "Lantakoh" = "yellow"),
  "3_Swap" = list("Scols" = "blue", "Himath" = "purple"),
  "4" = list("Tingi" = "green", "Vanai" = "red", "Kentoh" = "blue", "Mattai" = "yellow"),
  "4_Swap" = list("Lampan" = "orange", "Wakpan" = "purple"),
  "5" = list("Vilains" = "red", "Heros" = "blue"),
  "5_Swap" = list("Legendes" = "yellow", "Icones" = "purple"),
  "6" = list("Vepo" = "blue", "Midac" = "yellow", "Owtia" = "red"),
  "6_Swap" = list("Macayoco" = "orange", "Ratvan" = "green", "Lambok" = "purple"),
  "7" = list("Juwai" = "blue", "Palampan" = "orange"),
  "7_Swap" = list("Makunbo" = "grey", "Shawai" = "red"),
  "8" = list("Lokais" = "blue", "Oro" = "yellow"),
  "8_Swap" = list("Voompai" = "brown", "Pitjan" = "red"),
  "9" = list("Vedi" = "green", "Sillow" = "purple"),
  "9_Swap" = list("Yighi" = "yellow", "Tampala" = "blue")
)

for (season in 1:9) {
  for (type in c("Team", "Swap_Team")) {
    key <- ifelse(type == "Team", as.character(season), paste0(season, "_Swap"))
    if (!key %in% names(season_colors)) next
    
    s <- p + ggtitle(paste("Candidats de la saison", season, "-", type))
    players <- data %>% filter(Saison == season) %>% pull(Joueur)
    
    for (name in players) {
      index <- which(data$Joueur == name)
      coord <- ACP$ind$coord[index, ]
      team <- data[[type]][index]  # Récupération de la bonne colonne
      
      if (length(team) > 1) {
        team <- team[1]
      }
      color <- ifelse(team %in% names(season_colors[[key]]), season_colors[[key]][[team]], "black")
      
      s <- s + 
        annotate("point", x = coord[1], y = coord[2], color = color, size = 2) +
        annotate("text", x = coord[1], y = coord[2], label = name, vjust = -1, color = color, size = 3)
    }
    
    plots[[length(plots) + 1]] <- s
    print(s)
    filename <- ifelse(type == "Swap_Team", paste0("./Graphiques/equipes_swap_saison_", season, ".png"), 
                       paste0("./Graphiques/equipes_saison_", season, ".png"))
    
    ggsave(filename, plot = s)
  }
}

# --------------------------------------- Clustering par densité DBSCAN
data_clustering <- data %>%
  select(Merge, Classement, epreuves_team, Note_strategie, Note_social,
         Note_avantages, Swap, Nb_solo_epreuves, Note_epreuves) %>%
  scale()  # Normalisation des données

# On détermine la valeur de EPS via la méthode du coude
kNNdistplot(data_clustering, k = 9) # MINPTS = Nb variables
abline(h = 0.3, col = "red", lty = 2) 
# Après avoir déterminé le coude, EPS = 2

dbscan_result <- dbscan(data_clustering, eps = 2, minPts = 9)
data$cluster <- as.factor(dbscan_result$cluster)

p_dbscan <- fviz_pca_ind(ACP, 
                         geom.ind = "point",     
                         col.ind = data$cluster,
                         label = "none",   
                         title = "Clustering DBSCAN des joueurs"
)
print(p_dbscan)
ggsave(paste0("./Graphiques/clustering_DBSCAN.png"), plot = p_dbscan)

# Récapitulatif
recap_DBSCAN <- data %>%
  group_by(cluster) %>%
  summarise(Joueurs = paste(Joueur, collapse = ", "), .groups = 'drop')

View(recap_DBSCAN)
write.xlsx(recap_DBSCAN, file = "./Tableaux/recap_DBSCAN.xlsx")

# --------------------------------------- Cartes Auto-Organisatrices SOM
data_som <- data %>%
  select(-Joueur, -Saison, -Team, -Swap_Team) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()  # Normalisation des données

# Définition de la grille SOM
som_grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
som_model <- som(data_som, grid = som_grid, rlen = 100, alpha = c(0.05, 0.01))

# Récapitulatif 
affectation_neurone <- som_model$unit.classif
affectes_par_neurone <- data.frame(Neurone = 1:length(affectation_neurone), Joueur = data$Joueur, ID_Neurone = affectation_neurone)

recap_SOM <- affectes_par_neurone %>%
  group_by(ID_Neurone) %>%
  summarise(Joueurs = paste(Joueur, collapse = ", "), .groups = 'drop')

View(recap_SOM)
write.xlsx(recap_SOM, file = "./Tableaux/recap_SOM.xlsx")

# Carte des Clusters
png("Graphiques/carte_clusters.png")
plot(som_model, type = "mapping", col = "white", main = "Carte des Clusters") 
neurone_coords <- som_model$grid$pts
text(neurone_coords[, 1], neurone_coords[, 2], labels = recap_SOM$ID_Neurone, col = "red", cex = 1.2)
dev.off()
# Carte des Poids
png("Graphiques/carte_poids.png")
plot(som_model, type = "codes", main = "Carte des Poids")
dev.off()
# Carte des Densités
png("Graphiques/carte_densites.png")
plot(som_model, type = "counts", main = "Carte des Densités")
dev.off()
# Carte des Distances
png("Graphiques/carte_distances.png")
plot(som_model, type = "dist.neighbours", main = "Carte des Distances")
dev.off()

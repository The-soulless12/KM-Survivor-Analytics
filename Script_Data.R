library(FactoMineR)
library(factoextra)
library(readxl)
library(dplyr)
library(tidyr)

getwd()
setwd("C:/Users/admin/Documents/RAYANE DOSSIER ETUDES/KM-Survivor-Analytics")

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
  mutate(
    Swap = ifelse(Swap_Team == "None", 0, 1)
  )

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
                  col.ind = "blue",       
                  label = "none",   
                  title = "Projection des joueurs"
)
#p <- p + geom_text(aes(x = ACP$ind$coord[, 1], 
                   #y = ACP$ind$coord[, 2], 
                   #label = data$Joueur), 
                   #vjust = -1, 
                   #color = "orange", 
                   #size = 4)
print(p)

# Contributions des individus pour les 2 premiers axes
fviz_contrib(ACP, choice = "ind", axes = 1, top = 10)
fviz_contrib(ACP, choice = "ind", axes = 2, top = 10)

# Projections des variables 
fviz_pca_var(ACP, 
             col.var = "black", 
             axes = c(1, 2), 
             addlabels = TRUE, 
             legend.title = "¨Projections des variables"
)

# Contributions des variables pour les 5 premiers axes
fviz_contrib(ACP, choice = "var", axes = 1, top = 10)
fviz_contrib(ACP, choice = "var", axes = 2, top = 10)

# Positionnement sur le nuage 
players <- data %>% filter(Classement == 1) %>% pull(Joueur)

for (name in players) {
  index <- which(data$Joueur == name) 
  coord <- ACP$ind$coord[index, ] 
  
  p <- p + 
    annotate("point", x = coord[1], y = coord[2], color = "red", size = 4) +
    annotate("text", x = coord[1], y = coord[2], label = name, vjust = -1, color = "red")
}
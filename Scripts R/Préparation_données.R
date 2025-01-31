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

# --------------------------------------- Les colonnes Non utilisables pour l'analyse
data_old <- data # Sauvegarde du tableau pour plus tard
data <- data %>%
  select(-Nom, -Already, -Saison, -Team, -Swap_Team, -Joueur, -Avantages, -Score_indiv,
         -Total_Saison, -epreuves_solo, -Total_Epreuves_Solo)

View(data)

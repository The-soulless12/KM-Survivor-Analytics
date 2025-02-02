# KM-Survivor-Analytics
Analyse des profils des candidats d’un jeu d’aventure à l’aide de l’Analyse en Composantes Principales (ACP), du clustering par densité (DBSCAN) et des Cartes Auto-Organisatrices (SOM) en langage R.

# Fonctionnalités
- Réalisation d'une Analyse en Composantes Principales (ACP) dans le but d'explorer les données et de projeter les joueurs sur un espace 2D en fonction de leurs caractéristiques.
- Application du Clustering par Densité (DBSCAN) afin de regrouper les candidats selon leurs profils.
- Utilisation de Cartes Auto-Organisatrices (SOM) pour une visualisation des données dans un espace réduit.
- Interface interactive permettant d'afficher divers graphiques et de filtrer les résultats par saison.

# Structure du projet
- index.html : Fichier contenant le code source de l'interface utilisateur.
- Graphiques/ : Dossier contenant les graphiques générés par le script en R.
- Tableaux/ : Dossier contenant certaines tables produites par le script en R.
- Km_data.xlsx : Fichier Excel contenant les données des joueurs.
- Script Analyse KM.R : Script en R réalisant l'ACP ainsi que les techniques de clustering DBSCAN et SOM sur les données.

# Prérequis
- Langage R.
- Les packages : FactoMineR, factoextra, readxl, dplyr, tidyr, dbscan, kohonen & openxlsx.
- Navigateur internet.

# Note
- Concernant le code R, il est nécessaire de placer le fichier Excel `Km_data.xlsx` dans le répertoire indiqué par `getwd()` au début du script. Si votre fichier se trouve dans un autre emplacement, vous devrez décommenter la ligne `#setwd("chemin absolu")` et y indiquer le chemin absolu du répertoire contenant le fichier Excel.

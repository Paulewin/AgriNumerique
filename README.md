# AgriNumerique / Projet en Science de Données au féminin en Afrique francophone (SDA)

 **La solution numérique inclusive pour les producteurs béninois**

## Présentation du Projet
Ce dépôt contient les travaux de recherche et de développement menés dans le cadre du travail de recherche de fin de formation en sciences de données. Le but visé est l'amérioration de la résilience des producteurs face au changement climatique. Le projet s'articule autour de deux grands axes :
1. Analyses Statistiques 
2. Développement du MVP
   
##  Démarche Scientifique & Objectifs Spécifiques

Le design de cette solution numérique repose sur une phase de simulation et d'analyse de données quantitatives et qualitatives  autour de trois objectifs spécifiques fondamentaux. Ce travail statistique a permis de cartographier précisément la demande et les freins des producteurs.

### 1. Analyse Socio-Démographique
- **Objectif :** Caractériser le profil socio-économique et le système de production des agriculteurs enquêtés.
- **Résultat statistique :** Mise en évidence que la structuration des systèmes de production ne sont pas strictement liées aux caractéristiques sociales ou territoriales des producteurs..

### 2. Perception et adaptation au changement climatique  
- **Objectif :** Evaluer la perception des producteurs par rapport aux impacts du changement climatique sur la production agricole et leurs niveaux d’adaptation
- **Résultat statistique :** 91 % des producteurs ressentent intensément les changements climatiques et leurs conséquences. Les producteurs ayant un niveau d’instruction secondaire ou supérieur présentent des indices moyens
d’adaptation plus élevés que ceux du primaire ou sans instruction.
  
### 3. Obstacles au Numérique 
- **Objectif :** Évaluer le niveau d’accès des producteurs à l’information climatique et les obstacles à l’adoption de solutions numériques et agricoles.
- **Résultat statistique :** Le test de Fisher révèle de relation statistiquement significative entre la possession d’un téléphone et le type de culture pratiqué. Les obstacles, peu importe la culture frappent tout le monde avec la même intensité relative

### 4. Besoins des producteurs en terme de Numérique
- **Objectif :** Évaluer l'intérêt des producteurs pour l'intégration des technologies dans leur quotidien et définir le type d'outils qu'ils réclament.
- **Résultat statistique :** Une forte demande émerge pour une application qui fournit les conseils techniques de production en langue locale, perçus comme des leviers prioritaires face au changement climatique.



##  Du Besoin Terrain à la Solution Numérique (MVP)

Les conclusions de nos analyses statistiques R  ont directement dicté le cahier des charges et l'architecture de notre **Minimum Viable Product (MVP)** :

| Ce que l'analyse statistique a révélé | La réponse technique intégrée dans le MVP |
| :--- | :--- |
| **Obstacle :** Zones blanches récurrentes & accès internet limité |  **Architecture Offline-First :** L'application fonctionne à 100% sans réseau dans les champs et se synchronise uniquement lors des passages en zone connectée. |
| **Profil :** Producteurs traditionnels à faible niveau d'étude |  **Interface Inclusive :** Utilisation d'une sémiotique visuelle (icônes) et des modules d'assistance vocale en langues locales. |
| **Demande :** Besoin d'adaptation climatique et de suivi de production |  **OAD Spatio-Temporel :** Module de cartographie parcellaire (géospatial couplé à un calendrier agricole dynamique indexé sur les dates de semis et les alertes météo locales. |


##  Fonctionnalités du MVP
- Prédire les périodes favorables de semis ;
- Fournir des calendriers agricoles adaptés aux différentes zones ;
- Recommander les semences les mieux adaptées aux conditions climatiques ;
- Mettre les producteurs en relation avec des experts agricoles ;
- Diffuser des conseils techniques dans les langues locales.

## Structure du Repo
L'ensemble du code de nettoyage, simulation et traitement des données se trouve dans le dossier `/scripts`.
Les bases de données sont dans le dossier `/data`
Le dossier `/report` contient les versions rmd et pdf du rapport d'analyse 
Les niveaux de maturité technologiques et le MVP sont enregistrés dans le dossier app

##  Technologies utilisées pour le MVP
 L'architecture est exécutée via le script run_app.R. Ce fichier fait office de point d'entrée unique : il configure le serveur R Shiny.


### Packages R Prérequis 

Pour exécuter le MVP (`run_app.R`) et générer automatiquement les rapports d'analyses au format PDF, copiez et lancez cette commande unique dans votre console RStudio :

```R
install.packages(c(
  # --- Infrastructure MVP ---
  "shiny", 
  
  # --- Analyses Statistiques & Données ---
  "FactoMineR",   # Pour l'Analyse des Correspondances Multiples (ACM)
  "factoextra",   # Pour la visualisation des cartes factorielles
  "dplyr",        # Pour la manipulation et le nettoyage des données d'enquête
  "readr",        # Pour l'importation rapide des fichiers de données (.csv)
  
  # --- Visualisation & Rapports ---
  "ggplot2",      # Pour les graphiques descriptifs personnalisés
  "gtsummary",    # Pour générer de magnifiques tableaux socio-démographiques pros
  "rmarkdown",    # Pour la structure textuelle du rapport final
  "knitr",        # Pour la compilation dynamique du code dans le rapport
  "tinytex"       # Moteur LaTeX léger indispensable pour exporter en PDF
))

# CRUCIAL : Après l'installation, exécutez cette ligne une seule fois 
# pour finaliser l'installation des outils PDF en tâche de fond :
tinytex::install_tinytex()
```

##  Auteures
- **Paule DENAKPO** - *Analyses & Conception* - 
- **Bonifacia DEDEGBE** - *Développement & Intégration*
- **Oul'fath BOURAÏMA** - *Analyses* 

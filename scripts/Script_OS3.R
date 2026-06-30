# OUVRIR La base de données simulées
library(readr)
simulation <- read.csv("Simulation_AgriN.csv")
View(simulation)


# convertir les colonnes non numériques en factor
simulation$sexe <- as.factor(simulation$sexe)
simulation$age <- as.factor(simulation$age)
simulation$etude <- as.factor(simulation$etude)
simulation$vilage <- as.factor(simulation$vilage)
simulation$commune <- as.factor(simulation$commune)
simulation$type_agri <- as.factor(simulation$type_agri)
simulation$culture1 <- as.factor(simulation$culture1)
simulation$culture2 <- as.factor(simulation$culture2)
simulation$culture3 <- as.factor(simulation$culture3)
simulation$unite_superficie_maïs <- as.factor(simulation$unite_superficie_maïs)
simulation$unite_superficie_total <- as.factor(simulation$unite_superficie_total)
simulation$expérience <- as.factor(simulation$expérience)
simulation$phenomene_clima_obs <- as.factor(simulation$phenomene_clima_obs)
simulation$liste_phenomene_clima_obs <- as.factor(simulation$liste_phenomene_clima_obs)
simulation$moyen_gestion_risques <- as.factor(simulation$moyen_gestion_risques)
simulation$moyen1 <- as.factor(simulation$moyen1)
simulation$Moyen2 <- as.factor(simulation$Moyen2)
simulation$Moyen3 <- as.factor(simulation$Moyen3)
simulation$possession_telephone <- as.factor(simulation$possession_telephone)
simulation$acces_internet <- as.factor(simulation$acces_internet)
simulation$Utilisation_outils_numeriques <- as.factor(simulation$Utilisation_outils_numeriques)
simulation$precisez_lesquels <- as.factor(simulation$precisez_lesquels)
simulation$frequence_appli_mobiles <- as.factor(simulation$frequence_appli_mobiles)
simulation$frequence_plateformes_web. <- as.factor(simulation$frequence_plateformes_web.)
simulation$frequence_SMS_alerte.pretoce. <- as.factor(simulation$frequence_SMS_alerte.pretoce.)
simulation$frequence_reseaux_sociaux.agricoles <- as.factor(simulation$frequence_reseaux_sociaux.agricoles)
simulation$frequence_autres_outils <- as.factor(simulation$frequence_autres_outils)
simulation$obstacles_au_numerique <- as.factor(simulation$obstacles_au_numerique)
simulation$informations_souhaitee_recevoir <- as.factor(simulation$informations_souhaitee_recevoir)
simulation$forme_reception_préfere <- as.factor(simulation$forme_reception_préfere)
simulation$langues_adaptes <- as.factor(simulation$langues_adaptes)
simulation$Langue_locale_majoritaire <- as.factor(simulation$Langue_locale_majoritaire)
simulation$fonctionalite_application <- as.factor(simulation$fonctionalite_application)
simulation$caracteristique_principale_application <- as.factor(simulation$caracteristique_principale_application)
simulation$gestionnaire <- as.factor(simulation$gestionnaire)
simulation$appli_numerique_gere_aleas_climatiques <- as.factor(simulation$appli_numerique_gere_aleas_climatiques)
simulation$conditions_utilisation <- as.factor(simulation$conditions_utilisation)
simulation$contribution_symbolique <- as.factor(simulation$contribution_symbolique)
simulation$suggestions <- as.factor(simulation$suggestions)

# Analyse Univariée (le "Tri à plat") pour dresser le panorama global de l'accès à l'information et des obstacles
summary(simulation[, 22:31])


# création d'un graphique pour montrerla possession de telephone et l'accès à internet
install.packages("ggplot2")
library(ggplot2)
ggplot(simulation, aes(x = possession_telephone, fill = possession_telephone)) + geom_bar(position = "dodge") + labs(title = "Possession d'un téléphone", x = "Possède un téléphone", y = "Proportion", fill = "Accès à l'info") + theme_minimal()

ggplot(simulation, aes(x = acces_internet, fill = acces_internet)) + geom_bar(position = "dodge") + labs(title = "Accès à Internet", x = "A accès à Internet", y = "Nombre de producteurs", fill = "Accès à l'info") + theme_light()

# Tableau croisé pour montrer l'accès à internet et au téléphone selon le sexe 
tab_sexe_tel <- table(simulation$sexe, simulation$possession_telephone)
View(tab_sexe_tel)
tab_sexe_net <- table(simulation$sexe, simulation$acces_internet)
View(tab_sexe_net)
cat("Pourcentage de possession de téléphone par sexe \n")
round(prop.table(tab_sexe_tel, 1) * 100, 1)
cat("Pourcentage d'accès à internet par sexe \n")
round(prop.table(tab_sexe_net, 1) * 100, 1)

# Tableau croisé pour montrer l'accès à internet et au téléphone selon le niveau d'instruction
tab_scol_tel <- table(simulation$etude, simulation$possession_telephone)
View(tab_scol_tel)
tab_scol_net <- table(simulation$etude, simulation$acces_internet)
View(tab_scol_net)
cat("Accès à internet selon le niveau d'instruction\n")
round(prop.table(tab_scol_net, 1) * 100, 1)
cat("Possession de telephone selon le niveau d'instruction \n")
round(prop.table(tab_scol_tel, 1) * 100, 1)

# Tableau croisé pour montrer l'accès à internet et au téléphone selon la commune
cat(" Score moyen de maturité numérique par commune \n") 
aggregate(score_maturite ~ commune, data = simulation, FUN = mean)



# Les tests de dépendance
test_fisher <- fisher.test(table(simulation$acces_internet, simulation$possession_telephone))
print(test_fisher)

test_fisher_scol_net <- fisher.test(table(simulation$etude, simulation$acces_internet))
cat(" Résultat du Test de Fisher pour accès internet selon l'instruction \n")
print(test_fisher_scol_net)

test_fisher_scol_tel <- fisher.test(table(simulation$etude, simulation$possession_telephone))
cat(" Résultat du Test de Fisher pour possession de telephone selon l'instruction \n")
print(test_fisher_scol_tel)

test_fisher_com_net <- fisher.test(table(simulation$commune, simulation$acces_internet))
cat(" Résultat du Test de Fisher pour accès internet selon la commune \n")
print(test_fisher_com_net)

test_fisher_com_tel <- fisher.test(table(simulation$commune, simulation$possession_telephone))
cat(" Résultat du Test de Fisher pour possession de telephone selon la commune \n")
print(test_fisher_com_tel)


test_fisher_culture_tel <- fisher.test(table(simulation$culture1, simulation$possession_telephone))
cat(" Résultat du Test de Fisher pour possession de telephone selon le type de culture \n")
print(test_fisher_culture_tel)

test_fisher_culture_net <- fisher.test(table(simulation$culture1, simulation$acces_internet))
cat(" Résultat du Test de Fisher pour accès internet selon le type de culture \n")
print(test_fisher_culture_net)

## montrer les résultats dans un graphique
library(ggplot2)

# Graphique pour voir la Commune qui favorise l'accès à internet
ggplot(simulation, aes(x = commune, fill = acces_internet)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Disparité d'accès à Internet par Commune",
       y = "Pourcentage", x = "Communes") +
  theme_minimal()

# Graphique pour voir le type de culture qui favorise la possession de telephone
library(ggplot2)
ggplot(simulation, aes(x = culture1, fill = possession_telephone)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Disparité de possession de telephone par culture",
       y = "Pourcentage", x = "Communes") +
  theme_minimal()

# . Créer une typologie (Le profil du "Producteur Connecté") classement du plusconnecté au moins connecté
simulation$connecte_num <- ifelse(simulation$acces_internet == "Oui", 1, 0)
aggregate(connecte_num ~ sexe, data = simulation, FUN = mean)
aggregate(connecte_num ~ etude, data = simulation, FUN = mean)
aggregate(connecte_num ~ age, data = simulation, FUN = mean)
aggregate(connecte_num ~ culture1, data = simulation, FUN = mean)
aggregate(superficie_total_finale ~ acces_internet, data = simulation, FUN = mean)
typologie_finale <- aggregate(connecte_num ~ sexe + etude + age + commune + culture1, data = simulation, FUN = mean)
typologie_finale <- typologie_finale[order(-typologie_finale$connecte_num), ]
print(typologie_finale)

## Utilisation des outils numériques
# Transformation de la colonne précisez_lesquels
simulation$precisez_lesquels[simulation$Utilisation_outils_numeriques == "Non"] <- "Aucun"

# Transformation des autres colonnes de frequence d'utilisation
outils <- c("frequence_appli_mobiles", "frequence_plateformes_web.", "frequence_SMS_alerte.pretoce.", "frequence_reseaux_sociaux.agricoles", "frequence_autres_outils")
for (nom in outils) { simulation[[nom]][simulation$Utilisation_outils_numeriques == "Non"] <- "Jamais" }
simulation$frequence_appli_mobiles[simulation$frequence_appli_mobiles == "jamais"] <- "Jamais"
simulation$frequence_plateformes_web.[simulation$frequence_plateformes_web. == "jamais"] <- "Jamais"


## analyse de la frequence, du rythme d'utilisation des outils
# transformation des colonnes en frequences ordonnées
ordre_frequence <- c("Jamais", "Rarement", "Mensuellement", "Hebdomadairement","Quotidiennement" )
for (nom in outils) { simulation[[nom]] <- factor(simulation[[nom]], levels = ordre_frequence, ordered = TRUE) }
summary(simulation[, c("frequence_appli_mobiles", "frequence_plateformes_web.", "frequence_SMS_alerte.pretoce.", "frequence_reseaux_sociaux.agricoles", "frequence_autres_outils")])


# Transformer les fréquences en scores numériques
simulation$score_appli_mobile <- as.numeric(as.factor(simulation$frequence_appli_mobiles))
simulation$score_plateforme_web <- as.numeric(as.factor(simulation$frequence_plateformes_web.))
simulation$score_SMS_alerte <- as.numeric(as.factor(simulation$frequence_SMS_alerte.pretoce.))
simulation$score_resaux_sociaux <- as.numeric(as.factor(simulation$frequence_reseaux_sociaux.agricoles))
simulation$score_autre_outil <- as.numeric(as.factor(simulation$frequence_autres_outils))


# Comparer les moyennes par groupe (par Commune et par culture)
aggregate(score_appli_mobile ~ commune, data = simulation, FUN = mean)
aggregate(score_plateforme_web ~ commune, data = simulation, FUN = mean)
aggregate(score_SMS_alerte ~ commune, data = simulation, FUN = mean)
aggregate(score_resaux_sociaux ~ commune, data = simulation, FUN = mean)
aggregate(score_autre_outil ~ commune, data = simulation, FUN = mean)

# Comparer les moyennes par groupe ( par culture)
aggregate(score_appli_mobile ~ culture1, data = simulation, FUN = mean)
aggregate(score_plateforme_web ~ culture1, data = simulation, FUN = mean)
aggregate(score_SMS_alerte ~ culture1, data = simulation, FUN = mean)
aggregate(score_resaux_sociaux ~ culture1, data = simulation, FUN = mean)
aggregate(score_autre_outil ~ culture1, data = simulation, FUN = mean)

# analyse des moyennes pour les comparer les moyennes entre scores et commune
anova_appli_comune <- aov(score_appli_mobile ~ commune, data = simulation) 
summary(anova_appli_comune)

anova_web_comune <- aov(score_plateforme_web ~ commune, data = simulation) 
summary(anova_web_comune)

anova_SMS_comune <- aov(score_SMS_alerte ~ commune, data = simulation) 
summary(anova_SMS_comune)

anova_reseau_comune <- aov(score_resaux_sociaux ~ commune, data = simulation) 
summary(anova_reseau_comune)

anova_autre_outil_comune <- aov(score_autre_outil ~ commune, data = simulation) 
summary(anova_autre_outil_comune)


# analyse des moyennes pour les comparer les moyennes entre scores et culture
anova_appli_culture <- aov(score_appli_mobile ~ culture1, data = simulation) 
summary(anova_appli_comune)

anova_web_culture <- aov(score_plateforme_web ~ culture1, data = simulation) 
summary(anova_web_comune)

anova_SMS_culture <- aov(score_SMS_alerte ~ culture1, data = simulation) 
summary(anova_SMS_comune)

anova_reseau_culture <- aov(score_resaux_sociaux ~ culture1, data = simulation) 
summary(anova_reseau_comune)

anova_autre_outil_culture <- aov(score_autre_outil ~ culture1, data = simulation) 
summary(anova_autre_outil_comune)
#Fréquence (ANOVA) n'est pas significatif alors que l'usage des outls numériques l'est, cela veut dire que la barrière est l'accès (avoir l'outil), mais qu'une fois qu'ils l'ont, ils l'utilisent tous de la même façon.

# Visualiser la comparaison (Le "Radar Chart" ou Barres)

# Classement des obstacles par ordre croissant 
table_obstacle <- table(simulation$obstacles_au_numerique) 
table_obstacle_trie <- sort(table_obstacle, decreasing = TRUE) 
pourcent_obstacle <- prop.table(table_obstacle_trie) * 100 
print(pourcent_obstacle)

obstacles_liste <- c("Manque de réseau", "Cout d'accès à internet", "Manque de formation", "Langue/Compréhension", "Manque de pertinence des informations", "Autre")
counts <- sapply(obstacles_liste, function(x) sum(grepl(x, simulation$obstacles_au_numerique, ignore.case = TRUE)))
pourcent_obstacle <- (counts / nrow(simulation)) * 100
print(pourcent_obstacle)

# création d'un graphique qui montre les obstacles
df_graph <- data.frame( Obstacle = obstacles_liste, Nombre = sapply(obstacles_liste, function(x) sum(grepl(x, simulation$obstacles_au_numerique, ignore.case = TRUE))) )
ggplot(df_graph, aes(x = reorder(Obstacle, Nombre), y = Nombre, fill = Obstacle)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Principaux obstacles au numérique", x = "Obstacles", y = "Nombre de citations") + theme_minimal() + theme(legend.position = "none")

# graphique pour montrer les obstacles selon le type de culture (ne pas utiliser)
install.packages("dplyr")
library(dplyr)
cultures <- unique(simulation$culture1) 
obstacles <- c("Manque de réseau", "Cout d'accès à internet", "Manque de formation", "Langue/Compréhension", "Manque de pertinence des informations", "Autre")

df_croise <- expand.grid(Culture = cultures, Obstacle = obstacles)
df_croise$Nombre <- mapply(function(c, o) {
  simulation %>% filter(culture1 == c, grepl(o, obstacles_au_numerique)) %>% nrow()
}, df_croise$Culture, df_croise$Obstacle)
library(ggplot2)

ggplot(df_croise, aes(x = Culture, y = Nombre, fill = Obstacle)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Obstacles au numérique selon le type de culture", x = "Type de Culture", y = "Nombre de citations") + theme_minimal()

# vérifiez si le maïs est aussi celle qui a la plus grande taille d'exploitation ou le plus haut taux de connexion :
aggregate(superficie_total_finale ~ culture1, data = simulation, FUN = mean)

total_par_culture <- as.data.frame(table(simulation$culture1)) 
colnames(total_par_culture) <- c("Culture", "Total")
df_pourcent <- merge(df_croise, total_par_culture, by = "Culture")
df_pourcent$Pourcentage <- (df_pourcent$Nombre / df_pourcent$Total) * 100
ggplot(df_pourcent, aes(x = Culture, y = Pourcentage, fill = Obstacle)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(title = "Intensité des obstacles par culture (%)", y = "Pourcentage de citation")


# C'est une excellente nouvelle pour votre analyse, car cela confirme la cohérence de vos résultats précédents (ceux où vos p-values étaient supérieures à 0.5).

#L'implication est la suivante : Les obstacles (réseau, coût) sont uniformes. Ils frappent tout le monde avec la même intensité relative, peu importe la culture. Il n'y a pas une filière plus "sacrifiée" qu'une autre.
# Cela confirme les résultats des tests d'inférence ($p > 0.5$) : les barrières à l'adoption sont structurelles et ne dépendent pas du type de spéculation agricole.

# Puisque nous avons vu que les obstacles sont assez homogènes, la différence se jouera sur l'adéquation entre le profil social et la solution préférée.

# Typologie des solutions
# calcul de score pour chaque colonne
information_liste <- c("Prévisions météorologiques localisées", "Alertes  précoces (sècheresse, inondation, ravageurs )", "Conseils techniques de  production", "Gestion de l'irrigation et de l'eau", "Accès au crédit ou à l' assurance agricole")
reception_liste <- c("SMS", "Message vocaux", "Application mobile", "Site web", "Réseaux sociaux", "Radio locale")
langue_liste <- c("Francais", "Images/Pictogrammes", "Audio", "Langues locales")
fonctionalite_liste <- c("D'enregistrer  vos données de culture (dates, rendements,  superficies)", "D'échanger avec d'autres producteurs", "D' accéder à des formations ou tutoriels en ligne", "De signaler des problèmes ( maladies, manque d'eau etc...)", "D' obtenir des conseils personnalisés")
gestion_liste <- c("Coopérative/groupement", "ONG", "Ministère de l'Agriculture", "Recherche", "Entreprise privée")

counts_information <- sapply(information_liste, function(x) { sum(grepl(x, simulation$informations_souhaitee_recevoir, ignore.case = TRUE)) })
counts_reception <- sapply(reception_liste, function(x) { sum(grepl(x, simulation$forme_reception_préfere, ignore.case = TRUE)) })
counts_langue <- sapply(langue_liste, function(x) { sum(grepl(x, simulation$langues_adaptes, ignore.case = TRUE)) })
counts_fonctionalite <- sapply(fonctionalite_liste, function(x) { sum(grepl(x, simulation$fonctionalite_application, ignore.case = TRUE)) })
counts_gestion <- sapply(gestion_liste, function(x) { sum(grepl(x, simulation$gestionnaire, ignore.case = TRUE)) })

df_information <- data.frame(Information = information_liste, Nombre = counts_information)
df_reception <- data.frame(Reception = reception_liste, Nombre = counts_reception)
df_langue <- data.frame(Langue = langue_liste, Nombre = counts_langue)
df_fonctionalite <- data.frame(Fonctionalite = fonctionalite_liste, Nombre = counts_fonctionalite)
df_gestion <- data.frame(Gestion = gestion_liste, Nombre = counts_gestion)

# Recommandations : Tableau de synthèse final des solutions
solution_ideale <- data.frame(
  Categorie = c("Information prioritaire", "Canal de réception", "Langue préférée", "Fonctionnalité clé", "Gestionnaire de confiance"),
  Choix_Majoritaire = c(
    df_information$Information[which.max(df_information$Nombre)],
    df_reception$Reception[which.max(df_reception$Nombre)],
    df_langue$Langue[which.max(df_langue$Nombre)],
    df_fonctionalite$Fonctionalite[which.max(df_fonctionalite$Nombre)],
    df_gestion$Gestion[which.max(df_gestion$Nombre)]
  ))

print(solution_ideale)


# Générateur de typologies de solutions
analyser_typo <- function(variable_profil) { 
  expand.grid(Profil = unique(simulation[[variable_profil]]), 
              Type = c("Information", "Reception", "Langue", "Fonctionalite", "Gestion"),
              Option = c(information_liste, reception_liste, langue_liste, fonctionalite_liste, gestion_liste )) %>%
    rowwise() %>%
    mutate(Score = sum(simulation[[variable_profil]] == Profil & 
                         (grepl(Option, df_information, ignore.case = TRUE) | 
                            grepl(Option, df_reception, ignore.case = TRUE) |
                            grepl(Option, df_langue, ignore.case = TRUE) |
                            grepl(Option, df_fonctionalite, ignore.case = TRUE) |
                            grepl(Option, df_gestion, ignore.case = TRUE))))
  }

# Analyser la typologie de solutions par culture par sexe et par commune et faire le graphe pour rédiger des fiches de solutions précises
df_typo_culture <- analyser_typo("culture1")
df_typo_sexe <- analyser_typo("sexe")
df_typo_commune <- analyser_typo("commune")
df_typo_vilage <- analyser_typo("vilage")

#REPRESENTATION graphique des solutions
ggplot(df_typo_culture, aes(x = Option, y = Score, fill = Type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Profil) + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Choix des solutions par type de culture", x = "Solutions choisies", y = "Nombre de citations")


ggplot(df_typo_sexe, aes(x = Option, y = Score, fill = Type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Profil) + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Choix des solutions par genre", x = "Solutions choisies", y = "Nombre de citations")

ggplot(df_typo_commune, aes(x = Option, y = Score, fill = Type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Profil) + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Choix des solutions par commune", x = "Solutions choisies", y = "Nombre de citations")

ggplot(df_typo_vilage, aes(x = Option, y = Score, fill = Type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Profil) + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Choix des solutions par village", x = "Solutions choisies", y = "Nombre de citations")

# Recommandations : Tableau de synthèse final des solutions

solution_ideale <- data.frame(
  Categorie = c("Information prioritaire", "Canal de réception", "Langue préférée", "Fonctionnalité clé", "Gestionnaire de confiance"),
  Choix_Majoritaire = c(
    df_information$Information[which.max(df_information$Nombre)],
    df_reception$Reception[which.max(df_reception$Nombre)],
    df_langue$Langue[which.max(df_langue$Nombre)],
    df_fonctionalite$Fonctionalite[which.max(df_fonctionalite$Nombre)],
    df_gestion$Gestion[which.max(df_gestion$Nombre)]
  ))

print(solution_ideale)


 
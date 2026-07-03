# ============================================================
# AgriNumérique — MVP Professionnel Complet
# Architecture propre, charte graphique officielle,
# API Open-Meteo, alertes intelligentes, multi-cultures prêt
# install.packages(c("shiny","bslib","jsonlite"))
# Logo : www/logo01.png
# ============================================================

library(shiny)
library(bslib)
library(jsonlite)

# ============================================================
# SECTION 1 — PALETTE OFFICIELLE (un seul endroit à modifier)
# ============================================================
COULEURS <- list(
  fond_principal   = "#0F2F1F",
  fond_secondaire  = "#F4F1EC",
  fond_carte       = "#FCFBF8",
  vert_principal   = "#2E7D5B",
  vert_secondaire  = "#3D8B63",
  vert_fonce       = "#1F6B4F",
  fond_icone       = "#E8F3EA",
  bordure          = "#D8D2C8",
  texte_principal  = "#1A1A1A",
  texte_secondaire = "#666666"
)

# ============================================================
# SECTION 2 — VILLES DU BÉNIN (coordonnées GPS Open-Meteo)
# ============================================================
VILLES_BENIN <- list(
  "Allada"      = c(6.6576, 2.1569),
  "Abomey"      = c(7.1833, 2.0167),
  "Aplahoué"    = c(6.9279, 1.6591),
  "Bohicon"     = c(7.1791, 2.0670),
  "Cotonou"     = c(6.3654, 2.4183),
  "Dassa-Zoumé" = c(7.7444, 2.1899),
  "Djougou"     = c(9.7085, 1.6660),
  "Kandi"       = c(11.1279, 2.9379),
  "Lokossa"     = c(6.6362, 1.7146),
  "Malanville"  = c(11.8704, 3.3896),
  "Natitingou"  = c(10.3067, 1.3769),
  "Nikki"       = c(9.9382, 3.2113),
  "Ouidah"      = c(6.3559, 2.0828),
  "Parakou"     = c(9.3370, 2.6283),
  "Porto-Novo"  = c(6.4969, 2.6289),
  "Savè"        = c(8.0269, 2.4871),
  "Tanguiéta"   = c(10.6282, 1.2681)
)

# ============================================================
# SECTION 3 — ITINÉRAIRES TECHNIQUES (architecture multi-cultures)
# Pour ajouter une culture : ajouter une entrée dans ITINERAIRES
# et une entrée dans CULTURES_DISPONIBLES
# ============================================================
ITINERAIRES <- list(
  
  tomate = data.frame(
    offset = c(0,7,14,21,28,35,42,49,56,63,70,77,84,105,120),
    etape  = c("Semis","Suivi","Entretien","Repiquage","Entretien",
               "Fertilisation","Protection","Entretien","Fertilisation",
               "Protection","Floraison","Nouaison","Récolte",
               "Récoltes successives","Fin de cycle"),
    action = c(
      "Semis en pépinière",
      "Contrôle de la levée",
      "Désherbage de la pépinière si nécessaire",
      "Repiquage au champ",
      "Remplacement des plants manquants",
      "1er apport d'engrais",
      "Premier traitement phytosanitaire si nécessaire",
      "Tuteurage et/ou taille",
      "2e apport d'engrais",
      "Traitement phytosanitaire selon les observations",
      "Suivi de la floraison",
      "Contrôle de la formation des fruits",
      "Début de la récolte",
      "Récoltes successives tous les 2 à 4 jours",
      "Bilan de la campagne"
    ),
    icone = c("🌱","🔍","🌿","🪴","🔄","💧","🛡️","🪢",
              "💧","🛡️","🌸","🍅","✂️","🍅","📋"),
    stringsAsFactors = FALSE
  )
  # piment = data.frame(...)  # à ajouter facilement
  # mais   = data.frame(...)
  # soja   = data.frame(...)
)

CULTURES_DISPONIBLES <- c("Tomate" = "tomate")

# ============================================================
# SECTION 4 — UTILITAIRES
# ============================================================
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a) && nchar(as.character(a)) > 0) a else b

mois_fr <- c("janv","févr","mars","avr","mai","juin",
             "juil","août","sept","oct","nov","déc")

format_date_fr <- function(d) {
  paste0(format(d, "%d"), " ", mois_fr[as.integer(format(d, "%m"))])
}

get_coordonnees <- function(localite) {
  if (!is.null(localite) && localite %in% names(VILLES_BENIN)) {
    return(VILLES_BENIN[[localite]])
  }
  VILLES_BENIN[["Cotonou"]]
}

# ============================================================
# SECTION 5 — API OPEN-METEO
# ============================================================
get_meteo <- function(lat, lon) {
  tryCatch({
    url <- paste0(
      "https://api.open-meteo.com/v1/forecast",
      "?latitude=", lat, "&longitude=", lon,
      "&daily=temperature_2m_max,temperature_2m_min,",
      "precipitation_sum,precipitation_probability_max,",
      "windspeed_10m_max,et0_fao_evapotranspiration",
      "&current_weather=true",
      "&timezone=Africa%2FAbidjan",
      "&forecast_days=7"
    )
    
    resp <- fromJSON(url, simplifyDataFrame = FALSE)
    cw   <- resp$current_weather
    dd   <- resp$daily
    
    list(
      succes         = TRUE,
      temperature    = round(cw$temperature, 1),
      vent           = round(cw$windspeed, 1),
      code_meteo     = cw$weathercode,
      temp_max_j1    = round(dd$temperature_2m_max[[1]], 1),
      temp_min_j1    = round(dd$temperature_2m_min[[1]], 1),
      pluie_j1       = round(dd$precipitation_sum[[1]], 1),
      prob_pluie_j1  = dd$precipitation_probability_max[[1]],
      vent_max_j1    = round(dd$windspeed_10m_max[[1]], 1),
      etp_j1         = tryCatch(round(dd$et0_fao_evapotranspiration[[1]],1), error=function(e) NA),
      pluie_7j       = round(sum(unlist(dd$precipitation_sum)), 1),
      prob_pluie_max = max(unlist(dd$precipitation_probability_max)),
      temp_max_7j    = max(unlist(dd$temperature_2m_max))
    )
    
  }, error = function(e) {
    list(succes = FALSE, message = as.character(e))
  })
}

interpreter_code_meteo <- function(code) {
  if (is.null(code)) return(list(texte="Inconnu", icone="🌡️"))
  if (code == 0)           return(list(texte="Ciel dégagé",           icone="☀️"))
  if (code %in% 1:3)       return(list(texte="Partiellement nuageux",  icone="⛅"))
  if (code %in% 45:48)     return(list(texte="Brouillard",             icone="🌫️"))
  if (code %in% 51:67)     return(list(texte="Pluie légère",           icone="🌦️"))
  if (code %in% 80:82)     return(list(texte="Averses",                icone="🌧️"))
  if (code %in% 95:99)     return(list(texte="Orage",                  icone="⛈️"))
  list(texte="Variable", icone="🌡️")
}

# ============================================================
# SECTION 6 — MOTEUR D'ALERTES MÉTÉO (données Open-Meteo)
# ============================================================
generer_alertes_meteo <- function(meteo, jours_depuis, stade) {
  alertes <- list()
  
  if (!meteo$succes) {
    return(list(list(
      icone="⚠️", titre="Données météo indisponibles",
      desc="Impossible de se connecter à Open-Meteo. Vérifiez votre connexion internet.",
      conseil="Les alertes seront disponibles dès que la connexion sera rétablie.",
      classe="alerte-info", niveau="Info"
    )))
  }
  
  # Fortes pluies
  if (!is.null(meteo$prob_pluie_max) && meteo$prob_pluie_max >= 60) {
    alertes[[length(alertes)+1]] <- list(
      icone  = "🌧️",
      titre  = paste0("Risque de pluie — ", meteo$prob_pluie_max, "%"),
      desc   = paste0("Probabilité de pluie importante prévue. Cumul 7 jours estimé : ",
                      meteo$pluie_7j, " mm. ",
                      if (jours_depuis < 21) "Protégez la pépinière contre l'excès d'eau."
                      else if (jours_depuis < 56) "Vérifiez le drainage de votre parcelle."
                      else "Risque de maladies fongiques élevé sur les fruits."),
      conseil = "Creuser des rigoles de drainage. Traitement fongicide préventif si nécessaire.",
      classe  = "alerte-pluie", niveau = "Élevé"
    )
  }
  
  # Sécheresse
  if (!is.null(meteo$pluie_7j) && meteo$pluie_7j < 5 &&
      !is.null(meteo$temp_max_7j) && meteo$temp_max_7j > 30) {
    alertes[[length(alertes)+1]] <- list(
      icone  = "☀️",
      titre  = paste0("Risque de sécheresse — ", meteo$temp_max_7j, "°C max"),
      desc   = paste0("Faible pluviométrie prévue (", meteo$pluie_7j, " mm / 7 jours) ",
                      "avec des températures élevées. ",
                      if (jours_depuis < 21) "Les jeunes plants nécessitent un arrosage quotidien."
                      else "Stress hydrique possible — surveillance quotidienne recommandée."),
      conseil = "Arroser tôt le matin. Pailler le sol pour conserver l'humidité.",
      classe  = "alerte-secheresse", niveau = "Modéré"
    )
  }
  
  # Chaleur excessive
  if (!is.null(meteo$temp_max_j1) && meteo$temp_max_j1 > 35) {
    alertes[[length(alertes)+1]] <- list(
      icone  = "🌡️",
      titre  = paste0("Chaleur excessive — ", meteo$temp_max_j1, "°C demain"),
      desc   = "Au-delà de 35°C, la tomate avorte ses fleurs et la pollinisation est compromise. Stade de floraison particulièrement vulnérable.",
      conseil = "Arroser en fin de journée. Éviter tout stress durant la floraison.",
      classe  = "alerte-chaleur", niveau = "Élevé"
    )
  }
  
  # Vent fort
  if (!is.null(meteo$vent_max_j1) && meteo$vent_max_j1 > 30) {
    alertes[[length(alertes)+1]] <- list(
      icone  = "💨",
      titre  = paste0("Vent fort prévu — ", meteo$vent_max_j1, " km/h"),
      desc   = "Des vents forts sont attendus. Risque de verse et de dommages mécaniques sur les plants tuteurés.",
      conseil = "Vérifier les tuteurs. Ne pas appliquer de traitements par vent fort.",
      classe  = "alerte-vent", niveau = "Modéré"
    )
  }
  
  # Tout va bien
  if (length(alertes) == 0) {
    alertes[[1]] <- list(
      icone  = "✅",
      titre  = paste0("Conditions favorables — ", meteo$temperature, "°C"),
      desc   = "Aucune alerte météo particulière. Les conditions climatiques sont globalement favorables à la culture de la tomate.",
      conseil = "Continuez à suivre votre calendrier cultural normalement.",
      classe  = "alerte-ok", niveau = "Favorable"
    )
  }
  
  alertes
}

# ============================================================
# SECTION 7 — MOTEUR DE RECOMMANDATIONS AGRONOMIQUES
# (indépendant de l'interface — extensible par culture)
# ============================================================
get_recommandations <- function(culture, jours_depuis, meteo) {
  recs <- list()
  
  if (culture == "tomate") {
    if (jours_depuis >= 18 && jours_depuis <= 24) {
      recs[[length(recs)+1]] <- list(
        icone="🪴", titre="Repiquage imminent",
        desc="Le stade optimal de repiquage approche (J+21). Préparez votre parcelle dès maintenant.",
        priorite="haute"
      )
    }
    if (meteo$succes && !is.null(meteo$prob_pluie_j1) &&
        meteo$prob_pluie_j1 < 30 && jours_depuis %in% 34:37) {
      recs[[length(recs)+1]] <- list(
        icone="💧", titre="Bon moment pour fertiliser",
        desc="Faible risque de pluie. Conditions idéales pour l'application du 1er engrais.",
        priorite="haute"
      )
    }
  }
  
  recs
}

# ============================================================
# SECTION 8 — FONCTIONS UI RÉUTILISABLES
# ============================================================

# Bannière commune à toutes les pages
banniere_ui <- function() {
  div(class="banniere-app",
      tags$img(src="logo01.png", alt="SDA"),
      div(class="banniere-sep"),
      div(
        h1("AgriNumérique"),
        p("Calendrier agricole intelligent · Bénin")
      )
  )
}

# Champ de saisie avec icône à gauche
champ_icone_ui <- function(icone, input_ui) {
  fluidRow(
    column(2, div(class="icone-champ", icone)),
    column(10, input_ui)
  )
}

# Badge de statut dans le tableau
badge_statut <- function(statut) {
  switch(statut,
         "Fait"        = tags$span(class="badge-fait",   "✓ Fait"),
         "Aujourd'hui" = tags$span(class="badge-actuel", "● Aujourd'hui"),
         tags$span(class="badge-avenir",  "○ À venir")
  )
}

# Carte d'alerte météo
carte_alerte_ui <- function(al) {
  niveau_cls <- switch(al$niveau,
                       "Élevé"    = "niveau-eleve",
                       "Modéré"   = "niveau-modere",
                       "Favorable" = "niveau-favorable",
                       "Info"     = "niveau-info"
  )
  div(class=paste0("alerte-card ", al$classe),
      div(class="alerte-card-header",
          div(class="alerte-card-icone", al$icone),
          div(class="alerte-card-titre", al$titre),
          div(class=paste0("alerte-card-niveau ", niveau_cls), al$niveau)
      ),
      div(class="alerte-card-desc", al$desc),
      div(class="alerte-card-conseil", paste0("💡 ", al$conseil))
  )
}

# Section confidentialité
conf_section_ui <- function(icone, titre, desc) {
  div(class="conf-section",
      div(class="conf-section-titre", paste0(icone, " ", titre)),
      div(class="conf-section-desc", desc)
  )
}

# ============================================================
# SECTION 9 — THÈME BSLIB
# bg/fg neutres pour que le CSS contrôle tout
# ============================================================
theme_agri <- bs_theme(
  version        = 5,
  primary        = "#2E7D5B",
  secondary      = "#666666",
  success        = "#3D8B63",
  warning        = "#F0A500",
  danger         = "#E53E3E",
  bg             = "#FCFBF8",
  fg             = "#1A1A1A",
  base_font      = font_google("Poppins"),
  `border-radius` = "12px"
)

# ============================================================
# SECTION 10 — CSS CENTRALISÉ (généré depuis COULEURS)
# ============================================================
CSS <- paste0("
/* AgriNumérique — Charte graphique officielle */
:root {
  --fond-principal:   ", COULEURS$fond_principal,   ";
  --fond-secondaire:  ", COULEURS$fond_secondaire,  ";
  --fond-carte:       ", COULEURS$fond_carte,       ";
  --vert-principal:   ", COULEURS$vert_principal,   ";
  --vert-secondaire:  ", COULEURS$vert_secondaire,  ";
  --vert-fonce:       ", COULEURS$vert_fonce,       ";
  --fond-icone:       ", COULEURS$fond_icone,       ";
  --bordure:          ", COULEURS$bordure,          ";
  --texte:            ", COULEURS$texte_principal,  ";
  --texte-sec:        ", COULEURS$texte_secondaire, ";
}

/* Base */
* { box-sizing: border-box; }
body {
  background: var(--fond-principal) !important;
  font-family: 'Poppins', sans-serif;
  margin: 0; padding: 0;
}
.container-fluid { padding: 0 !important; max-width: 100% !important; }

/* Pages */
.page-centree {
  display: flex; justify-content: center; align-items: center;
  min-height: 100vh; padding: 30px 16px;
  background: var(--fond-principal);
}
.page-fond-clair {
  min-height: 100vh; padding: 28px 16px;
  background: var(--fond-secondaire);
}
.page-dashboard {
  min-height: 100vh; padding: 24px 16px 40px;
  background: var(--fond-secondaire);
}

/* Cartes */
.carte {
  background: var(--fond-carte); border: 1px solid var(--bordure);
  border-radius: 22px; padding: 36px;
  box-shadow: 0 12px 32px rgba(0,0,0,0.15);
}
.carte-sm {
  background: var(--fond-carte); border: 1px solid var(--bordure);
  border-radius: 16px; padding: 20px;
  box-shadow: 0 4px 16px rgba(0,0,0,0.07);
  margin-bottom: 16px;
}

/* Bannière commune */
.banniere-app {
  background: linear-gradient(130deg, #123524, var(--vert-fonce), var(--vert-principal));
  border-radius: 18px; padding: 16px 22px; margin-bottom: 22px;
  display: flex; align-items: center; gap: 16px;
  box-shadow: 0 8px 28px rgba(0,0,0,0.35);
}
.banniere-app img {
  height: 52px; border-radius: 8px; background: white; padding: 4px;
  filter: drop-shadow(0 2px 4px rgba(0,0,0,0.2));
}
.banniere-app .banniere-sep { width:1px; height:42px; background:rgba(255,255,255,0.2); }
.banniere-app h1 {
  font-size:1.5rem; font-weight:800; color:white; margin:0;
  text-shadow:0 2px 6px rgba(0,0,0,0.2);
}
.banniere-app p { font-size:0.76rem; color:rgba(255,255,255,0.72); margin:3px 0 0; }

/* Titres de section */
.section-titre { font-size:1.2rem; font-weight:800; color:var(--vert-fonce); margin-bottom:5px; }
.section-sous  { font-size:0.86rem; color:var(--texte-sec); margin-bottom:22px; }

/* CHAMPS DE SAISIE — LISIBILITÉ MAXIMALE */
.form-control, .form-select {
  background-color: white !important;
  color: var(--texte) !important;
  border: 1.5px solid var(--bordure) !important;
  border-radius: 10px !important;
  font-size: 0.88rem !important;
  padding: 10px 13px !important;
}
.form-control:focus, .form-select:focus {
  border-color: var(--vert-principal) !important;
  box-shadow: 0 0 0 3px rgba(46,125,91,0.18) !important;
  color: var(--texte) !important;
}
.form-control[type=date] { color: var(--texte) !important; }
.form-label, label { color: var(--texte) !important; font-weight:600 !important; font-size:0.84rem !important; }
.form-text, .help-block { color: var(--texte-sec) !important; font-size:0.74rem !important; }
.shiny-input-container .radio label,
.shiny-input-container .checkbox label { color: var(--texte) !important; font-weight:500 !important; }
select option { color: var(--texte) !important; background: white !important; }

/* Icônes de champ */
.icone-champ {
  height:50px; background:var(--fond-icone); border:1px solid var(--bordure);
  border-radius:12px; display:flex; align-items:center; justify-content:center;
  font-size:1.4rem; margin-top:26px;
}

/* Boutons */
.btn-principal {
  width:100% !important; height:56px; background:var(--vert-fonce) !important;
  color:white !important; border:none !important; border-radius:14px !important;
  font-size:0.98rem !important; font-weight:700 !important;
  box-shadow:0 6px 18px rgba(31,107,79,0.3) !important;
  font-family:'Poppins',sans-serif !important; cursor:pointer;
}
.btn-principal:hover { background:var(--vert-principal) !important; }
.btn-secondaire {
  background:var(--texte-sec) !important; color:white !important;
  border:none !important; border-radius:12px !important;
  padding:10px 22px !important; font-size:0.86rem !important;
  font-weight:600 !important; cursor:pointer;
}
.btn-secondaire:hover { background:#555 !important; }

/* Statistiques */
.stats-row { display:flex; gap:10px; margin-bottom:20px; flex-wrap:wrap; }
.stat-box {
  flex:1; min-width:90px; background:var(--fond-carte); border:1px solid var(--bordure);
  border-radius:14px; padding:13px 8px; text-align:center;
}
.stat-val { font-size:1.1rem; font-weight:900; color:var(--vert-fonce); display:block; }
.stat-lbl { font-size:0.56rem; color:var(--texte-sec); font-weight:700;
  text-transform:uppercase; letter-spacing:0.05em; display:block; margin-top:2px; }

/* Carte statut */
.cal-header {
  background:linear-gradient(135deg,var(--vert-fonce),var(--vert-principal));
  border-radius:16px; padding:18px 22px; margin-bottom:18px;
  display:flex; align-items:center; gap:14px; color:white;
  box-shadow:0 6px 20px rgba(31,107,79,0.3);
}
.cal-header-icone { font-size:2rem; }
.cal-header-titre { font-size:0.98rem; font-weight:800; color:white; margin-bottom:3px; }
.cal-header-sous  { font-size:0.74rem; color:rgba(255,255,255,0.78); }
.cal-header-tag {
  margin-left:auto; background:rgba(255,255,255,0.18);
  border:1px solid rgba(255,255,255,0.3); border-radius:20px;
  padding:4px 13px; font-size:0.66rem; font-weight:700;
  color:white; white-space:nowrap;
}

/* Box action urgente */
.prochaine-box {
  background:#FFF8E7; border:2px solid #F0A500;
  border-radius:14px; padding:14px 16px; margin-bottom:18px;
  display:flex; gap:12px; align-items:center;
}
.prochaine-icone { font-size:1.5rem; }
.prochaine-titre { font-weight:800; font-size:0.86rem; color:#7A4400; margin-bottom:2px; }
.prochaine-desc  { font-size:0.77rem; color:#6B5020; }

/* Tableau */
.cal-table { width:100%; border-collapse:collapse; }
.cal-table th {
  font-size:0.6rem; font-weight:700; text-transform:uppercase;
  letter-spacing:0.06em; color:var(--texte-sec);
  padding:7px 9px; border-bottom:2px solid var(--bordure); text-align:left;
}
.cal-table td {
  padding:10px 9px; font-size:0.8rem; color:var(--texte);
  border-bottom:1px solid #F0EEE8; vertical-align:middle;
}
.cal-table tr:last-child td { border-bottom:none; }
.cal-table tr.ligne-active td { background:#F0FBF4; }
.cal-table tr.ligne-passee   { opacity:0.5; }

.badge-fait   { background:#D1FAE5; color:#065F46; padding:3px 9px; border-radius:20px; font-size:0.62rem; font-weight:700; display:inline-block; }
.badge-actuel { background:#FEF3C7; color:#92400E; padding:3px 9px; border-radius:20px; font-size:0.62rem; font-weight:700; display:inline-block; }
.badge-avenir { background:#F3F4F6; color:#6B7280; padding:3px 9px; border-radius:20px; font-size:0.62rem; font-weight:700; display:inline-block; }

/* Météo */
.meteo-intro {
  background:linear-gradient(135deg,#1565C0,#1E88E5);
  border-radius:16px; padding:16px 20px; margin-bottom:16px;
  color:white; display:flex; align-items:center; gap:14px;
  box-shadow:0 6px 20px rgba(21,101,192,0.3);
}
.meteo-intro-ico   { font-size:1.9rem; }
.meteo-intro-titre { font-size:0.95rem; font-weight:800; color:white; margin-bottom:2px; }
.meteo-intro-sous  { font-size:0.72rem; color:rgba(255,255,255,0.8); }

.meteo-grid { display:grid; grid-template-columns:repeat(3,1fr); gap:10px; margin-bottom:16px; }
.meteo-item {
  background:var(--fond-carte); border:1px solid var(--bordure);
  border-radius:12px; padding:12px 8px; text-align:center;
}
.meteo-item-val { font-size:1.05rem; font-weight:800; color:var(--vert-fonce); display:block; }
.meteo-item-lbl { font-size:0.58rem; color:var(--texte-sec); font-weight:700;
  text-transform:uppercase; display:block; margin-top:2px; }

/* Alertes */
.alerte-card { border-radius:13px; padding:15px 17px; margin-bottom:11px; border-left:5px solid; }
.alerte-card-header { display:flex; align-items:center; gap:10px; margin-bottom:7px; }
.alerte-card-icone  { font-size:1.4rem; }
.alerte-card-titre  { font-weight:800; font-size:0.86rem; color:var(--texte); flex:1; }
.alerte-card-niveau { font-size:0.6rem; font-weight:700; padding:2px 10px; border-radius:20px; white-space:nowrap; }
.alerte-card-desc   { font-size:0.77rem; color:#374151; line-height:1.55; margin-bottom:7px; }
.alerte-card-conseil { font-size:0.73rem; font-weight:600; font-style:italic;
  padding:7px 12px; border-radius:8px; background:rgba(0,0,0,0.04); color:var(--texte); }
.alerte-pluie      { background:#EFF6FF; border-color:#2563EB; }
.alerte-secheresse { background:#FFF7ED; border-color:#EA580C; }
.alerte-vent       { background:#F5F3FF; border-color:#7C3AED; }
.alerte-chaleur    { background:#FFFBEB; border-color:#D97706; }
.alerte-ok         { background:#F0FDF4; border-color:#16A34A; }
.alerte-info       { background:#F8FAFC; border-color:#64748B; }
.niveau-eleve    { background:#FEE2E2; color:#991B1B; }
.niveau-modere   { background:#FEF3C7; color:#92400E; }
.niveau-favorable { background:#D1FAE5; color:#065F46; }
.niveau-info     { background:#E0F2FE; color:#0369A1; }

/* Onglets */
.nav-pills .nav-link {
  color:var(--vert-fonce) !important; font-weight:700 !important;
  border-radius:10px !important; padding:8px 16px;
  font-size:0.8rem !important; transition:all .15s;
  background:rgba(31,107,79,0.06) !important;
}
.nav-pills .nav-link.active {
  background:var(--vert-fonce) !important; color:white !important;
}
.tab-content { padding-top:18px !important; }

/* Profil */
.profil-header {
  background:linear-gradient(135deg,var(--vert-fonce),var(--vert-principal));
  border-radius:14px; padding:16px 20px; margin-bottom:16px;
  display:flex; align-items:center; gap:14px; color:white;
  box-shadow:0 4px 14px rgba(31,107,79,0.25);
}
.profil-avatar {
  width:48px; height:48px; background:rgba(255,255,255,0.2);
  border-radius:50%; display:flex; align-items:center;
  justify-content:center; font-size:1.4rem;
  border:2px solid rgba(255,255,255,0.3); flex-shrink:0;
}
.profil-nom { font-size:0.98rem; font-weight:800; color:white; margin:0 0 2px; }
.profil-sub { font-size:0.72rem; color:rgba(255,255,255,0.75); }
.profil-table { width:100%; border-collapse:collapse; border-radius:10px; overflow:hidden; border:1px solid var(--bordure); }
.profil-table tr:nth-child(odd) td  { background:#F7F4EF; }
.profil-table td { padding:10px 14px; font-size:0.82rem; border-bottom:1px solid var(--bordure); color:var(--texte); }
.profil-table td:first-child { font-weight:700; color:var(--vert-fonce); width:40%; }
.profil-table tr:last-child td { border-bottom:none; }

/* Confidentialité */
.conf-section {
  background:var(--fond-carte); border:1px solid var(--bordure);
  border-radius:13px; padding:16px 18px; margin-bottom:12px;
}
.conf-section-titre { font-weight:800; font-size:0.88rem; margin-bottom:6px; color:var(--vert-fonce); }
.conf-section-desc  { font-size:0.78rem; color:var(--texte-sec); line-height:1.65; }

/* Notes */
.note-bas {
  font-size:0.68rem; color:var(--texte-sec); font-style:italic;
  margin-top:14px; padding-top:12px; border-top:1px solid var(--bordure);
}
.note-demo {
  font-size:0.68rem; color:var(--texte-sec); font-style:italic;
  text-align:center; margin-top:14px; padding:9px 12px;
  background:#F9FAFB; border-radius:8px; border:1px dashed var(--bordure);
}

/* Footer */
.footer-app {
  text-align:center; color:rgba(255,255,255,0.35);
  font-size:0.66rem; margin-top:28px; padding-bottom:12px;
}
.footer-app img { height:13px; vertical-align:middle; margin-right:5px;
  border-radius:3px; background:white; padding:1px; }

@media(max-width:600px){
  .stats-row { flex-direction:column; }
  .meteo-grid { grid-template-columns:repeat(2,1fr); }
  .carte { padding:22px; }
}
")

# ============================================================
# SECTION 11 — UI
# ============================================================
ui <- fluidPage(
  theme = theme_agri,
  tags$head(tags$style(HTML(CSS))),
  uiOutput("page")
)

# ============================================================
# SECTION 12 — SERVEUR
# ============================================================
server <- function(input, output, session) {
  
  # Navigation
  page <- reactiveVal("accueil")
  observeEvent(input$continuer,      { page("producteur") })
  observeEvent(input$suite,          { page("culture")    })
  observeEvent(input$dashboard,      { page("dashboard")  })
  observeEvent(input$retour_culture, { page("culture")    })
  
  # Données météo (chargées à l'ouverture du dashboard)
  meteo_data <- reactiveVal(list(succes = FALSE))
  observeEvent(input$dashboard, {
    coords <- get_coordonnees(input$localite)
    meteo_data(get_meteo(coords[1], coords[2]))
  })
  
  output$page <- renderUI({
    
    # ====== ACCUEIL ======
    if (page() == "accueil") {
      div(class="page-centree",
          div(style="width:100%; max-width:480px;",
              div(style="text-align:center; margin-bottom:28px;",
                  tags$img(src="logo01.png", style="height:88px; border-radius:14px; margin-bottom:14px;"),
                  h1("AgriNumérique", style="color:white; font-weight:900; font-size:1.9rem; margin:0;"),
                  p("Planifiez · Cultivez · Récoltez",
                    style="color:rgba(255,255,255,0.7); font-size:0.95rem; margin-top:5px;")
              ),
              div(class="carte",
                  div(class="section-titre", "\U0001f30d Choisissez votre langue"),
                  div(class="section-sous",  "L'application s'adaptera à votre choix."),
                  selectInput("langue", NULL,
                              choices=c("Français","Fon","Yoruba","Bariba"), width="100%"),
                  br(),
                  actionButton("continuer","Commencer →", class="btn-principal"),
                  br(), br(),
                  p("\U0001f331 Votre assistant agricole intelligent",
                    style="text-align:center; color:var(--texte-sec); font-size:0.82rem;")
              )
          )
      )
      
      # ====== PROFIL PRODUCTEUR ======
    } else if (page() == "producteur") {
      div(class="page-fond-clair",
          div(style="max-width:660px; margin:0 auto;",
              banniere_ui(),
              div(class="carte",
                  div(style="text-align:center; margin-bottom:26px;",
                      div(style=paste0("width:70px; height:70px; margin:auto; border-radius:50%;",
                                       " background:var(--fond-icone); border:1px solid var(--bordure);",
                                       " display:flex; align-items:center; justify-content:center;",
                                       " font-size:1.9rem; margin-bottom:12px;"), "\U0001f464"),
                      div(class="section-titre","Informations du producteur"),
                      div(class="section-sous","Ces informations personnalisent vos recommandations.")
                  ),
                  champ_icone_ui("\U0001f464",
                                 textInput("nom","Nom et prénom", placeholder="Ex : Adjoavi Koudjo", width="100%")),
                  br(),
                  champ_icone_ui("\U0001f4de",
                                 textInput("telephone","Téléphone", placeholder="Ex : 97 00 00 00", width="100%")),
                  br(),
                  champ_icone_ui("\U0001f4cd",
                                 selectInput("localite","Village ou commune",
                                             choices=c("Choisir..."="", sort(names(VILLES_BENIN))),
                                             selected="", width="100%")),
                  br(),
                  champ_icone_ui("\U0001f33f",
                                 numericInput("superficie","Superficie (ha)", value=1, min=0.1, step=0.1, width="100%")),
                  br(), br(),
                  actionButton("suite","Continuer →", class="btn-principal")
              )
          )
      )
      
      # ====== CULTURE ======
    } else if (page() == "culture") {
      div(class="page-fond-clair",
          div(style="max-width:600px; margin:0 auto;",
              banniere_ui(),
              div(class="carte",
                  div(class="section-titre","\U0001f331 Ma culture"),
                  div(class="section-sous","Renseignez les informations de votre culture."),
                  selectInput("culture","Culture pratiquée",
                              choices=CULTURES_DISPONIBLES, width="100%"),
                  textInput("variete","Variété (optionnel)",
                            placeholder="Ex : Mongal F1", width="100%"),
                  radioButtons("semis","Avez-vous déjà semé ?",
                               choices=c("Oui"="oui","Non"="non"), selected="non"),
                  dateInput("date_semis","Date de semis",
                            value=Sys.Date(), language="fr", format="dd/mm/yyyy", width="100%"),
                  br(),
                  actionButton("dashboard","Voir mon calendrier →", class="btn-principal")
              )
          )
      )
      
      # ====== DASHBOARD ======
    } else if (page() == "dashboard") {
      req(input$date_semis)
      
      # Calculs
      semis_date   <- as.Date(input$date_semis)
      jours_depuis <- as.integer(Sys.Date() - semis_date)
      culture_id   <- input$culture %||% "tomate"
      df           <- ITINERAIRES[[culture_id]]
      df$date_op   <- semis_date + df$offset
      df$statut    <- ifelse(jours_depuis > df$offset, "Fait",
                             ifelse(jours_depuis >= df$offset - 1, "Aujourd'hui","À venir"))
      
      a_venir        <- df[df$offset > jours_depuis,]
      prochaine      <- if (nrow(a_venir)>0) a_venir[1,] else df[nrow(df),]
      jours_restants <- max(0, prochaine$offset - jours_depuis)
      passes         <- df[df$offset <= jours_depuis,]
      stade_actuel   <- if (nrow(passes)>0) tail(passes,1)$etape else "Semis"
      j_recolte      <- max(df$offset) - jours_depuis
      
      meteo          <- meteo_data()
      alertes        <- generer_alertes_meteo(meteo, jours_depuis, stade_actuel)
      recs           <- get_recommandations(culture_id, jours_depuis, meteo)
      cond_actuelle  <- interpreter_code_meteo(if(meteo$succes) meteo$code_meteo else NULL)
      
      nom_aff <- input$nom %||% "Producteur"
      prenom  <- strsplit(nom_aff," ")[[1]][1]
      loc_aff <- input$localite %||% "Bénin"
      if (!nzchar(loc_aff)) loc_aff <- "Bénin"
      
      div(class="page-dashboard",
          div(style="max-width:860px; margin:0 auto;",
              
              banniere_ui(),
              
              # Bienvenue
              div(style="display:flex; align-items:center; justify-content:space-between; margin-bottom:18px;",
                  div(
                    h2(paste0("Bonjour, ",prenom," \U0001f44b"),
                       style="color:white; font-weight:800; font-size:1.3rem; margin:0;"),
                    p(paste0(loc_aff," · ",toupper(culture_id)," · ",format_date_fr(Sys.Date())),
                      style="color:rgba(255,255,255,0.55); font-size:0.76rem; margin:3px 0 0;")
                  )
              ),
              
              # Carte statut
              div(class="cal-header",
                  div(class="cal-header-icone", prochaine$icone),
                  div(
                    div(class="cal-header-titre",paste0("Prochaine : ",prochaine$etape)),
                    div(class="cal-header-sous", paste0(prochaine$action," · ",format_date_fr(prochaine$date_op)))
                  ),
                  div(class="cal-header-tag",
                      if(jours_restants>0) paste0("Dans ",jours_restants,"j") else "Aujourd'hui !")
              ),
              
              # Stats
              div(class="stats-row",
                  div(class="stat-box",
                      tags$span(class="stat-val",paste0("J+",max(0,jours_depuis))),
                      tags$span(class="stat-lbl","Depuis semis")),
                  div(class="stat-box",
                      tags$span(class="stat-val",stade_actuel),
                      tags$span(class="stat-lbl","Stade actuel")),
                  div(class="stat-box",
                      tags$span(class="stat-val",if(j_recolte>0)paste0(j_recolte,"j")else"\U0001f389"),
                      tags$span(class="stat-lbl","Avant récolte")),
                  div(class="stat-box",
                      tags$span(class="stat-val",paste0(sum(df$statut=="Fait"),"/",nrow(df))),
                      tags$span(class="stat-lbl","Étapes faites")),
                  div(class="stat-box",
                      tags$span(class="stat-val",if(meteo$succes)paste0(meteo$temperature,"°C")else"—"),
                      tags$span(class="stat-lbl","Temp. actuelle"))
              ),
              
              # ONGLETS
              tabsetPanel(id="onglets", type="pills",
                          
                          # -- Calendrier --
                          tabPanel(title="\U0001f4c5 Calendrier", value="cal",
                                   
                                   if (length(recs)>0) tagList(lapply(recs, function(r) {
                                     div(class="prochaine-box",
                                         div(class="prochaine-icone",r$icone),
                                         div(div(class="prochaine-titre",r$titre),
                                             div(class="prochaine-desc",r$desc)))
                                   })),
                                   
                                   div(class="prochaine-box",
                                       div(class="prochaine-icone","\u26a0\ufe0f"),
                                       div(div(class="prochaine-titre",
                                               paste0("\u00c0 faire dans ",jours_restants,
                                                      if(jours_restants>1)" jours"else" jour",
                                                      " \u2014 ",prochaine$etape)),
                                           div(class="prochaine-desc",prochaine$action))
                                   ),
                                   
                                   div(class="carte-sm",
                                       h4("\U0001f4c5 Calendrier complet \u2014 Tomate",
                                          style="color:var(--vert-fonce); font-weight:800; margin-bottom:14px; font-size:0.95rem;"),
                                       tags$table(class="cal-table",
                                                  tags$thead(tags$tr(
                                                    tags$th("Jour"),tags$th("Date"),
                                                    tags$th("\u00c9tape"),tags$th("Action"),tags$th("Statut")
                                                  )),
                                                  tags$tbody(lapply(seq_len(nrow(df)), function(i) {
                                                    row <- df[i,]
                                                    cls <- if(row$statut=="Aujourd'hui")"ligne-active"
                                                    else if(row$statut=="Fait")"ligne-passee" else ""
                                                    tags$tr(class=cls,
                                                            tags$td(paste0("J+",row$offset)),
                                                            tags$td(format_date_fr(row$date_op)),
                                                            tags$td(paste0(row$icone," ",row$etape)),
                                                            tags$td(row$action),
                                                            tags$td(badge_statut(row$statut)))
                                                  }))
                                       ),
                                       div(class="note-bas",
                                           "Calendrier calcul\u00e9 depuis la date de semis \u00b7 Tomate \u00b7 B\u00e9nin")
                                   )
                          ),
                          
                          # -- Alertes météo --
                          tabPanel(
                            title=paste0("\U0001f326\ufe0f Alertes (",length(alertes),")"),
                            value="alertes",
                            
                            div(class="meteo-intro",
                                div(class="meteo-intro-ico",cond_actuelle$icone),
                                div(
                                  div(class="meteo-intro-titre",
                                      paste0(cond_actuelle$texte," \u2014 ",loc_aff)),
                                  div(class="meteo-intro-sous",
                                      paste0(length(alertes)," alerte(s) \u00b7 ",
                                             if(meteo$succes)"Open-Meteo en temps r\u00e9el"
                                             else"Connexion indisponible"))
                                )
                            ),
                            
                            if (meteo$succes) {
                              div(class="meteo-grid",
                                  div(class="meteo-item",
                                      tags$span(class="meteo-item-val",paste0(meteo$temperature,"\u00b0C")),
                                      tags$span(class="meteo-item-lbl","Temp\u00e9rature")),
                                  div(class="meteo-item",
                                      tags$span(class="meteo-item-val",
                                                if(!is.null(meteo$prob_pluie_j1))paste0(meteo$prob_pluie_j1,"%")else"\u2014"),
                                      tags$span(class="meteo-item-lbl","Prob. pluie")),
                                  div(class="meteo-item",
                                      tags$span(class="meteo-item-val",paste0(meteo$pluie_j1,"mm")),
                                      tags$span(class="meteo-item-lbl","Pluie j+1")),
                                  div(class="meteo-item",
                                      tags$span(class="meteo-item-val",paste0(meteo$vent,"km/h")),
                                      tags$span(class="meteo-item-lbl","Vent actuel")),
                                  div(class="meteo-item",
                                      tags$span(class="meteo-item-val",paste0(meteo$temp_max_j1,"\u00b0C")),
                                      tags$span(class="meteo-item-lbl","Max demain")),
                                  div(class="meteo-item",
                                      tags$span(class="meteo-item-val",
                                                if(!is.na(meteo$etp_j1)&&!is.null(meteo$etp_j1))paste0(meteo$etp_j1,"mm")else"\u2014"),
                                      tags$span(class="meteo-item-lbl","ETP j+1"))
                              )
                            },
                            
                            tagList(lapply(alertes, carte_alerte_ui)),
                            
                            div(class="note-demo",
                                "\U0001f4a1 Donn\u00e9es m\u00e9t\u00e9o : Open-Meteo (open-meteo.com) \u00b7 ",loc_aff)
                          ),
                          
                          # -- Profil --
                          tabPanel(title="\U0001f464 Profil", value="profil",
                                   div(class="profil-header",
                                       div(class="profil-avatar","\U0001f469\u200d\U0001f33e"),
                                       div(
                                         p(class="profil-nom",nom_aff),
                                         p(class="profil-sub",
                                           paste0(loc_aff," \u00b7 ",toupper(culture_id),
                                                  " \u00b7 J+",max(0,jours_depuis)))
                                       )
                                   ),
                                   div(class="carte-sm",
                                       tags$table(class="profil-table",tags$tbody(
                                         tags$tr(tags$td("Nom"),       tags$td(nom_aff)),
                                         tags$tr(tags$td("T\u00e9l\u00e9phone"), tags$td(if(nzchar(input$telephone%||%""))input$telephone else"\u2014")),
                                         tags$tr(tags$td("Localit\u00e9"), tags$td(loc_aff)),
                                         tags$tr(tags$td("Culture"),   tags$td("Tomate")),
                                         tags$tr(tags$td("Vari\u00e9t\u00e9"), tags$td(if(nzchar(input$variete%||%""))input$variete else"\u2014")),
                                         tags$tr(tags$td("Semis le"),  tags$td(format_date_fr(semis_date))),
                                         tags$tr(tags$td("Superficie"),tags$td(paste0(input$superficie%||%"\u2014"," ha")))
                                       ))
                                   ),
                                   br(),
                                   actionButton("retour_culture","\u270e Modifier ma culture",
                                                style="background:var(--vert-fonce); color:white; border:none;
                  border-radius:12px; padding:11px 22px; font-weight:700; cursor:pointer;")
                          ),
                          
                          # -- Confidentialité --
                          tabPanel(title="\U0001f512 Confidentialit\u00e9", value="conf",
                                   div(class="carte-sm",
                                       h4("\U0001f512 Politique de confidentialit\u00e9 \u2014 AgriNum\u00e9rique",
                                          style="color:var(--vert-fonce); font-weight:800; margin-bottom:6px; font-size:0.95rem;"),
                                       p("AgriNum\u00e9rique s'engage \u00e0 prot\u00e9ger vos donn\u00e9es. Cette politique explique de mani\u00e8re transparente quelles donn\u00e9es sont collect\u00e9es, pourquoi, et comment elles sont utilis\u00e9es.",
                                         style="font-size:0.8rem; color:var(--texte-sec); margin-bottom:16px;")
                                   ),
                                   conf_section_ui("\U0001f4cb","Donn\u00e9es collect\u00e9es",
                                                   "Nous collectons uniquement : nom et pr\u00e9nom, t\u00e9l\u00e9phone, localit\u00e9, superficie, culture, vari\u00e9t\u00e9 et date de semis. Aucune donn\u00e9e bancaire ni document d'identit\u00e9."),
                                   conf_section_ui("\U0001f3af","Utilisation des donn\u00e9es",
                                                   "Vos donn\u00e9es servent exclusivement \u00e0 g\u00e9n\u00e9rer votre calendrier agricole personnalis\u00e9, vos alertes m\u00e9t\u00e9o et vos recommandations agronomiques. Elles ne sont jamais partag\u00e9es \u00e0 des fins commerciales."),
                                   conf_section_ui("\U0001f510","S\u00e9curit\u00e9 et confidentialit\u00e9",
                                                   "Vos informations sont stock\u00e9es de fa\u00e7on s\u00e9curis\u00e9e et accessibles uniquement \u00e0 l'\u00e9quipe AgriNum\u00e9rique dans le cadre du programme Science des Donn\u00e9es au F\u00e9minin en Afrique (SDA). Vos donn\u00e9es personnelles ne sont jamais revendues."),
                                   conf_section_ui("\U0001f324\ufe0f","Donn\u00e9es m\u00e9t\u00e9orologiques",
                                                   "Les donn\u00e9es m\u00e9t\u00e9o proviennent d'Open-Meteo (open-meteo.com), une API gratuite et open source. Ces donn\u00e9es sont r\u00e9cup\u00e9r\u00e9es automatiquement selon votre localit\u00e9. Open-Meteo ne collecte aucune donn\u00e9e personnelle."),
                                   conf_section_ui("\u26a0\ufe0f","Recommandations agronomiques",
                                                   "Les recommandations g\u00e9n\u00e9r\u00e9es par AgriNum\u00e9rique constituent une aide \u00e0 la d\u00e9cision. Elles ne remplacent pas l'avis d'un technicien agricole. L'\u00e9quipe AgriNum\u00e9rique d\u00e9cline toute responsabilit\u00e9 en cas d'utilisation inappropri\u00e9e."),
                                   conf_section_ui("\u270b","Vos droits",
                                                   "Vous pouvez \u00e0 tout moment demander la consultation, la modification ou la suppression de vos donn\u00e9es en contactant directement l'\u00e9quipe SDA."),
                                   div(style="font-size:0.68rem; color:var(--texte-sec); margin-top:14px; font-style:italic; text-align:center;",
                                       "Derni\u00e8re mise \u00e0 jour : juillet 2026 \u00b7 AgriNum\u00e9rique MVP \u00b7 Programme SDA \u00b7 B\u00e9nin")
                          )
                          
              ), # fin tabsetPanel
              br(),
              actionButton("retour_culture","\u2190 Modifier ma culture", class="btn-secondaire"),
              div(class="footer-app",
                  tags$img(src="logo01.png"),
                  "AgriNum\u00e9rique \u00b7 MVP \u00b7 B\u00e9nin \u00b7 Science des Donn\u00e9es au F\u00e9minin en Afrique (SDA) \u00b7 2026")
          )
      )
    } # fin dashboard
  }) # fin renderUI
} # fin server

# ============================================================
# LANCEMENT
# ============================================================
shinyApp(ui, server)



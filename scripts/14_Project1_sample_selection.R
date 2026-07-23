# ============================================================
# PROJET 1 — CRÉATION DE L'ÉCHANTILLON ANALYTIQUE
# ============================================================
#
# Objectifs de ce script :
# 1. Charger les données préparées par les scripts précédents.
# 2. Vérifier les identifiants et les doublons.
# 3. Fusionner les données cliniques et métabolomiques.
# 4. Identifier les participants ayant des données métabolomiques.
# 5. Appliquer les exclusions cliniques.
# 6. Appliquer le contrôle qualité DXA.
# 7. Calculer les variables de composition corporelle.
# 8. Conserver les effectifs après chaque étape pour le futur
#    diagramme de sélection des participants.
# ============================================================


# ============================================================
# 0. PACKAGES ET DOSSIERS
# ============================================================

library(dplyr)
library(here)

# Dossier contenant les fichiers déjà préparés
dir_processed <- here::here("data", "processed")


# ============================================================
# 1. CHARGEMENT DES DONNÉES PRÉPARÉES
# ============================================================
#
# Ces fichiers ont été produits par les scripts :
# - 10_Data_preparation.R
# - 13_Data_preparation_metabo.R
#
# sociodemo_fup0 :
#   données sociodémographiques et conditions cliniques au baseline.
#
# outcome_fup0 :
#   données de force, performance physique, DXA et autres outcomes.
#
# metabo_id :
#   table de correspondance entre entity_id et identifiant Metabolon.
#
# metabo_norm_all :
#   données métabolomiques normalisées, mais non imputées.
#
# metabo_norm_imp_all :
#   données métabolomiques normalisées et imputées.
#
# metabo_annotation :
#   informations descriptives sur chaque métabolite.
# ============================================================

sociodemo_fup0 <- readRDS(
  file.path(dir_processed, "sociodemo_fup0.rds")
)

outcome_fup0 <- readRDS(
  file.path(dir_processed, "outcome_fup0.rds")
)

metabo_id <- readRDS(
  file.path(dir_processed, "metabo_id.rds")
)

metabo_norm_all <- readRDS(
  file.path(dir_processed, "metabo_norm_all.rds")
)

metabo_norm_imp_all <- readRDS(
  file.path(dir_processed, "metabo_norm_imp_all.rds")
)

metabo_annotation <- readRDS(
  file.path(dir_processed, "metabo_annotation.rds")
)


# ============================================================
# 2. VÉRIFICATION INITIALE DES DIMENSIONS
# ============================================================
#
# Pour chaque fichier, on compare :
# - le nombre total de lignes;
# - le nombre d'identifiants uniques.
#
# Si les deux nombres sont identiques, cela suggère qu'il y a
# une seule ligne par participant ou par échantillon.
# ============================================================

nrow(sociodemo_fup0)
n_distinct(sociodemo_fup0$entity_id)

nrow(outcome_fup0)
n_distinct(outcome_fup0$entity_id)

nrow(metabo_id)
n_distinct(metabo_id$entity_id)
n_distinct(metabo_id$sd_metabo_key)

nrow(metabo_norm_all)
n_distinct(metabo_norm_all$sd_metabo_key)


# ============================================================
# 3. RECHERCHE EXPLICITE DES DOUBLONS
# ============================================================
#
# Ces tableaux doivent normalement être vides.
#
# La présence de doublons pourrait entraîner une multiplication
# des lignes lors des jointures.
# ============================================================

sociodemo_fup0 %>%
  count(entity_id) %>%
  filter(n > 1)

outcome_fup0 %>%
  count(entity_id) %>%
  filter(n > 1)

metabo_id %>%
  count(entity_id) %>%
  filter(n > 1)

metabo_id %>%
  count(sd_metabo_key) %>%
  filter(n > 1)

metabo_norm_all %>%
  count(sd_metabo_key) %>%
  filter(n > 1)


# ============================================================
# 4. CONTRÔLE AUTOMATIQUE DES DOUBLONS
# ============================================================
#
# Le script s'arrête si un identifiant est dupliqué.
#
# Cela évite de poursuivre les analyses avec une base dans
# laquelle une même personne ou un même échantillon apparaît
# plusieurs fois.
# ============================================================

stopifnot(!anyDuplicated(sociodemo_fup0$entity_id))
stopifnot(!anyDuplicated(outcome_fup0$entity_id))
stopifnot(!anyDuplicated(metabo_id$entity_id))
stopifnot(!anyDuplicated(metabo_id$sd_metabo_key))
stopifnot(!anyDuplicated(metabo_norm_all$sd_metabo_key))


# ============================================================
# 5. FUSION DES DONNÉES SOCIODÉMOGRAPHIQUES ET DES OUTCOMES
# ============================================================
#
# sociodemo_fup0 constitue ici la base de départ.
#
# Le left_join conserve tous les participants présents dans
# sociodemo_fup0, même si certaines données d'outcomes sont
# manquantes.
# ============================================================

project1_clinical <- sociodemo_fup0 %>%
  left_join(
    outcome_fup0,
    by = "entity_id"
  )


# ============================================================
# 6. VÉRIFICATION DE LA FUSION CLINIQUE
# ============================================================
#
# Le nombre de lignes ne devrait pas changer après la jointure.
#
# Une augmentation du nombre de lignes indiquerait généralement
# la présence d'identifiants dupliqués.
# ============================================================

nrow(sociodemo_fup0)
nrow(project1_clinical)

stopifnot(
  nrow(project1_clinical) == nrow(sociodemo_fup0)
)


# ============================================================
# 7. PRÉPARATION DE LA TABLE DES IDENTIFIANTS MÉTABOLOMIQUES
# ============================================================
#
# On conserve uniquement :
# - l'identifiant du participant ELCV;
# - l'identifiant unique de l'échantillon Metabolon.
#
# distinct() garantit qu'une même combinaison ne soit conservée
# qu'une seule fois.
# ============================================================

metabo_id_clean <- metabo_id %>%
  select(
    entity_id,
    sd_metabo_key
  ) %>%
  distinct()


# ============================================================
# 8. IDENTIFICATION DES ÉCHANTILLONS PRÉSENTS DANS LA MATRICE
#    MÉTABOLOMIQUE
# ============================================================
#
# Une clé Metabolon présente dans les données sociodémographiques
# ne garantit pas nécessairement que l'échantillon soit présent
# dans la matrice métabolomique finale.
#
# On crée donc un indicateur confirmant que des valeurs
# métabolomiques sont réellement disponibles.
# ============================================================

metabo_available <- metabo_norm_all %>%
  distinct(sd_metabo_key) %>%
  mutate(
    metabo_data_available = TRUE
  )


# ============================================================
# 9. CRÉATION DE LA BASE MASTER
# ============================================================
#
# Étapes :
# 1. Retirer l'ancienne colonne sd_metabo_key, si elle existe
#    déjà dans les données sociodémographiques.
# 2. Ajouter la clé provenant de metabo_id.
# 3. Vérifier si cette clé est présente dans la matrice
#    métabolomique.
# 4. Créer deux indicateurs :
#    - has_metabo_key;
#    - metabo_data_available.
# ============================================================

project1_master <- project1_clinical %>%
  select(
    -any_of("sd_metabo_key")
  ) %>%
  left_join(
    metabo_id_clean,
    by = "entity_id"
  ) %>%
  left_join(
    metabo_available,
    by = "sd_metabo_key"
  ) %>%
  mutate(
    # Le participant possède une clé Metabolon
    has_metabo_key =
      !is.na(sd_metabo_key),
    
    # Le participant possède réellement des données
    # dans la matrice métabolomique
    metabo_data_available =
      coalesce(
        metabo_data_available,
        FALSE
      )
  )


# ============================================================
# 10. VÉRIFICATION DE L'APPARIEMENT MÉTABOLOMIQUE
# ============================================================
#
# Les quatre combinaisons théoriques sont :
#
# FALSE / FALSE :
#   aucune clé et aucune donnée métabolomique.
#
# TRUE / FALSE :
#   clé disponible, mais aucun échantillon retrouvé dans la
#   matrice métabolomique.
#
# TRUE / TRUE :
#   appariement métabolomique réussi.
#
# FALSE / TRUE :
#   situation normalement impossible.
# ============================================================

project1_master %>%
  count(
    has_metabo_key,
    metabo_data_available
  )


# Vérifier que le nombre de lignes n'a pas changé
stopifnot(
  nrow(project1_master) == nrow(project1_clinical)
)


# ============================================================
# 11. CALCUL DE L'INDICE DE MASSE CORPORELLE
# ============================================================
#
# Le fichier préparé ne contient pas directement HWT_DBMI_COM.
#
# L'IMC est donc recalculé à partir :
# - du poids en kilogrammes;
# - de la taille en mètres.
#
# Formule :
# IMC = poids / taille²
#
# Une valeur est calculée uniquement si le poids et la taille
# sont disponibles et strictement positifs.
# ============================================================

project1_master <- project1_master %>%
  mutate(
    bmi = case_when(
      !is.na(sd_bodyweight) &
        !is.na(sd_height) &
        sd_bodyweight > 0 &
        sd_height > 0 ~
        sd_bodyweight / sd_height^2,
      
      TRUE ~ NA_real_
    )
  )


# ============================================================
# 12. VÉRIFICATION DE L'IMC
# ============================================================
#
# On examine :
# - le nombre de valeurs disponibles;
# - le nombre de valeurs manquantes;
# - la moyenne et l'écart-type;
# - les valeurs minimale et maximale.
#
# Les valeurs extrêmes seront examinées ultérieurement avant de
# décider s'il s'agit de valeurs plausibles ou invalides.
# ============================================================

project1_master %>%
  summarise(
    n_bmi_available =
      sum(!is.na(bmi)),
    
    n_bmi_missing =
      sum(is.na(bmi)),
    
    mean_bmi =
      mean(bmi, na.rm = TRUE),
    
    sd_bmi =
      sd(bmi, na.rm = TRUE),
    
    min_bmi =
      min(bmi, na.rm = TRUE),
    
    max_bmi =
      max(bmi, na.rm = TRUE)
  )


# ============================================================
# 13. DÉFINITION DE LA POPULATION DE DÉPART
# ============================================================
#
# data_0 contient tous les participants présents dans la base
# clinique préparée.
#
# Cette base représente la première case potentielle du futur
# flow diagram.
# ============================================================

data_0 <- project1_master

n_data_0 <- nrow(data_0)

n_data_0


# ============================================================
# 14. SÉLECTION DES PARTICIPANTS AVEC DONNÉES MÉTABOLOMIQUES
# ============================================================
#
# Pour être retenu, un participant doit :
# 1. avoir une clé Metabolon;
# 2. avoir cette clé présente dans la matrice métabolomique.
#
# data_1 représente donc la population métabolomique appariée.
# ============================================================

data_1 <- data_0 %>%
  filter(
    has_metabo_key,
    metabo_data_available
  )

n_data_1 <- nrow(data_1)

n_data_1


# ============================================================
# 15. EFFECTIFS EXCLUS À L'ÉTAPE MÉTABOLOMIQUE
# ============================================================
#
# On distingue :
# - les participants sans clé Metabolon;
# - les participants avec une clé, mais sans donnée présente
#   dans la matrice métabolomique;
# - le nombre total exclu à cette étape.
# ============================================================

n_without_key <- sum(
  !data_0$has_metabo_key
)

n_key_without_data <- sum(
  data_0$has_metabo_key &
    !data_0$metabo_data_available
)

n_excluded_metabo <- nrow(data_0) - nrow(data_1)

n_without_key
n_key_without_data
n_excluded_metabo


# Contrôle de cohérence
stopifnot(
  n_excluded_metabo ==
    n_without_key + n_key_without_data
)


# ============================================================
# 16. VÉRIFICATION DES VARIABLES D'EXCLUSION CLINIQUE
# ============================================================
#
# Avant de créer les exclusions, on vérifie la correspondance
# entre les codes numériques originaux et les facteurs créés à
# partir du dictionnaire de l'ELCV.
#
# Les critères cliniques retenus sont :
# - sclérose en plaques;
# - maladie de Parkinson;
# - antécédent d'AVC;
# - polio;
# - chirurgie dans les trois derniers mois;
# - chimiothérapie dans les quatre dernières semaines;
# - dialyse;
# - dépistage positif du TSPT.
#
# Alzheimer/démence et traumatisme crânien avec problème de
# mémoire ne sont finalement pas retenus comme exclusions.
# ============================================================


# Sclérose en plaques
table(
  data_1$sd_cc_ms,
  data_1$sd_cc_ms_f,
  useNA = "ifany"
)


# Maladie de Parkinson
table(
  data_1$sd_cc_park,
  data_1$sd_cc_park_f,
  useNA = "ifany"
)


# Antécédent d'AVC
table(
  data_1$sd_cc_stroke,
  data_1$sd_cc_stroke_f,
  useNA = "ifany"
)


# Polio
table(
  data_1$sd_cc_polio,
  data_1$sd_cc_polio_f,
  useNA = "ifany"
)


# Chirurgie dans les trois derniers mois
table(
  data_1$sd_cc_sx_3m,
  data_1$sd_cc_sx_3m_f,
  useNA = "ifany"
)


# Chimiothérapie dans les quatre dernières semaines
table(
  data_1$sd_cc_chemo_4wk,
  data_1$sd_cc_chemo_4wk_f,
  useNA = "ifany"
)


# Dialyse
table(
  data_1$cc_dialysis,
  data_1$cc_dialysis_f,
  useNA = "ifany"
)


# Dépistage positif du TSPT
table(
  data_1$cc_ptsd,
  data_1$cc_ptsd_f,
  useNA = "ifany"
)

# ============================================================
# 17. CRÉATION DES INDICATEURS D'EXCLUSION CLINIQUE
# ============================================================
#
# Pour chaque critère, on crée une variable logique :
#
# TRUE  = critère d'exclusion présent;
# FALSE = critère d'exclusion absent;
# NA    = information manquante ou indéterminée.
#
# D'après les vérifications précédentes :
# - le code 1 correspond à la présence de la condition;
# - pour CCC_DITYP_COM, seul le code 1 est considéré comme
#   une dialyse actuelle correspondant au critère d'exclusion.
# ============================================================

data_1 <- data_1 %>%
  mutate(
    excl_ms =
      sd_cc_ms == 1,
    
    excl_parkinson =
      sd_cc_park == 1,
    
    excl_stroke =
      sd_cc_stroke == 1,
    
    excl_polio =
      sd_cc_polio == 1,
    
    excl_recent_surgery =
      sd_cc_sx_3m == 1,
    
    excl_recent_chemo =
      sd_cc_chemo_4wk == 1,
    
    excl_dialysis =
      cc_dialysis == 1,
    
    excl_ptsd =
      cc_ptsd == 1
  )

# ============================================================
# 18. LISTE DES INDICATEURS D'EXCLUSION CLINIQUE
# ============================================================
#
# Cette liste centralise les variables utilisées pour calculer :
# - le nombre d'exclusions par participant;
# - l'indicateur global d'exclusion;
# - la présence d'informations manquantes.
# ============================================================

clinical_exclusion_vars <- c(
  "excl_ms",
  "excl_parkinson",
  "excl_stroke",
  "excl_polio",
  "excl_recent_surgery",
  "excl_recent_chemo",
  "excl_dialysis",
  "excl_ptsd"
)

# ============================================================
# 19. INDICATEURS GLOBAUX D'EXCLUSION CLINIQUE
# ============================================================
#
# n_clinical_exclusions :
#   nombre de critères d'exclusion positifs par participant.
#
# any_clinical_exclusion :
#   TRUE lorsqu'au moins un critère est positif.
#
# missing_clinical_exclusion_info :
#   TRUE lorsqu'au moins une variable source est manquante.
#
# Important :
# une donnée manquante n'est pas automatiquement considérée
# comme un critère d'exclusion. Elle est identifiée séparément.
# ============================================================

data_1 <- data_1 %>%
  mutate(
    n_clinical_exclusions = rowSums(
      across(
        all_of(clinical_exclusion_vars),
        ~ as.integer(.x %in% TRUE)
      )
    ),
    
    any_clinical_exclusion =
      n_clinical_exclusions >= 1,
    
    missing_clinical_exclusion_info = if_any(
      all_of(clinical_exclusion_vars),
      is.na
    )
  )

# ============================================================
# 20. VÉRIFICATION DES EXCLUSIONS CLINIQUES
# ============================================================
#
# On examine :
# - l'effectif associé à chaque critère;
# - la distribution du nombre de critères par participant;
# - le nombre de participants avec au moins une exclusion;
# - le nombre de participants avec une information manquante.
# ============================================================


# Effectif associé à chaque critère individuel
clinical_exclusion_counts <- data_1 %>%
  summarise(
    across(
      all_of(clinical_exclusion_vars),
      ~ sum(.x %in% TRUE)
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "exclusion",
    values_to = "n"
  )

clinical_exclusion_counts


# Nombre de critères par participant
table(
  data_1$n_clinical_exclusions,
  useNA = "ifany"
)


# Présence d'au moins un critère
table(
  data_1$any_clinical_exclusion,
  useNA = "ifany"
)


# Informations manquantes pour au moins un critère
table(
  data_1$missing_clinical_exclusion_info,
  useNA = "ifany"
)


# Résumé global
clinical_exclusion_summary <- data_1 %>%
  summarise(
    n_before_exclusions = n(),
    
    n_with_clinical_exclusion =
      sum(any_clinical_exclusion),
    
    n_without_clinical_exclusion =
      sum(!any_clinical_exclusion),
    
    n_with_missing_exclusion_information =
      sum(missing_clinical_exclusion_info),
    
    maximum_exclusions_per_participant =
      max(n_clinical_exclusions)
  )

clinical_exclusion_summary

# ============================================================
# 21. CRÉATION DE L'ÉCHANTILLON APRÈS EXCLUSIONS CLINIQUES
# ============================================================
#
# data_2 conserve les participants :
# - disposant de données métabolomiques appariées;
# - ne présentant aucun critère clinique d'exclusion.
#
# Les participants ayant une information manquante, mais aucun
# critère positif documenté, sont conservés à cette étape.
# Leur nombre reste documenté pour une future analyse de
# sensibilité.
# ============================================================

data_2 <- data_1 %>%
  filter(
    !any_clinical_exclusion
  )


# Effectif après les exclusions cliniques
n_data_2 <- nrow(data_2)

n_data_2


# Nombre de participants exclus à cette étape
n_excluded_clinical <- nrow(data_1) - nrow(data_2)

n_excluded_clinical


# Contrôle de cohérence
stopifnot(
  n_excluded_clinical ==
    sum(data_1$any_clinical_exclusion)
)


# Vérifier qu'aucun participant retenu ne possède encore
# un critère clinique positif
table(
  data_2$any_clinical_exclusion,
  useNA = "ifany"
)

# ============================================================
# 22. VÉRIFICATION DU CODAGE DU CONTRÔLE QUALITÉ DXA
# ============================================================
#
# y_dxa_status_quality contient le code numérique original.
#
# y_dxa_status_quality_f contient le libellé provenant du
# dictionnaire.
#
# Cette table permet de confirmer que :
# - 1 correspond à Pass;
# - 0 correspond à Fail.
# ============================================================

table(
  data_2$y_dxa_status_quality,
  data_2$y_dxa_status_quality_f,
  useNA = "ifany"
)


# ============================================================
# 23. CRÉATION DU STATUT DE QUALITÉ DXA
# ============================================================
#
# Le statut est regroupé en quatre catégories :
# - Pass;
# - Fail;
# - Missing;
# - Other.
#
# La catégorie Other permet de détecter un code inattendu.
# ============================================================

data_2 <- data_2 %>%
  mutate(
    dxa_qc_status = case_when(
      y_dxa_status_quality == 1 ~ "Pass",
      y_dxa_status_quality == 0 ~ "Fail",
      is.na(y_dxa_status_quality) ~ "Missing",
      TRUE ~ "Other"
    )
  )


# ============================================================
# 24. VÉRIFICATION DU STATUT DXA
# ============================================================

table(
  data_2$dxa_qc_status,
  useNA = "ifany"
)


# Vérifier qu'aucun code inattendu n'est présent
data_2 %>%
  filter(dxa_qc_status == "Other") %>%
  count(y_dxa_status_quality)


# ============================================================
# 25. SÉLECTION DES PARTICIPANTS AVEC DXA VALIDE
# ============================================================
#
# data_3 contient les participants :
# - avec données métabolomiques;
# - sans exclusion clinique;
# - avec un contrôle qualité DXA réussi.
#
# Les DXA manquantes et les QC échoués sont exclus à cette étape.
# ============================================================

data_3 <- data_2 %>%
  filter(
    dxa_qc_status == "Pass"
  )

n_data_3 <- nrow(data_3)

n_data_3


# Effectifs exclus selon le motif
n_dxa_fail <- sum(
  data_2$dxa_qc_status == "Fail",
  na.rm = TRUE
)

n_dxa_missing <- sum(
  data_2$dxa_qc_status == "Missing",
  na.rm = TRUE
)

n_dxa_other <- sum(
  data_2$dxa_qc_status == "Other",
  na.rm = TRUE
)

n_excluded_dxa <- nrow(data_2) - nrow(data_3)

n_dxa_fail
n_dxa_missing
n_dxa_other
n_excluded_dxa


# Contrôle de cohérence
stopifnot(
  n_excluded_dxa ==
    n_dxa_fail +
    n_dxa_missing +
    n_dxa_other
)


# ============================================================
# 26. CALCUL DE LA MASSE MAIGRE APPENDICULAIRE
# ============================================================
#
# L'ALM correspond à la somme de la masse maigre :
# - du bras gauche;
# - du bras droit;
# - de la jambe gauche;
# - de la jambe droite.
#
# Les variables DXA étant exprimées en grammes, la somme est
# divisée par 1 000 pour obtenir des kilogrammes.
# ============================================================

data_3 <- data_3 %>%
  mutate(
    ALM_kg = (
      y_dxa_arm_left_lean +
        y_dxa_arm_right_lean +
        y_dxa_leg_left_lean +
        y_dxa_leg_right_lean
    ) / 1000
  )


# ============================================================
# 27. CALCUL DES INDICES DE MASSE MAIGRE APPENDICULAIRE
# ============================================================
#
# ALMI_kg_m2 :
#   ALM divisée par la taille au carré.
#
# ALM_BMI :
#   ALM divisée par l'IMC.
#
# Ces deux indices pourront être utilisés selon la définition
# retenue pour la faible masse musculaire.
# ============================================================

data_3 <- data_3 %>%
  mutate(
    ALMI_kg_m2 =
      case_when(
        !is.na(ALM_kg) &
          !is.na(sd_height) &
          sd_height > 0 ~
          ALM_kg / sd_height^2,
        
        TRUE ~ NA_real_
      ),
    
    ALM_BMI =
      case_when(
        !is.na(ALM_kg) &
          !is.na(bmi) &
          bmi > 0 ~
          ALM_kg / bmi,
        
        TRUE ~ NA_real_
      )
  )


# ============================================================
# 28. VÉRIFICATION DES VARIABLES DE COMPOSITION CORPORELLE
# ============================================================
#
# On vérifie :
# - le nombre de données manquantes;
# - la moyenne et l'écart-type;
# - les valeurs minimale et maximale.
#
# À ce stade, les valeurs extrêmes sont seulement décrites.
# Elles ne sont pas encore automatiquement exclues.
# ============================================================

data_3 %>%
  summarise(
    n_ALM_missing =
      sum(is.na(ALM_kg)),
    
    mean_ALM =
      mean(ALM_kg, na.rm = TRUE),
    
    sd_ALM =
      sd(ALM_kg, na.rm = TRUE),
    
    min_ALM =
      min(ALM_kg, na.rm = TRUE),
    
    max_ALM =
      max(ALM_kg, na.rm = TRUE),
    
    n_ALMI_missing =
      sum(is.na(ALMI_kg_m2)),
    
    mean_ALMI =
      mean(ALMI_kg_m2, na.rm = TRUE),
    
    sd_ALMI =
      sd(ALMI_kg_m2, na.rm = TRUE),
    
    min_ALMI =
      min(ALMI_kg_m2, na.rm = TRUE),
    
    max_ALMI =
      max(ALMI_kg_m2, na.rm = TRUE),
    
    n_ALM_BMI_missing =
      sum(is.na(ALM_BMI)),
    
    mean_ALM_BMI =
      mean(ALM_BMI, na.rm = TRUE),
    
    sd_ALM_BMI =
      sd(ALM_BMI, na.rm = TRUE),
    
    min_ALM_BMI =
      min(ALM_BMI, na.rm = TRUE),
    
    max_ALM_BMI =
      max(ALM_BMI, na.rm = TRUE)
  )


# ============================================================
# 29. PREMIER RÉSUMÉ DES EFFECTIFS DU FLOW DIAGRAM
# ============================================================
#
# Ce tableau présente les effectifs restant après les étapes
# actuellement réalisées.
#
# Il pourra être complété plus tard avec :
# - données nécessaires aux phénotypes;
# - valeurs invalides;
# - contrôle qualité métabolomique;
# - outliers globaux;
# - échantillon final.
# ============================================================

flow_counts <- tibble::tibble(
  step = c(
    "Participants with baseline data",
    "Participants with metabolomics data",
    "After clinical exclusions",
    "After DXA quality control"
  ),
  
  n_remaining = c(
    nrow(data_0),
    nrow(data_1),
    nrow(data_2),
    nrow(data_3)
  ),
  
  n_excluded_at_step = c(
    NA_integer_,
    nrow(data_0) - nrow(data_1),
    nrow(data_1) - nrow(data_2),
    nrow(data_2) - nrow(data_3)
  )
)

flow_counts

# ============================================================
# 30. VÉRIFICATION DE LA PRÉSENCE DES VARIABLES NÉCESSAIRES
# ============================================================
#
# Avant de poursuivre, on vérifie que les variables nécessaires
# à la définition future des phénotypes sont bien présentes.
#
# Variables envisagées :
# - sexe;
# - IMC;
# - pourcentage de masse grasse;
# - force de préhension;
# - ALM;
# - ALMI;
# - ALM ajustée pour l'IMC;
# - performances physiques.
#
# Cette étape ne supprime encore aucun participant.
# ============================================================

variables_phenotypes <- c(
  "sd_sex",
  "bmi",
  "y_dxa_pfat",
  "y_str_grip_max",
  "ALM_kg",
  "ALMI_kg_m2",
  "ALM_BMI",
  "y_func_walk_time",
  "y_func_tug_time",
  "y_func_chair_time_total",
  "y_func_balance_time_best"
)

# La sortie doit être vide
setdiff(
  variables_phenotypes,
  names(data_3)
)

# ============================================================
# 31. VÉRIFICATION DES STATUTS DES TESTS MUSCULAIRES
# ============================================================
#
# Les temps ou valeurs manquantes peuvent correspondre à :
# - un test non réalisé;
# - un test impossible;
# - un refus;
# - une donnée réellement manquante.
#
# Les variables de statut permettent de mieux interpréter
# les données manquantes avant de les recoder ou de les exclure.
# ============================================================

table(
  data_3$y_str_grip_status,
  data_3$y_str_grip_status_f,
  useNA = "ifany"
)

table(
  data_3$y_func_walk_status,
  data_3$y_func_walk_status_f,
  useNA = "ifany"
)

table(
  data_3$y_func_tug_status,
  data_3$y_func_tug_status_f,
  useNA = "ifany"
)

table(
  data_3$y_func_chair_status,
  data_3$y_func_chair_status_f,
  useNA = "ifany"
)

table(
  data_3$y_func_balance_status,
  data_3$y_func_balance_status_f,
  useNA = "ifany"
)

# ============================================================
# 32. NETTOYAGE DES VARIABLES DE FORCE ET DE PERFORMANCE
# ============================================================
#
# Les codes spéciaux négatifs, comme -88, représentent
# généralement des données manquantes dans l'ELCV.
#
# On crée des variables nettoyées :
# - grip_strength_kg;
# - walk_time_4m_sec;
# - tug_time_sec;
# - chair_time_5_sec;
# - balance_time_sec.
#
# Les variables originales sont conservées pour assurer la
# traçabilité du nettoyage.
# ============================================================

data_3 <- data_3 %>%
  mutate(
    grip_strength_kg = case_when(
      is.na(y_str_grip_max) ~ NA_real_,
      y_str_grip_max < 0 ~ NA_real_,
      TRUE ~ as.numeric(y_str_grip_max)
    ),
    
    walk_time_4m_sec = case_when(
      is.na(y_func_walk_time) ~ NA_real_,
      y_func_walk_time < 0 ~ NA_real_,
      TRUE ~ as.numeric(y_func_walk_time)
    ),
    
    tug_time_sec = case_when(
      is.na(y_func_tug_time) ~ NA_real_,
      y_func_tug_time < 0 ~ NA_real_,
      TRUE ~ as.numeric(y_func_tug_time)
    ),
    
    chair_time_5_sec = case_when(
      is.na(y_func_chair_time_total) ~ NA_real_,
      y_func_chair_time_total < 0 ~ NA_real_,
      TRUE ~ as.numeric(y_func_chair_time_total)
    ),
    
    balance_time_sec = case_when(
      is.na(y_func_balance_time_best) ~ NA_real_,
      y_func_balance_time_best < 0 ~ NA_real_,
      TRUE ~ as.numeric(y_func_balance_time_best)
    )
  )

# ============================================================
# 33. VÉRIFICATION DES DONNÉES MUSCULAIRES MANQUANTES
# ============================================================
#
# On documente la disponibilité de chaque mesure.
#
# Cette étape permet de déterminer quelle variable risque de
# réduire le plus fortement l'échantillon analytique.
# ============================================================

muscle_missing_summary <- data_3 %>%
  summarise(
    n_total = n(),
    
    n_grip_available =
      sum(!is.na(grip_strength_kg)),
    
    n_grip_missing =
      sum(is.na(grip_strength_kg)),
    
    n_ALM_available =
      sum(!is.na(ALM_kg)),
    
    n_ALM_missing =
      sum(is.na(ALM_kg)),
    
    n_ALMI_available =
      sum(!is.na(ALMI_kg_m2)),
    
    n_ALMI_missing =
      sum(is.na(ALMI_kg_m2)),
    
    n_ALM_BMI_available =
      sum(!is.na(ALM_BMI)),
    
    n_ALM_BMI_missing =
      sum(is.na(ALM_BMI)),
    
    n_body_fat_available =
      sum(!is.na(y_dxa_pfat)),
    
    n_body_fat_missing =
      sum(is.na(y_dxa_pfat)),
    
    n_walk_available =
      sum(!is.na(walk_time_4m_sec)),
    
    n_walk_missing =
      sum(is.na(walk_time_4m_sec)),
    
    n_tug_available =
      sum(!is.na(tug_time_sec)),
    
    n_tug_missing =
      sum(is.na(tug_time_sec)),
    
    n_chair_available =
      sum(!is.na(chair_time_5_sec)),
    
    n_chair_missing =
      sum(is.na(chair_time_5_sec)),
    
    n_balance_available =
      sum(!is.na(balance_time_sec)),
    
    n_balance_missing =
      sum(is.na(balance_time_sec))
  )

muscle_missing_summary

# ============================================================
# 34. VÉRIFICATION DES PLAGES DE VALEURS
# ============================================================
#
# Les valeurs minimale, maximale et certains percentiles
# permettent de repérer :
# - des valeurs impossibles;
# - des erreurs d'unité;
# - des valeurs extrêmes, mais potentiellement plausibles.
#
# Aucune exclusion n'est appliquée à cette étape.
# ============================================================

muscle_distribution_summary <- data_3 %>%
  summarise(
    bmi_min =
      min(bmi, na.rm = TRUE),
    
    bmi_p01 =
      quantile(bmi, 0.01, na.rm = TRUE),
    
    bmi_median =
      median(bmi, na.rm = TRUE),
    
    bmi_p99 =
      quantile(bmi, 0.99, na.rm = TRUE),
    
    bmi_max =
      max(bmi, na.rm = TRUE),
    
    body_fat_min =
      min(y_dxa_pfat, na.rm = TRUE),
    
    body_fat_p01 =
      quantile(y_dxa_pfat, 0.01, na.rm = TRUE),
    
    body_fat_median =
      median(y_dxa_pfat, na.rm = TRUE),
    
    body_fat_p99 =
      quantile(y_dxa_pfat, 0.99, na.rm = TRUE),
    
    body_fat_max =
      max(y_dxa_pfat, na.rm = TRUE),
    
    grip_min =
      min(grip_strength_kg, na.rm = TRUE),
    
    grip_p01 =
      quantile(grip_strength_kg, 0.01, na.rm = TRUE),
    
    grip_median =
      median(grip_strength_kg, na.rm = TRUE),
    
    grip_p99 =
      quantile(grip_strength_kg, 0.99, na.rm = TRUE),
    
    grip_max =
      max(grip_strength_kg, na.rm = TRUE),
    
    ALM_min =
      min(ALM_kg, na.rm = TRUE),
    
    ALM_p01 =
      quantile(ALM_kg, 0.01, na.rm = TRUE),
    
    ALM_median =
      median(ALM_kg, na.rm = TRUE),
    
    ALM_p99 =
      quantile(ALM_kg, 0.99, na.rm = TRUE),
    
    ALM_max =
      max(ALM_kg, na.rm = TRUE)
  )

muscle_distribution_summary

# ============================================================
# 35. IDENTIFICATION DES VALEURS CLAIREMENT INVALIDES
# ============================================================
#
# On distingue ici les valeurs invalides des simples outliers.
#
# Une valeur invalide est une valeur qui ne peut pas être
# physiologiquement ou mathématiquement interprétée, par exemple :
# - taille ou poids inférieur ou égal à zéro;
# - IMC inférieur ou égal à zéro;
# - pourcentage de masse grasse hors de l'intervalle 0-100 %;
# - force négative;
# - ALM inférieure ou égale à zéro;
# - temps de test inférieur ou égal à zéro.
#
# Les valeurs très élevées ne sont pas automatiquement classées
# comme invalides.
# ============================================================

data_3 <- data_3 %>%
  mutate(
    invalid_height =
      !is.na(sd_height) &
      sd_height <= 0,
    
    invalid_weight =
      !is.na(sd_bodyweight) &
      sd_bodyweight <= 0,
    
    invalid_bmi =
      !is.na(bmi) &
      bmi <= 0,
    
    invalid_body_fat =
      !is.na(y_dxa_pfat) &
      (
        y_dxa_pfat < 0 |
          y_dxa_pfat > 100
      ),
    
    invalid_grip =
      !is.na(grip_strength_kg) &
      grip_strength_kg < 0,
    
    invalid_ALM =
      !is.na(ALM_kg) &
      ALM_kg <= 0,
    
    invalid_ALMI =
      !is.na(ALMI_kg_m2) &
      ALMI_kg_m2 <= 0,
    
    invalid_ALM_BMI =
      !is.na(ALM_BMI) &
      ALM_BMI <= 0,
    
    invalid_walk_time =
      !is.na(walk_time_4m_sec) &
      walk_time_4m_sec <= 0,
    
    invalid_tug_time =
      !is.na(tug_time_sec) &
      tug_time_sec <= 0,
    
    invalid_chair_time =
      !is.na(chair_time_5_sec) &
      chair_time_5_sec <= 0,
    
    invalid_balance_time =
      !is.na(balance_time_sec) &
      balance_time_sec < 0
  )

# ============================================================
# 36. RÉSUMÉ DES VALEURS INVALIDES
# ============================================================

invalid_value_vars <- c(
  "invalid_height",
  "invalid_weight",
  "invalid_bmi",
  "invalid_body_fat",
  "invalid_grip",
  "invalid_ALM",
  "invalid_ALMI",
  "invalid_ALM_BMI",
  "invalid_walk_time",
  "invalid_tug_time",
  "invalid_chair_time",
  "invalid_balance_time"
)

invalid_value_counts <- data_3 %>%
  summarise(
    across(
      all_of(invalid_value_vars),
      ~ sum(.x %in% TRUE)
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "invalid_value",
    values_to = "n"
  )

invalid_value_counts

# ============================================================
# 37. CRÉATION DE L'INDICATEUR GLOBAL DE VALEUR INVALIDE
# ============================================================
#
# any_invalid_value vaut TRUE lorsqu'un participant possède
# au moins une valeur clairement invalide.
#
# On conserve aussi le nombre total de valeurs invalides par
# participant pour faciliter les vérifications.
# ============================================================

data_3 <- data_3 %>%
  mutate(
    n_invalid_values = rowSums(
      across(
        all_of(invalid_value_vars),
        ~ as.integer(.x %in% TRUE)
      )
    ),
    
    any_invalid_value =
      n_invalid_values >= 1
  )

table(
  data_3$n_invalid_values,
  useNA = "ifany"
)

table(
  data_3$any_invalid_value,
  useNA = "ifany"
)

# ============================================================
# 38. EXAMEN DES PARTICIPANTS AVEC VALEURS INVALIDES
# ============================================================
#
# Avant de les exclure, on affiche les participants concernés
# et les variables principales.
#
# Cette vérification est importante pour déterminer si le
# problème provient réellement des données ou d'une erreur dans
# le nettoyage, le renommage ou les unités.
# ============================================================

invalid_participants <- data_3 %>%
  filter(
    any_invalid_value
  ) %>%
  select(
    entity_id,
    sd_metabo_key,
    sd_height,
    sd_bodyweight,
    bmi,
    y_dxa_pfat,
    grip_strength_kg,
    ALM_kg,
    ALMI_kg_m2,
    ALM_BMI,
    walk_time_4m_sec,
    tug_time_sec,
    chair_time_5_sec,
    balance_time_sec,
    all_of(invalid_value_vars)
  )

invalid_participants

# ============================================================
# 39. CONCLUSION SUR LES VALEURS INVALIDES
# ============================================================
#
# Aucun participant ne présente de valeur clairement invalide
# parmi les variables cliniques, musculaires et de composition
# corporelle vérifiées.
#
# Par conséquent :
# - aucune exclusion supplémentaire n'est appliquée à cette étape;
# - data_4 est identique à data_3;
# - les valeurs extrêmes, mais physiologiquement possibles,
#   seront étudiées séparément comme outliers potentiels.
# ============================================================

data_4 <- data_3

n_data_4 <- nrow(data_4)

n_data_4


# Vérification
stopifnot(
  nrow(data_4) == nrow(data_3)
)

# ============================================================
# 40. MISE À JOUR DU FLOW DIAGRAM
# ============================================================
#
# Comme aucune valeur invalide n'a été détectée, cette étape
# n'entraîne aucune exclusion.
# ============================================================

flow_counts <- tibble::tibble(
  step = c(
    "Participants with baseline data",
    "Participants with metabolomics data",
    "After clinical exclusions",
    "After DXA quality control",
    "After invalid-value screening"
  ),
  
  n_remaining = c(
    nrow(data_0),
    nrow(data_1),
    nrow(data_2),
    nrow(data_3),
    nrow(data_4)
  ),
  
  n_excluded_at_step = c(
    NA_integer_,
    nrow(data_0) - nrow(data_1),
    nrow(data_1) - nrow(data_2),
    nrow(data_2) - nrow(data_3),
    nrow(data_3) - nrow(data_4)
  )
)

flow_counts
#### Aller chercher les données ####
#
# 4 fichiers: 
#     "2.CLSA METABOLOMICS_v2_July2024"
#     "25CA004_UdeM_AJTessier_Baseline"
#     "25CA004_UdeM_AJTessier_FUP1"
#     "25CA004_UdeM_AJTessier_FUP2"

list.files("/project/def-ajtess/clsa_data/25CA004_UdeM_AJTessier_Baseline")
baseline <- read.csv("/project/def-ajtess/clsa_data/25CA004_UdeM_AJTessier_Baseline/25CA004_UdeM_AJTessier_Baseline_CoPv7-1_Qx_PA_BS.csv")
View(baseline)
dictionary <- read.csv2("~/thelo.projet1/projet1 - baseline_dictionary.csv")
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––
#
#
#
install.packages("dplyr",)
install.packages("haven",)
install.packages("purrr",)
install.packages("tidyr",)
install.packages("labelled")
install.packages("ggplot2")
library(ggplot2)
library(labelled)
library(dplyr)
library(haven)
library(purrr)
library(tidyr)

variables_exclusion_clinique <- c(
  "CCC_MS_COM",         # Sclérose en plaques
  "CCC_ALZH_COM",       # Alzheimer ou démence
  "CCC_PARK_COM",       # Parkinson
  "CCC_CVA_COM",       # AVC/stroke
  "CCC_TIA_COM",        # AIT
  "DXA_POLIO_COM",       # Polio
  "ICQ_SRG3MO_COM",     # Chirurgie dans les 3 derniers mois
  "ICQ_CHEMO4WK_COM",   # Chimiothérapie dans les 4 dernières semaines
  "CCC_DITYP_COM",      # Dialyse
  "PSD_DCTOFF_COM",     # Dépistage positif du TSPT
  "TBI_PROB_MEM_COM"    # Problèmes de mémoire liés à un traumatisme crânien
)

#Avant de créer les exclusions, il faut confirmer quelles valeurs correspondent à « Oui », « Non », « Refus », « Ne sait pas », etc.
#Cette partie affichera, pour chaque variable: les catégories observées, leur fréquence, les codes numériques associés aux libellés.
for (variable in variables_exclusion_clinique) {
  
  cat("\n\n==============================\n")
  cat(variable, "\n")
  cat("==============================\n")
  
  print(
    table(
      as_factor(baseline[[variable]]),
      useNA = "ifany"
    )
  )
  
  if (inherits(baseline[[variable]], "haven_labelled")) {
    cat("\nCodes et libellés :\n")
    print(val_labels(baseline[[variable]]))
  }
}
#
# ============================================================
# 1. CRÉATION DES INDICATEURS INDIVIDUELS D’EXCLUSION CLINIQUE
# ============================================================
#on crée une variable vrai/faux distincte pour chacun des critères cliniques d’exclusion
baseline <- baseline %>%
  mutate(
    excl_sclerose_plaques =
      CCC_MS_COM == 1,
    
    excl_alzheimer_demence =
      CCC_ALZH_COM == 1,
    
    excl_parkinson =
      CCC_PARK_COM == 1,
    
    excl_avc =
      CCC_CVA_COM == 1,
    
    excl_ait =
      CCC_TIA_COM == 1,
    
    excl_polio =
      DXA_POLIO_COM == 1,
    
    excl_chirurgie_3mois =
      ICQ_SRG3MO_COM == 1,
    
    excl_chimiotherapie_4semaines =
      ICQ_CHEMO4WK_COM == 1,
    
    excl_dialyse =
      CCC_DITYP_COM == 1,
    
    excl_tspt =
      PSD_DCTOFF_COM == 1,
    
    excl_traumatisme_memoire =
      TBI_PROB_MEM_COM == 1
  )
#
# Vérification des effectifs
baseline %>%
  summarise(
    sclerose_plaques =
      sum(excl_sclerose_plaques, na.rm = TRUE),
    
    alzheimer_demence =
      sum(excl_alzheimer_demence, na.rm = TRUE),
    
    parkinson =
      sum(excl_parkinson, na.rm = TRUE),
    
    avc =
      sum(excl_avc, na.rm = TRUE),
    
    ait =
      sum(excl_ait, na.rm = TRUE),
    
    polio =
      sum(excl_polio, na.rm = TRUE),
    
    chirurgie_3mois =
      sum(excl_chirurgie_3mois, na.rm = TRUE),
    
    chimiotherapie_4semaines =
      sum(excl_chimiotherapie_4semaines, na.rm = TRUE),
    
    dialyse =
      sum(excl_dialyse, na.rm = TRUE),
    
    tspt =
      sum(excl_tspt, na.rm = TRUE),
    
    traumatisme_memoire =
      sum(excl_traumatisme_memoire, na.rm = TRUE)
  )
#
# ============================================================
# 2. LISTE DES INDICATEURS D’EXCLUSION
# ============================================================
#on regroupe les noms de toutes les variables d’exclusion afin de les utiliser ensemble dans les étapes suivantes
variables_indicateurs_exclusion <- c(
  "excl_sclerose_plaques",
  "excl_alzheimer_demence",
  "excl_parkinson",
  "excl_avc",
  "excl_ait",
  "excl_polio",
  "excl_chirurgie_3mois",
  "excl_chimiotherapie_4semaines",
  "excl_dialyse",
  "excl_tspt",
  "excl_traumatisme_memoire"
)
#
# ============================================================
# 3. INDICATEUR GLOBAL ET NOMBRE DE CRITÈRES PAR PARTICIPANT
# ============================================================
#on détermine si chaque participant présente au moins un critère d’exclusion et compte le nombre total de critères présents
baseline <- baseline %>%
  mutate(
    exclusion_clinique = if_any(
      all_of(variables_indicateurs_exclusion),
      ~ .x %in% TRUE
    )
  )
#
# Pour vérifier le nombre total de participants avec au moins une exclusion
table(
  baseline$exclusion_clinique,
  useNA = "ifany"
)
#
# Compter le nombre de critères d’exclusion par participant pour vérifier les chevauchements entre conditions avant de filtrer.
baseline <- baseline %>%
  mutate(
    nombre_exclusions_cliniques = rowSums(
      across(
        all_of(variables_indicateurs_exclusion),
        ~ as.integer(.x %in% TRUE)
      )
    )
  )
#on vérifie les effectifs individuels
baseline %>%
  summarise(
    sclerose_plaques =
      sum(excl_sclerose_plaques, na.rm = TRUE),
    
    alzheimer_demence =
      sum(excl_alzheimer_demence, na.rm = TRUE),
    
    parkinson =
      sum(excl_parkinson, na.rm = TRUE),
    
    avc =
      sum(excl_avc, na.rm = TRUE),
    
    ait =
      sum(excl_ait, na.rm = TRUE),
    
    polio =
      sum(excl_polio, na.rm = TRUE),
    
    chirurgie_3mois =
      sum(excl_chirurgie_3mois, na.rm = TRUE),
    
    chimiotherapie_4semaines =
      sum(excl_chimiotherapie_4semaines, na.rm = TRUE),
    
    dialyse =
      sum(excl_dialyse, na.rm = TRUE),
    
    tspt =
      sum(excl_tspt, na.rm = TRUE),
    
    traumatisme_memoire =
      sum(excl_traumatisme_memoire, na.rm = TRUE)
  )
#on vérifie la distribution
table(
  baseline$nombre_exclusions_cliniques,
  useNA = "ifany"
)
# On contrôle
baseline %>%
  summarise(
    n_total = n(),
    n_sans_exclusion = sum(nombre_exclusions_cliniques == 0),
    n_avec_exclusion = sum(nombre_exclusions_cliniques >= 1),
    maximum_exclusions = max(nombre_exclusions_cliniques)
  )
#
#
# 
# ============================================================
# 4. ÉCHANTILLON APRÈS EXCLUSIONS CLINIQUES
# ============================================================
#créer l’échantillon après exclusions cliniques
baseline_clinique <- baseline %>%
  filter(exclusion_clinique == FALSE)

# on vérifie le nombre de participants
nrow(baseline_clinique)
# on vérifie qu’aucun participant restant ne présente de critère d’exclusion
table(
  baseline_clinique$exclusion_clinique,
  useNA = "ifany"
)
#
# 
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# Analyse de Bland–Altman
# Comparaison entre le poids mesuré sur la balance et
# la masse corporelle totale calculée par le scan DXA


# ============================================================
# 5. ÉCHANTILLON AVEC LES DEUX MESURES DISPONIBLES
# ============================================================
# On conserve uniquement les participants ayant :
# 1) une masse corporelle totale DXA valide;
# 2) un poids mesuré sur la balance disponible.

baseline_bland_altman <- baseline_clinique %>%
  filter(
    !is.na(DXA_WBC_WBTOT_MASS_COM),
    DXA_WBC_WBTOT_MASS_COM > 0,
    !is.na(WGT_WEIGHT_KG_COM),
    WGT_WEIGHT_KG_COM > 0
  ) %>%
  mutate(
    # Conversion de la masse totale DXA de grammes en kilogrammes
    masse_totale_dxa_kg =
      DXA_WBC_WBTOT_MASS_COM / 1000
  )


# Vérification du nombre de participants
nrow(baseline_bland_altman)


# ============================================================
# 6. MOYENNE ET DIFFÉRENCE ENTRE LES DEUX MESURES
# ============================================================
# Pour chaque participant :
# - moyenne des deux méthodes;
# - différence entre la masse totale DXA et le poids balance.

baseline_bland_altman <- baseline_bland_altman %>%
  mutate(
    moyenne_poids = (
      masse_totale_dxa_kg +
        WGT_WEIGHT_KG_COM
    ) / 2,
    
    difference_poids =
      masse_totale_dxa_kg -
      WGT_WEIGHT_KG_COM
  )


# ============================================================
# 7. CALCUL DES LIMITES D’ACCORD À 95 %
# ============================================================
# On estime :
# - le biais moyen entre les deux méthodes;
# - les limites d’accord à 95 %.

biais_moyen <- mean(
  baseline_bland_altman$difference_poids,
  na.rm = TRUE
)

ecart_type_difference <- sd(
  baseline_bland_altman$difference_poids,
  na.rm = TRUE
)

limite_inferieure <-
  biais_moyen - 1.96 * ecart_type_difference

limite_superieure <-
  biais_moyen + 1.96 * ecart_type_difference


# Affichage des résultats
c(
  biais_moyen = biais_moyen,
  ecart_type_difference = ecart_type_difference,
  limite_inferieure = limite_inferieure,
  limite_superieure = limite_superieure
)


# ============================================================
# 8. INDICATEUR HORS DES LIMITES DE BLAND–ALTMAN
# ============================================================
# On identifie les participants dont la différence entre les
# deux méthodes se situe en dehors des limites d’accord.

baseline_bland_altman <- baseline_bland_altman %>%
  mutate(
    exclusion_bland_altman =
      difference_poids < limite_inferieure |
      difference_poids > limite_superieure
  )


# Vérification
table(
  baseline_bland_altman$exclusion_bland_altman,
  useNA = "ifany"
)


# ============================================================
# 9. GRAPHIQUE DE BLAND–ALTMAN
# ============================================================

ggplot(
  baseline_bland_altman,
  aes(
    x = moyenne_poids,
    y = difference_poids
  )
) +
  geom_point(alpha = 0.35) +
  geom_hline(
    yintercept = biais_moyen,
    linetype = "solid"
  ) +
  geom_hline(
    yintercept = limite_inferieure,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = limite_superieure,
    linetype = "dashed"
  ) +
  labs(
    x = "Moyenne des deux mesures, kg",
    y = "Masse totale DXA − poids balance, kg",
    title = "Graphique de Bland–Altman",
    subtitle = paste0(
      "Biais moyen = ",
      round(biais_moyen, 2),
      " kg; limites d’accord = [",
      round(limite_inferieure, 2),
      "; ",
      round(limite_superieure, 2),
      "] kg"
    )
  ) +
  theme_minimal()


# ============================================================
# 10. RATTACHER L’INDICATEUR À L’ÉCHANTILLON CLINIQUE
# ============================================================
# On ajoute les résultats du Bland–Altman à baseline_clinique
# pour les participants ayant les deux mesures disponibles.

baseline_clinique <- baseline_clinique %>%
  left_join(
    baseline_bland_altman %>%
      select(
        entity_id,
        masse_totale_dxa_kg,
        difference_poids,
        moyenne_poids,
        exclusion_bland_altman
      ),
    by = "entity_id"
  )


# Vérification
table(
  baseline_clinique$exclusion_bland_altman,
  useNA = "ifany"
)


# ============================================================
# 11. ÉCHANTILLON APRÈS VALIDATION BLAND–ALTMAN
# ============================================================
# On conserve uniquement les participants :
# - ayant les deux mesures disponibles;
# - dont la différence est située dans les limites d’accord.

baseline_dxa_valide <- baseline_clinique %>%
  filter(
    exclusion_bland_altman == FALSE
  )


# Vérifications
nrow(baseline_dxa_valide)

table(
  baseline_dxa_valide$exclusion_bland_altman,
  useNA = "ifany"
)
#
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#
#
#
# ============================================================
# 12. LISTE DES VARIABLES ALIMENTAIRES
# ============================================================
# on regroupe toutes les variables alimentaires nécessaires afin de pouvoir vérifier leur disponibilité et leur complétude ensemble
variables_alimentation <- c(
  "NUT_FBR_NB_COM",
  "NUT_BRD_NB_COM",
  "NUT_MEAT_NB_COM",
  "NUT_MTOT_NB_COM",
  "NUT_CHCK_NB_COM",
  "NUT_FISH_NB_COM",
  "NUT_SASG_NB_COM",
  "NUT_PATE_NB_COM",
  "NUT_SAUC_NB_COM",
  "NUT_O3EG_NB_COM",
  "NUT_EGGS_NB_COM",
  "NUT_LEGM_NB_COM",
  "NUT_NUTS_NB_COM",
  "NUT_FRUT_NB_COM",
  "NUT_GREEN_NB_COM",
  "NUT_PTTO_NB_COM",
  "NUT_FRIE_NB_COM",
  "NUT_CRRT_NB_COM",
  "NUT_VGOT_NB_COM",
  "NUT_LWCS_NB_COM",
  "NUT_CHSE_NB_COM",
  "NUT_LWYG_NB_COM",
  "NUT_YOGR_NB_COM",
  "NUT_CALC_NB_COM",
  "NUT_DAIR_NB_COM",
  "NUT_SALT_NB_COM"
)
# on vérifie qu'elles existe dans baseline_dxa_valide
setdiff(
  variables_alimentation,
  names(baseline_dxa_valide)
)
#
# ============================================================
# 13. VÉRIFICATION DES DONNÉES ALIMENTAIRES MANQUANTES
# ============================================================
#on compte, pour chaque variable alimentaire, le nombre de participants ayant une valeur manquante
baseline_dxa_valide %>%
  summarise(
    across(
      all_of(variables_alimentation),
      ~ sum(is.na(.x))
    )
  )
#
#
# ============================================================
# 14. INDICATEUR DE COMPLÉTUDE ALIMENTAIRE
# ============================================================
#on indique si chaque participant possède une valeur pour toutes les variables alimentaires requises
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    alimentation_complete = if_all(
      all_of(variables_alimentation),
      ~ !is.na(.x)
    )
  )
#on vérifie
table(
  baseline_dxa_valide$alimentation_complete,
  useNA = "ifany"
)
#
#
#
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#
#
#
# ============================================================
# 15. CRÉATION DE LA VARIABLE D’ETHNICITÉ
# ============================================================
#on distingue les participants non européens, les réponses « Other », les participants présumés européens et les réponses inconnues ou refusées
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    ethnicity = case_when(
      
      # Réponse inconnue ou refusée
      SDC_ETHN_DK_NA_COM == 1 |
        SDC_ETHN_REFUSED_COM == 1 ~ NA_character_,
      
      # Autre origine ethnique
      SDC_ETHN_OT_COM == 1 ~ "Other",
      
      # Groupes définis comme non européens
      SDC_ETHN_ZH_COM == 1 |
        SDC_ETHN_SA_COM == 1 |
        SDC_ETHN_HE_COM == 1 ~ "Non-European",
      
      # Aucune des catégories précédentes
      SDC_ETHN_ZH_COM == 0 &
        SDC_ETHN_SA_COM == 0 &
        SDC_ETHN_HE_COM == 0 &
        SDC_ETHN_OT_COM == 0 &
        SDC_ETHN_DK_NA_COM == 0 &
        SDC_ETHN_REFUSED_COM == 0 ~ "European",
      
      # Toute combinaison ambiguë ou manquante
      TRUE ~ NA_character_
    )
  )
#on vérifie
table(
  baseline_dxa_valide$ethnicity,
  useNA = "ifany"
)
#
#
#
# ============================================================
# 17. NETTOYAGE DES COMPOSANTES DU SPPB MODIFIÉ
# ============================================================
#on prépare les temps valides de marche, de levers de chaise et d’équilibre unipodal avant leur transformation en sous-scores
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    walk_time_4m = na_if(WLK_TIME_COM, -88),
    chair_time_5 = na_if(CR_TIME_COM, -88),
    balance_unipodal_sec = BAL_BEST_COM
  )
#
#
# ============================================================
# 18. VÉRIFICATION DES COMPOSANTES DU SPPB_UNIPODAL
# ============================================================
#on vérifie le nombre de mesures disponibles et la plage des temps pour la marche, les levers de chaise et l’équilibre unipodal avant de créer les sous-scores
baseline_dxa_valide %>%
  summarise(
    n_total = n(),
    
    n_marche_disponible =
      sum(!is.na(walk_time_4m)),
    
    n_marche_manquante =
      sum(is.na(walk_time_4m)),
    
    min_marche =
      min(walk_time_4m, na.rm = TRUE),
    
    max_marche =
      max(walk_time_4m, na.rm = TRUE),
    
    n_chaise_disponible =
      sum(!is.na(chair_time_5)),
    
    n_chaise_manquante =
      sum(is.na(chair_time_5)),
    
    min_chaise =
      min(chair_time_5, na.rm = TRUE),
    
    max_chaise =
      max(chair_time_5, na.rm = TRUE),
    
    n_equilibre_disponible =
      sum(!is.na(balance_unipodal_sec)),
    
    n_equilibre_manquant =
      sum(is.na(balance_unipodal_sec)),
    
    min_equilibre =
      min(balance_unipodal_sec, na.rm = TRUE),
    
    max_equilibre =
      max(balance_unipodal_sec, na.rm = TRUE)
  )
#
#
# ============================================================
# 19. VÉRIFICATION DU STATUT DES TESTS FONCTIONNELS
# ============================================================
#on détermine si les valeurs manquantes correspondent à un test non applicable, sauté ou réellement absent avant d’attribuer 0 ou NA dans le SPPB_unipodal
table(
  baseline_dxa_valide$WLK_STATUS_COM,
  useNA = "ifany"
)

table(
  baseline_dxa_valide$CR_STATUS_COM,
  useNA = "ifany"
)

table(
  baseline_dxa_valide$BAL_STATUS_COM,
  useNA = "ifany"
)
#on croise le statut avec la disponibilité du temps
baseline_dxa_valide %>%
  count(
    WLK_STATUS_COM,
    marche_manquante = is.na(walk_time_4m)
  )

baseline_dxa_valide %>%
  count(
    CR_STATUS_COM,
    chaise_manquante = is.na(chair_time_5)
  )

baseline_dxa_valide %>%
  count(
    BAL_STATUS_COM,
    equilibre_manquant = is.na(balance_unipodal_sec)
  )
#
#
#
# ============================================================
# 20. COMPLÉTUDE DES TROIS COMPOSANTES DU SPPB_UNIPODAL
# ============================================================
#on identifie les participants ayant complété les trois tests et disposant d’une mesure valide pour chaque composante
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    sppb_unipodal_complet =
      WLK_STATUS_COM == 1 &
      CR_STATUS_COM == 1 &
      BAL_STATUS_COM == 1 &
      !is.na(walk_time_4m) &
      !is.na(chair_time_5) &
      !is.na(balance_unipodal_sec)
  )
#on vérifie
table(
  baseline_dxa_valide$sppb_unipodal_complet,
  useNA = "ifany"
)
#
baseline_dxa_valide %>%
  summarise(
    n_total = n(),
    n_sppb_complet = sum(sppb_unipodal_complet),
    n_sppb_incomplet = sum(!sppb_unipodal_complet)
  )
#
#
#
#
# ============================================================
# 21. CRÉATION DES SOUS-SCORES DU SPPB_UNIPODAL
# ============================================================
#on transforme les trois performances en sous-scores de 0 à 4 et les additionne pour créer le SPPB_unipodal, compris entre 0 et 12
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    
    # --------------------------------------------------------
    # Sous-score de marche sur 4 mètres : 0 à 4 points
    # --------------------------------------------------------
    
    score_marche_4m = case_when(
      !sppb_unipodal_complet ~ NA_real_,
      
      walk_time_4m < 4.82 ~ 4,
      walk_time_4m <= 6.20 ~ 3,
      walk_time_4m <= 8.70 ~ 2,
      walk_time_4m > 8.70 ~ 1,
      
      TRUE ~ NA_real_
    ),
    
    # --------------------------------------------------------
    # Sous-score des cinq levers de chaise : 0 à 4 points
    # --------------------------------------------------------
    
    score_lever_chaise = case_when(
      !sppb_unipodal_complet ~ NA_real_,
      
      chair_time_5 <= 11.19 ~ 4,
      chair_time_5 <= 13.69 ~ 3,
      chair_time_5 <= 16.69 ~ 2,
      chair_time_5 >= 16.70 ~ 1,
      
      TRUE ~ NA_real_
    ),
    
    # --------------------------------------------------------
    # Sous-score modifié d’équilibre unipodal : 0 à 4 points
    # Seuils définis spécifiquement pour cette étude
    # --------------------------------------------------------
    
    score_equilibre_unipodal = case_when(
      !sppb_unipodal_complet ~ NA_real_,
      
      balance_unipodal_sec < 5 ~ 0,
      balance_unipodal_sec < 10 ~ 1,
      balance_unipodal_sec < 20 ~ 2,
      balance_unipodal_sec < 40 ~ 3,
      balance_unipodal_sec >= 40 ~ 4,
      
      TRUE ~ NA_real_
    ),
    
    # --------------------------------------------------------
    # Score total modifié : 0 à 12 points
    # --------------------------------------------------------
    
    SPPB_unipodal =
      score_marche_4m +
      score_lever_chaise +
      score_equilibre_unipodal
  )
#on vérifie
table(
  baseline_dxa_valide$score_marche_4m,
  useNA = "ifany"
)

table(
  baseline_dxa_valide$score_lever_chaise,
  useNA = "ifany"
)

table(
  baseline_dxa_valide$score_equilibre_unipodal,
  useNA = "ifany"
)

table(
  baseline_dxa_valide$SPPB_unipodal,
  useNA = "ifany"
)
#on confirme que le score reste bien entre 0 et 12
baseline_dxa_valide %>%
  summarise(
    n_score_disponible = sum(!is.na(SPPB_unipodal)),
    n_score_manquant = sum(is.na(SPPB_unipodal)),
    score_minimum = min(SPPB_unipodal, na.rm = TRUE),
    score_maximum = max(SPPB_unipodal, na.rm = TRUE)
  )
#
# le score est donc un SPPB_unipodal chez les participants ayant complété les trois tests, et non un score attribuant automatiquement 0 aux tests impossibles ou non réalisés
#
#
# ============================================================
# 22. CALCUL DE LA MASSE MAIGRE APPENDICULAIRE (ALM)
# ============================================================
#on additionne la masse maigre des deux bras et des deux jambes, puis convertit le résultat de grammes en kilogrammes
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    ALM_kg = (
      DXA_WBC_LARM_LEAN_COM +
        DXA_WBC_RARM_LEAN_COM +
        DXA_WBC_L_LEG_LEAN_COM +
        DXA_WBC_R_LEG_LEAN_COM
    ) / 1000
  )
#on vérifie
baseline_dxa_valide %>%
  summarise(
    n_manquant = sum(is.na(ALM_kg)),
    moyenne = mean(ALM_kg, na.rm = TRUE),
    ecart_type = sd(ALM_kg, na.rm = TRUE),
    minimum = min(ALM_kg, na.rm = TRUE),
    maximum = max(ALM_kg, na.rm = TRUE)
  )
#
#
# ============================================================
# 23. CALCUL DE L’ALMI
# ============================================================
#on divise l’ALM par la taille au carré afin d’obtenir l’ALMI en kg/m²
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    ALMI_kg_m2 = ALM_kg / (HGT_HEIGHT_M_COM^2)
  )
#on vérifie
baseline_dxa_valide %>%
  summarise(
    n_manquant = sum(is.na(ALMI_kg_m2)),
    moyenne = mean(ALMI_kg_m2, na.rm = TRUE),
    ecart_type = sd(ALMI_kg_m2, na.rm = TRUE),
    minimum = min(ALMI_kg_m2, na.rm = TRUE),
    maximum = max(ALMI_kg_m2, na.rm = TRUE)
  )
#
#
#
#
# ============================================================
# CRÉATION DES VARIABLES WHOLE BODY
# ============================================================
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    total_mass_kg =
      DXA_WBC_WBTOT_MASS_COM / 1000,
    
    total_fat_mass_kg =
      DXA_WBC_WBTOT_FAT_COM / 1000,
    
    total_percent_fat =
      DXA_WBC_WBTOT_PFAT_COM,
    
    total_lean_mass_kg =
      DXA_WBC_WBTOT_LEAN_COM / 1000
  )
# ============================================================
# 24. LISTE DES COVARIABLES REQUISES (non finale)
# ============================================================
#on regroupe les noms exacts des covariables et les variables dérivées nécessaires à ton analyse
variables_covariables <- c(
  "AGE_NMBR_COM",
  "SEX_ASK_COM",
  "ethnicity",
  "ED_HIGH_COM",
  "INC_TOT_COM",
  "HWT_DBMI_COM",
  "WHC_WAIST_CM_COM",
  
  # Composition corporelle DXA
  "total_mass_kg",
  "total_fat_mass_kg",
  "total_percent_fat",
  "total_lean_mass_kg",
  "ALM_kg",
  "ALMI_kg_m2",
  
  # Force et performance physique
  "GS_EXAM_MAX_COM",
  "walk_time_4m",
  "TUG_TIME_COM",
  "chair_time_5",
  "balance_unipodal_sec",
  "SPPB_unipodal",
  
  # Tabagisme
  "ICQ_SMOKE_COM"
)
#on vérifie qu'elles existent toutes
setdiff(
  variables_covariables,
  names(baseline_dxa_valide)
)
#
#
# ============================================================
# 25. VÉRIFICATION DES COVARIABLES MANQUANTES
# ============================================================
#on compte, pour chaque covariable finale, le nombre de participants ayant une donnée manquante
baseline_dxa_valide %>%
  summarise(
    across(
      all_of(variables_covariables),
      ~ sum(is.na(.x))
    )
  )
#
# ============================================================
# 26. VÉRIFICATION DES CODES SPÉCIAUX DES COVARIABLES
# ============================================================
#on vérifie si les variables catégorielles contiennent des codes de refus, d’incertitude ou de non-réponse qui ne sont pas actuellement reconnus comme NA
variables_categorielles <- c(
  "SEX_ASK_COM",
  "ED_HIGH_COM",
  "INC_TOT_COM",
  "ICQ_SMOKE_COM"
)

for (variable in variables_categorielles) {
  
  cat("\n\n==============================\n")
  cat(variable, "\n")
  cat("==============================\n")
  
  print(
    table(
      baseline_dxa_valide[[variable]],
      useNA = "ifany"
    )
  )
}
#on vérifie
baseline_dxa_valide %>%
  summarise(
    min_grip = min(GS_EXAM_MAX_COM, na.rm = TRUE),
    max_grip = max(GS_EXAM_MAX_COM, na.rm = TRUE),
    
    min_tug = min(TUG_TIME_COM, na.rm = TRUE),
    max_tug = max(TUG_TIME_COM, na.rm = TRUE),
    
    min_waist = min(WHC_WAIST_CM_COM, na.rm = TRUE),
    max_waist = max(WHC_WAIST_CM_COM, na.rm = TRUE)
  )
#
#
# ============================================================
# 27. VÉRIFICATION DES CODES D’ÉDUCATION ET DE REVENU
# ============================================================
#on identifie précisément les catégories valides et les codes de non-réponse pour l’éducation et le revenu avant leur recodage
dictionary %>%
  filter(
    variable %in% c(
      "ED_HIGH_COM",
      "INC_TOT_COM"
    )
  ) %>%
  select(
    variable,
    code,
    label,
    missing
  )
#
#
#
# ============================================================
# 27. NETTOYAGE DE L’ÉDUCATION, DU REVENU ET DU TUG
# ============================================================
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    # Éducation : 97 = autre, donc catégorie valide
    education_clean = case_when(
      ED_HIGH_COM %in% c(98, 99) ~ NA_real_,
      TRUE ~ as.numeric(ED_HIGH_COM)
    ),
    
    # Revenu : 8 = ne sait pas, 9 = refus
    income_clean = case_when(
      INC_TOT_COM %in% c(8, 9) ~ NA_real_,
      TRUE ~ as.numeric(INC_TOT_COM)
    ),
    
    # TUG : -88 = donnée manquante
    tug_time = na_if(TUG_TIME_COM, -88)
  )
#on vérifie
baseline_dxa_valide %>%
  summarise(
    education_manquante = sum(is.na(education_clean)),
    revenu_manquant = sum(is.na(income_clean)),
    tug_manquant = sum(is.na(tug_time))
  )
#
#
# ============================================================
# 28. MISE À JOUR DE LA LISTE DES COVARIABLES
# ============================================================
variables_covariables <- c(
  "AGE_NMBR_COM",
  "SEX_ASK_COM",
  "ethnicity",
  "education_clean",
  "income_clean",
  "HWT_DBMI_COM",
  "WHC_WAIST_CM_COM",
  
  # Composition corporelle DXA
  "total_mass_kg",
  "total_fat_mass_kg",
  "total_percent_fat",
  "total_lean_mass_kg",
  "ALM_kg",
  "ALMI_kg_m2",
  
  # Force et performance physique
  "GS_EXAM_MAX_COM",
  "walk_time_4m",
  "tug_time",
  "chair_time_5",
  "balance_unipodal_sec",
  "SPPB_unipodal",
  
  # Tabagisme
  "ICQ_SMOKE_COM"
)
#on vérifie
setdiff(
  variables_covariables,
  names(baseline_dxa_valide)
)
#
#
#
#
# ============================================================
# 29. COMPLÉTUDE GLOBALE DES COVARIABLES
# ============================================================
#on indique combien de participants disposent de toutes les covariables requises
baseline_dxa_valide <- baseline_dxa_valide %>%
  mutate(
    covariables_completes = if_all(
      all_of(variables_covariables),
      ~ !is.na(.x)
    )
  )

table(
  baseline_dxa_valide$covariables_completes,
  useNA = "ifany"
)
#
#
# ============================================================
# 30. CRÉATION DE L’ÉCHANTILLON ANALYTIQUE FINAL
# ============================================================
baseline_final <- baseline_dxa_valide %>%
  filter(covariables_completes == TRUE)
#on vérifie
nrow(baseline_final)
#
#
#
#
# ============================================================
# 31. VARIABLES LES PLUS SOUVENT MANQUANTES
# CHEZ LES PARTICIPANTS INCOMPLETS
# ============================================================

baseline_dxa_valide %>%
  filter(covariables_completes == FALSE) %>%
  summarise(
    across(
      all_of(variables_covariables),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_manquant"
  ) %>%
  mutate(
    pourcentage = 100 * n_manquant / sum(!baseline_dxa_valide$covariables_completes)
  ) %>%
  arrange(desc(n_manquant))
#
#
#
# ============================================================
# 30. TABLEAU DES LIBELLÉS DES COVARIABLES
# ============================================================

table_covariables <- tibble::tribble(
  ~variable, ~libelle, ~type, ~unite_categories, ~origine,
  
  "AGE_NMBR_COM",
  "Âge",
  "Continue",
  "Années",
  "ELCV",
  
  "SEX_ASK_COM",
  "Sexe",
  "Catégorielle",
  "F = Femme; M = Homme",
  "ELCV",
  
  "ethnicity",
  "Origine ethnique regroupée",
  "Catégorielle",
  "European; Non-European; Other",
  "Variable dérivée",
  
  "education_clean",
  "Plus haut niveau d’éducation atteint",
  "Catégorielle",
  paste(
    "1 = Aucun diplôme postsecondaire;",
    "2 = École de métiers ou apprentissage;",
    "3 = Collège ou CÉGEP;",
    "4 = Certificat universitaire inférieur au baccalauréat;",
    "5 = Baccalauréat;",
    "6 = Diplôme universitaire supérieur au baccalauréat;",
    "97 = Autre"
  ),
  "ED_HIGH_COM nettoyée",
  
  "income_clean",
  "Revenu total annuel du ménage",
  "Catégorielle",
  paste(
    "1 = < 20 000 $;",
    "2 = 20 000 à < 50 000 $;",
    "3 = 50 000 à < 100 000 $;",
    "4 = 100 000 à < 150 000 $;",
    "5 = ≥ 150 000 $"
  ),
  "INC_TOT_COM nettoyée",
  
  "HWT_DBMI_COM",
  "Indice de masse corporelle",
  "Continue",
  "kg/m²",
  "ELCV",
  
  "WHC_WAIST_CM_COM",
  "Tour de taille",
  "Continue",
  "cm",
  "ELCV",
  
  "DXA_WB_WEIGHT_COM",
  "Poids corporel mesuré lors de l’examen DXA",
  "Continue",
  "kg",
  "DXA_WB_WEIGHT_COM",
  
  "total_mass_kg",
  "Masse corporelle totale calculée par le scan DXA Whole Body",
  "Continue",
  "kg",
  "DXA_WBC_WBTOT_MASS_COM divisée par 1 000",
  
  "total_fat_mass_kg",
  "Masse grasse totale mesurée par DXA Whole Body",
  "Continue",
  "kg",
  "DXA_WBC_WBTOT_FAT_COM divisée par 1 000",
  
  "total_percent_fat",
  "Pourcentage de masse grasse totale mesuré par DXA Whole Body",
  "Continue",
  "%",
  "DXA_WBC_WBTOT_PFAT_COM",
  
  "total_lean_mass_kg",
  "Masse maigre totale mesurée par DXA Whole Body",
  "Continue",
  "kg",
  "DXA_WBC_WBTOT_LEAN_COM divisée par 1 000",
  
  "ALM_kg",
  "Masse maigre appendiculaire",
  "Continue",
  "kg",
  paste(
    "Somme de DXA_WBC_LARM_LEAN_COM,",
    "DXA_WBC_RARM_LEAN_COM,",
    "DXA_WBC_L_LEG_LEAN_COM et",
    "DXA_WBC_R_LEG_LEAN_COM, divisée par 1 000"
  ),
  
  "ALMI_kg_m2",
  "Indice de masse maigre appendiculaire",
  "Continue",
  "kg/m²",
  "ALM_kg divisée par HGT_HEIGHT_M_COM au carré",
  
  "GS_EXAM_MAX_COM",
  "Force de préhension maximale",
  "Continue",
  "kg",
  "ELCV",
  
  "walk_time_4m",
  "Temps de marche sur 4 mètres",
  "Continue",
  "Secondes",
  "WLK_TIME_COM nettoyée",
  
  "tug_time",
  "Temps au test Timed Up and Go",
  "Continue",
  "Secondes",
  "TUG_TIME_COM nettoyée",
  
  "chair_time_5",
  "Temps pour réaliser cinq levers de chaise",
  "Continue",
  "Secondes",
  "CR_TIME_COM nettoyée",
  
  "balance_unipodal_sec",
  "Meilleur temps d’équilibre unipodal",
  "Continue",
  "Secondes, maximum 60",
  "BAL_BEST_COM",
  
  "SPPB_unipodal",
  "Score de performance physique modifié avec équilibre unipodal",
  "Continue discrète",
  "Score de 0 à 12",
  "Variable dérivée",
  
  "ICQ_SMOKE_COM",
  "Statut tabagique",
  "Catégorielle",
  paste(
    "1 = Fumeur actuel;",
    "2 = N’a jamais fumé;",
    "3 = Ancien fumeur"
  ),
  "ELCV"
)
View(table_covariables)
#on vérifie que tous les noms correspondent bien à des variables présentes dans baseline_final
setdiff(
  table_covariables$variable,
  names(baseline_final)
)

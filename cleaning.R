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
# On va identifier les participants présentant au moins un critère d’exclusion, sans encore les retirer.
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
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# créer l’échantillon sans exclusions cliniques
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
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# vérifier le codage du contrôle qualité DXA
table(
  baseline_clinique$DXA_WB_QC_PASS_COM,
  useNA = "ifany"
)
# on vérifie le type de variable et ses éventuels attributs
class(baseline_clinique$DXA_WB_QC_PASS_COM)
attributes(baseline_clinique$DXA_WB_QC_PASS_COM)
# On regarde le croisement avec la disponibilité du poids DXA
baseline_clinique %>%
  count(
    DXA_WB_QC_PASS_COM,
    poids_dxa_manquant = is.na(DXA_WB_WEIGHT_COM)
  )
#
#
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# On ne prend que ceux avec un bon DXA
baseline_dxa <- baseline_clinique %>%
  filter(DXA_WB_QC_PASS_COM == 1)

nrow(baseline_dxa)
#

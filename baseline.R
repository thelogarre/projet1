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

# ethnicity
baseline$ethnicity <- ifelse(
  baseline$SDC_ETHN_ZH_COM == 1 |
    baseline$SDC_ETHN_SA_COM == 1 |
    baseline$SDC_ETHN_HE_COM == 1,
  "Non-European",
  "European"
)

# alcool (nb de verres par semaine)
baseline$alcool <- with(
  baseline,
  ALC_RDWD_NB_COM +
    ALC_WHWD_NB_COM +
    ALC_BRWD_NB_COM +
    ALC_LQWD_NB_COM +
    ALC_OTWD_NB_COM +
    ALC_RDWE_NB_COM +
    ALC_WHWE_NB_COM +
    ALC_BRWE_NB_COM +
    ALC_LQWE_NB_COM +
    ALC_OTWE_NB_COM
)
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––
# Nettoyage
# 1. Créer la liste de toutes les variables à nettoyer
variables_nutrition <- c(
  "NUT_FBR_NB_COM", "NUT_BRD_NB_COM", "NUT_MEAT_NB_COM", "NUT_MTOT_NB_COM",
  "NUT_CHCK_NB_COM", "NUT_FISH_NB_COM", "NUT_SASG_NB_COM", "NUT_PATE_NB_COM",
  "NUT_SAUC_NB_COM", "NUT_O3EG_NB_COM", "NUT_EGGS_NB_COM", "NUT_LEGM_NB_COM",
  "NUT_NUTS_NB_COM", "NUT_FRUT_NB_COM", "NUT_GREEN_NB_COM", "NUT_PTTO_NB_COM",
  "NUT_FRIE_NB_COM", "NUT_CRRT_NB_COM", "NUT_VGOT_NB_COM", "NUT_LWCS_NB_COM",
  "NUT_CHSE_NB_COM", "NUT_LWYG_NB_COM", "NUT_YOGR_NB_COM", "NUT_CALC_NB_COM",
  "NUT_DAIR_NB_COM", "NUT_SALT_NB_COM", "NUT_DSRT_NB_COM", "NUT_CHOC_NB_COM",
  "NUT_BTTR_NB_COM", "NUT_DRSG_NB_COM", "NUT_CAJC_NB_COM", "NUT_PURE_NB_COM",
  "NUT_CAML_NB_COM", "NUT_WHML_NB_COM", "NUT_LFML_NB_COM", "NUT_CADR_NB_COM"
)

# 2. Appliquer le nettoyage automatiquement sur chaque colonne de 'baseline'
for (var in variables_nutrition) {
  baseline[[var]][baseline[[var]] %in% c(9998,9999,7777)] <- NA
}
for (var in variables_nutrition) {
  baseline[[var]][baseline[[var]] == 9996] <- 0
}
#
#
# Pour remettre les variables sur une consommation par semaine
# 3. Reconvertir les fréquences quotidiennes en fréquences hebdomadaires (multiplication par 7)
for (var in variables_nutrition) {
  baseline[[var]] <- baseline[[var]] * 7
}
#
# Remplacer les valeurs aberrantes par NA
# pour ne pas avoir par exemple 51 consommation de viande pas semaine
seuil_max <- 28

for (var in variables_nutrition) {
  baseline[[var]][baseline[[var]] > seuil_max] <- NA
}
#
#
#
# Rich-protein food frequency
# Beef/Pork | Other meats | Chicken/Turkey | Fish | Omega-3 eggs | Eggs | Legumes | Nuts | Low-fat cheese | Regular cheese | Low-fat yogurt | Regular yogurt | Whole milk | Low-fat milk
baseline$protein_score <- with(
  baseline,
  NUT_MEAT_NB_COM +
    NUT_MTOT_NB_COM +
    NUT_CHCK_NB_COM +
    NUT_FISH_NB_COM +
    NUT_O3EG_NB_COM +
    NUT_EGGS_NB_COM +
    NUT_LEGM_NB_COM +
    NUT_NUTS_NB_COM +
    NUT_LWCS_NB_COM +
    NUT_CHSE_NB_COM +
    NUT_LWYG_NB_COM +
    NUT_YOGR_NB_COM +
    NUT_WHML_NB_COM +
    NUT_LFML_NB_COM
  ,
  na.rm = TRUE
)
# Animal Protein Food Frequency Score
# Beef/Pork | Other meats | Chicken/Turkey | Fish | Processed meats | Omega-3 eggs | Eggs | Low-fat cheese | Regular cheese | Low-fat yogurt | Regular yogurt | Whole milk | Low-fat milk
baseline$animalprotein_score <- with(
  baseline,
  NUT_MEAT_NB_COM +
    NUT_MTOT_NB_COM +
    NUT_CHCK_NB_COM +
    NUT_FISH_NB_COM +
    NUT_SASG_NB_COM +
    NUT_O3EG_NB_COM +
    NUT_EGGS_NB_COM +
    NUT_LWCS_NB_COM +
    NUT_CHSE_NB_COM +
    NUT_LWYG_NB_COM +
    NUT_YOGR_NB_COM +
    NUT_WHML_NB_COM +
    NUT_LFML_NB_COM
  ,
  na.rm = true
)

# Plant Protein Food Frequency Score
# Legumes | Nuts, seeds and peanut butter
baseline$plantprotein_score <- with(baseline,
                                    NUT_LEGM_NB_COM +
                                      NUT_NUTS_NB_COM, na.rm = TRUE)
#
# Healthy food frequency 
# Whole-grain cereals | Whole-grain breads | Fruit | Green salad | Carrots | Other vegetables | Legumes | Nuts | Fish | 100% fruit juice
baseline$healthydiet_score <- with(
  baseline,
  NUT_FBR_NB_COM +
    NUT_BRD_NB_COM +
    NUT_FRUT_NB_COM +
    NUT_GREEN_NB_COM +
    NUT_CRRT_NB_COM +
    NUT_VGOT_NB_COM +
    NUT_LEGM_NB_COM +
    NUT_NUTS_NB_COM +
    NUT_FISH_NB_COM +
    NUT_PURE_NB_COM
  ,
  na.rm = TRUE
)
#
# Unhealthy food frequency
# Sausages/processed meats | Patés | French fries/poutine | Salty snacks | Pastries | Chocolate bars | Butter/regular margarine | Regular dressings/dips | Milk-based desserts
baseline$unhealthydiet_score <- with(
  baseline,
  NUT_SASG_NB_COM +
    NUT_PATE_NB_COM +
    NUT_FRIE_NB_COM +
    NUT_SALT_NB_COM +
    NUT_DSRT_NB_COM +
    NUT_CHOC_NB_COM +
    NUT_BTTR_NB_COM +
    NUT_DRSG_NB_COM +
    NUT_DAIR_NB_COM
  ,
  na.rm = TRUE
)
#
#
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––
#### Faire la table 1 ####
# 1. Installer et charger tableone (si pas encore fait)
install.packages("tableone")
library(tableone)

# 2. Lister vos variables d'intérêt (remplacez avec les vrais codes du dictionnaire)
variables_interet <-
  c(
    "AGE_NMBR_COM",
    "SEX_ASK_COM",
    "ED_HIGH_COM",
    "INC_TOT_COM",
    "HWT_DBMI_COM",
    "WHC_WAIST_CM_COM",
    "DXA_TOTAL_MASS_COM",
    "DXA_TOTAL_FAT_MASS_COM",
    "DXA_TOTAL_FAT_PERCENT_COM",
    "DXA_APPENDAGE_LEAN_MASS_COM",
    "DXA_APPEND_LEAN_MASS_H2_COM",
    "DXA_TOTAL_LEAN_MASS_COM",
    "GS_EXAM_MAX_COM",
    "WLK_TIME_COM",
    "TUG_TIME_COM",
    "CR_TIME_COM",
    "BAL_BEST_COM",
    "ICQ_SMOKE_COM",
    "etnicity"
  )

# 3. Préciser quelles variables sont catégorielles
vars_cat <- c("SEX_ASK_COM", "ED_HIGH_COM", "INC_TOT_COM", "etnicity")

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#### création des groupes ####
# 1. Création des indicateurs de base selon le sexe
baseline$sarcopenic_ind <- with(baseline, ifelse(
  (SEX_ASK_COM == "Male" & DXA_APPEND_LEAN_MASS_H2_COM < 7.76) | 
    (SEX_ASK_COM == "Female" & DXA_APPEND_LEAN_MASS_H2_COM < 5.72), 1, 0
))

baseline$dynapenic_ind <- with(baseline, ifelse(
  (SEX_ASK_COM == "Male" & GS_EXAM_MAX_COM < 33.1) | 
    (SEX_ASK_COM == "Female" & GS_EXAM_MAX_COM < 20.4), 1, 0
))

baseline$obese_ind <- with(baseline, ifelse(
  HWT_DBMI_COM > 30 & (
    (SEX_ASK_COM == "Male" & WHC_WAIST_CM_COM > 102) | 
      (SEX_ASK_COM == "Female" & WHC_WAIST_CM_COM > 88)
  ), 1, 0
))

# 2. Création de la variable finale mutuellement exclusive
baseline$groups <- "Healthy" # Par défaut, tout le monde est Healthy

# On applique les critères du plus spécifique au plus général
baseline$groups[baseline$sarcopenic_ind == 1] <- "Sarcopenic"
baseline$groups[baseline$dynapenic_ind == 1] <- "Dynapenic"
baseline$groups[baseline$obese_ind == 1] <- "Obese"

# Le groupe combiné écrase les autres si les 3 conditions sont réunies
baseline$groups[baseline$sarcopenic_ind == 1 & 
                        baseline$dynapenic_ind == 1 & 
                        baseline$obese_ind == 1] <- "Sarcopenic Obese"

# Convertir en facteur pour la Table 1
baseline$groups <- as.factor(baseline$groups)

# 3. Vérification des effectifs par groupe
table(baseline$groups, useNA = "always")
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# 4. Créer la table descriptive globale et par groupe
tableau1 <- CreateTableOne(vars = variables_interet, 
                           strata = "groups", 
                           data = baseline, 
                           factorVars = vars_cat)

# 5. Afficher le tableau avec les p-values (ANOVA / Chi-square gérés automatiquement)
print(tableau1, nonnormal = c("variables_non_normales_si_besoin"), formatOptions = list(big.mark = ","))


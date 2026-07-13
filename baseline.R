#### Aller chercher les données ####
#
# 4 fichiers: 
#     "2.CLSA METABOLOMICS_v2_July2024"
#     "25CA004_UdeM_AJTessier_Baseline"
#     "25CA004_UdeM_AJTessier_FUP1"
#     "25CA004_UdeM_AJTessier_FUP2"

list.files("/project/def-ajtess/clsa_data/25CA004_UdeM_AJTessier_Baseline")
baseline<- read.csv("/project/def-ajtess/clsa_data/25CA004_UdeM_AJTessier_Baseline/25CA004_UdeM_AJTessier_Baseline_CoPv7-1_Qx_PA_BS.csv")
View(baseline)

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
    "DXA_TOTAL_FAT_PERCENT_COM",
    "DXA_APPENDAGE_LEAN_MASS_COM",
    "DXA_APPEND_LEAN_MASS_H2_COM",
    "GS_EXAM_MAX_COM",
    "WLK_TIME_COM",
    "TUG_TIME_COM",
    "CR_TIME_COM",
    "BAL_BEST_COM"
  )

# 3. Préciser quelles variables sont catégorielles
vars_cat <- c("SEX_ASK_COM", "ED_HIGH_COM", "INC_TOT_COM")


###########
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






##############
# 4. Créer la table descriptive globale et par groupe
tableau1 <- CreateTableOne(vars = variables_interet, 
                           strata = "groups", 
                           data = baseline, 
                           factorVars = vars_cat)

# 5. Afficher le tableau avec les p-values (ANOVA / Chi-square gérés automatiquement)
print(tableau1, nonnormal = c("variables_non_normales_si_besoin"), formatOptions = list(big.mark = ","))


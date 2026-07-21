install.packages("gtsummary")
install.packages("gt")
install.packages("dplyr")
install.packages("broom")
install.packages("writexl")
library(writexl)
library(broom)
library(gtsummary)
library(gt)
library(dplyr)

# ============================================================
# 1. CRÉATION DES INDICATEURS CLINIQUES
# ============================================================

baseline_final <- baseline_final %>%
  mutate(
    # --------------------------------------------------------
    # Sexe avec libellé explicite
    # --------------------------------------------------------
    sex = case_when(
      SEX_ASK_COM == "M" ~ "Men",
      SEX_ASK_COM == "F" ~ "Women",
      TRUE ~ NA_character_
    ),
    
    sex = factor(
      sex,
      levels = c("Men", "Women")
    ),
    
    # --------------------------------------------------------
    # Faible ALMI selon le sexe
    # --------------------------------------------------------
    low_almi = case_when(
      SEX_ASK_COM == "M" & ALMI_kg_m2 < 7.76 ~ TRUE,
      SEX_ASK_COM == "M" & ALMI_kg_m2 >= 7.76 ~ FALSE,
      
      SEX_ASK_COM == "F" & ALMI_kg_m2 < 5.72 ~ TRUE,
      SEX_ASK_COM == "F" & ALMI_kg_m2 >= 5.72 ~ FALSE,
      
      TRUE ~ NA
    ),
    
    # --------------------------------------------------------
    # Dynapénie selon le sexe
    # --------------------------------------------------------
    dynapenia = case_when(
      SEX_ASK_COM == "M" & GS_EXAM_MAX_COM < 33.1 ~ TRUE,
      SEX_ASK_COM == "M" & GS_EXAM_MAX_COM >= 33.1 ~ FALSE,
      
      SEX_ASK_COM == "F" & GS_EXAM_MAX_COM < 20.4 ~ TRUE,
      SEX_ASK_COM == "F" & GS_EXAM_MAX_COM >= 20.4 ~ FALSE,
      
      TRUE ~ NA
    ),
    
    # --------------------------------------------------------
    # Obésité selon l’IMC ET le tour de taille
    # --------------------------------------------------------
    obesity = case_when(
      SEX_ASK_COM == "M" &
        HWT_DBMI_COM > 30 
      #&
       # WHC_WAIST_CM_COM > 102 
        ~ TRUE,
      
      SEX_ASK_COM == "F" &
        HWT_DBMI_COM > 30 
      #&
       # WHC_WAIST_CM_COM > 88 
      ~ TRUE,
      
      SEX_ASK_COM %in% c("M", "F") ~ FALSE,
      
      TRUE ~ NA
    ),
    
    # --------------------------------------------------------
    # Sarcopénie : faible ALMI ET dynapénie
    # --------------------------------------------------------
    sarcopenia =
      low_almi == TRUE &
      dynapenia == TRUE
  )


# ============================================================
# 2. VÉRIFICATION DES INDICATEURS PAR SEXE
# ============================================================

baseline_final %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    
    n_low_almi = sum(low_almi, na.rm = TRUE),
    pct_low_almi = 100 * mean(low_almi, na.rm = TRUE),
    
    n_dynapenia = sum(dynapenia, na.rm = TRUE),
    pct_dynapenia = 100 * mean(dynapenia, na.rm = TRUE),
    
    n_sarcopenia = sum(sarcopenia, na.rm = TRUE),
    pct_sarcopenia = 100 * mean(sarcopenia, na.rm = TRUE),
    
    n_obesity = sum(obesity, na.rm = TRUE),
    pct_obesity = 100 * mean(obesity, na.rm = TRUE)
  )


# ============================================================
# 3. CRÉATION DES GROUPES CLINIQUES MUTUELLEMENT EXCLUSIFS
# ============================================================

baseline_final <- baseline_final %>%
  mutate(
    clinical_group = case_when(
      
      # Aucun phénotype
      obesity == FALSE &
        low_almi == FALSE &
        dynapenia == FALSE ~ "Healthy",
      
      # Obésité isolée
      obesity == TRUE &
        low_almi == FALSE &
        dynapenia == FALSE ~ "Obesity only",
      
      # Faible ALMI isolée
      obesity == FALSE &
        low_almi == TRUE &
        dynapenia == FALSE ~ "Low ALMI only",
      
      # Dynapénie isolée
      obesity == FALSE &
        low_almi == FALSE &
        dynapenia == TRUE ~ "Dynapenia only",
      
      # Sarcopénie sans obésité
      obesity == FALSE &
        low_almi == TRUE &
        dynapenia == TRUE ~ "Sarcopenia only",
      
      # Obésité + faible ALMI, sans dynapénie
      obesity == TRUE &
        low_almi == TRUE &
        dynapenia == FALSE ~ "Obesity + low ALMI only",
      
      # Obésité + dynapénie, sans faible ALMI
      obesity == TRUE &
        low_almi == FALSE &
        dynapenia == TRUE ~ "Obesity + dynapenia only",
      
      # Obésité sarcopénique complète
      obesity == TRUE &
        low_almi == TRUE &
        dynapenia == TRUE ~ "Sarcopenic obesity",
      
      TRUE ~ NA_character_
    ),
    
    clinical_group = factor(
      clinical_group,
      levels = c(
        "Healthy",
        "Obesity only",
        "Low ALMI only",
        "Dynapenia only",
        "Sarcopenia only",
        "Obesity + low ALMI only",
        "Obesity + dynapenia only",
        "Sarcopenic obesity"
      )
    )
  )
#on vérifie que chaque participant appartient à un groupe
table(
  baseline_final$clinical_group,
  useNA = "ifany"
)
#on vérifie séparément chez les hommes et les femmes
table(
  baseline_final$sex,
  baseline_final$clinical_group,
  useNA = "ifany"
)
#on regarde les pourcentages
prop.table(
  table(
    baseline_final$sex,
    baseline_final$clinical_group
  ),
  margin = 1
) * 100



# ============================================================
# 4. EFFECTIFS ET POURCENTAGES PAR SEXE ET GROUPE
# ============================================================

table_groupes_sexe <- baseline_final %>%
  count(
    sex,
    clinical_group,
    name = "n"
  ) %>%
  group_by(sex) %>%
  mutate(
    total_sex = sum(n),
    percentage = round(
      100 * n / total_sex,
      1
    )
  ) %>%
  ungroup()
View(table_groupes_sexe)


#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# ============================================================
# 5. PRÉPARATION DES VARIABLES POUR LE TABLEAU 1
# ============================================================

baseline_final <- baseline_final %>%
  mutate(
    sex = factor(
      sex,
      levels = c("Men", "Women"),
      labels = c("Hommes", "Femmes")
    ),
    
    ethnicity = factor(
      ethnicity,
      levels = c(
        "European",
        "Non-European",
        "Other"
      ),
      labels = c(
        "Européenne",
        "Non européenne",
        "Autre"
      )
    ),
    
    education_clean = factor(
      education_clean,
      levels = c(1, 2, 3, 4, 5, 6, 97),
      labels = c(
        "Aucun diplôme postsecondaire",
        "École de métiers ou apprentissage",
        "Collège ou CÉGEP",
        "Certificat universitaire",
        "Baccalauréat",
        "Diplôme supérieur au baccalauréat",
        "Autre"
      )
    ),
    
    income_clean = factor(
      income_clean,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "< 20 000 $",
        "20 000 à < 50 000 $",
        "50 000 à < 100 000 $",
        "100 000 à < 150 000 $",
        "≥ 150 000 $"
      )
    ),
    
    ICQ_SMOKE_COM = factor(
      ICQ_SMOKE_COM,
      levels = c(1, 2, 3),
      labels = c(
        "Fumeur actuel",
        "Jamais fumé",
        "Ancien fumeur"
      )
    )
  )



# ============================================================
# 6. VARIABLES DU TABLEAU 1
# ============================================================

variables_table1 <- c(
  "AGE_NMBR_COM",
  "ethnicity",
  "education_clean",
  "income_clean",
  
  # Anthropométrie et composition corporelle
  "HWT_DBMI_COM",
  "WHC_WAIST_CM_COM",
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
  
  # Mode de vie
  "ICQ_SMOKE_COM"
)


# ============================================================
# 7. TABLEAU 1 — HOMMES
# ============================================================

table1_hommes <- baseline_final %>%
  filter(sex == "Hommes") %>%
  select(
    clinical_group,
    all_of(variables_table1)
  ) %>%
  tbl_summary(
    by = clinical_group,
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    
    missing = "no",
    
    label = list(
      AGE_NMBR_COM ~ "Âge, années",
      ethnicity ~ "Origine ethnique",
      education_clean ~ "Niveau d’éducation",
      income_clean ~ "Revenu annuel du ménage",
      
      HWT_DBMI_COM ~ "IMC, kg/m²",
      WHC_WAIST_CM_COM ~ "Tour de taille, cm",
      total_fat_mass_kg ~ "Masse grasse totale, kg",
      total_percent_fat ~ "Masse grasse totale, %",
      total_lean_mass_kg ~ "Masse maigre totale, kg",
      ALM_kg ~ "Masse maigre appendiculaire, kg",
      ALMI_kg_m2 ~ "ALMI, kg/m²",
      
      GS_EXAM_MAX_COM ~ "Force de préhension maximale, kg",
      walk_time_4m ~ "Temps de marche sur 4 m, s",
      tug_time ~ "Timed Up and Go, s",
      chair_time_5 ~ "Cinq levers de chaise, s",
      balance_unipodal_sec ~ "Équilibre unipodal, s",
      SPPB_unipodal ~ "SPPB modifié, score /12",
      
      ICQ_SMOKE_COM ~ "Statut tabagique"
    )
  ) %>%
  add_n() %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  bold_labels() %>%
  modify_header(
    label ~ "**Caractéristique**",
    n ~ "**N disponible**",
    p.value ~ "**p**"
  ) %>%
  modify_caption(
    "**Tableau 1. Caractéristiques cliniques des hommes selon le phénotype**"
  )

table1_hommes


# ============================================================
# 8. TABLEAU 1 — FEMMES
# ============================================================

table1_femmes <- baseline_final %>%
  filter(sex == "Femmes") %>%
  select(
    clinical_group,
    all_of(variables_table1)
  ) %>%
  tbl_summary(
    by = clinical_group,
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    
    missing = "no",
    
    label = list(
      AGE_NMBR_COM ~ "Âge, années",
      ethnicity ~ "Origine ethnique",
      education_clean ~ "Niveau d’éducation",
      income_clean ~ "Revenu annuel du ménage",
      
      HWT_DBMI_COM ~ "IMC, kg/m²",
      WHC_WAIST_CM_COM ~ "Tour de taille, cm",
      total_fat_mass_kg ~ "Masse grasse totale, kg",
      total_percent_fat ~ "Masse grasse totale, %",
      total_lean_mass_kg ~ "Masse maigre totale, kg",
      ALM_kg ~ "Masse maigre appendiculaire, kg",
      ALMI_kg_m2 ~ "ALMI, kg/m²",
      
      GS_EXAM_MAX_COM ~ "Force de préhension maximale, kg",
      walk_time_4m ~ "Temps de marche sur 4 m, s",
      tug_time ~ "Timed Up and Go, s",
      chair_time_5 ~ "Cinq levers de chaise, s",
      balance_unipodal_sec ~ "Équilibre unipodal, s",
      SPPB_unipodal ~ "SPPB modifié, score /12",
      
      ICQ_SMOKE_COM ~ "Statut tabagique"
    )
  ) %>%
  add_n() %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  bold_labels() %>%
  modify_header(
    label ~ "**Caractéristique**",
    n ~ "**N disponible**",
    p.value ~ "**p**"
  ) %>%
  modify_caption(
    "**Tableau 1. Caractéristiques cliniques des femmes selon le phénotype**"
  )

table1_femmes

#
#
#on combine les tableaux
table1_combine <- tbl_stack(
  list(
    table1_hommes,
    table1_femmes
  ),
  group_header = c(
    "Hommes",
    "Femmes"
  )
)

table1_combine


#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#exporter le tableau en excel
table1_hommes_df <- table1_hommes %>%
  as_tibble()

table1_femmes_df <- table1_femmes %>%
  as_tibble()

write_xlsx(
  list(
    Hommes = table1_hommes_df,
    Femmes = table1_femmes_df
  ),
  path = "Tableau_1_groupes_cliniques.xlsx"
)

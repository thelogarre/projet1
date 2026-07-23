#' ---
#' title: "Generate processed data for covariate (eg, sociodemographic), outcome (eg, muscle, cognition)"
#' author: "Didier Brassard"
#' date: "`r Sys.Date()`"
#' code checked by:
#' code checked date:
#' run time: <10 min.
#' output:
#'  html_document:
#'    code_folding: "show"
#' ---

#'
#### Set-up and Library ####
# *********************************************************************** #
#                           Set-up and Library                            #
# *********************************************************************** #

#'
##### Library #####
# ********************************************** #
#                    Library                     #
# ********************************************** #

## data
install.packages("dplyr")
install.packages("tidylog")
install.packages("readxl")
install.packages("data.table")
install.packages("purrr")
install.packages("data.table")
install.packages("summarytools")
install.packages("writexl")
install.packages("gtsummary")
library(dplyr)
library(tidylog)
library(readxl)
library(data.table); if(is.na(parallel::detectCores())==FALSE & parallel::detectCores()>1){available_core <- parallel::detectCores()-1} else {available_core <- 1}
data.table::setDTthreads(threads = available_core)
data.table::getDTthreads()
library(purrr)
library(summarytools)
library(writexl)

## presentation
library(gtsummary)

## project
library(here)

## suppress scientific notation
options(scipen = 9999)

#'
##### Directories #####
# ********************************************** #
#                  Directories                   #
# ********************************************** #

## Local directory
dir_scripts <- here::here("scripts")
dir_meta <- here::here("data", "metadata")
dir_processed <- here::here("data", "processed")
dir_raw <-  here::here("data", "raw")
dir_results <- here::here("data", "results")

# common prefix for all data file
project_id <- "25CA004_UdeM_AJTessier_"

#'
##### Questionnaire data filename ##### 
# ********************************************** #
#          Questionnaire data filename           #
# ********************************************** #

# note: these data names are used throughout to load complete questionnaire data

data_name_questionnaire <- 
  paste0(project_id,
         c("Baseline_CoPv7-1_Qx_PA_BS.csv",
           "FUP1_CoPv5_Qx_PA_BS.csv",
           "FUP2_CoPv3_Qx_PA.csv"))

data_name_dictionary <- 
  paste0(project_id,
         c("Baseline_CoPv7-1_Qx_PA_BS-dictionary.xlsx",
           "FUP1_CoPv5_Qx_PA_BS-dictionary.xlsx",
           "FUP2_CoPv3_Qx_PA-dictionary.xlsx"))


#'
##### Load in-house functions  #####
# ********************************************** #
#            Load in-house functions             #
# ********************************************** #

# save data, create (processed) data dictionary
source(file.path(dir_scripts, "save_and_summarize_data.R"))

# prepare a generic questionnaire data (selection, rename, labels)
source(file.path(dir_scripts, "prepare_clsa_questionnaire.R"))

# aux. function to support data cleaning
source(file.path(dir_scripts, "helper_data_cleaning.R"))

#'
#### 0) Status data preparation ####
# *********************************************************************** #
#                        Status data preparation                          #
# *********************************************************************** #

status_var_all <- 
  c("entity_id", "cohort",
    "clsa_baseline_date",
    "clsa_fup1",
    "clsa_fup1_date",
    "clsa_fup2",
    "clsa_fup2_date",
    "death",
    "source_death",
    "date_death",
    "withdrawn",
    "date_withdrawn",
    "clsa_last_seen",
    "clsa_last_known",
    "clsa_status_last_known",
    "clsa_status_date")

clsa_status <- 
  prepare_clsa_questionnaire(
    dir_data_raw            = file.path(dir_raw),
    filename_dic            = paste0(project_id,"ParticipantStatus_CoP_v3_Sep2022-dictionary.xlsx"),
    filename_data           = paste0(project_id, "ParticipantStatus_CoP_v3_Sep2022.csv"),
    original_variable_names = status_var_all,
    do_rename               = FALSE,
    show_rename             = FALSE,
    recode_factor           = TRUE)

dim(clsa_status); names(clsa_status)

# Due to the compact nature of data, perform recoding here

clsa_status_fmt <- 
  clsa_status |>
  mutate(
    # change char to date, mdy
    across(.cols = c("clsa_baseline_date"),
           .fns  = function(x) lubridate::as_date(lubridate::mdy(x))),
    # fup 1 and 2 dates - not efficient but only a few follow-ups
    clsa_fup1_date = ifelse(clsa_fup1==0, NA_real_, clsa_fup1_date),
    clsa_fup2_date = ifelse(clsa_fup2==0, NA_real_, clsa_fup2_date),
    across(.cols = c("source_death", "date_death"),
           .fns = function(x) ifelse(death==0,NA_real_,x)),
    across(.cols = c("date_withdrawn"),
           .fns = function(x) ifelse(withdrawn==0,NA_real_,x)),
    # Uniformize date variables
    across(.cols = c("clsa_fup1_date", "clsa_fup2_date", "clsa_status_date", "date_withdrawn","date_death" ),
           .fns  = function(x) ifelse(x %in% c("-99999", "-88888"), NA_real_, x)),
    across(.cols = c("clsa_fup1_date", "clsa_fup2_date", "clsa_status_date","date_withdrawn", "date_death"),
           .fns  = function(x) lubridate::as_date(lubridate::ymd(x))),
  ) |> 
  select(-c("cohort")) 


#'
##### Summary and Save #####
# ********************************************** #
#                Summary and Save                #
# ********************************************** #

fup_status <- clsa_status_fmt

save_and_summarize_data(
  data = fup_status,
  dir = dir_processed,
  dir_metadata = dir_meta
)


#'
#### 1) Sociodemo data preparation ####
# *********************************************************************** #
#                       Sociodemo data preparation                        #
# *********************************************************************** #

#'
##### Baseline #####
# ********************************************** #
#                    Baseline                    #
# ********************************************** #

# Select, Rename and Label variables from the complete questionnaire data

## Select: all variables to keep from baseline data

### list of variables for lengthy questionnaire

study <-
  tibble::tribble(
    ~ original, ~ new, 
    "startdate_COM"      , "sd_date", 
    "WGHTS_PROV_COM"     , "clsa_recruit_prov",
    "WGHTS_ANALYTIC_COM" , "clsa_analytic_weight",
    "WGHTS_INFLATION_COM", "clsa_inflation_weight",
    "GEOSTRATA_COM"      , "clsa_strata"
  ) |>
  mutate(
    category = "study"
  )

basic  <-
  tibble::tribble(
    ~ original, ~ new, 
    "SEX_ASK_COM"   , "sd_sex", 
    "AGE_NMBR_COM"  , "sd_age", 
    "ED_ELHS_COM"   , "sd_elem_education",
    "ED_HIGH_COM"   , "sd_education",
    "ED_HIGH_OTSP_COM", "sd_education_comment",
    "INC_TOT_COM"   , "sd_household_income", 
    "RET_RTRD_COM", "sd_job_retired", 
    "LBF_EVER_COM", "sd_job_ever",
    "LBF_CURR_COM", "sd_job_current",
    "SDC_MRTL_COM"  , "sd_marital", 
    "SN_LIVH_NB_COM", "sd_household_size",
    "HGT_HEIGHT_M_COM" , "sd_height",
    "WGT_WEIGHT_KG_COM",  "sd_bodyweight",
    "SMK_CURRCG_COM", "sd_smoking_current", 
    "SMK_EVRDL_COM",  "sd_smoking_ever_daily",
    "SMK_100CG_COM" , "sd_smoking_100u",
    "SMK_WHLCG_COM" , "sd_smoking_1u",
    "ALC_FREQ_COM"  , "sd_roh_freq", 
    # "DSU_MLTV_MCQ"  , "sd_diet_supplement", # note: not kept anymore because 1) MEDI module more complete; 2) not collected after baseline
    "MEDI_NO_COM"   , "sd_med_number",
    "ADM_GWAS3_COM" , "sd_gwas_key",
    "ADM_EPIGEN2_COM", "sd_epigen_key",
    "ADM_METABOLON2_COM", "sd_metabo_key"
  ) |>
  mutate(
    category = "basic"
  )

pase <- 
  tibble::tribble(
    ~ original, ~ new, 
    "PA2_SIT_MCQ"    , "sd_pase_freq_sitting",
    "PA2_SITHR_MCQ"  , "sd_pase_time_sitting",
    "PA2_WALK_MCQ"   , "sd_pase_freq_walking", 
    "PA2_WALKHR_MCQ" , "sd_pase_time_walking", 
    "PA2_LSPRT_MCQ"  , "sd_pase_freq_light", 
    "PA2_LSPRTHR_MCQ", "sd_pase_time_light", 
    "PA2_MSPRT_MCQ"  , "sd_pase_freq_moderate",
    "PA2_MSPRTHR_MCQ", "sd_pase_time_moderate",
    "PA2_SSPRT_MCQ"  , "sd_pase_freq_high",
    "PA2_SSPRTHR_MCQ", "sd_pase_time_high",
    "PA2_EXER_MCQ"   , "sd_pase_freq_exercise",
    "PA2_EXERHR_MCQ" , "sd_pase_time_exercise",
    "PA2_DSCR2_MCQ"  , "sd_pase_score",
    "PA2_REPRTN_MCQ" , "sd_pase_vs_usual_rep",
    "PA2_PALVL_MCQ"  , "sd_pase_vs_usual_diff"
  ) |>
  mutate(
    category = "pase"
  )

spa <- 
  tibble::tribble(
    ~ original, ~ new, 
    "SPA_OUTS_COM" , "sd_spa_out", 
    "SPA_CHRCH_COM", "sd_spa_church",
    "SPA_SPORT_COM", "sd_spa_sport",
    "SPA_EDUC_COM" , "sd_spa_educ",
    "SPA_CLUB_COM" , "sd_spa_club",
    "SPA_NEIBR_COM", "sd_spa_neibr",
    "SPA_VOLUN_COM", "sd_spa_volun", 
    "SPA_OTACT_COM", "sd_spa_other"
  ) |>
  mutate(
    category = "spa"
  )

who <- 
  tibble::tribble(
    ~ original, ~ new, 
    "WHO_MENOP_COM"   , "sd_who_meno",
    "WHO_MPAG_AG_COM" , "sd_who_meno_age",
    "WHO_HRT_COM"     , "sd_who_hrt_ever",
    "WHO_TYPE_COM"    , "sd_who_hrt_type",
    "WHO_HRTAG_AG_COM", "sd_who_hrt_age",
    "WHO_HRTYR_YR_COM", "sd_who_hrt_duration"
  ) |>
  mutate(
    category = "who"
  )


ccc <-
  tibble::tribble(
    ~ original, ~ new, 
    "CCC_HEART_COM" , "sd_cc_heart", 
    "CCC_PVD_COM"   , "sd_cc_pvd",
    "CCC_MEMPB_COM" , "sd_cc_mempb",
    "CCC_ALZH_COM"  , "sd_cc_alzh",
    "CCC_PARK_COM"  , "sd_cc_park",
    "CCC_MS_COM"    , "sd_cc_ms",
    "CCC_EPIL_COM"  , "sd_cc_epil",
    "CCC_MGRN_COM"  , "sd_cc_mgrn",
    "CCC_ULCR_COM"  , "sd_cc_ulcr",
    "CCC_IBDIBS_COM", "sd_cc_bowdis",
    "CCC_BOWINC_COM", "sd_cc_bowinc",
    "CCC_URIINC_COM", "sd_cc_uriinc",
    "CCC_MACDEG_COM", "sd_cc_macdeg",
    "CCC_CANC_COM"  , "sd_cc_cancer",
    "CCC_AMI_COM"   , "sd_cc_mi",
    "DIA_DIAB_COM"  , "sd_cc_db", 
    "CCC_HBP_COM"   , "sd_cc_hbp",
    "CCC_UTHYR_COM" , "sd_cc_thyr",
    "CCC_ANGI_COM"  , "sd_cc_angina",
    "CCC_CVA_COM"   , "sd_cc_stroke",
    "ICQ_POLIO_COM" , "sd_cc_polio",
    "ICQ_CHEMO4WK_COM" , "sd_cc_chemo_4wk",
    "ICQ_SRG3MO_COM"   , "sd_cc_sx_3m", 
    "CCC_DITYP_COM"    , "cc_dialysis",
    "TBI_PROB_MEM_COM" , "cc_tbi_mem_prob", # note: variable already coded as yes/no, hence not entered with suffix 'sd'
    "PSD_DCTOFF_COM"   , "cc_ptsd" # note: variable already coded as yes/no, hence not entered with suffix 'sd'
  ) |>
  mutate(
    category = "ccc"
  )

med <- 
  tibble::tribble(
    ~ original, ~ new, 
    "STR_MED_COM" , "sd_med_stroke",
    "HBP_MED_COM" , "sd_med_hbp", 
    "DIA_MED_COM" , "sd_med_diabetes",
    "IHD_MED_COM" , "sd_med_heart",
    "DPR_MED_COM" , "sd_med_depress",
    "OST_MED_COM" , "sd_med_osteo",
    "PKD_MED_COM" , "sd_med_parkinson"
  ) |>
  mutate(
    category = "med"
  )

# append and finalize vector (original and new names)
baseline_sd_var <- rbind(study, basic, pase, spa, who, ccc, med)

# Prepare generic data using 'prepare_clsa_questionnaire' function (located in './dir_scripts/')
sociodemo_fup0 <- 
  prepare_clsa_questionnaire(
    dir_data_raw            = file.path(dir_raw),
    filename_dic            = data_name_dictionary[1], # note: value 1 corresponds to baseline 
    filename_data           = data_name_questionnaire[1],
    original_variable_names = baseline_sd_var$original,
    new_variable_names      = baseline_sd_var$new,
    do_rename               = TRUE,
    show_rename             = FALSE,
    recode_factor           = TRUE)

dim(sociodemo_fup0); names(sociodemo_fup0)

#'
##### Follow-up 1 #####
# ********************************************** #
#                  Follow-up 1                   #
# ********************************************** #

# note: same variables are used to have consistent data

fup1_sd_var <- 
  baseline_sd_var |>
  # add new follow-up variables
  rbind(
    tibble::tribble(
      ~ original , ~ new, ~ category,
      "WHO_HRTCURR_COF1", "sd_who_hrt_current", "who",
      "WHO_HRTSTIL_COF1", "sd_who_hrt_still", "who"
    )
  ) |>
  mutate(
    # modify name that were changed
    original = gsub("SITHR_", "SITHR_SIT_", original),
    original = gsub("LSPRTHR_", "LSRTHR_", original),
    # change suffix
    original = gsub("_COM", "_COF1", original),
    original = gsub("_MCQ", "_COF1", original),
    # flag variables that were not collected at follow-ups
    not_collected_flag = 
      ifelse(
        original %in% c("GEOSTRATA_COF1", "SEX_ASK_COF1","ED_ELHS_COF1", "ED_HIGH_COF1", "ED_HIGH_OTSP_COF1","LBF_EVER_COF1",
                        "SMK_100CG_COF1", "SMK_WHLCG_COF1", "SMK_EVRDL_COF1", "DSU_MLTV_COF1",
                        "ADM_GWAS3_COF1", "ADM_EPIGEN2_COF1", "ADM_METABOLON2_COF1", "ICQ_POLIO_COF1", "PSD_DCTOFF_COF1") , 1, 0
      )
  ) 


sociodemo_fup1 <- 
  prepare_clsa_questionnaire(
    dir_data_raw            = file.path(dir_raw),
    filename_dic            = data_name_dictionary[2],
    filename_data           = data_name_questionnaire[2],
    original_variable_names = subset(fup1_sd_var, not_collected_flag==0)$original,
    new_variable_names      = subset(fup1_sd_var, not_collected_flag==0)$new,
    do_rename               = TRUE,
    show_rename             = FALSE,
    recode_factor           = TRUE)

dim(sociodemo_fup1); names(sociodemo_fup1)


#'
##### Follow-up 2 #####
# ********************************************** #
#                  Follow-up 2                   #
# ********************************************** #

# note: same variables are used to have consistent data

fup2_sd_var <- 
  fup1_sd_var |>
  mutate(
    # update variable name given a few changes
    original = ifelse(original=="WHO_HRTYR_YR_COF1", "WHO_HRTDR_YR_COF2", original),
    original = ifelse(original=="CCC_IBDIBS_COF1", "CCC_IBSYD_COF2", original),
    original = ifelse(original=="CCC_PVD_COF1", "CCC_PAD_COF2", original),
    
    # change suffix
    original = gsub("_COF1", "_COF2", original),
    
    # flag variables that were not collected at follow-ups
    not_collected_flag = 
      ifelse(
        original %in% c("MEDI_NO_COF2", "PA2_DSCR2_COF2", "CCC_EPIL_COF2") , 1, not_collected_flag
      )
  ) 

#' note: total PASE score ('PA2_DSCR2_\*') not calculated at follow-up 2  
#' note2: number of medication not collected ('MEDI_NO_\*') at follow-up 2

sociodemo_fup2 <- 
  prepare_clsa_questionnaire(
    dir_data_raw            = file.path(dir_raw),
    filename_dic            = data_name_dictionary[3],
    filename_data           = data_name_questionnaire[3],
    original_variable_names = subset(fup2_sd_var, not_collected_flag==0)$original,
    new_variable_names      = subset(fup2_sd_var, not_collected_flag==0)$new,
    do_rename               = TRUE,
    show_rename             = FALSE,
    recode_factor           = TRUE)

dim(sociodemo_fup2); names(sociodemo_fup2)

#'
##### Medication/Supplement Questionnaire #####
# ********************************************** #
#      Medication/Supplement Questionnaire       #
# ********************************************** #

# goal: identity consumption of vitamin, mineral, natural health products
# note: every follow-up are dealt with here because similar steps and coding

# 1) Flag words for common vitamin, mineral, multi, omega or natural health products (not case sensitive)

words_vitamin <- c("vitamin", "vitamine", "B Complex", 
                   "C-Force", "C Extra", "Vitaminc",
                   "Beta Carotene", "BETACAROTENE", "B100", "B50", "B6", "BIOTIN", "B Formula", 
                   "D3", "D-TABS", "D-Gel", "JAMP-VITAMIN D", 
                   "FOLIC", "EURO FOLIC", "Folique") 
words_mineral <- c("mineral", "calcium", "cal", "Calm","Calmag", "Calcia", "M-Cal", "Ci-Cal", 
                   "iron", "FERROUS", "Feramax", "Fer", "Zinc", "Euro-Ferrous Sulfate",
                   "magnesium", "Ortho-Minerals")
words_mixed <- c("centrum", "multivitamin", "Multivit", "Multi-Vitamin", "Nutricap", "Vita", "Vita-Vim", "Formula Forte", "Complex",
                 "Multi", "Boost", "Metabolic Nutrition Capsules", "Vitalux", "Vitalux-S")
words_omega <- c("omega", "oméga", "Omega3", "oméga-", "omega-", "omega-3", "oméga-3", "O3mega", 
                 "fish", "dha", "epa", "Omega3", "COD", "Krill", "Nutrasea", "Salmon", "Seal","FOIE DE MORUE", "Lin", "Oregano", "D'Onagre", "Bourrache", "Castor", "Chia", "ala",
                 "FLAX", "Flaxseed", "Hemp") 
words_natural <- c("Curcumin", "Glucosamine", "Turmeric", "Chondroitin", "Glucosamine/Chondroitin", "Msm", "Collagène", "Collagen",
                   "GARLIC", "CURCUMA", "Probiotics", "Probiotiques", "Probio+", "Multi-Probiotic", "Cranberry", "Grape Seed", "Grapeseed", "Grapefruit", "Eucalyptus", 
                   "GINKGO BILOBA", "Gingko", "Ginkgo-Ps", "ECHINACEA", "Garcinia", "Chromium", "Q10", "Q-10", "Coq10", "COQ-10", "GINSENG",
                   "Multigreens",  "Herb", "Herbal","Maca", "5-Htp", "Greens", "Green", "Greens+", "Nutri-Flex", "Antioxidant", "CITRICIDAL",  
                   "Cla", "Conjugated Linoleic Acid", 
                   "Melatonin", "Mélatonine", "Creatine", "Whey", "Glutamine", "Goji", 
                   "Lysine", "L-Arginine", "L-Glutamine", "L-Tryptophan", "L-Tyrosine", "Proargi-9+",
                   "Ketone", "Ketones", "Isoflavones", "Raspberry" , "St John'S Wort", "Vega One")

# 2) Helper function to apply common formatting to output of 'extract_medi_data'
# note : assumes c('flag_vitamin', 'flag_mineral', 'flag_mixed', 'flag_omega', 'flag_natural') variables exist
# note2: these variables do not have the 'sd_' prefix as above since they are already analysis-ready 

create_supplement_variable <- function(data){
  
  # indicate prefix for flag variables and columns for dietary supplements (vitamin, mineral or both)
  prefix_any <- c("flag_")
  cols_diet <- c("flag_vitamin", "flag_mineral", "flag_mixed") 
  
  # recode and create variables
  data_recoded <-
    data |>
    dplyr::mutate(
      supplement_any  = ifelse(rowSums(across(.cols = dplyr::starts_with(prefix_any)))>0, 1, 0),
      supplement_diet = ifelse(rowSums(across(.cols = dplyr::all_of(cols_diet)))>0, 1, 0),
      supplement_natural =  ifelse(flag_natural >0, 1, 0),
      supplement_omega = ifelse(flag_omega >0, 1, 0)
    ) |>
    dplyr::select(-c(dplyr::starts_with("flag_")))
  
  # add labels
  labelled::var_label(data_recoded) <- 
    list(
      entity_id          = "Participant id",
      supplement_any     = "Taking any dietary supplement or natural health product",
      supplement_diet    = "Taking vitamin, mineral or multi- supplement",
      supplement_natural = "Taking natural health product",
      supplement_omega   = "Taking omega 3-6-9 supplement")
  
  return(data_recoded)
}

# 3) flag words in each questionnaire data, MEDI module and create variables

# ************************** #
#     Apply to each data     #
# ************************** #

# note: data were NOT collected at follow-up 2, hardcoded 1:2
# note2: scanning each text field is computationally intensive.
# Despite using data.table., code below takes at least 3-5 minutes to run.

medi_data_processed <-
  lapply(X = data_name_questionnaire[1:2],
         function(x) {
           message("Preparing MEDI from data file: ", x)
           medi_Data_Tt <- 
             extract_medi_data(
               path_data     = file.path(dir_raw, x),
               words_vitamin  ,
               words_mineral  ,
               words_mixed    ,
               words_omega    ,
               words_natural  ,
               return_long   = FALSE, # note: not needed since goal here is to flag consumption only
               show_progress = FALSE) |>
             create_supplement_variable()
           return(medi_Data_Tt)
         }
  )

# Prepare pseudo-data dictionary for consistency purpose
baseline_medi_var <- 
  tibble::tribble(
    ~ original, ~ new, 
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_any",
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_diet", 
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_natural",
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_omega"
  ) |>
  mutate(
    category = "medi"
  )

fup1_medi_var <- 
  tibble::tribble(
    ~ original, ~ new, 
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_any",
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_diet", 
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_natural",
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_omega"
  ) |>
  mutate(
    original = gsub("_COM", "_COF1", original),
    category = "medi",
    not_collected_flag = 0
  )

fup2_medi_var <- 
  tibble::tribble(
    ~ original, ~ new, 
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_any",
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_diet", 
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_natural",
    "MEDI_ID_NAME_SP_OUT_*_COM" , "supplement_omega"
  ) |>
  mutate(
    original = gsub("_COM", "_COF2", original),
    category = "medi",
    not_collected_flag = 1
  )

#'
##### Summary and Save #####
# ********************************************** #
#                Summary and Save                #
# ********************************************** #

#' Merge questionnaire datasets with optional missing-value handling
#'
#' This function merges two questionnaire datasets using a full join.
#' Optionally, values that are `NA` in variables originating from `y` can be
#' replaced with a specified value (e.g., 0). If the `y` dataset contains
#' variable labels (from the `labelled` package), these are transferred to the
#' merged dataset for the new variables.
#'
#' @param x A data frame or tibble containing questionnaire data.
#' @param y A data frame or tibble containing additional questionnaire data.
#' @param by Character vector of variable names to join by. Defaults to `NULL`
#'   (all variables in common between `x` and `y`).
#' @param set_missing_new_variable_to Optional value used to replace `NA` in
#'   variables that are present only in `y` (default = `NULL`, no replacement).
#'
#' @return A data frame (tibble) containing the full join of `x` and `y`,
#'   with optional missing-value replacement and preserved variable labels.
#'
#' @importFrom labelled get_variable_labels var_label
#' @importFrom dplyr mutate across all_of 
#'
#' @examples
#' \dontrun{
#' merged <- merge_questionnaire_data(df1, df2, by = "id", set_missing_new_variable_to = 0)
#' }
#'
#' @export
merge_questionnaire_data <- function(x, y, by = NULL, set_missing_new_variable_to = NULL) {
  #merge 
  out <- dplyr::full_join(x, y, by = by)
  
  # Identify variables that came uniquely from y
  new_vars <- setdiff(names(y), names(x))
  
  if (!is.null(set_missing_new_variable_to) && length(new_vars) > 0) {
    out <- 
      out |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(new_vars),
          .fns  = ~ ifelse(is.na(.x), set_missing_new_variable_to, .x)
        )
      )
    
    # Add back variable labels if present
    var_labels <- labelled::get_variable_labels(y)
    var_labels <- var_labels[names(var_labels) %in% new_vars]
    if (length(var_labels) > 0) {
      labelled::var_label(out)[names(var_labels)] <- var_labels
    }
  }
  
  return(out)
}



# Save variable tracking data for reference using the save_and_summarize_data function
sociodemo_var <- 
  rbind(
    rbind(baseline_sd_var,baseline_medi_var) |> mutate(time=0,not_collected_flag=NA),
    rbind(fup1_sd_var, fup1_medi_var) |> mutate(time=1),
    rbind(fup2_sd_var, fup2_medi_var) |> mutate(time=2)
  )

labelled::var_label(sociodemo_var) <- 
  list(
    original           = "Original variable names",
    new                = "New variable names (current study)",
    category           = "Category",
    time               = "Time point",
    not_collected_flag = "Flag for variables not collected at a follow-up")

save_and_summarize_data(
  data = sociodemo_var,
  dir = dir_meta,
  save_csv = FALSE,
  save_xlsx = TRUE,
  save_metadata = TRUE,
  dir_metadata = dir_meta
)

# save all sociodemo data using the save_and_summarize_data function

## merge diet supplement data 
## note: list index is hardcoded for first, ie, baseline
## note2: missing values (supplement questionaire not completed OR non response) are recoded as '0' (ie, no supplement) = ignores missing values

sociodemo_fup0 <- 
  merge_questionnaire_data(
    x = sociodemo_fup0,
    y = medi_data_processed[[1]],
    set_missing_new_variable_to = 0) 

save_and_summarize_data(
  data = sociodemo_fup0,
  dir = dir_processed,
  dir_metadata = dir_meta
)

## merge diet supplement data 
## note: list index is hardcoded for second, ie, fup1
## note2: missing values (supplement questionaire not completed OR non response) are recoded as '0' (ie, no supplement) = ignores missing values

sociodemo_fup1 <- 
  merge_questionnaire_data(
    x = sociodemo_fup1,
    y = medi_data_processed[[2]],
    set_missing_new_variable_to = 0) 

save_and_summarize_data(
  data = sociodemo_fup1,
  dir = dir_processed,
  dir_metadata = dir_meta
)

# no diet supplement information at fup2 (likely COVID missing data)
save_and_summarize_data(
  data = sociodemo_fup2,
  dir = dir_processed,
  dir_metadata = dir_meta
)

#' note: all sociodemo data could have been binded together to save, but kept separated to better deal with
#' variables that were not measured at given follow-ups.


#'
#### 2) Outcome data preparation ####
# *********************************************************************** #
#                        Outcome data preparation                         #
# *********************************************************************** #

#'
##### Baseline #####
# ********************************************** #
#                    Baseline                    #
# ********************************************** #

# Select, Rename and Label variables from the complete questionnaire data

## Select: all variables to keep from baseline data

muscle_function <-
  tibble::tribble(
    ~ original, ~ new, 
    "WLK_STATUS_COM"     , "y_func_walk_status",
    "WLK_RES_STOP_COM"   , "y_func_walk_stop",
    "WLK_TIME_COM"       , "y_func_walk_time",
    "TUG_STATUS_COM"   , "y_func_tug_status", 
    "TUG_DEVICE_COM"   , "y_func_tug_device_use",
    # "TUG_DEVICE_SP_COM", "y_func_tug_device_type"
    "TUG_TIME_COM"     ,"y_func_tug_time",
    "CR_STATUS_COM"  , "y_func_chair_status",
    "CR_NB_COM"      , "y_func_chair_number",
    "CR_TIME_COM"    , "y_func_chair_time_total",
    "CR_AVG_TIME_COM", "y_func_chair_time_per_rep",
    "BAL_STATUS_COM"      , "y_func_balance_status",
    "BAL_BEST_COM"        , "y_func_balance_time_best",
    "FAL_NMBR_NB_COM" , "y_fall_number",
    "FAL_HOSP_COM", "y_fall_hosp"
  ) |>
  mutate(
    category = "muscle_function"
  )

muscle_strength <- 
  tibble::tribble(
    ~ original, ~ new, 
    "GS_STATUS_COM", "y_str_grip_status", 
    "GS_EXAM_MAX_COM", "y_str_grip_max",
    "GS_EXAM_AVG_COM", "y_str_grip_avg"
  ) |>
  mutate(
    category = "muscle_strength"
  )

bodycomposition <-
  tibble::tribble(
    ~ original, ~ new, 
    "DXA_WB_WEIGHT_COM", "y_dxa_weight",
    "DXA_WB_HEIGHT_COM", "y_dxa_height",
    "DXA_WB_QC_PASS_COM", "y_dxa_status_quality",
    "DXA_WB_STATUS_COM" , "y_dxa_status_bmd",
    "DXA_WB_WBTOT_BMC_COM" ,  "y_dxa_totbmc",
    "DXA_WB_WBTOT_BMD_COM" ,  "y_dxa_totbmd",
    "DXA_WB_SUBTOT_BMC_COM",  "y_dxa_totbmc_exclh",
    "DXA_WB_SUBTOT_BMD_COM",  "y_dxa_totbmd_exclh",
    "DXA_WBC_TRUNK_FAT_COM" , "y_dxa_trunk_fat",
    "DXA_WBC_TRUNK_LEAN_COM", "y_dxa_trunk_lean", 
    "DXA_WBC_TRUNK_MASS_COM", "y_dxa_trunk_mass", 
    "DXA_WBC_TRUNK_PFAT_COM", "y_dxa_trunk_pfat",
    "DXA_WBC_LARM_FAT_COM",  "y_dxa_arm_left_fat",
    "DXA_WBC_LARM_LEAN_COM", "y_dxa_arm_left_lean",
    "DXA_WBC_LARM_MASS_COM", "y_dxa_arm_left_mass",
    "DXA_WBC_LARM_PFAT_COM", "y_dxa_arm_left_pfat",
    "DXA_WBC_RARM_FAT_COM",  "y_dxa_arm_right_fat",
    "DXA_WBC_RARM_LEAN_COM", "y_dxa_arm_right_lean",
    "DXA_WBC_RARM_MASS_COM", "y_dxa_arm_right_mass",
    "DXA_WBC_RARM_PFAT_COM", "y_dxa_arm_right_pfat",
    "DXA_WBC_L_LEG_FAT_COM",  "y_dxa_leg_left_fat",
    "DXA_WBC_L_LEG_LEAN_COM", "y_dxa_leg_left_lean",
    "DXA_WBC_L_LEG_MASS_COM", "y_dxa_leg_left_mass",
    "DXA_WBC_L_LEG_PFAT_COM", "y_dxa_leg_left_pfat",
    "DXA_WBC_R_LEG_FAT_COM",  "y_dxa_leg_right_fat",
    "DXA_WBC_R_LEG_LEAN_COM", "y_dxa_leg_right_lean",
    "DXA_WBC_R_LEG_MASS_COM", "y_dxa_leg_right_mass",
    "DXA_WBC_R_LEG_PFAT_COM", "y_dxa_leg_right_pfat",
    "DXA_WBC_WBTOT_FAT_COM"    , "y_dxa_totfat",  
    "DXA_WBC_WBTOT_LEAN_COM"   , "y_dxa_totlean", 
    "DXA_WBC_WBTOT_MASS_COM"   , "y_dxa_totmass",
    "DXA_WBC_WBTOT_PFAT_COM"   , "y_dxa_pfat",
    "DXA_SYM_WB_WBTOT_BMC_COM" , "y_dxa_totbmc_sym",
    "DXA_SYM_WB_WBTOT_BMD_COM" , "y_dxa_totbmd_sym",
    "DXA_SYM_WB_SUBTOT_BMC_COM", "y_dxa_totbmc_sym_exclh",
    "DXA_SYM_OI_TOTAL_LEAN_MASS_COM" , "y_dxa_totlm_sym",
    "DXA_SYM_OI_TOTAL_PURE_LEAN_COM" , "y_dxa_totlmp_sym",
    "DXA_SYM_OI_TOTAL_FAT_MASS_COM"  , "y_dxa_totfat_sym",
    "DXA_DRV_SUBTOT_PURE_LEAN_COM"   , "y_dxa_totlmp_nobo_exclh"
  ) |>
  mutate(
    category = "bodycomposition"
  )

cardio <- 
  tibble::tribble(
    ~ original, ~ new, 
    "BP_STATUS_COM"            , "y_bp_status",
    "BP_SYSTOLIC_ALL_AVG_COM"  , "y_bp_systolic",
    "BP_DIASTOLIC_ALL_AVG_COM" , "y_bp_diastolic",
    "BP_PULSE_ALL_AVG_COM"     , "y_bp_pulse",
    "WHC_CLTHLAYERS_COM"       , "y_waist_type",
    "WHC_WAIST_CM_COM"         , "y_waist_value",
    "BLD_HSCRP_COM"        , "y_bld_crp",
    "BLD_HSCRP_CMT_COM"    , "y_bld_crp_cmt",
    "BLD_CHOL_COM"         , "y_bld_chol",
    "BLD_CHOL_CMT_COM"     , "y_bld_chol_cmt",
    "BLD_HDL_COM"          , "y_bld_hdl",
    "BLD_HDL_CMT_COM"      , "y_bld_hdl_cmt",
    "BLD_LDL_COM"          , "y_bld_ldl", 
    "BLD_LDL_CMT_COM"      , "y_bld_ldl_cmt",
    "BLD_nonHDL_COM"       , "y_bld_nhdl",
    "BLD_nonHDL_CMT_COM"   , "y_bld_nhdl_cmt",
    "BLD_TRIG_COM"         , "y_bld_tg",
    "BLD_TRIG_CMT_COM"     , "y_bld_tg_cmt",
    "BLD_TSH_COM"          , "y_bld_tsh",
    "BLD_TSH_CMT_COM"      , "y_bld_tsh_cmt",
    "BLD_BCtestdate_COM"   , "y_bld_date",
    "BLD_TNFtestdate_COM"  , "y_bld_tnf_date",
    "BLD_TNF_COM"          , "y_bld_tnf",
    "BLD_IL6testdate_COM"  , "y_bld_il6_date",
    "BLD_IL6_COM"          , "y_bld_il6"
  ) |>
  mutate(
    category = "cardiometabolic"
  )

cognition <- 
  tibble::tribble(
    ~ original, ~ new, 
    "COG_REYI_SCORE_COM", "y_cog_reyi_score",
    "COG_REYII_SCORE_COM", "y_cog_reyii_score",
    "COG_AFT_SCORE_1_COM", "y_cog_af1_score",
    "COG_AFT_SCORE_2_COM", "y_cog_af2_score",
    "COG_MAT_SCORE_COM", "y_cog_mat_score",
    "FAS_TOTAL_SCORE_COM", "y_cog_fas_score",
    "STP_INTFR_RATIO_COM", "y_cog_stroop_ratio",
    "STP_INTFR_RATIO_EXFLAG_COM", "y_cog_stroop_flag",
    "COG_REYI_NORMED_ZSCORE_COM", "y_cog_reyi_norm_zscore",
    "COG_REYI_NORMED_ORIGSCALE_COM", "y_cog_reyi_norm_original",
    "COG_REYII_NORMED_ZSCORE_COM", "y_cog_reyii_norm_zscore",
    "COG_REYII_NORMED_ORIGSCALE_COM", "y_cog_reyii_norm_original",
    "COG_AF1_NORMED_ZSCORE_COM", "y_cog_af1_norm_zscore",
    "COG_AF1_NORMED_ORIGSCALE_COM", "y_cog_af1_norm_original",
    "COG_AF2_NORMED_ZSCORE_COM", "y_cog_af2_norm_zscore",
    "COG_AF2_NORMED_ORIGSCALE_COM", "y_cog_af2_norm_original",
    "COG_MAT_NORMED_ZSCORE_COM", "y_cog_mat_norm_zscore",
    "COG_MAT_NORMED_ORIGSCALE_COM", "y_cog_mat_norm_original",
    "STP_RATIO_NORMED_ZSCORE_COM",  "y_cog_stroop_norm_zscore",
    "STP_RATIO_NORMED_ORIGSCALE_COM", "y_cog_stroop_norm_original",
    "FAS_TOTAL_NORMED_ZSCORE_COM", "y_cog_fas_norm_zscore",
    "FAS_TOTAL_NORMED_ORIGSCALE_COM", "y_cog_fas_norm_original"
  ) |>
  mutate(
    category = "cognition"
  )
epigenetic <- 
  tibble::tribble(
    ~ original, ~ new, 
    "DNAmAge_COM", "y_epiage_abs", 
    "AgeAccelerationDifference_COM","y_epiage_acc_diff", 
    "AgeAccelerationResidual_COM", "y_epiage_acc_res",
    "IEAA_COM", "y_epiage_acc_int",
    "EEAA_COM", "y_epiage_acc_ext",
    "Hannum_Age_COM", "y_epiage_hannum"
  ) |>
  mutate(
    category = "epigenetic"
  )

control <-
  tibble::tribble(
    ~ original, ~ new, 
    "INJ_OCC_COM", "y_control_injury_flag",
    "INJ_CAUS_FL_COM", "y_control_injury_fall",
    "INJ_CAUS_VH_COM", "y_control_injury_vehicule",
    "INJ_CAUS_WK_COM", "y_control_injury_work",
    "INJ_CAUS_NONE_COM", "y_control_injury_other",
    "INJ_CAUS_DK_NA_COM", "y_control_injury_unk",
    "INJ_CAUS_REFUSED_COM", "y_control_injury_refused",
    "INJ_HOW_COM", "y_control_injury_how"
  ) |>
  mutate(
    category = "negativecontrol"
  )

fup0_y_var <- rbind(muscle_function, muscle_strength, bodycomposition, cognition, cardio, epigenetic, control)

outcome_fup0 <- 
  prepare_clsa_questionnaire(
    dir_data_raw            = file.path(dir_raw),
    filename_dic            = data_name_dictionary[1],
    filename_data           = data_name_questionnaire[1],
    original_variable_names = fup0_y_var$original,
    new_variable_names      = fup0_y_var$new,
    do_rename               = TRUE,
    show_rename             = FALSE,
    recode_factor           = TRUE)

dim(outcome_fup0); names(outcome_fup0)

#'
##### Follow-up 1 #####
# ********************************************** #
#                  Follow-up 1                   #
# ********************************************** #

# note: same variables are used to have consistent data

fup1_y_var <- 
  fup0_y_var |>
  mutate(
    # change suffix
    original = gsub("_COM", "_COF1", original),
    # flag variables that were not collected at follow-ups
    not_collected_flag = 
      ifelse(
        original %in% c(
          "DXA_WB_QC_PASS_COF1", "DXA_WB_WEIGHT_COF1", "DXA_WB_HEIGHT_COF1",
          "DXA_SYM_OI_TOTAL_LEAN_MASS_COF1", "DXA_SYM_OI_TOTAL_PURE_LEAN_COF1", "DXA_SYM_OI_TOTAL_FAT_MASS_COF1",
          "DXA_SYM_WB_WBTOT_BMC_COF1", "DXA_SYM_WB_WBTOT_BMD_COF1", "DXA_SYM_WB_SUBTOT_BMC_COF1",
          "BLD_HSCRP_CMT_COF1", "BLD_CHOL_CMT_COF1", "BLD_HDL_CMT_COF1", 
          "BLD_LDL_CMT_COF1", "BLD_nonHDL_CMT_COF1", "BLD_TRIG_CMT_COF1",
          "BLD_TSH_CMT_COF1", "BLD_BCtestdate_COF1", "BLD_TNFtestdate_COF1",
          "BLD_TNF_COF1", "BLD_IL6testdate_COF1", "BLD_IL6_COF1", 
          "COG_REYI_NORMED_ORIGSCALE_COF1", "COG_REYII_NORMED_ORIGSCALE_COF1", "COG_AF1_NORMED_ORIGSCALE_COF1", 
          "COG_AF2_NORMED_ORIGSCALE_COF1", "COG_MAT_NORMED_ORIGSCALE_COF1", "STP_RATIO_NORMED_ORIGSCALE_COF1", "FAS_TOTAL_NORMED_ORIGSCALE_COF1",
          "DNAmAge_COF1","AgeAccelerationDifference_COF1", "AgeAccelerationResidual_COF1", 
          "IEAA_COF1", "EEAA_COF1", "Hannum_Age_COF1") , 1, 0
      )
  ) 


outcome_fup1 <- 
  prepare_clsa_questionnaire(
    dir_data_raw            = file.path(dir_raw),
    filename_dic            = data_name_dictionary[2],
    filename_data           = data_name_questionnaire[2],
    original_variable_names = subset(fup1_y_var, not_collected_flag==0)$original,
    new_variable_names      = subset(fup1_y_var, not_collected_flag==0)$new,
    do_rename               = TRUE,
    show_rename             = FALSE,
    recode_factor           = TRUE)

dim(outcome_fup1); names(outcome_fup1)

#'
##### Follow-up 2 #####
# ********************************************** #
#                  Follow-up 2                   #
# ********************************************** #

# note: same variables are used to have consistent data

fup2_y_var <- 
  fup1_y_var |>
  # add one variable 
  rbind(
    tibble::tribble(
      ~ original, ~ new, ~category, ~ not_collected_flag,
      "HCU_HAVEFAM_COF2", "y_control_doctor", "negativecontrol", 0)
  ) |>
  mutate(
    # change suffix
    original = gsub("_COF1", "_COF2", original),
    # flag variables that were not collected at follow-ups
    not_collected_flag =
      ifelse(
        original %in% c(
          "DXA_DRV_SUBTOT_PURE_LEAN_COF2",
          "BLD_HSCRP_COF2", "BLD_CHOL_COF2", "BLD_HDL_COF2",
          "BLD_LDL_COF2", "BLD_nonHDL_COF2", "BLD_TRIG_COF2", "BLD_TSH_COF2",
          "FAS_TOTAL_SCORE_COF2", "STP_INTFR_RATIO_COF2", "STP_INTFR_RATIO_EXFLAG_COF2", "COG_REYI_NORMED_ZSCORE_COF2", 
          "COG_REYII_NORMED_ZSCORE_COF2", "COG_AF1_NORMED_ZSCORE_COF2", "COG_AF2_NORMED_ZSCORE_COF2", "COG_MAT_NORMED_ZSCORE_COF2",
          "STP_RATIO_NORMED_ZSCORE_COF2", "FAS_TOTAL_NORMED_ZSCORE_COF2"
        ) , 1, not_collected_flag
      )
  ) 

outcome_fup2 <- 
  prepare_clsa_questionnaire(
    dir_data_raw            = file.path(dir_raw),
    filename_dic            = data_name_dictionary[3],
    filename_data           = data_name_questionnaire[3],
    original_variable_names = subset(fup2_y_var, not_collected_flag==0)$original,
    new_variable_names      = subset(fup2_y_var, not_collected_flag==0)$new,
    do_rename               = TRUE,
    show_rename             = FALSE,
    recode_factor           = TRUE)

dim(outcome_fup2); names(outcome_fup2)

#'
##### Summary and Save #####
# ********************************************** #
#                Summary and Save                #
# ********************************************** #

# save variable tracking data for reference

outcome_var <- 
  rbind(
    fup0_y_var |> mutate(time=0,not_collected_flag=NA),
    fup1_y_var |> mutate(time=1),
    fup2_y_var |> mutate(time=2)
  )

labelled::var_label(outcome_var) <- 
  list(
    original           = "Original variable names",
    new                = "New variable names (current study)",
    category           = "Category",
    time               = "Time point",
    not_collected_flag = "Flag for variables not collected at a follow-up")


# save all using the save_and_summarize_data function

save_and_summarize_data(
  data = outcome_var,
  dir = dir_meta,
  save_csv = FALSE,
  save_xlsx = TRUE,
  save_metadata = TRUE,
  dir_metadata = dir_meta
)

save_and_summarize_data(
  data = outcome_fup0,
  dir = dir_processed,
  dir_metadata = dir_meta
)

save_and_summarize_data(
  data = outcome_fup1,
  dir = dir_processed,
  dir_metadata = dir_meta
)

save_and_summarize_data(
  data = outcome_fup2,
  dir = dir_processed,
  dir_metadata = dir_meta
)


#'
#### End of code ####
# *********************************************************************** #
#                              End of code                                #
# *********************************************************************** #

sessionInfo()

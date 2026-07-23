#' ---
#' title: "Generate processed nutrition data: aggregated Short Diet Questionnaire (SDQ) variables"
#' author: "Didier Brassard"
#' date: "`r Sys.Date()`"
#' code checked by:
#' code checked date:
#' run time: <2min
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
library(dplyr)
library(tidylog)
library(readxl)
library(data.table); if(is.na(parallel::detectCores())==FALSE & parallel::detectCores()>1){available_core <- parallel::detectCores()-1} else {available_core <- 1}
data.table::setDTthreads(threads = available_core)
data.table::getDTthreads()
library(purrr)
library(ncimultivar) # used for data winsorization, details below

## presentation
library(gtsummary)
library(ggplot2)

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

# prepare a generic sociodemo data (selection, rename, labels)
source(file.path(dir_scripts, "prepare_clsa_questionnaire.R"))

# aux. function to support data cleaning
source(file.path(dir_scripts, "helper_data_cleaning.R"))

#'
#### 2) Diet data preparation (SDQ) ####
# *********************************************************************** #
#                      Diet data preparation (SDQ)                        #
# *********************************************************************** #

##### Load auxiliary data #####
# ********************************************** #
#              Load auxiliary data               #
# ********************************************** #

# 1) Expanded SDQ dictionary data with pseudo Canada's Food Guide classification
load(file = file.path(dir_processed, "sdq_dictionary_expanded.rdata"))

## overview, food changes across measurements
dim(dictionary_sdq_expanded); names(dictionary_sdq_expanded)
with(dictionary_sdq_expanded, table(cfg2019_primary_f, time))

## keep only most relevant information
pseudo_cfg_classification <- 
  dictionary_sdq_expanded |>
  select("table", "name", "time", "cfg2019_primary")

## create food categorization for further analyses
food_classification <- 
  dictionary_sdq_expanded |>
  select("table", "name", "time","cfg2019_primary", "cfg2019_secondary") |>
  # revise classification for a few exceptions
  mutate(
    cfg2019_secondary = 
      case_when( 
        grepl("NUT_CAML", name) ~ "milk_ca",
        grepl("NUT_LFML", name) ~ "milk_low",
        grepl("NUT_WHML", name) ~ "milk_regular",
        grepl("NUT_BTTR", name) ~ "fat",
        cfg2019_primary == "unclassified" ~ "unclassified",
        is.na(cfg2019_secondary) & cfg2019_primary =="plantbev" ~ "plantbev",
        .default = cfg2019_secondary
      )
  ) |>
  tidyr::separate_wider_delim(
    cols  = "cfg2019_secondary",
    delim ="_",
    names = c("category", "subcategory"),
    too_few = "align_start"
  )

rm(dictionary_sdq_expanded)


# 2) Levels and labels for 'cfg2019_primary' variable
cfg2019_primary_data <- 
  read_xlsx(path = file.path(dir_processed, "cfg2019_primary.xlsx"))

cfg2019_primary_data

#' note: 'cfg2019_primary_data' file used to auto-assign variable labels

#'
##### Load CLSA datasets #####
# ********************************************** #
#               Load CLSA datasets               #
# ********************************************** #

# note: 'load_questionnaire_data' function located in './dir_scripts/prepare_clsa questionnaire'


# ************************** #
#     Apply to each data     #
# ************************** #

sdq_data <- 
  lapply(X = data_name_questionnaire,
         function(x) {
           message("Loading data file: ", x)
           sdq_data_Tt <- 
             load_questionnaire_data(
               path_data = file.path(dir_raw, x),
               keep_var_prefix = "NUT_")
           print(dim(sdq_data_Tt))
           return(sdq_data_Tt)
         }
  )

#'
##### Recoding #####
# ********************************************** #
#                    Recoding                    #
# ********************************************** #

#' Change Values for Nutrition Variables  
#'
#' This function identifies columns in a dataset that begin with a specified prefix (default is `"NUT_"`) 
#' and replaces specific values (e.g., `9998`, `9999`, `7777`, `9996`) with defined replacements. 
#' It returns a modified dataset with these values substituted.  
#'
#' @param data A data.table object  
#' @param prefix A string representing the prefix of column names to be modified. Default is `"NUT_"`.  
#' @param value_for_9998 The replacement value for `9998` in the specified columns. Default is `0`.  
#' @param value_for_9999 The replacement value for `9999` in the specified columns. Default is `NA`.  
#' @param value_for_7777 The replacement value for `7777` in the specified columns. Default is `NA`.  
#' @param value_for_9996 The replacement value for `9996` in the specified columns. Default is `0`.  
#' @param value_for_lt0 The replacement value for any negative number in the specified columns. Default is `NA`.  
#'
#' @importFrom data.table setDT  
#' @importFrom tidyfast dt_case_when  
#' 
#' @return A data.table with the specified values replaced in columns that start with the given prefix.  
#' @examples  
#' \dontrun{
#' # Example usage
#' data_bl_nut <- recode_nut_values(data_bl)
#' }
#' 
#' @export  

recode_nut_values <- function(data, prefix = "NUT_", value_for_9998 = 0, value_for_9999 = NA, 
                              value_for_7777 = NA, value_for_9996 = 0, value_for_lt0 = NA) {
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  # Convert to data.table
  data.table::setDT(data)
  
  # Get variables with the specified prefix
  nut_variables <- grep(paste0("^", prefix), names(data), value = TRUE)
  
  cat(paste0("change_nut_values: recoding ",length(nut_variables)," variables\n"))
  
  data[, (nut_variables) := lapply(.SD, function(x) {
    x <- as.numeric(x)
    tidyfast::dt_case_when(
      x == 9998 ~ value_for_9998,
      x == 9999 ~ value_for_9999,
      x == 7777 ~ value_for_7777,
      x == 9996 ~ value_for_9996,
      x < 0     ~ value_for_lt0,
      TRUE      ~ x
    )
  }), .SDcols = nut_variables]
  
  return(data)
}


# ************************** #
#     Apply to each data     #
# ************************** #

# Apply recode_nut_values to each dataset in the list
sdq_data_clean <- lapply(X = sdq_data, FUN = recode_nut_values)

#'
##### Count number of missing SDQ columns #####
# ********************************************** #
#        Count number of missing columns         #
# ********************************************** #

#' note: `_NB_` used to identify the 'final' frequency per day among nutrition variables

# Wrapper to calculate total missing values for selected variables
get_sum_missing <- function(data){
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  sdq_freq_variables <- grep("_NB_", names(data), value = TRUE)
  if (length(sdq_freq_variables) == 0) {
    stop("No columns in `data` includes '_NB_'. Check input data.")
  }
  
  cat(paste0("sdq_freq_variables: ",length(sdq_freq_variables)," variables\n"))
  
  data <- 
    data |>
    missing_indicator_sum_dt(columns = sdq_freq_variables) |>
    dplyr::rename(nut_missing_sdq_items =  missing_sum ) 
  
  # note: 'missing_indicator_sum_dt' function located in ./dir_scripts/helper_data_cleaning.R 
  
  return(data)
  
}

# ************************** #
#     Apply to each data     #
# ************************** #

# Apply 'get_sum_missing' to each dataset in the list
sdq_data_clean_nmiss <- 
  lapply(X = sdq_data_clean,
         FUN = function(x) {
           get_sum_missing(
             # drop "Time on current diet" variables
             x |> dplyr::select(-c(starts_with("NUT_DTIM_"))))
         })

#'
##### Sum across pseudo CFG categories  #####
# ********************************************** #
#        Sum across pseudo CFG categories        #
# ********************************************** #

# objective: derive total frequency of consumption for pseudo CFG categories

#' Aggregate Diet Variables Frequency by CFG Classification  
#'
#' This function processes a dataset by aggregating diet frequency data according to the specified 
#' CFG (Canada Food Guide) classification. It selects columns with a specified suffix, transposes the data 
#' to a long format, merges with an external CFG classification dataset, and aggregates frequency counts.  
#'
#' @param data A data frame containing the dietary intake data, with columns that starts in `NUT_`.  
#' @param pseudo_cfg_classification A data frame with the CFG classification data, including columns `"name"`
#' (variable name) and `"cfg2019_primary"` (primary CFG category).  
#'
#' @return A data frame with summed food frequencies across CFG categories for each entity, 
#'         in a wide format with category-specific columns.  
#'         
#' @import dplyr  
#' @import tidyr  
#' 
#' @examples  
#' \dontrun{
#' # Example usage
#' nut_cfg2019_bl <- aggregate_nutrition_by_cfg(data_bl_nut, pseudo_cfg_classification)
#' }
#' 
#' @export

aggregate_nutrition_by_cfg <- function(data, pseudo_cfg_classification) {
  
  # check input
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  if (!"entity_id" %in% names(data)) {
    stop("`data` must contain a column named 'entity_id'.")
  }
  
  nut_vars <- grep("^NUT_", names(data), value = TRUE)
  if (length(nut_vars) == 0) {
    stop("No columns in `data` start with 'NUT_'. Check input data.")
  }
  
  if (!is.data.frame(pseudo_cfg_classification)) {
    stop("`pseudo_cfg_classification` must be a data frame.")
  }
  
  required_cols <- c("name", "cfg2019_primary")
  missing_cols <- setdiff(required_cols, names(pseudo_cfg_classification))
  if (length(missing_cols) > 0) {
    stop("`pseudo_cfg_classification` is missing required column(s): ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # Process the data and aggregate by CFG classification
  result <- 
    data |>
    dplyr::select("entity_id", dplyr::starts_with("NUT_")) |>
    # Transpose data to have nutrition variables as rows
    tidyr::pivot_longer(
      cols = dplyr::starts_with("NUT_"),
      names_to = "name",
      values_to = "freq"
    ) |>
    # Merge with CFG classification using variable name as key
    dplyr::inner_join(
      subset(pseudo_cfg_classification, 
             # subset = time==0, # note: not needed since variable names are time point specific
             select = c("name", "cfg2019_primary")), 
      by = "name"
    ) |>
    # Summarize frequency across CFG categories
    dplyr::group_by(entity_id, cfg2019_primary) |>
    dplyr::summarise(sum_frequency = sum(freq, na.rm = TRUE), .groups = 'drop') |>
    # Pivot to wide format
    tidyr::pivot_wider(
      names_from = cfg2019_primary,
      values_from = sum_frequency,
      names_prefix = "nut_cfg2019_"
    )
  
  return(result)
}

# ************************** #
#     Apply to each data     #
# ************************** #

dim(pseudo_cfg_classification); table(pseudo_cfg_classification$time)

# Apply 'aggregate_nutrition_by_cfg' to each dataset in the list
sdq_data_cfg_sumfreq <- 
  lapply(sdq_data_clean_nmiss,
         aggregate_nutrition_by_cfg,
         pseudo_cfg_classification = pseudo_cfg_classification)

#' note: 'rows only in tidyr::pivot_longer' reflects `NUT_` variables that are NOT dietary intakes  
#' note: 'rows only in subset' reflect T!=t (e.g., at baseline, time 1 and 2 excluded; and so on.)  

#'
##### Add derived variable labels #####
# ********************************************** #
#                   Add labels                   #
# ********************************************** #

#' Automatically Label Variables Based on 'cfg2019_primary' excel file
#'
#' This function applies consistent labels to variables in a dataset, aligning with
#' the primary data structure in a 'cfg2019' Excel file. It constructs labels in the 
#' format `labels, freq/d` for each variable, adding prefixes to ensure compatibility 
#' with the variable names in `data`.
#'
#' @param data A data frame containing the variables to be labeled.  
#' @param cfg2019_primary_data A data frame representing primary data extracted from 
#' the 'cfg2019' Excel file. This should include columns named `labels` and `levels`.  
#'
#' @details This function expects `data` to contain variables prefixed with `cfg2019`. 
#' It matches these variables with corresponding entries in `cfg2019_primary_data` to 
#' generate expanded labels. The resulting labels are then added to `data`.  
#'
#' @return A data frame with labeled variables, where labels follow the format specified 
#' in `cfg2019_primary_data` with frequency descriptors appended.  
#' 
#' @examples
#' # Assuming `data` and `cfg2019_primary_data` are pre-loaded data frames:
#' labeled_data <- auto_label_cfg2019_variables(data, cfg2019_primary_data)  
#'
#' @importFrom labelled set_variable_labels  
#' @importFrom dplyr mutate right_join  
#' @export  


auto_label_cfg2019_variables <- function(data, cfg2019_primary_data){
  
  # note: assuming data is fully consistent with 'cfg2019_primary_data' excel file
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  if (!is.data.frame(cfg2019_primary_data)) {
    stop("`pseudo_cfg_classification` must be a data frame.")
  }
  
  required_cols <- c("levels", "labels")
  missing_cols <- setdiff(required_cols, names(cfg2019_primary_data))
  if (length(missing_cols) > 0) {
    stop("`cfg2019_primary_data` is missing required column(s): ", 
         paste(missing_cols, collapse = ", "))
  }
  
  cfg2019_primary_for_data <- 
    cfg2019_primary_data |>
    dplyr::mutate(
      # expand labels
      labels_expanded  = paste0(cfg2019_primary_data$labels, ", freq/d"),
      # derive variable names consistent with those in 'data'
      varname = paste0("nut_cfg2019_", levels)
    ) |>
    # trick to ensure that variable in the label list will match those in "data"
    dplyr::right_join(
      data.frame(varname = grep("cfg2019",names(data),value=TRUE))
    )
  # assemble labeling list
  var_labels <-
    setNames(as.list(cfg2019_primary_for_data$labels_expanded), cfg2019_primary_for_data$varname)
  
  ## add labels
  data <- data |> labelled::set_variable_labels(.labels = var_labels,  .strict = FALSE)
  
  return(data)
  
}

# ************************** #
#     Apply to each data     #
# ************************** #

sdq_data_cfg_sumfreq_label <- 
  lapply(X = sdq_data_cfg_sumfreq,
         FUN = auto_label_cfg2019_variables, cfg2019_primary_data = cfg2019_primary_data)

#' note: 'rows only in x' correspond to pseudo CFG categories that do no exist at T=t (i.e., 'rg', 'ufa', 'sfa' at T>3)


# add time points indicator + missing items

# Loop over each element in the list
for (i in seq_along(sdq_data_cfg_sumfreq_label)) {
  sdq_data_cfg_sumfreq_label[[i]] <- 
    sdq_data_cfg_sumfreq_label[[i]] |>
    mutate(time = i-1) |>
    full_join(
      sdq_data_clean_nmiss[[i]] |> select("entity_id", "nut_missing_sdq_items"),
      by = "entity_id"
    )
}

#' note: 'rows only in ...' should equal to zero

for (i in seq_along(sdq_data_cfg_sumfreq_label)) {
  labelled::var_label(sdq_data_cfg_sumfreq_label[[i]]) <-
    list(
      time = "Time point",
      nut_missing_sdq_items = "Number of missing SDQ items"
    )
}


# append all data
nut_cfg2019_t <- 
  sdq_data_cfg_sumfreq_label |>
  purrr::reduce(rbind) 

#'
##### Summary and Save #####
# ********************************************** #
#                Summary and Save                #
# ********************************************** #

dim(nut_cfg2019_t); names(nut_cfg2019_t); table(nut_cfg2019_t$time)

nut_cfg2019_t[,-1] |>
  gtsummary::tbl_summary(
    by = "time",
    # statistic = list(all_continuous() ~ "{median} ({p25}, {p75}); [{min}-{max}]")
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
  ) |>
  gtsummary::modify_caption(caption = "Total frequency of pseudo CFG 2019 categories in the CLSA, by follow-up")

# Save 
save_and_summarize_data(
  data = nut_cfg2019_t,
  dir = dir_processed,
  dir_metadata = dir_meta
)

####  3) Generate data with secondary classification ####
# *********************************************************************** #
#              Generate data with secondary classification                #
# *********************************************************************** #

#'
##### Sum across food categories  #####
# ********************************************** #
#           Sum across food categories           #
# ********************************************** #

# objective: derive total frequency of consumption for common food categories

#' Aggregate Diet Variables Frequency
#'
#' This function processes a dataset by aggregating diet frequency data according to common
#' food categories. It selects columns with a specified suffix, transposes the data 
#' to a long format, merges with an external food classification dataset, and aggregates frequency counts.  
#'
#' @param data A data frame containing the dietary intake data, with columns that starts in `NUT_`.  
#' @param food_classification A data frame with the food classification data, including columns `"name"`
#' (variable name) and `"category"` (primary food category).  
#'
#' @return A data frame with summed food frequencies across categories for each entity, 
#'         in a wide format with category-specific columns.  
#'         
#' @import dplyr  
#' @import tidyr  
#' 
#' @examples  
#' \dontrun{
#' # Example usage
#' nut_food_bl <- aggregate_nutrition_by_food(data_bl_nut, food_classification)
#' }
#' 
#' @export

aggregate_nutrition_by_food <- function(data, food_classification) {
  
  # check input
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  
  if (!"entity_id" %in% names(data)) {
    stop("`data` must contain a column named 'entity_id'.")
  }
  
  nut_vars <- grep("^NUT_", names(data), value = TRUE)
  if (length(nut_vars) == 0) {
    stop("No columns in `data` start with 'NUT_'. Check input data.")
  }
  
  if (!is.data.frame(food_classification)) {
    stop("`food_classification` must be a data frame.")
  }
  
  required_cols <- c("name", "category")
  missing_cols <- setdiff(required_cols, names(food_classification))
  if (length(missing_cols) > 0) {
    stop("`food_classification` is missing required column(s): ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # Process the data and aggregate by food category
  result <- 
    data |>
    dplyr::select("entity_id", dplyr::starts_with("NUT_")) |>
    # Transpose data to have nutrition variables as rows
    tidyr::pivot_longer(
      cols = dplyr::starts_with("NUT_"),
      names_to = "name",
      values_to = "freq"
    ) |>
    # Merge with CFG classification using variable name as key
    dplyr::inner_join(
      subset(food_classification, 
             # subset = time==0, # note: not needed since variable names are time point specific
             select = c("name", "category")), 
      by = "name"
    ) |>
    # Summarize frequency across CFG categories
    dplyr::group_by(entity_id, category) |>
    dplyr::summarise(sum_frequency = sum(freq, na.rm = TRUE), .groups = 'drop') |>
    # Pivot to wide format
    tidyr::pivot_wider(
      names_from = category,
      values_from = sum_frequency,
      names_prefix = "nut_"
    )
  
  return(result)
}

# ************************** #
#     Apply to each data     #
# ************************** #

dim(food_classification); table(food_classification$time)

# Apply 'aggregate_nutrition_by_food' to each dataset in the list
sdq_data_food_sumfreq <- 
  lapply(sdq_data_clean_nmiss,
         aggregate_nutrition_by_food,
         food_classification = food_classification)

#' note: 'rows only in tidyr::pivot_longer' reflects `NUT_` variables that are NOT dietary intakes  
#' note: 'rows only in subset' reflect T!=t (e.g., at baseline, time 1 and 2 excluded; and so on.)  


# add time points indicator + missing items

# Loop over each element in the list
for (i in seq_along(sdq_data_food_sumfreq)) {
  sdq_data_food_sumfreq[[i]] <- 
    sdq_data_food_sumfreq[[i]] |>
    mutate(time = i-1) |>
    full_join(
      sdq_data_clean_nmiss[[i]] |> select("entity_id", "nut_missing_sdq_items"),
      by = "entity_id"
    )
}

#' note: 'rows only in ...' should equal to zero

# append all data
nut_food_t <- 
  sdq_data_food_sumfreq |>
  purrr::reduce(dplyr::bind_rows) |>
  # add labels
  labelled::set_variable_labels(
    time = "Time point",
    nut_missing_sdq_items = "Number of missing SDQ items",
    nut_bread             = "Bread",
    nut_carrots           = "Carrots",
    nut_cereal            = "Cereals",
    nut_dairy             = "Dairy (cheese, yogurt)",
    nut_dessert           = "Dessert",
    nut_eggs              = "Eggs",
    nut_fish              = "Fish",
    nut_fruit             = "Fruit",
    nut_fruitjuice        = "Fruit juice",
    nut_legumes           = "Legumes",
    nut_milk              = "Milk (low and regular fat)",
    nut_fat               = "Butter or regular margarine",
    nut_nuts              = "Nuts",
    nut_others            = "Other vegetables",
    nut_plantbev          = "Plant-based beverages",
    nut_potato            = "Potato",
    nut_processedmeat     = "Processed meat",
    nut_redmeat           = "Red meat",
    nut_salad             = "Green salad",
    nut_sauce             = "Sauce",
    nut_snacks            = "Snacks",
    nut_ufa               = "Regular vinaigrettes, dressings, and dips",
    nut_unclassified      = "Unclassified",
    nut_whitemeat         = "Poultry",
    nut_energydrinks      = "Energy drinks",
    nut_packaged          = "Packaged foods",
    nut_fruitdrinks       = "Fruit drinks",
    nut_softdrinks        = "Soft drinks"
  )

##### winsorization of diet data #####
# ********************************************** #
#           Winsorization of diet data           #
# ********************************************** #

# goal: cap dietary data distribution at a somewhat high value to avoid unduly influence of outliers
# note: this step is performed in code 22_ for other diet data. Here, we apply winsorization because
# individual food groups are not used for the primary analyses

# specify id variables
participant_identifier <- "entity_id"
time_identifier <- "time"

# output all diet variables based on their prefix 
vars_diet <- names(nut_food_t |> select(starts_with("nut_")))

# note: doesnt matter if some variables are not used afterward, winsorization across var. is independant

# Prelim. Output distribution BEFORE (baseline only)
data_distrib_diet_before <- 
  ncimultivar::nci_multivar_summary(
    nut_food_t ,
    population.name = "All eligible, baseline",
    row.subset = nut_food_t[[time_identifier]]==0, 
    variables = vars_diet,
    quantiles = seq(0,100)/100
  ) |>
  dplyr::mutate(
    # add numerical percentile
    percentile = ifelse(statistic!="Mean", as.numeric(gsub("[^\\d]+", "", statistic, perl=TRUE)), NA)
  ) |>
  labelled::set_variable_labels(
    population = "Sample",
    variable   = "Diet variables",
    statistic  = "Statistic",
    value      = "Value",
    percentile = "Percentile"
  )


# Prelim. For comparison purpose, find the maximum value for each variable BEFORE winsorization
max_values_before <- apply(nut_food_t[,c(vars_diet), with = FALSE], 2, max, na.rm=TRUE)

# Step 1: initiate list
winsorize_x <- list()

# Step 2: loop through all dietary constituents in 'vars_diet'
for (i in 1:length(vars_diet)){
  
  # 1) get a lower threshold to avoid winsorization of lower values
  p5_nonzero_x <- 
    nut_food_t |>
    dplyr::filter(.data[[time_identifier]]==2 & .data[[vars_diet[i]]]>2) |>
    dplyr::summarise(p5 = quantile(.data[[vars_diet[i]]], probs = 0.05, na.rm = TRUE)) |>
    dplyr::pull(p5)
  # note: 5th non zero percentile is arbitrary, does not matter as long as value is low
  
  # 2) apply algorithm
  winsorize <-
    ncimultivar::boxcox_survey(
      input.data            = nut_food_t ,
      row.subset            = nut_food_t[[time_identifier]]==2,
      id                    = participant_identifier ,
      repeat.obs            = time_identifier,
      variable              = vars_diet[i],
      # weight                = ,
      do.winsorization      = TRUE,
      is.episodic           = vars_diet[i] %in% c("nut_cfg2019_plantbev"), # assess non-zero values for episodic foods
      iqr.multiple          = 2, # note: upper cut-off as: p75 + IQR * 3
      print.winsorization   = FALSE)
  
  # add generic names
  names(attributes(winsorize)$winsorization.report) <- c(participant_identifier, time_identifier, "original.value", "winsorized.value")
  
  # 3) keep only the upper threshold, and ignore lower winsorization
  winsorize_x[[i]] <- 
    attributes(winsorize)$winsorization.report |>
    # remove rows that were 'low outlier' (i.e., winsorize only upper values)
    dplyr::filter(original.value > p5_nonzero_x)
  
  rm(winsorize)
  
  # output current variable name for simplicity
  current_x <- vars_diet[i]
  
  # 4) Extract the upper threshold and winsorize accordingly
  if(length(winsorize_x[[i]]$winsorized.value)>0){
    message(glue::glue("Winsorization of x{i}, {current_x}: max. value {round(winsorize_x[[i]]$winsorized.value[1],2)}"))
    
    nut_food_t <- 
      nut_food_t |>
      dplyr::mutate(
        !!sym(current_x) := pmin(get(current_x), winsorize_x[[i]]$winsorized.value[1])
      )
    
  } else {
    message(glue::glue("Winsorization of x{i}, {current_x}: none."))
  }
  rm(current_x)
}

# Step 3) Assessment. Find the maximum value for each variable AFTER winsorization
max_values_after <- apply(nut_food_t[,c(vars_diet), with = FALSE], 2, max, na.rm=TRUE)

print(max_values_before) -
  print(max_values_after)

# Add variable names for clarity
names(winsorize_x) <- vars_diet

# Output each variable with its max AFTER, i.e., winsorization cut-off value
data_winsorization <- 
  tibble::tibble(variable =names(max_values_after),
                 max_values_before,
                 cut_off = max_values_after) |>
  labelled::set_variable_labels(
    variable          = "Diet variable",
    max_values_before = "Maximum value in raw data",
    cut_off           = "Maximum value in winsorized data" 
  )

# Check distributions vs. winsorization threshold
fig_winsorization <-
  ggplot(data = data_distrib_diet_before, aes(x=value,y=percentile),stat="identity") + 
  # note: <percentile> indicates percentile value for each X (e.g., 0, 1, 2, 3, ... 100)
  geom_line(linewidth=1.2) + 
  geom_vline(data = data_winsorization, aes(xintercept = cut_off), linetype="longdash", color="red") +
  ggplot2::facet_wrap(~variable, ncol=4, scales="free") + 
  ggplot2::labs(
    title = "Raw distribution of diet variables among eligible CLSA participants at baseline",
    subtitle = "The dashed red line indicates the (upper) winsorization cut-off",
    y= "Percentile",
    x= "Frequency of consumption per day"
  ) +
  ggplot2::theme_bw() 

fig_winsorization

# save data for further analysis or reporting
save(data_distrib_diet_before, data_winsorization, fig_winsorization, winsorize_x,
     file = file.path(dir_results, "diet_foodgroup_winsorization.rdata"))

#'
##### Summary and Save #####
# ********************************************** #
#                Summary and Save                #
# ********************************************** #

dim(nut_food_t); names(nut_food_t); table(nut_food_t$time)

nut_food_t[,-1] |>
  gtsummary::tbl_summary(
    by = "time",
    # statistic = list(all_continuous() ~ "{median} ({p25}, {p75}); [{min}-{max}]")
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
  ) |>
  gtsummary::modify_caption(caption = "Total frequency of food categories in the CLSA, by follow-up")

# Save 
save_and_summarize_data(
  data = nut_food_t,
  dir = dir_processed,
  dir_metadata = dir_meta
)


#'
#### End of code ####
# *********************************************************************** #
#                              End of code                                #
# *********************************************************************** #

sessionInfo()
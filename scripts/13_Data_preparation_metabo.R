#' ---
#' title: "Combine and prepare metabolomics data"
#' author: "Didier Brassard"
#' date: "`r Sys.Date()`"
#' code checked by:
#' code checked date:
#' run time: <1min
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
library(tidyr)
library(tidylog)
library(lubridate)
library(data.table); if(is.na(parallel::detectCores())==FALSE & parallel::detectCores()>1){available_core <- parallel::detectCores()-1} else {available_core <- 1}
data.table::setDTthreads(threads = available_core)
data.table::getDTthreads()

## presentation
library(gtsummary)
install.packages("ggflowchart")
library(ggflowchart)

## project
library(here)

## other
install.packages("janitor")
install.packages("tictoc")
library(tictoc)
library(janitor)

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
dir_raw_metabo <-  here::here("data", "raw", "metabolomics")
dir_results <- here::here("data", "results")

# common prefix for all data file
project_id <- "25CA004_UdeM_AJTessier_"

#'
##### Load in-house functions  #####
# ********************************************** #
#            Load in-house functions             #
# ********************************************** #

# save data, create (processed) data dictionary
source(file.path(dir_scripts, "save_and_summarize_data.R"))

# prepare a generic questionnaire data (selection, rename, labels) and other aux. functions
source(file.path(dir_scripts, "prepare_clsa_questionnaire.R"))

# aux. function to support data cleaning
source(file.path(dir_scripts, "helper_data_cleaning.R"))

# common identification function:
make_metabo_id <- function(data){
  out<- 
    data |>
    dplyr::mutate(
      # flag "lqc" identifier
      lqc_flag = ifelse(grepl("LQC",adm_metabolon2_com),1,0)
    ) |>
    # remove LQc
    dplyr::filter(lqc_flag==0) |>
    dplyr::mutate(
      # remove letters from id
      adm_metabolon2_com = as.numeric(gsub("[^\\d]+", "", adm_metabolon2_com, perl=TRUE))
    ) |> 
    dplyr::rename(sd_metabo_key = adm_metabolon2_com) |>
    labelled::set_variable_labels(
      sd_metabo_key = "Metabolon unique sample id"
    )
  return(out)
}

#### Load processed data ####
# ********************************************** #
#              Load processed data               #
# ********************************************** #

# Link between the omic and sociodemo data
sd_id_key <- 
  readRDS(file = file.path(dir_processed, "sociodemo_fup0.rds")) |>
  select("entity_id", ends_with("_key")) |>
  select(-c("sd_gwas_key", "sd_epigen_key")) |>
  filter(is.na(sd_metabo_key)==FALSE)


#### Load raw data ####
# *********************************************************************** #
#                             Load raw data                               #
# *********************************************************************** #

metabo_id_raw <- 
  load_questionnaire_data(path_data = 
                            file.path(dir_raw_metabo, "CLSA COHORT_COMBINED METADATA_12072024-de-identified.csv")
  )  |> 
  janitor::clean_names() 

dim(metabo_id_raw); names(metabo_id_raw)

metabo_id <- 
  metabo_id_raw |>
  make_metabo_id()

dim(metabo_id); names(metabo_id)


## confirm lack of duplicate id (should be the case because low quality were filtered out)
subset(metabo_id, duplicated(metabo_id$sd_metabo_key))


# add identifier for sociodemograpghic data
metabo_id <- 
  full_join(sd_id_key, metabo_id) |>
  # keep it simple with only id variables
  select("entity_id", "sd_metabo_key") |>
  labelled::set_variable_labels(
    sd_metabo_key = "Metabolon unique sample id"
  )
dim(metabo_id); names(metabo_id)

# note: all should be 'matched rows'


save_and_summarize_data(
  data = metabo_id ,
  dir = dir_processed,
  dir_metadata = dir_meta
)

#'
#### Ouptut metabolite annotation file ####
# *********************************************************************** #
#                   Ouptut metabolite annotation file                     #
# *********************************************************************** #

metabo_annotation <- 
  load_questionnaire_data(path_data = 
                            file.path(dir_raw_metabo, "ANNOTATION TABLE_V2.CSV")
  )  |> 
  janitor::clean_names() |>
  mutate(
    named = ifelse(type=="NAMED",1,0)
  ) |>
  labelled::set_variable_labels(
    chem_id           = "Unique biochemical identifier",
    chro_lib_entry_id = "",
    comp_id           = "Metabolon compound identifier",
    lib_id            = "",
    super_pathway     = "General biochemical class",
    sub_pathway       = "Specific biochemical class",
    pathway_sortorder = "Pathway sorting number",
    named             = "Compound is named (or unnamed/unknown)",
    inchikey          = "IUPAC textual chemical identifier derived from INChI",
    smiles            = "Simplified molecular-input line-entry system (SMILES) line notation string",
    chemical_name     = "Name of the identified biochemical",
    plot_name         = "Name of the identified biochemical",
    cas               = "Unique numerical identifier assigned by the Chemical Abstracts Service (CAS)",
    chemspider        = "Unique numerical identifier as maintained in the ChemSpider database",
    hmdb              = "Identifier and link to compound information maintained by the Human Metabolome Database (HMDB)",
    kegg              = "Identifier and link to compound information in the Kyoto Encyclopedia of Genes and Genomes (KEGG) ",
    pubchem           = "Identifier assigned by the National Center for Biotechnology Information (NCBI) and searchable in the PubChem database",
    platform          = "Metabolon platform used for identification"
  ) |>
  rename(chemical_name_short = plot_name)

dim(metabo_annotation); names(metabo_annotation)


save_and_summarize_data(
  data = metabo_annotation ,
  dir = dir_processed,
  dir_metadata = dir_meta
)

#'
#### Output prepared metabolites ####
# *********************************************************************** #
#                      Output prepared metabolites                        #
# *********************************************************************** #
tictoc::tic()
metabo_norm_imp_all <- 
  load_questionnaire_data(path_data = 
                            file.path(dir_raw_metabo, "CLSA NORMIMPDATAALL_12072024.CSV"),
                          header = TRUE # option needed to load metabolomics data correctly
  )  |> 
  janitor::clean_names() |>
  make_metabo_id()
tictoc::toc()
dim(metabo_norm_imp_all); names(metabo_norm_imp_all)[1:50]

tictoc::tic()
metabo_norm_all <- 
  load_questionnaire_data(path_data = 
                            file.path(dir_raw_metabo, "CLSA NORMDATAALL_12072024.CSV"),
                          header = TRUE
  )  |> 
  janitor::clean_names() |>
  make_metabo_id()
tictoc::toc()
dim(metabo_norm_all); names(metabo_norm_all)[1:50]


## change text-field as missing for all metabolites
tictoc::tic()
metabo_norm_all[, (cols) := lapply(.SD, function(x) {
  x[x == "Metabolite_not_called_in_this_set"] <- NA
  as.numeric(x)
}), .SDcols = cols <- grep("^x", names(metabo_norm_all), value = TRUE)]
tictoc::toc()

# ********************************************** #
#                Summary and Save                #
# ********************************************** #

# note: metadata not kept since it would only reflect the 1400+ metabolites (x1, x2, ..., x[chem_id])

save_and_summarize_data(
  data = metabo_norm_imp_all ,
  dir = dir_processed,
  dir_metadata = dir_meta,
  save_metadata = FALSE
)

save_and_summarize_data(
  data = metabo_norm_all ,
  dir = dir_processed,
  dir_metadata = dir_meta,
  save_metadata = FALSE
)

# note: columns are metabolites identified by 'chem_id' variable from 'metabo_annotation' above


#'
#### End of code ####
# *********************************************************************** #
#                              End of code                                #
# *********************************************************************** #


#' ---
#' title: "Combine and prepare data to derive miscellaneous complementary variables (eg, follow-up dates)"
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
library(ggflowchart)

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

#'
##### Load processed data #####
# ********************************************** #
#              Load processed data               #
# ********************************************** #
processed_sd_data <- c("sociodemo_fup0", "sociodemo_fup1", "sociodemo_fup2")

sd_data_fup <- 
  lapply(X = processed_sd_data ,
         FUN = function(x) {
           readRDS(file = file.path(dir_processed, paste0(x, ".rds")))
         })

names(sd_data_fup) <- processed_sd_data

basic_id <- c("entity_id")
basic_clsa <- c("clsa_recruit_prov", "clsa_recruit_prov_f", "clsa_strata", "clsa_analytic_weight", "clsa_inflation_weight")
basic_study <- c(basic_id, basic_clsa)

#'
##### Calculate time of follow-up variables #####
# *********************************************************************** #
#                 Calculate time of follow-up variables                   #
# *********************************************************************** #

# note: can handle additional follow-up (3, 4 ...) without modification

# Helper function to create Y-M-D variable
create_ymd_fup <- function(data){
  out <- 
    data |>
    dplyr::select(all_of(basic_id), "sd_date") |>
    dplyr::mutate(
      temp = lubridate::as_date(sd_date),
      date = lubridate::ymd(temp)
    ) |>
    dplyr::select(all_of(basic_id), "date")
  return(out)
}

# Apply to each questionnaire (sociodemo) data, adding time point identifier
sd_data_fup_dates <- 
  Map(
    f = \(x, i) { create_ymd_fup(x) |> dplyr::mutate(time = i-1) },
    sd_data_fup,
    seq_along(sd_data_fup)
  )

# transfer list to df
fup_dates_t <- do.call(rbind, sd_data_fup_dates)

# Transpose 
fup_dates_w <- 
  fup_dates_t |>
  pivot_wider(
    values_from = date,
    names_from  = time,
    names_prefix = "date_t"
  ) 


# code snippet to calculate follow-up time once merged with eligibility data
notrun <- function(fup_dates_w){
  fup_dates_w |>
    rowwise() |> 
    mutate(
      last_date = max(c_across(starts_with("date_t")), na.rm = TRUE),
      total_fup_time = as.numeric(last_date - date_t0, units = "days") / 30.4
    ) |>
    ungroup()
}




#'
##### Summary and Save #####
# ********************************************** #
#                Summary and Save                #
# ********************************************** #

# save variable tracking data for reference

labelled::var_label(fup_dates_w) <- 
  list(
    entity_id = "Participant id",
    date_t0   = "Date, baseline",
    date_t1   = "Date, follow-up 1",
    date_t2   = "Date, follow-up 2")


# save all using the save_and_summarize_data function
save_and_summarize_data(
  data = fup_dates_w,
  dir = dir_processed,
  dir_metadata = dir_meta
)

#'
#### End of code ####
# *********************************************************************** #
#                              End of code                                #
# *********************************************************************** #

sessionInfo()

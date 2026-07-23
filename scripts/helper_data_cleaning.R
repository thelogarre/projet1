#### rename_data_variables ####

#' Rename variables in a dataset with optional prefix and/or suffix
#'
#' This function renames variables in a data frame by adding a prefix 
#' and/or a suffix, while optionally excluding specified variables from renaming.
#'
#' @param data A \code{data.frame} whose variables will be renamed.
#' @param exceptions A character vector of variable names to exclude from renaming. 
#'   Defaults to \code{NULL}, meaning all variables will be renamed.
#' @param add_prefix A character string to be added before variable names. 
#'   Defaults to \code{NULL}.
#' @param add_suffix A character string to be added after variable names. 
#'   Defaults to \code{NULL}.
#'
#' @return A \code{data.frame} with renamed variables.
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#' rename_data_variables(df, exceptions = "b", add_prefix = "x_")
#' rename_data_variables(df, add_suffix = "_new")
#'
#' @export

rename_data_variables <- function(data, exceptions=NULL, add_prefix=NULL, add_suffix=NULL){
  
  # confirm input
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  
  if (is.null(add_prefix) & is.null(add_suffix)) {
    stop("Both 'add_prefix' and 'add_suffix' are NULL. Please specify at least one.")
  }
  
  if (!is.null(exceptions) && !all(exceptions %in% names(data))) {
    stop("Some 'exceptions' are not column names in 'data'")
  }
  
  
  # define variables to rename
  vars_to_rename <- if (!is.null(exceptions)) setdiff(names(data), exceptions) else names(data)
  
  # build new names
  new_names <- names(data)
  new_names[match(vars_to_rename, names(data))] <- paste0(add_prefix, vars_to_rename, add_suffix)
  
  # check for duplicates
  if (anyDuplicated(new_names)) {
    stop("Renaming resulted in duplicate variable names. Please adjust prefix/suffix.")
  }
  
  # assign new names and return
  names(data) <- new_names
  return(data)
  
}

#### assign_labels ####

#' Assign variable labels to a data frame
#'
#' @param data A data.frame (or tibble/data.table).
#' @param labels A named list of variable labels, where names correspond to variable names.
#'
#' @return The data with "label" attributes assigned to matching variables.
#' @export
#'
#' @examples
#' labels <- list(
#'   age = "Age, y",
#'   sex = "Sex"
#' )
#' df <- data.frame(age = c(20, 30), sex = c("M", "F"))
#' df <- assign_labels(df, labels)
#' attr(df$age, "label")
assign_labels <- function(data, labels) {
  # Check input
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame, tibble, or data.table.")
  }
  if (!is.list(labels) || is.null(names(labels))) {
    stop("`labels` must be a *named list* of variable labels.")
  }
  # Identify present absent
  matched <- intersect(names(labels), names(data))
  not_found <- setdiff(names(labels), names(data))
  
  if (length(not_found) > 0) {
    message(
      "assign_labels: ", length(not_found), 
      " variable(s) were not found in `data`: ",
      paste(not_found, collapse = ", ")
    )
  }
  
  # Assign labels
  for (var_name in names(labels)) {
    if (var_name %in% names(data)) {
      attr(data[[var_name]], "label") <- labels[[var_name]]
    }
  }
  return(data)
}

#### label_dummy_vars ####

#' Auto-label many variables with a common suffix (e.g. var_1, ..., var_k)
#'
#' Generates labels of the form "Var level X" and either:
#' 1. Applies them directly to a data frame (`mode = "apply"`), or
#' 2. Prints R code using labelled::set_variable_labels() (`mode = "print"`).
#'
#' @param data Optional data frame. Required only when mode = "apply".
#' @param k Integer. Number of variables (var_1 to var_k).
#' @param prefix Character. Variable name prefix. Default is "var".
#' @param mode "apply" (apply to a dataset) or "print" (print code). Default "print".
#'
#' @return If mode = "apply", returns the updated data frame.
#' @export
label_dummy_vars <- function(data = NULL, k,
                             prefix = "var",
                             mode = c("print", "apply")) {
  
  mode <- match.arg(mode)
  
  vars <- paste0(prefix,"_", 1:k)
  labels <- paste(stringr::str_to_sentence(prefix), "level", 1:k)
  named_labels <- setNames(labels, vars)
  
  # print code
  if (mode == "print") {
    for (i in seq_len(k)) {
      cat(
        glue::glue(
          'labelled::set_variable_labels({vars[i]} = "{labels[i]}")\n'
        )
      )
    }
    return(invisible(NULL))
  }
  
  # apply to data
  if (mode == "apply") {
    if (is.null(data)) {
      stop("`data` must be supplied when mode = 'apply'.")
    }
    
    # Identify missing variables
    missing_vars <- setdiff(vars, names(data))
    
    if (length(missing_vars) > 0) {
      message(
        "label_dummy_vars: Bypassing the following variables because they are not in the data: ",
        paste(missing_vars, collapse = ", ")
      )
    }
    
    # Keep only labels for variables actually present
    vars_present <- intersect(vars, names(data))
    labels_present <- named_labels[vars_present]
    
    if (length(vars_present) == 0) {
      message("label_dummy_vars: No matching variables found in data. No labels applied.")
      return(data)
    }
    
    data <- labelled::set_variable_labels(data, !!!labels_present)
    return(data)
  }
}

#### categorize_var ####

#' Categorize a numeric variable into specified quantiles.
#'
#' This function categorizes a numeric variable into the specified number of quantiles,
#' providing automatic labeling for the resulting factor levels.
#'
#' @param x A numeric vector to be categorized.
#' @param divide_in Number of groups to divide the variable into (3, 4, or 5).
#' @param quantile_type The type of quantile calculation (default is 7).
#' @param as_factor If TRUE, the result is returned as a factor with automatic labels (default is TRUE).
#' @param label_accuracy The precision of the labels (default is 0.1).
#'
#' @return A factor with categorized levels and optional automatic labels.
#'
#' @examples
#' # categorize_var(c(1, 2, 3, 4, 5, 6), divide_in = 3)
#' 
#' @importFrom scales number
#'
#' @export

categorize_var <- function(x, divide_in = 4, quantile_type = 7, as_factor = TRUE, label_accuracy = 0.1) {
  if (!(divide_in %in% c(3, 4, 5))) {
    stop('Categorization not supported, please use 3, 4 or 5.')
  }
  
  message(paste("categorize_var: X in", divide_in,"groups"))
  
  # get prefix
  prefix <- ifelse(divide_in==3,"T", "Q")
  
  # get quantile
  list_quantile <- seq(0,1, by= 1/divide_in)
  # remove outer quantile
  list_quantile <- round(list_quantile[2:(length(list_quantile)-1)],4)
  
  # get quantile values of x
  list_quantile_values <- 
    quantile(
      x,
      probs = list_quantile, 
      type  = quantile_type, 
      na.rm = TRUE
    )
  
  # categorize X
  x_cat <- cut(x,
               breaks = c(-Inf, list_quantile_values, Inf),
               include.lowest = TRUE,
               right=TRUE, # includes upper bound of intervals
               labels = FALSE)
  
  if (as_factor) {
    # Generate automatic labels
    label_first <- paste0(prefix,"1", " (min to ", scales::number(list_quantile_values[1], label_accuracy),")")
    label_last  <- paste0(prefix, divide_in, " (>", scales::number(list_quantile_values[divide_in-1], label_accuracy)," to max)")
    interval_labels <- c()
    for (i in 1:(length(list_quantile_values) - 1)) {
      interval_labels <-
        c(interval_labels, paste0(prefix, i+1, " (>",
                                  scales::number(list_quantile_values[i], label_accuracy),
                                  " to ",
                                  scales::number(list_quantile_values[i + 1], label_accuracy),")"))
    }
    
    # apply labels
    x_cat <-
      factor(x_cat,
             levels = seq_len(divide_in),
             labels = c(label_first, interval_labels, label_last))
  }
  
  return(x_cat)
}

#### check_y_censoring ####

#' Check consistency between outcome missingness and censoring
#'
#' Verifies that missing outcome values at the last follow-up time are
#' consistent with the censoring indicator. Flags individuals who have
#' a missing outcome but are not censored.
#'
#' @param data A data frame containing longitudinal follow-up data.
#'   Must include variables \code{time} and \code{entity_id}.
#' @param current_outcome Character string giving the name of the outcome
#'   variable to be checked.
#' @param censoring Character string giving the name of the censoring
#'   indicator (0 = not censored, 1 = censored).
#'
#' @details
#' The check is performed at the maximum observed value of \code{time}.
#' If missing outcomes are found among non-censored individuals, a warning
#' is issued and a data frame identifying problematic observations is
#' returned. Otherwise, the function prints a confirmation message and
#' returns \code{NULL} invisibly.
#'
#' @return
#' A data frame with \code{entity_id}, outcome, censoring indicator, and
#' a \code{flag} variable when inconsistencies are detected;
#' otherwise \code{NULL} (invisibly).
#'
#' @examples
#' \dontrun{
#' check_y_censoring(
#'   data = df,
#'   current_outcome = "y",
#'   censoring = "censored"
#' )
#' }
#'
#' @seealso
#' \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}
#'
#' @importFrom dplyr filter select mutate arrange
#' @importFrom glue glue
#'
#' @export

check_y_censoring <- function(data, current_outcome, censoring){
  # note: 'time' + 'entity_id' variables are hardcoded
  
  last_t <- max(data$time, na.rm = TRUE)
  
  dat_last <- data |>
    dplyr::filter(time == last_t)
  
  # Logical vectors
  y_missing   <- is.na(dat_last[[current_outcome]])
  not_censored <- dat_last[[censoring]] == 0
  
  n_problem <- sum(y_missing & not_censored, na.rm = TRUE)
  
  if (n_problem > 0) {
    warning(
      glue::glue(
        "Missing <{current_outcome}> outcome in {n_problem} individuals that are NOT censored!"
      )
    )
    
    return(
      dat_last |>
        dplyr::select(
          entity_id,
          dplyr::all_of(censoring),
          dplyr::all_of(current_outcome)
        ) |>
        dplyr::mutate(
          flag = as.integer(is.na(.data[[current_outcome]]) &
                              .data[[censoring]] == 0)
        ) |>
        dplyr::arrange(dplyr::desc(flag))
    )
    
  } else {
    message("check_y_censoring:")
    message(
      glue::glue(
        "No missing <{current_outcome}> outcomes among non-censored individuals"
      )
    )
    invisible(NULL)
  }
}

#### is_word_present ####

#' Detect presence of one or more words in character strings
#'
#' Checks whether any of the specified words are present in each element
#' of a character vector, using word boundaries.
#'
#' @param string Character vector to be searched.
#' @param words Character vector of words to detect. If `NULL` or empty,
#'   returns a logical vector of `FALSE` with the same length as `string`.
#' @param ignore_case Logical; whether matching should be case-insensitive.
#'   Default is `TRUE`.
#'
#' @return A logical vector of the same length as `string`, indicating
#'   whether at least one word was detected.
#'
#' @details
#' Words are matched using regular expression word boundaries (`\\b`),
#' meaning that partial matches within longer words are not detected.
#' Special characters in `words` are automatically escaped.
#'
#' Missing values in `string` return `NA`.
#'
#' @keywords internal
#' @examples
#' is_word_present("This is a test", c("test", "foo"))
#' is_word_present(c("apple pie", "banana"), "apple")
#' is_word_present(NA_character_, "apple")
#'
is_word_present <- function(string, words, ignore_case = TRUE) {
  
  # check inputs
  if (!is.character(string)) {
    stop("`string` must be a character vector.", call. = FALSE)
  }
  
  if (is.null(words) || length(words) == 0) {
    return(rep(FALSE, length(string)))
  }
  
  if (!is.character(words)) {
    stop("`words` must be a character vector.", call. = FALSE)
  }
  
  # find words
  escaped <- stringr::str_replace_all(words, "([\\W])", "\\\\\\1")
  
  pattern <- paste0("\\b(", paste(escaped, collapse = "|"), ")\\b")
  
  grepl(
    pattern,
    string,
    ignore.case = ignore_case
  )
}



#### make_time_fixed ####

#' Create a Time-Fixed Version of a Time-Varying Variable
#'
#' @description
#' Takes a long-format dataset and converts a time-varying variable into a
#' time-fixed variable (fixed at a chosen time point). Optionally prints all
#' pairwise cross-tabulations across the three time points to assess stability.
#'
#' @param data A long-format `data.frame` or `data.table`.
#' @param variable A character string with the **name of the time-varying variable**.
#' @param participant_identifier A character string giving the participant ID variable.
#' @param time_identifier A character string giving the time variable (e.g., `"time"`).
#' @param fix_at_time A value of the time variable indicating which time point to fix at.
#' @param remove_variable Logical; if `TRUE` (default), the original time-varying variable is not kept in the data.
#' @param cross_tabs Logical; if `TRUE`, prints pairwise cross-tabulations using `tab_n_time()`.
#'
#' @details
#' The function pivots the long-format data to wide format, extracts the value
#' of the variable at the chosen time point, and merges it back into the original
#' dataset as a time-fixed variable.
#'
#' Requires a helper function `tab_n_time()` available in the environment.
#'
#' @return A dataset containing the original data plus one additional
#'         time-fixed variable named `variable_t<fix_time>`.
#'
#' @examples
#' \dontrun{
#' data_fixed <- make_time_fixed(
#'   data = long_data,
#'   variable = "cc_cancer",
#'   participant_identifier = "id",
#'   time_identifier = "time",
#'   fix_at_time = 0,
#'   cross_tabs = TRUE
#' )
#' }
#'
#' @export
make_time_fixed <- function(data,
                            variable,
                            participant_identifier,
                            time_identifier,
                            fix_at_time,
                            remove_variable = TRUE,
                            cross_tabs = TRUE) {
  
  # check inputs
  required_cols <- c(participant_identifier, time_identifier, variable)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following required variables are missing from `data`: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # Check that time variable contains the requested time point
  if (!fix_at_time %in% unique(data[[time_identifier]])) {
    stop("Requested fix_at_time = ", fix_at_time,
         " does not exist in the dataset. Available times: ",
         paste(sort(unique(data[[time_identifier]])), collapse = ", "))
  }
  
  # Check variable is not constant across all time points (common error)
  n_time_values <- length(unique(data[[time_identifier]]))
  if (n_time_values < 2) {
    stop("Only one time point detected. Cannot create a time-fixed variable.")
  }
  
  # transpose for long to wide
  wide <-
    data |>
    dplyr::select(
      dplyr::all_of(participant_identifier),
      dplyr::all_of(time_identifier),
      dplyr::all_of(variable)
    ) |>
    tidyr::pivot_wider(
      values_from  = dplyr::all_of(variable),
      names_from   = dplyr::all_of(time_identifier),
      names_prefix = paste0(variable, "_t")
    )
  
  # if needed, cross-tabs across time points
  if (cross_tabs) {
    print( tab_n_time(wide, variable)) 
  }
  
  # output name of variable at T=t
  variable_Tt <- paste0(variable, "_t", fix_at_time)
  
  # Validate that variable_tX exists
  if (!variable_Tt %in% names(wide)) {
    stop("The time-fixed variable '", variable_Tt,
         "' was not created. Check time values and naming.")
  }
  
  # drop original, if need be
  input_data <- if(remove_variable) data |> dplyr::select(-dplyr::all_of(variable)) else data
  # merge time-fixed variable back into original data
  data_out <-
    dplyr::full_join(
      input_data,
      wide  |> dplyr::select(
        dplyr::all_of(participant_identifier),
        dplyr::all_of(variable_Tt)
      ),
      by = participant_identifier
    )
  
  return(data_out)
}



#### missing_indicator_sum ####

#' Identify Missing Values and Calculate Missingness Sum
#'
#' This function checks specified columns in a dataframe for missing values (NA) and generates new indicator columns where missing values are represented by 1 and non-missing values by 0. Additionally, it calculates the sum of missing values across the specified columns for each row.
#'
#' @param data A dataframe or tibble to check for missing values.
#' @param columns A character vector specifying the columns to check for missing values.
#' @param prefix A character string specifying the prefix for the indicator columns. Default is "miss_".
#' 
#' @return The original dataframe with additional columns indicating missing values (`<prefix><colname>`) for each specified column and a column `missing_sum` which provides the sum of missing values for each row.
#'
#' @examples
#' df <- data.frame(
#'   col1 = c(1, 2, NA, 4),
#'   col2 = c("A", NA, "C", "D"),
#'   col3 = c(NA, 2, 3, 4)
#' )
#' columns_to_check <- c("col1", "col2", "col3")
#' missing_indicator_sum(df, columns_to_check)
#'
#' @import dplyr
#' @export

missing_indicator_sum <- function(data, columns, prefix="miss_") {
  # Check if columns exist in data
  columns <- columns[columns %in% colnames(data)]
  
  # Create new data to indicate missing values (1 if missing, 0 otherwise)
  return(
    data |>
      dplyr::mutate(
        dplyr::across(
          .cols  = dplyr::all_of(columns),
          .fns   = function(x) if_else(is.na(x), 1, 0),
          .names = "{prefix}{col}")) |>
      # Add a new column for the sum of missing indicators across the specified columns
      dplyr::rowwise() |>
      dplyr::mutate(missing_sum = sum(dplyr::c_across(dplyr::starts_with(prefix)))) |>
      dplyr::ungroup()
  )
}

#### missing_indicator_sum_dt ####

#' Identify Missing Values and Calculate Missingness Sum (data.table)
#'
#' This function checks specified columns in a `data.table` for missing values (NA) and generates new indicator columns where missing values are represented by 1 and non-missing values by 0. Additionally, it calculates the sum of missing values across the specified columns for each row. The user can specify a custom prefix for the indicator column names.
#'
#' @param data A `data.table` or `data.frame` to check for missing values. If a `data.frame` is provided, it will be converted to a `data.table` internally.
#' @param columns A character vector specifying the columns to check for missing values.
#' @param prefix A character string specifying the prefix for the indicator columns. Default is "miss_".
#' @param keep_miss_columns_flag A logical value indicating if individual column flag for missing values should be kept. Default is FALSE
#'
#' @return The original `data.table` with additional columns indicating missing values (`<prefix><colname>`) for each specified column and a column `missing_sum` which provides the sum of missing values for each row.
#'
#' @examples
#' df <- data.frame(
#'   col1 = c(1, 2, NA, 4),
#'   col2 = c("A", NA, "C", "D"),
#'   col3 = c(NA, 2, 3, 4)
#' )
#' columns_to_check <- c("col1", "col2", "col3")
#' missing_indicator_sum_dt(df, columns_to_check)
#'
#' @import data.table
#' @export


missing_indicator_sum_dt <- function(data, columns, prefix="miss_", keep_miss_columns_flag = FALSE) {
  # Convert data to data.table if it's not already
  data.table::setDT(data)
  
  # Check if columns exist in data
  columns <- columns[columns %in% colnames(data)]
  
  # Ensure no duplicate (would cause data.table error)
  columns <- unique(columns)
  
  # Create missing value indicator columns
  for (col in columns) {
    data[, paste0(prefix, col) := fifelse(is.na(get(col)), 1, 0)]
  }
  
  # Calculate the sum of missing indicators across the specified columns
  miss_columns <- paste0(prefix, columns)
  data[, missing_sum := rowSums(.SD), .SDcols = miss_columns]
  
  # Optionally remove "prefix" columns
  if (!keep_miss_columns_flag) {
    data[, (miss_columns) := NULL]
  }
  
  return(data)
}


#### tab_n_time ####

#' Pairwise Cross-Tabulations Across Any Number of Time Points
#'
#' @description
#' Generates all pairwise cross-tabulations for a variable measured across an
#' arbitrary number of time points. Assumes the wide dataset contains
#' columns named `variable_t<time>`, e.g. `cc_cancer_t0`, `cc_cancer_t1`, etc.
#'
#' @param data A wide-format dataset containing the variable across time.
#' @param variable A character string indicating the variable prefix.
#' @param useNA Passed to [table()], typically `"always"`.
#'
#' @return A named list of contingency tables.  
#' @export
tab_n_time <- function(data, variable, useNA = "always") {
  
  # Identify all variable_t* columns
  pattern <- paste0("^", variable, "_t")
  vars <- grep(pattern, names(data), value = TRUE)
  
  if (length(vars) < 2) {
    stop("Need at least two time points to compute pairwise cross-tabulations.")
  }
  
  # Get the embedded time point values (after _t)
  times <- sub(paste0(variable, "_t"), "", vars)
  
  # All pairwise combinations
  combs <- combn(seq_along(vars), 2, simplify = FALSE)
  
  out <- lapply(combs, function(idx) {
    table(
      data[[vars[idx[1]]]],
      data[[vars[idx[2]]]],
      useNA = useNA
    )
  })
  
  # Name the tables meaningfully
  names(out) <- sapply(combs, function(idx) {
    paste0(
      variable, "_t", times[idx[1]],
      "_vs_",
      variable, "_t", times[idx[2]]
    )
  })
  
  return(out)
}
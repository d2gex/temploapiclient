#' @title FieldValidatio class
#'
#' @description
#' Provide validator for certain relevant fields of data that needs to be send to the API
FieldValidation <- R6::R6Class("FieldValidation", public = list( # nolint

  # @formatter:off
  #' @description
  #' Validate sex column of a dataframe  as positive integer
  #'
  #' @param df dataframe where the values of the field are
  #' @param field field name
  #' @export
  # @formatter:on
  valid_sex = function(df, field) {
    values <- df[[field]]
    # All values must be integers
    if (any(is.na(values))) {
      return(FALSE)
    }
    if (sum(values - floor(values)) != 0) {
      return(FALSE)
    }
    if (any(values < 0)) {
      return(FALSE)
    }
    return(TRUE)
  },
  # @formatter:off
  #' @description
  #' Validate all values of a column are indeed boolean and not equivalents
  #'
  #' @param df dataframe where the values of the field are
  #' @param field field name
  #' @export
  # @formatter:on
  valid_bool_vector = function(df, field) {
    values <- df[[field]]
    if (!all(is.logical(values))) {
      return(FALSE)
    }
    return(TRUE)
  },
  # @formatter:off
  #' @description
  #' Validate all values of a column are either datetime in ymd_hms format or a date in ymd format.
  #'
  #' @param df dataframe where the values of the field are
  #' @param field field name
  #' @param is_datetime boolean flag indicating whether to check datetime or date
  #' @export
  # @formatter:on
  validate_datetime = function(df, field, is_datetime = TRUE) {
    values <- df[[field]]
    date_func <- ifelse(is_datetime, lubridate::ymd_hms, lubridate::ymd)
    # Any wrong date?
    if (sum(!is.na(date_func(values, quiet = TRUE))) != length(values)) {
      return(FALSE)
    }
    return(TRUE)
  },
  # @formatter:off
  #' @description
  #' Check whether there are NA values in some columns of a given dataframe
  #'
  #' @param df dataframe
  #' @param col_names character vector with the columnnames
  #' @export
  # @formatter:on
  check_na_fields = function(df, col_names) {
    messages <- list()
    for (c_name in col_names) {
      if (sum(is.na(df[[c_name]])) >= 1) {
        messages[[c_name]] <- paste0("Column '", c_name, "' cannot hold NA values")
      }
    }
    return(messages)
  }
))

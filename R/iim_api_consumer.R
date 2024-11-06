#' @title IIMApiClient class
#'
#' @description
#' Provides the end-to-end queries required to update all details about the live acoustic telemetry system in place
IIMApiClient <- R6::R6Class("IIMApiClient", public = list( # nolint
  api_url = NULL,
  initialize = function(api_url) {
    self$api_url <- api_url
  },
  # @formatter:off
  #' @description
  #' Add a sensor type to the database
  #'
  #' @param end_point string with the name of the end point being queried. Default "sensortypes/"
  #' @param df single-column dataframe with as many variables as required, i.e., DEPTH, TEMPERATURE.
  #' @export
  # @formatter:on
  add_sensor_type = function(df, end_point = "sensortypes/") {
    num_rows <- nrow(df)
    if (num_rows > 1) {
      logger::log_error(paste("Sensor types can only be inserted one at the time. Please provide a
      single row dataframe. Instead", num_rows, "rows were provided"))
      return(FALSE)
    }

    http_client <- HttpClientJsonBase$new(self$api_url, end_point)
    ret <- http_client$post_dataframe(df)
    if (!ret[[1]]$success) {
      private$print_error_context(ret[[1]]$errors)
      return(FALSE)
    }
    logger::log_info("The sensor type was successfully inserted")
    return(TRUE)
  },
  # @formatter:off
  #' @description
  #' Post a nested list to the database through an API.
  #'
  #' @param end_point string with the name of the end point being queried. Default "tags_with_sensors/"
  #' @param obj a nested list resulting from applying \link{NestedListBuilder}
  #' @export
  # @formatter:on
  post_data_to_api = function(obj, end_point) {
    http_client <- HttpClientJsonBase$new(self$api_url, end_point)
    ret <- http_client$post_object(obj)
    if (!ret$success) {
      private$print_error_context(ret$errors)
      return(FALSE)
    }
    return(ret)
  },
  # @formatter:off
  #' @description
  #' Make a request to the API and return the data in a json object.
  #'
  #' @param end_point string with the name of the end point being queried. Default "tags_with_sensors/"
  #' @param obj a nested list resulting from applying \link{NestedListBuilder}
  #' @export
  # @formatter:on
  get_from_api = function(end_point) {
    http_client <- HttpClientJsonBase$new(self$api_url, end_point)
    ret <- http_client$get()
    if (!ret$success) {
      private$print_error_context(ret$errors)
      return(FALSE)
    }
    return(ret)
  },
  get_from_api_as_dataframe = function(end_point, parameters = NULL) {
    http_client <- HttpClientJsonBase$new(self$api_url, end_point)
    ret <- http_client$get_dataframe(parameters)
    if (!ret$success) {
      private$print_error_context(ret$errors)
      return(FALSE)
    }
    return(ret)
  }
), private = list(
  print_error_context = function(error_context) {
    messages <- tolower(unname(unlist(error_context, recursive = TRUE)))
    context_message <- NULL
    if (TRUE %in% stringr::str_detect(messages, "duplicate")) {
      context_message <- "Detected a 'duplicate' keyword. You may have inserted a record that already exist
      in the database. See error below."
    } else if (TRUE %in% stringr::str_detect(messages, "field required")) {
      context_message <- "Detected a 'field required' keyword. You may be missing an essential field in your query. Look
      'loc' section for clues"
    }

    if (!is.null(context_message)) {
      logger::log_error(stringr::str_replace_all(context_message, "\n", ""))
    }
    cat(jsonlite::toJSON(error_context, pretty = TRUE))
  }
))

#' @title IIMApiClient class
#'
#' @description
#' Provides the end-to-end queries required to update all details about the live acoustic telemetry system in place
IIMApiClient <- R6::R6Class("IIMApiClient", public = list( # nolint
  # @formatter:off
  #' @field base_url string containing the base url of the querying API (without the end_point) part
  api_url = NULL,
  #' @field headers list of HTTP headers
  headers = NULL,
  #' @field token string to authenticate against the API
  token = NULL,
  #' @description
  #' Initialise IIMApiClient
  #'
  #' @param base_url string containing the base url of the querying API (without the end_point) part
  #' @param headers list of HTTP headers
  #' @param token string to authenticate against the API
  #' @export
  initialize = function(api_url, headers, token) {
    self$api_url <- api_url
    self$headers <- headers
    self$token <- token
  },

  #' @description
  #' Post a nested list to the database through an API.
  #'
  #' @param end_point string with the name of the end point being queried. Default "tags_with_sensors/"
  #' @param obj a nested list resulting from applying \link{NestedListBuilder}
  #' @export
  # @formatter:on
  post_data_to_api = function(obj, end_point) {
    http_client <- httpeasyrest::HttpRestClient$new(self$api_url, self$headers, self$token)
    ret <- http_client$post_object(end_point, obj)
    if (!ret$success) {
      private$print_error_context(ret$errors)
      return(FALSE)
    }
    return(ret)
  },
  # @formatter:off
  #' @description
  #' Delete a single record from the database through the API
  #'
  #' @param end_point string with the name of the end point being queried. Default "tags_with_sensors/"
  #' @param obj a flat list with the ids or any unique field that identifies the record to be erased
  #' @export
  # @formatter:on
  delete_data_from_api = function(obj, end_point) {
    http_client <- httpeasyrest::HttpRestClient$new(self$api_url, self$headers, self$token)
    ret <- http_client$delete_object(end_point, obj)
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
  get_from_api = function(end_point, parameters = NULL) {
    http_client <- httpeasyrest::HttpRestClient$new(self$api_url, self$headers, self$token)
    ret <- http_client$get_object(end_point, parameters)
    if (!ret$success) {
      private$print_error_context(ret$errors)
      return(FALSE)
    }
    return(ret)
  },
  get_from_api_as_dataframe = function(end_point, parameters = NULL) {
    http_client <- httpeasyrest::HttpRestClient$new(self$api_url, self$headers, self$token)
    ret <- http_client$get_dataframe(end_point, parameters)
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

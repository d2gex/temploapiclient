#' @title TemploApi class
#'
#' @description
#' API client to consume TEMPLO's API
TemploApiClient <- R6::R6Class("TemploApiClient", public = list( # nolint
  api_url = NULL,
  initialize = function(api_url) {
    self$api_url <- api_url
  },
  # @formatter:off
  #' Add a tag with a sensor details to the database.
  #'
  #' @param end_point string with the name of the end point being queried. Default "tags_with_sensors/"
  #' @param obj a nested list resulting from applying \link{NestedListBuilder}
  #' @returns TRUE or FALSE
  #' @export
  # @formatter:on
  add_tag_with_sensor = function(df,
                                 tag_prefix,
                                 sensor_prefix,
                                 field_names,
                                 end_point = "tags_with_sensors/") {
    # Check that certain fields aren't NA
    not_na_fields <- names(df)
    field_validator <- FieldValidation$new()
    na_messages <- field_validator$check_na_fields(df, not_na_fields)
    if (length(na_messages)) {
      logger::log_error(paste("There are fields in the dataframe containing NA.", na_messages, sep = "\n"))
      return(FALSE)
    }
    # Do the api call and deal with errors.
    nested_list_builder <- NestedListBuilder$new()
    obj <- nested_list_builder$build_tag_with_sensor_list(df, tag_prefix, sensor_prefix, field_names)

    test_tag_with_sensor <- IIMApiClient$new(self$api_url)
    ret <- test_tag_with_sensor$post_data_to_api(obj, end_point)
    if (isTRUE(ret)) {
      logger::log_info("All tagged individuals were successfully inserted")
    }
    return(ret)
  },
  add_species = function(df, end_point = "species/") {
    # Check NA fields
    not_na_fields <- c("scientific_names", "common_name")
    field_validator <- FieldValidation$new()
    na_messages <- field_validator$check_na_fields(df, not_na_fields)
    if (length(na_messages)) {
      logger::log_error(paste("There are fields in the dataframe containing NA.", na_messages, sep = "\n"))
      return(FALSE)
    }

    # Send dataframe to API
    successful_insertions <- list()
    nested_list_builder <- NestedListBuilder$new()
    iim_api <- IIMApiClient$new(self$api_url)
    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      scientific_name <- row$scientific_name
      obj <- nested_list_builder$build_row_data(row)
      ret <- iim_api$post_data_to_api(obj, end_point)
      if (isFALSE(ret)) { # First error found abort the operation
        logger::log_error(paste("The species", scientific_name, "failed to insert"))
        break
      } else {
        successful_insertions[[length(successful_insertions) + 1]] <- scientific_name
      }
    }

    if (length(successful_insertions) == 0) {
      logger::log_error("None of the species were inserted")
      return(FALSE)
    }
    # Report what potential records failed to be inserted
    if (length(successful_insertions) != nrow(df) & length(successful_insertions) > 0) {
      logger::log_warn(paste("The following species were inserted only", paste0(successful_insertions, collapse = ",")))
      return(FALSE)
    }
    # Otherwise All is good
    logger::log_info("All species were successfully inserted")
    return(TRUE)
  },
  add_tagged_individuals = function(df,
                                    individual_prefix,
                                    tag_prefix,
                                    sensor_prefix,
                                    tagged_individual_prefix,
                                    field_names,
                                    end_point = "tagged_individuals/") {
    # Check NA fields
    not_na_fields <- c(
      paste0(individual_prefix, "scientific_name"),
      paste0(individual_prefix, "common_name"),
      paste0(individual_prefix, "individual_id"),
      paste0(tag_prefix, "serial_number"),
      paste0(sensor_prefix, "id")
    )
    field_validator <- FieldValidation$new()
    na_messages <- field_validator$check_na_fields(df, not_na_fields)
    if (length(na_messages)) {
      logger::log_error(paste("There are fields in the dataframe containing NA.", na_messages, sep = "\n"))
      return(FALSE)
    }
    # Check Sex is allright
    if (!field_validator$valid_sex(df, paste0(individual_prefix, "sex"))) {
      logger::log_error("'sex' can only contain integer values")
      return(FALSE)
    }
    # Check tagged_date
    if (!field_validator$validate_datetime(df, paste0(individual_prefix, "tagged_date"))) {
      logger::log_error("'tagged_date' can only only be in '%Y-%m-%d %H:%M:%S' format")
      return(FALSE)
    }

    nested_list_builder <- NestedListBuilder$new()
    iim_api <- IIMApiClient$new(self$api_url)
    successful_insertions <- list()

    # Convert dataframe to list
    individual_details <- nested_list_builder$build_tagged_individual_list(
      df,
      individual_prefix,
      tag_prefix,
      sensor_prefix,
      tagged_individual_prefix,
      field_names
    )

    logger::log_info("Fetching data from the api... This may take a few seconds")
    # Send dataframe to API
    for (ind_details in individual_details) {
      ind_id <- ind_details$individual_data$individual_id
      ret <- iim_api$post_data_to_api(ind_details, end_point)
      if (isFALSE(ret)) { # First error found abort the operation
        logger::log_error(paste("The individual", ind_id, "failed to insert"))
      } else {
        successful_insertions[[length(successful_insertions) + 1]] <- ind_id
      }
    }
    # Report what potential records failed to be inserted
    if (length(successful_insertions) == 0) {
      logger::log_error("None of the individuals were inserted")
      return(FALSE)
    }

    if (length(successful_insertions) != length(individual_details)) {
      logger::log_warn(paste(
        "The following individuals were inserted only",
        paste0(successful_insertions, collapse = ",")
      ))
      return(FALSE)
    }

    # Otherwise All is good
    logger::log_info("All individuals were successfully inserted")
    logger::log_info("--->Finished")
    return(TRUE)
  },
  add_single_record_and_fetch_id = function(end_point, data) {
    logger::log_info(paste("Sending data to ", end_point, "..."))
    iim_api <- IIMApiClient$new(self$api_url)
    ret <- iim_api$post_data_to_api(data, end_point)
    if (isFALSE(ret)) {
      logger::log_error(paste("Data could not be send"))
      return(FALSE)
    }
    logger::log_info("--->Finished")
    return(ret$http_resp)
  },
  get_sensor_types = function(end_point = "sensortypes/") {
    logger::log_info("Fetching all sensor types from the api... This may take a few seconds")
    iim_api <- IIMApiClient$new(self$api_url)
    ret <- iim_api$get_from_api_as_dataframe(end_point)
    if (!isFALSE(ret)) {
      ret <- ret$http_resp %>%
        dplyr::rename(c(sensor_types_id = "id", sensor_type = "type_name"))
    }
    logger::log_info("--->Finished")
    return(ret)
  },
  get_tagged_individuals = function(end_point = "tagged_individuals/") {
    logger::log_info("Fetching all tagged individuals from the api... This may take a few seconds")
    iim_api <- IIMApiClient$new(self$api_url)
    ret <- iim_api$get_from_api(end_point)
    if (!isFALSE(ret)) {
      logger::log_info("---> Converting results to dataframe...")
      result_formatter <- ListsToDataFrame$new()
      ret <- result_formatter$tagged_individuals(ret$http_resp, tagged_individual_fields)
      sensor_types <- self$get_sensor_types()
      ret <- dplyr::left_join(ret, sensor_types, by = "sensor_types_id") %>%
        dplyr::select(-sensor_types_id)
    }

    logger::log_info("--->Finished")
    return(ret)
  },
  get_environmental_readings = function(api_url,
                                        end_point = "environmental_readings/data_exports/",
                                        from_date,
                                        to_date) {
    df <- data.frame(
      from_date = from_date,
      to_date = to_date
    )
    field_validator <- FieldValidation$new()
    if (!field_validator$validate_datetime(df, "from_date", is_datetime = FALSE)) {
      logger::log_error("'from_date' can only only be in '%Y-%m-%d' format")
      return(FALSE)
    }
    if (!field_validator$validate_datetime(df, "to_date", is_datetime = FALSE)) {
      logger::log_error("'to_date' can only only be in '%Y-%m-%d' format")
      return(FALSE)
    }

    logger::log_info(paste(
      "Fetching environmental readings from ",
      from_date, "to", to_date, "... This may take a few seconds"
    ))
    iim_api <- IIMApiClient$new(self$api_url)
    ret <- iim_api$get_from_api_as_dataframe(end_point, list(
      from_date = from_date,
      to_date = to_date
    ))
    if (!isFALSE(ret)) {
      ret <- ret$http_resp
    }
    logger::log_info("--->Finished")
    return(ret)
  },

  # @formatter:off
  #' General method to get data from the API in a dataframe format.
  #'
  #' @param end_point string with the name of the end point being queried.
  #' @param parameters a named list with potential parameters to pass on to the API query. Default is NULL
  #' @returns a dataframe with the sought information.
  #' @export
  # @formatter:on
  get_dataframe_end_point = function(end_point, parameters = NULL) {
    logger::log_info(paste("Fetching data from api... This may take a few seconds"))
    iim_api <- IIMApiClient$new(self$api_url)
    ret <- iim_api$get_from_api_as_dataframe(end_point, parameters)
    if (!isFALSE(ret)) {
      ret <- ret$http_resp
    }
    logger::log_info("--->Finished")
    return(ret)
  }
))

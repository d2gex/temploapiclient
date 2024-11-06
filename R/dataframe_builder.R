ListsToDataFrame <- R6::R6Class("ListsToDataFrame", public = list( # nolint

  # @formatter:off
  #' @description
  #' Build a dataframe of results of the nested lists returned from the API.
  #'
  #' @param data nested list where each root level element represents an individual
  #' @returns a dataframe where each individual may be mapped across multiple rows due to its tag having
  #' multiple sensors
  #' @export
  # @formatter:on
  tagged_individuals = function(data, field_structure) {
    return(
      lapply(seq_along(data), function(x) {
        private$build_single_tagged_individual(data[[x]], field_structure)
      }) %>%
        purrr::reduce(dplyr::bind_rows) %>%
        dplyr::rename(sensor_id = id)
    )
  }
), private = list(
  build_single_row_df = function(data, fields) {
    sel_list <- data[fields]
    df <- data.frame(sel_list)
    return(df)
  },
  build_multiple_row_df = function(data, fields) {
    return(
      lapply(data, function(l) {
        private$build_single_row_df(l, fields)
      }) %>% purrr::reduce(dplyr::bind_rows)
    )
  },
  build_single_tagged_individual = function(data, field_structure) {
    # (1) Build data sensor dataframe
    if (!length(data[["sensors"]])) { # Backward compatibility reasons, build df full of NA
      df_sensors <- as.list(rep(NA, length(field_structure[["sensors"]])))
      names(df_sensors) <- field_structure[["sensors"]]
      df_sensors <- data.frame(df_sensors)
    } else {
      data_ <- private$null_to_na(data[["sensors"]])
      df_sensors <- private$build_multiple_row_df(data_, field_structure[["sensors"]])
    }
    df_sensors$fake_id <- 1


    # (2) Build rest of the dataframe
    sections <- field_structure[c("tagged_individual", "individual", "tag")]
    df_rest <- lapply(names(sections), function(x) {
      data_ <- private$null_to_na(data[[x]])
      fields <- field_structure[[x]]
      df <- private$build_single_row_df(data_, fields)
      df$fake_id <- 1
      return(df)
    }) %>% purrr::reduce(dplyr::full_join, by = "fake_id")
    df <- dplyr::full_join(df_sensors, df_rest, by = "fake_id") %>% dplyr::select(-fake_id)
  },
  null_to_na = function(data) {
    for (field in names(data)) {
      if (is.null(data[[field]])) {
        data[[field]] <- NA
      }
    }
    return(data)
  }
))

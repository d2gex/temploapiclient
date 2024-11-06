#' @title NestedListBuilder class
#'
#' @description
#' Convert dataframes into nested lists according to each required case
NestedListBuilder <- R6::R6Class("NestedListBuilder", public = list( # nolint
  # @formatter:off
  #' @description
  #' Given a dataframe with the tag and sensor details, it returns 4-level nested list containing all details
  #' about each tag along with their sensor details.
  #'
  #' @param df dataframe with all tag and sensor details
  #' @param tag_prefix string with the expected tag field prefix
  #' @param sensor_prefix string with the expected sensor field prefix
  #' @param root_field_names string vector with the names of the outer element of the built list
  #' @returns a nested list where each element at the root level contains all details for each tag and its sensor
  #' details.
  #' @export
  # @formatter:on
  build_tag_with_sensor_list = function(df, tag_prefix, sensor_prefix, root_field_names) {
    # Split the dataframe in a list of dataframes by unique serial number
    df_list <- df %>%
      dplyr::group_by_at(.vars = paste0(tag_prefix, "serial_number")) %>%
      dplyr::group_split()

    return(
      lapply(df_list, function(x) {
        private$tag_sensor_df_to_list(x, tag_prefix, sensor_prefix, root_field_names)
      })
    )
  },
  # @formatter:off
  #' @description
  #' Given a dataframe with the individual, tag and sensor and tagged_individual details it returns 3-level nested list
  #' containing all such details
  #'
  #' @param df dataframe with all tag and sensor details
  #' @param tag_prefix string with the expected tag field prefix
  #' @param sensor_prefix string with the expected sensor field prefix
  #' @param ind_prefix string with the expected individual field prefix
  #' @param tagged_ind_prefix string with the expected tagged_individual field prefix
  #' @param root_field_names string vector with the names of the outer element of the built list in the strict order
  #' of (individual, tag, sensor)
  #' @returns a nested list where each element at the root level contains all details for each tagged individual.
  #' @export
  # @formatter:on
  build_tagged_individual_list = function(df,
                                          ind_prefix,
                                          tag_prefix,
                                          sensor_prefix,
                                          tagged_ind_prefix,
                                          root_field_names) {
    # (1)Fetch the field names for each type of column: individual, tag, senso or tagged_individuals
    ind_field_names <- root_field_names[1]
    tag_sensor_field_names <- root_field_names[2:(length(root_field_names) - 1)]
    tagged_individual_field_names <- root_field_names[length(root_field_names)]

    # (2) Split the dataframe in a list of dataframes by unique individual
    df_list <- df %>%
      dplyr::group_by_at(.vars = paste0(ind_prefix, "individual_id")) %>%
      dplyr::group_split()

    # (3) Build a list of lists, where each sublist contains all details about one single individual
    list_of_individuals <- lapply(df_list, function(x) {
      tag_sensor_details <- private$tag_sensor_df_to_list(x, tag_prefix, sensor_prefix, tag_sensor_field_names)
      individual_details <- private$individual_df_to_list(x, ind_prefix, ind_field_names)
      tagged_individual_details <- private$tagged_individual_df_to_list(
        x, tagged_ind_prefix,
        tagged_individual_field_names
      )
      c(individual_details, tag_sensor_details, tagged_individual_details)
    })
    return(list_of_individuals)
  }
), private = list(
  tag_sensor_df_to_list = function(df, tag_prefix, sensor_prefix, field_names) {
    df_tag <- private$fetch_field_with_prefix(df, tag_prefix) %>%
      private$extract_prefix_from_col(tag_prefix)

    df_sensor <- private$fetch_field_with_prefix(df, sensor_prefix) %>%
      private$extract_prefix_from_col(sensor_prefix)

    api_data <- list(
      tag_data = private$build_tag_data(df_tag),
      sensors_data = private$build_sensor_data(df_sensor)
    )
    names(api_data) <- field_names
    return(api_data)
  },
  tagged_individual_df_to_list = function(df, tag_ind_prefix, field_names) {
    return(private$df_to_list(df, tag_ind_prefix, field_names))
  },
  individual_df_to_list = function(df, ind_prefix, field_names) {
    c_names <- unique(df[[paste0(ind_prefix, "common_name")]])
    s_names <- unique(df[[paste0(ind_prefix, "scientific_name")]])
    if (length(c_names) != length(s_names)) {
      stop(paste(
        "There seems to be an individual with either multiple scientific or common names:",
        paste0(c_names, collapse = ","), paste0(s_names, collapse = ",")
      ))
    }
    return(private$df_to_list(df, ind_prefix, field_names))
  },
  # @formatter:off
  #' @description
  #'
  #' Build and return a single list with the individual or tagged_individual details. A multile row dataframe may be
  #' provided however we are only interested in one set of data as it is supposed to be the same across all rows.
  # @formatter:on
  df_to_list = function(df, prefix, field_names) {
    df <- private$fetch_field_with_prefix(df[1, ], prefix) %>%
      private$extract_prefix_from_col(prefix)
    api_data <- list()
    api_data[[field_names]] <- private$build_row_data(df)
    return(api_data)
  },
  # @formatter:off
  #' @description
  #'
  #' Build and return a single list with columns and their associated values from a single-row dataframe
  # @formatter:on
  build_row_data = function(df) {
    col_names <- names(df)
    data <- lapply(col_names, function(c_name) {
      value <- unique(df[[c_name]])
      if (is.na(value)) { # Weird bug with httr2 conversion
        value <- NA
      }
      return(value)
    })
    names(data) <- col_names
    return(data)
  },
  # @formatter:off
  #' @description
  #'
  #' Build and return a single list with the tag details. Expect a single-row dataframe
  # @formatter:on
  build_tag_data = function(df) {
    # Get tag details
    tokens <- stringr::str_split_1(unique(df$frequency), " ")
    return(list(
      serial_number = unique(df$serial_number[!is.na(df$serial_number)]),
      frequency = as.numeric(tokens[1]),
      comm_protocol = unique(df$comm_protocol)
    ))
  },
  # @formatter:off
  #' @description
  #'
  #' Build and return a nested list where each item in the outer list is another list holding each sensor details.
  #' Expect multiple rows
  # @formatter:on
  build_sensor_data = function(df) {
    # Get sensor details
    ids <- unique(df$id)
    sensor_types_name <- unique(df$sensor)
    if (length(ids) != length(sensor_types_name)) {
      stop(paste(
        "There seems to be a tag which sensor either ids or types are not unique:",
        paste0(ids, collapse = ","), paste0(sensor_types_name, collapse = ",")
      ))
    }
    sensor_details <- lapply(seq_along(ids), function(offset) {
      list(
        id = ids[offset],
        sensor_types_name = toupper(sensor_types_name[offset])
      )
    })
    return(sensor_details)
  },
  extract_prefix_from_col = function(df, prefix) {
    col_names <- names(df)
    col_names <- stringr::str_replace_all(col_names, prefix, "")
    names(df) <- col_names
    return(df)
  },
  fetch_field_with_prefix = function(df, prefix) {
    return(
      df %>% dplyr::select(tidyselect::starts_with(prefix))
    )
  }
))

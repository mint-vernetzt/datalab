#' A function to create filter rows
#'
#' @description A function to create a UI row for filtering the table output for
#'   international data.
#'
#' @details The maximum number of possible filters is given by the number of
#' rows of the input data. In the international table code, the "next" row will
#' use the smallest free row index
#'
#'
#' @return The return value is a fluidRow UI-object
#' @param i_ibnput A numeric or numeric string, that indicates which row index
#'   is used.
#' @param ns a session$ns object
#' @noRd

create_filter_row <- function(i_input, ns) {
  fluidRow(
    id = paste0("map_int_table_input_row_", i_input),
    # column(
    #   width = 1,
    #   p(i_input)
    # ),
    column(
      width = 3,
      shinyWidgets::pickerInput(
        inputId = ns(paste0("map_int_table_indikator_", i_input)),
        label = NULL,
        choices = international_zentral_get_unique_values(
          var = "bereich",
          filter = list(region = "Europa")),
        selected = NULL,
        options = list(title = "Indikator wählen"),
        multiple = FALSE
      )
    ),
    column(
      width = 3,
      shinyWidgets::pickerInput(
        inputId = ns(paste0("map_int_table_gruppe_", i_input)),
        label = NULL,
        # choices = international_zentral_get_unique_values(var = "gruppe"),
        choices = c(),
        selected = NULL,
        options = list(title = "Gruppe wählen"),
        multiple = FALSE
      )
    ),
    column(
      width = 3,
      shinyWidgets::pickerInput(
        inputId = ns(paste0("map_int_table_fachbereich_", i_input)),
        label = NULL,
        choices = international_zentral_get_unique_values(var = "fach"),
        selected = NULL,
        options = list(title = "Fachbereich wählen"),
        multiple = FALSE
      )
    ),
    column(
      width = 2,
      shinyWidgets::pickerInput(
        inputId = ns(paste0("map_int_table_year_", i_input)),
        label = NULL,
        choices = international_zentral_get_unique_values(var = "jahr"),
        selected = NULL,
        options = list(title = "Jahr wählen"),
        multiple = FALSE
      )
    ),
    column(
      width = 1,
      actionButton(
        inputId = ns(paste0("int_table_removeBtn_", i_input)),
        label = "",
        icon = icon("trash"))
    )
  )
}


#' A function to create filters
#'
#' @description A function to create a filter from the user input.
#' All inputs are parsed as strings at the moment!
#'
#' @return A string that can be used as a filter
#' @param filter_list A named list with the filter info. The list elements contain
#'   the values and the names are the respective variables.
#' @noRd
create_filter_rule <- function(filter_list = list()) {
  #filter_list <- list(Jahr = 2013, Gruppe = "AA", Indikator = "BB")
  # filter_list <- list(Jahr = c(2013, 2015), Gruppe = "AA")
  vars <- names(filter_list)

  if (length(vars) == 0 | !is.list(filter_list)) {
    logger::log_warn("create_filter_rule: Filter list must be a named list")
    return(NULL)
  }

  if (any(unlist(filter_list) == "")) {
    out <- NULL
  } else {
    filter_length <- sapply(filter_list, length)
    this_operator <- dplyr::if_else(filter_length == 1, "==", "%in%")

    out <- paste0(
      vars, this_operator, "c('",
      lapply(filter_list, paste0, collapse = "','"), "')",
      collapse = " & "
    )
  }

  return(out)
}

# TEST
# test_df <- data.frame(Jahr = c(2011, 2012, 2013),
#                       Gruppe = c("AA", "BB", "AA"),
#                       Indikator = c("AA", "BB", "BB"))
# test_df %>% dplyr::filter(eval(parse(text = create_filter_rule(
#  list(Jahr = c(2013, 2015), Gruppe = "AA", Indikator = "BB")))))


#' A function to get unique values from the international lookup data
#'
#' @return A string vector with the uniqe values of `var`
#' @param var The name of the column
#' @noRd
international_zentral_get_unique_values <- function(var, filter = NULL) {
  # var <- "jahr"
  if (!var %in% names(international_zentral)) {
    logger::log_warn("Column '", var, "' not in the data!")
    return(NULL)
  }

  if (is.null(filter)) {
    tmp_df <- international_zentral
  } else {
    tmp_df <- international_zentral %>%
      dplyr::filter(eval(parse(text = create_filter_rule(filter))))
  }

  selection <- tmp_df %>%
    dplyr::pull(!!var) %>%
    unique() %>%
    sort()

  return(selection)
}

#' A function to adjust the data to be downloaded
#'
#' @return A data.frame
#' @param data the data to be downloaded, this should be the custom data in `r$int_table`
#' @noRd
prep_download_data <- function(data) {
  # browser()
  out <- data
  return(out)
}

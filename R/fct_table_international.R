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
#' @param region A string with the used region (eg "Europa" or "OECD Länder") to
#'   set the correct initial values for the filters
#' @param land A vector with the used countries (eg c("Deutschland") ) to set
#'   the correct initial values for the filters
#' @noRd

create_filter_row <- function(
    i_input,
    ns,
    region = "Europa",
    land = "Deutschland") {

  fluidRow(
    id = paste0("map_int_table_input_row_", i_input),
    # column(
    #   width = 1,
    #   p(i_input)
    # ),
    column(
      width = 2,
      shinyWidgets::pickerInput(
        inputId = ns(paste0("map_int_table_indikator_", i_input)),
        label = NULL,
        choices = international_zentral_get_unique_values(
          var = "bereich",
          filter = list(region = region)),
        selected = NULL,
        options = list(title = "Indikator wählen"),
        multiple = FALSE
      )
    ),
    column(
      width = 4,
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
      width = 4,
      shinyWidgets::pickerInput(
        inputId = ns(paste0("map_int_table_fachbereich_", i_input)),
        label = NULL,
        choices = c(),
        selected = NULL,
        options = list(title = "Fachbereich wählen"),
        multiple = FALSE
      )
    ),
    column(
      width = 1,
      shinyWidgets::pickerInput(
        inputId = ns(paste0("map_int_table_year_", i_input)),
        label = NULL,
        choices = international_zentral_get_unique_values(
          var = "jahr",
          filter = list(land = land, region = region)),
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
    #logger::log_warn("create_filter_rule: Filter list must be a named list")
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


#' A function to get unique values from the international lookup data
#'
#' @return A string vector with the uniqe values of `var`
#' @param var The name of the column
#' @noRd
international_zentral_get_unique_values <- function(var, filter = NULL) {
  if (!var %in% names(international_zentral)) {
    #logger::log_warn("Column '", var, "' not in the data!")
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

download_table <- function(table,
                           filename = "table.png",
                           width = 450,
                           height = 300) {

  # Save the table as a standalone HTML file
  html_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(table, file = html_file, selfcontained = TRUE)

  # Capture the HTML as a PNG image
  webshot2::webshot(url = html_file,
                    file = filename,
                    delay = 2,
                    zoom = 2,
                    vwidth = width,
                    vheight = height)

  shiny::showNotification(
    ui = paste0("Gespeichert als '", filename, "'"),
    type = "message",
    id = "download_notification")
}

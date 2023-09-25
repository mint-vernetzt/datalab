#' international UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidPage(
      fluidRow(
        mod_international_start_ui("mod_international_start_ui_1")
      )
    )
  )


}

#' international Server Functions
#'
#' @noRd
mod_international_server <- function(id,
                                     data_studierende_absolventen_weltweit,
                                     data_studierende_anzahl_oecd,
                                     data_studierende_europa,
                                     data_countries_names,
                                     r){

  logger::log_debug("Setup Seiten-Module: INTERNATIONAL")
  r <- reactiveValues()

  print("mod_international_server")
  # Box 1
  mod_international_start_server("mod_international_start_ui_1", r)
  mod_international_map_server("mod_international_map_ui_1", r)

  logger::log_debug("Seiten-Module INTERNATIONAL done")
}

## To be copied in the UI
# mod_international_ui("international_1")

## To be copied in the server
# mod_international_server("international_1")

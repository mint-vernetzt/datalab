#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        start_time <- Sys.time(),
        mod_home_start_ui("mod_home_start_ui_1"),
        end_time <- Sys.time(),
        message("Home wurde in ", round(end_time - start_time, 2), " Sekunden berechnet.")

      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, r){


  r <- reactiveValues()

  # Home gesamt
  mod_home_start_server("mod_home_start_ui_1",r)

  # Box 1
  mod_home_start_einstieg_server("mod_home_start_einstieg_ui_1", r)
  mod_home_start_multiple_server("mod_home_start_multiple_ui_1", r)

  # Box 2
  mod_home_start_einstieg_gender_server("mod_home_start_einstieg_gender_ui_1", r)
  mod_home_start_comparison_server("mod_home_start_comparison_ui_1", r)

}


## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")


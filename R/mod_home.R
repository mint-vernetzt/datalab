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
        mod_home_start_ui("mod_home_start_ui_1")
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, data_zentral, data_ausbildungsvertraege, r){


  r <- reactiveValues(indikator_start_einstieg_1 = NULL,
                      date_start_comparison = NULL,
                      date_start_comparison_mint = NULL,
                      indikator_start_comparison = NULL,
                      date_start_leaky = NULL,
                      date_start_multiple = NULL,
                      indikator_start_multiple_1 = NULL)

  mod_home_start_einstieg_server("mod_home_start_einstieg_ui_1", r)
  mod_home_start_multiple_server("mod_home_start_multiple_ui_1", r)
  mod_home_start_leaky_server("mod_home_start_leaky_ui_1", r)
  mod_home_start_comparison_server("mod_home_start_comparison_ui_1", r)
  mod_home_start_comparison_mint_server("mod_home_start_comparison_mint_ui_1", r)
  mod_home_start_server("mod_home_start_ui_1", data_zentral, data_ausbildungsvertraege, r)

}


## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")


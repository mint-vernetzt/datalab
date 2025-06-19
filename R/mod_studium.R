#' studium UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        mod_studium_studienzahl_ui("mod_studium_studienzahl_ui_1")
      )
    )
  )
}

#' studium Server Functions
#'
#' @noRd
mod_studium_server <- function(id,r){
  r <- reactiveValues()

  # Studienzahl
  mod_studium_studienzahl_server("mod_studium_studienzahl_ui_1",r)

  # Box 1
  mod_studium_studienzahl_anteil_server("mod_studium_studienzahl_anteil_ui_1",  r)
  mod_studium_studienzahl_einstieg_verlauf_server("mod_studium_studienzahl_einstieg_verlauf_ui_1", r)
  mod_studium_studienzahl_bundeslandvergleich_server("mod_studium_studienzahl_bundeslandvergleich_ui_1", r)

  # Box 2
  mod_studium_studienzahl_mint_fach_server("mod_studium_studienzahl_mint_fach_ui_1", r)
  mod_studium_studienzahl_mint_anteile_server("mod_studium_studienzahl_mint_anteile_ui_1",  r)
  mod_studium_studienzahl_bulas_faecher_server("mod_studium_studienzahl_bulas_faecher_ui_1", r)

  # Box 3
  mod_studium_studienzahl_einstieg_gender_server("mod_studium_studienzahl_einstieg_gender_ui_1", r)
  mod_studium_studienzahl_einstieg_verlauf_gender_server("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1", r)
  mod_studium_choice_gender_server("mod_studium_studienzahl_choice_gender_ui",r)
  mod_studium_top_faecher_server("mod_studium_top_faecher", r)
  mod_studium_studienzahl_mintfrauen_server("mod_studium_studienzahl_mintfrauen_ui_1",r)

  # Box 4
  mod_studium_studienzahl_ausl_server("mod_studium_studienzahl_ausl_ui",  r)
  mod_studium_studienzahl_ausl_zeit_server("mod_studium_studienzahl_ausl_zeit_ui", r)
  mod_studium_studienzahl_international_bundeslandvergleich_server("mod_studium_studienzahl_international_bundeslandvergleich_ui", r)

}

## To be copied in the UI
# mod_studium_ui("studium_1")

## To be copied in the server
# mod_studium_server("studium_1")

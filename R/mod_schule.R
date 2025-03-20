#' schule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        start_time <- Sys.time(),
        mod_schule_kurse_ui("mod_schule_kurse_ui_1"),
        end_time <- Sys.time(),
        message("Schule wurde in ", round(end_time - start_time, 2), " Sekunden berechnet.")
      )
    )
  )
}

#' schule Server Functions
#'
#' @noRd

mod_schule_server <- function(id, r){

  r <- reactiveValues()

  # Kurse
  mod_schule_kurse_server("mod_schule_kurse_ui_1", r)

  # Box 1
  mod_schule_kurse_einstieg_comparison_server("mod_schule_kurse_einstieg_comparison_ui_1", r)
  mod_schule_kurse_einstieg_verlauf_server("mod_schule_kurse_einstieg_verlauf_ui_1", r)
  mod_schule_kurse_mint_map_server("mod_schule_kurse_mint_map_ui_1", r)

  # Box 2
  mod_schule_kurse_multiple_mint_server("mod_schule_kurse_multiple_mint_ui_1", r)
  mod_schule_kurse_verlauf_bl_subjects_server("mod_schule_kurse_verlauf_bl_subjects_ui_1", r)
  mod_schule_kurse_map_server("mod_schule_kurse_map_ui_1", r)

  # Box 3
  mod_schule_kurse_comparison_gender_server("mod_schule_kurse_comparison_gender_ui_1", r)
  mod_schule_kurse_verlauf_gender_server("mod_schule_kurse_verlauf_gender_ui_1", r)
  mod_schule_kurse_multiple_server("mod_schule_kurse_multiple_ui_1", r)

  # Box 4
  mod_schule_kurse_iqb_standard_zeitverlauf_server("mod_schule_kurse_iqb_standard_zeitverlauf_ui_1", r)
  mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_server("mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_ui_1", r)
  mod_schule_kurse_iqb_fragen_server("mod_schule_kurse_iqb_fragen_ui_1", r)

}

## To be copied in the UI
# mod_schule_ui("schule_1")

## To be copied in the server
# mod_schule_server("schule_1")

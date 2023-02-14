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
mod_studium_server <- function(id, data_studierende, data_studierende2, data_studierende_faecher, r){
  r <- reactiveValues()

  # Studienzahl
  mod_studium_studienzahl_server("mod_studium_studienzahl_ui_1", data_studierende, data_studierende2, data_studierende_faecher, r)

  # Box 2
  mod_studium_studienzahl_einstieg_server("mod_studium_studienzahl_einstieg_ui_1", r)
  mod_studium_studienzahl_einstieg_verlauf_server("mod_studium_studienzahl_einstieg_verlauf_ui_1", r)
  mod_studium_studienzahl_einstieg_comparison_server("mod_studium_studienzahl_einstieg_comparison_ui_1", r)
  mod_studium_studienzahl_test_server("mod_studium_studienzahl_test_ui_1",  r)


  # Box 3
  mod_studium_studienzahl_einstieg_gender_server("mod_studium_studienzahl_einstieg_gender_ui_1", r)
  mod_studium_studienzahl_einstieg_verlauf_gender_server("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1", r)
  mod_studium_studienzahl_einstieg_comparison_gender_server("mod_studium_studienzahl_einstieg_comparison_gender_ui_1", r)

  # Box 4
  mod_studium_studienzahl_choice_1_server("mod_studium_studienzahl_choice_ui_1_1", r)
  mod_studium_studienzahl_verlauf_bl_subject_server("mod_studium_studienzahl_verlauf_bl_subject_ui_1", r)
  mod_studium_studienzahl_ranking_bl_subject_server("mod_studium_studienzahl_ranking_bl_subject_ui_1", r)

  # Box 5
  mod_studium_choice_gender_server("mod_studium_studienzahl_choice_gender_ui",r)
  mod_studium_studienzahl_verlauf_bl_subject_gender_server("mod_studium_studienzahl_verlauf_bl_subject_gender_ui_1",r)
  mod_studium_studienzahl_ranking_bl_subject_gender_server("mod_studium_studienzahl_ranking_bl_subject_gender_ui_1", r)

  # Box 6
  mod_studium_studienzahl_bl_map_server("mod_studium_studienzahl_bl_map", r)
  mod_studium_studienzahl_bl_verlauf_server("mod_studium_studienzahl_bl_verlauf", r)
  mod_studium_studienzahl_bl_vergleich_server("studium_studienzahl_bl_vergleich", r)

  # Box 7
  mod_studium_studienzahl_bl_map_gender_server("mod_studium_studienzahl_bl_map_gender", r)
  mod_studium_studienzahl_bl_verlauf_gender_server("mod_studium_studienzahl_bl_verlauf_gender", r)
  mod_studium_studienzahl_bl_vergleich_gender_server("mod_studium_studienzahl_bl_vergleich_gender_ui", r)

  # Box 8
  mod_studium_top_faecher_server("mod_studium_top_faecher", r)

}

## To be copied in the UI
# mod_studium_ui("studium_1")

## To be copied in the server
# mod_studium_server("studium_1")

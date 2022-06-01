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
mod_studium_server <- function(id, data_studierende, r){

  r <- reactiveValues(date_studierende_einstieg = NULL,
                      nurLehramt_studierende_einstieg = NULL,
                      hochschulform_studierende_einstieg_1 = NULL,
                      hochschulform_studierende_einstieg_2 = NULL,

                      date_studierende_einstieg_gender = NULL,
                      nurLehramt_studierende_einstieg_gender = NULL,
                      hochschulform_studierende_einstieg_1_gender = NULL,
                      hochschulform_studierende_einstieg_2_gender = NULL,

                      date_studienzahl_einstieg_verlauf = NULL,
                      nurLehramt_studierende_einstieg_verlauf = NULL,
                      hochschulform_studierende_einstieg_verlauf_1 = NULL,
                      hochschulform_studierende_einstieg_verlauf_2 = NULL,

                      date_studienzahl_einstieg_verlauf_gender = NULL,
                      nurLehramt_studierende_einstieg_verlauf_gender = NULL,
                      hochschulform_studierende_einstieg_verlauf_gender_1 = NULL,
                      hochschulform_studierende_einstieg_verlauf_gender_2 = NULL,

                      date_kurse_einstieg_comparison = NULL,
                      date_kurse_einstieg_comparison_gender = NULL,


                       gender_switch = NULL,
                       date_studierende = NULL,
                       nurLehramt_studierende = NULL,
                       hochschulform_studierende_1 = NULL,
                       hochschulform_studierende_2 = NULL,
                       date_studierende_verlauf = NULL,
                       indikator_studierende_verlauf = NULL,
                       topic_studierende_verlauf = NULL,
                       nurLehramt_studierende_verlauf = NULL,
                       states_studierende_verlauf = NULL,
                       date_studierende_verlauf_bl = NULL,
                       topic_studierende_verlauf_bl = NULL,
                       nurLehramt_studierende_verlauf_bl = NULL,
                       states_studierende_verlauf_bl = NULL,
                       date_verlauf_subject_bl = NULL,
                       topic_selected_subject_bl = NULL,
                       subject_selected_bl = NULL,
                       states_verlauf_subject_bl = NULL,
                       nurLehramt_studierende_verlauf_bl_subject = NULL,
                       subjects_aggregated = NULL,
                       subject_selected = NULL,
                       subjects_aggregated_bl = NULL,
                       subject_selected_bl = NULL
                     )



  # Studienzahl
  mod_studium_studienzahl_choice_1_server("mod_studium_studienzahl_choice_ui_1_1", r)

  mod_studium_studienzahl_einstieg_server("mod_studium_studienzahl_einstieg_ui_1", r)
  mod_studium_studienzahl_einstieg_gender_server("mod_studium_studienzahl_einstieg_gender_ui_1", r)

  mod_studium_studienzahl_einstieg_verlauf_server("mod_studium_studienzahl_einstieg_verlauf_ui_1", r)
  mod_studium_studienzahl_einstieg_verlauf_gender_server("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1", r)

  mod_studium_studienzahl_einstieg_comparison_server("mod_studium_studienzahl_einstieg_comparison_ui_1", r)
  mod_studium_studienzahl_einstieg_comparison_gender_server("mod_studium_studienzahl_einstieg_comparison_gender_ui_1", r)

  mod_studium_studienzahl_verlauf_server("mod_studium_studienzahl_verlauf_ui_1", r)
  mod_studium_studienzahl_verlauf_bl_server("mod_studium_studienzahl_verlauf_bl_ui_1", r)
  mod_studium_studienzahl_verlauf_bl_subject_server("mod_studium_studienzahl_verlauf_bl_subject_ui_1", r)
  mod_studium_studienzahl_server("mod_studium_studienzahl_ui_1", data_studierende, r)

}

## To be copied in the UI
# mod_studium_ui("studium_1")

## To be copied in the server
# mod_studium_server("studium_1")

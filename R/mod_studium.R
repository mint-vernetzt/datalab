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
      ),
      # fluidRow(
      #   shinydashboard::box(
      #     title = "Abschlusszahlen von Studierenden unterteilt nach akademischem Grad",
      #     width = 12,
      #     shiny::mainPanel(
      #       mod_studium_abschluss_ui("mod_studium_abschluss_ui_1")
      #     )
      #   )
      # ),
      # fluidRow(
      #   shinydashboard::box(
      #     title = "Verhältnis der Geschlechter in MINT für den akademischen Bereich",
      #     width = 12,
      #     shiny::mainPanel(
      #       mod_studium_compare_ui("mod_studium_compare_ui_1")
      #     )
      #   )
      # )
    )
  )
}

#' studium Server Functions
#'
#' @noRd
mod_studium_server <- function(id, data_studierende, r){

  r <- reactiveValues(geschlecht_studierende_einstieg = NULL,
                      indikator_studierende_einstieg = NULL,
                      date_studierende_einstieg = NULL,
                      nurLehramt_studierende_einstieg = NULL,
                      indikator_studierende = NULL,
                      date_studierende = NULL,
                      nurLehramt_studierende = NULL,
                      mint_vs_rest_studierende = NULL,
                      geschlecht_studierende = NULL,
                      hochschulform_studierende_1 = NULL,
                      hochschulform_studierende_2 = NULL,
                      switch_rel_abs = NULL,
                      date_studierende_verlauf = NULL,
                      indikator_studierende_verlauf = NULL,
                      topic_studierende_verlauf = NULL,
                      nurLehramt_studierende_verlauf = NULL,
                      states_studierende_verlauf = NULL,
                      geschlecht = NULL,
                      date = NULL,
                      indikator = NULL,
                      date_abschluss = NULL,
                      geschlecht_abschluss = NULL,
                      date_abschluss_1 = NULL,
                      geschlecht_abschluss_1 = NULL,
                      indikator_abschluss_1 = NULL,
                      durchgefallen = NULL,
                      ing_natwi = NULL,
                      ing_natwi_1 = NULL,
                      durchgefallen_1 = NULL,
                      date_compare = NULL
    )

  r_abschluss <- reactiveValues(indikator_compare_1 = NULL,
                                durchgefallen_compare = NULL,
                                ing_natwi_compare_3 = NULL)

  r_studienzahl <- reactiveValues(indikator_compare_2 = NULL,
                                  ing_natwi_compare_2 = NULL)

  r_habil <- reactiveValues(ing_natwi_compare_1 = NULL)


  # Studienzahl
  mod_studium_studienzahl_choice_1_server("mod_studium_studienzahl_choice_ui_1_1", r)
  mod_studium_studienzahl_choice_2_server("mod_studium_studienzahl_choice_ui_2_1", r)
  mod_studium_studienzahl_einstieg_server("mod_studium_studienzahl_einstieg_ui_1", r)
  mod_studium_studienzahl_verlauf_server("mod_studium_studienzahl_verlauf_ui_1", r)
  mod_studium_studienzahl_server("mod_studium_studienzahl_ui_1", data_studierende, r)

  # comment
  mod_studium_abschluss_choice_1_server("mod_studium_abschluss_choice_ui_1_1", r)
  mod_studium_abschluss_choice_2_server("mod_studium_abschluss_choice_ui_2_1", r)
  mod_studium_abschluss_server("mod_studium_abschluss_ui_1", r)

  #comment
  mod_studium_compare_choice_server("mod_studium_compare_choice_ui_1", r, r_abschluss,
                                    r_studienzahl, r_habil)
  mod_studium_compare_server("mod_studium_compare_ui_1", r, r_abschluss,
                             r_studienzahl, r_habil)
}

## To be copied in the UI
# mod_studium_ui("studium_1")

## To be copied in the server
# mod_studium_server("studium_1")

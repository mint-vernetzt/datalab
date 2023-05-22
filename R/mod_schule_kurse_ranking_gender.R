#' schule_kurse_ranking_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_ranking_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_ranking_gender"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
      selected = 2021
    ),

    p("Fach/Fächergruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_kurse_ranking_gender"),
      choices = c("MINT-Fächer (gesamt)",
                  "Mathematik",
                  "Informatik",
                  "Physik",
                  "Chemie",
                  "Biologie",
                  "andere Fächer (gesamt)",
                  "Deutsch",
                  "Fremdsprachen",
                  "Gesellschaftswissenschaften",
                  "Musik/Kunst",
                  "Religion/Ethik",
                  "Sport"),
      selected = "MINT-Fächer (gesamt)"
    )

  )
}

#' schule_kurse_ranking_gender Server Functions
#'
#' @noRd
mod_schule_kurse_ranking_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_ranking_gender, {
      r$date_kurse_ranking_gender <- input$date_kurse_ranking_gender
    })

    observeEvent(input$subject_kurse_ranking_gender, {
      r$subject_kurse_ranking_gender <- input$subject_kurse_ranking_gender
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_ranking_gender_ui("schule_kurse_ranking_gender_1")

## To be copied in the server
# mod_schule_kurse_ranking_gender_server("schule_kurse_ranking_gender_1")

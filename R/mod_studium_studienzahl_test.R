#' studium_studienzahl_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_test_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("testy"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
      selected = "2021"
    ),
    p("Auswahl der Indikatoren (max. 3):"),
    shinyWidgets::pickerInput(
      inputId = ns("testl"),
      choices = c("Studienanfänger:innen (1.Fachsemester)",
                  "Studienanfänger:innen (1.Hochschulsemester)",
                  "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
                  "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
                  "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
                  "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
                  "Studienanfänger:innen (Universität, 1.Fachsemester)",
                  "Studienanfänger:innen (Universität, 1.Hochschulsemester)",
                  "Studierende",
                  "Studierende (Fachhochschulen)",
                  "Studierende (Lehramt, Universität)",
                  "Studierende (Universität)"
      ),
      selected = c("Studierende"
                   , "Studienanfänger:innen (1.Fachsemester)"
                   ),
      multiple = TRUE,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
    )
    )

}

#' studium_choice_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_test_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$testy, {
      r$testy <- input$testy
    })

    observeEvent(input$testl, {
      r$testl <- input$testl
    })

    # observeEvent(input$hochschulform_studium_choice_gender1, {
    #   r$hochschulform_studium_choice_gender1 <- input$hochschulform_studium_choice_gender1
    # })
    #
    # observeEvent(input$hochschulform_studium_choice_gender2, {
    #   r$hochschulform_studium_choice_gender2 <- input$hochschulform_studium_choice_gender2
    # })
    #
    # observeEvent(input$level_studium_choice_gender, {
    #   r$level_studium_choice_gender <- input$level_studium_choice_gender
    # })


  })
}

## To be copied in the UI
# mod_studium_choice_gender_ui("studium_choice_gender_1")

## To be copied in the server
# mod_studium_choice_gender_server("studium_choice_gender_1")

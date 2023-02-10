#' studium_studienzahl_einstieg_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studienzahl_einstieg_verlauf"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021),
      selected = c(2015,2021)
    ),
    p("Auswahl der Indikatoren (max. 3):"),
    shinyWidgets::pickerInput(
      inputId = ns("studienzahl_einstieg_verlauf_indi"),
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


#' studium_studienzahl_einstieg_verlauf Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){


    observeEvent(input$date_studienzahl_einstieg_verlauf, {
      r$date_studienzahl_einstieg_verlauf <- input$date_studienzahl_einstieg_verlauf
    })

    observeEvent(input$studienzahl_einstieg_verlauf_indi, {
      r$studienzahl_einstieg_verlauf_indi <- input$studienzahl_einstieg_verlauf_indi
    })

#
#     observeEvent(input$hochschulform_studierende_einstieg_verlauf_1, {
#       r$hochschulform_studierende_einstieg_verlauf_1 <- input$hochschulform_studierende_einstieg_verlauf_1
#     })
#
#
#     observeEvent(input$hochschulform_studierende_einstieg_verlauf_2, {
#       r$hochschulform_studierende_einstieg_verlauf_2 <- input$hochschulform_studierende_einstieg_verlauf_2
#     })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_verlauf_ui("studium_studienzahl_einstieg_verlauf_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_verlauf_server("studium_studienzahl_einstieg_verlauf_1")

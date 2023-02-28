#' studium_studienzahl_einstieg_verlauf_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_verlauf_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("genz_date"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
      selected = c(2015, 2021)
    ),
    # p("Nur Lehramt anzeigen:"),
    # tags$div(
    #   shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_einstieg_verlauf_gender"), label = "Nein", inline = TRUE),
    #   tags$span("Ja")
    # ),p("Auswahl der Indikatoren (max. 3):"),
    shinyWidgets::pickerInput(
      inputId = ns("genzl"),
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
      ),multiple = TRUE,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
    )
    # p("Auswahl der Hochschulform:"),
    # conditionalPanel(condition = "input.nurLehramt_studierende_einstieg_verlauf_gender == false",
    #                  ns = ns,
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("hochschulform_studierende_einstieg_verlauf_gender_1"),
    #                    choices = c("Alle Hochschulen"="insgesamt", "Universität" = "Uni", "Fachhochschule" = "FH")
    #                  )),
    # conditionalPanel(condition = "input.nurLehramt_studierende_einstieg_verlauf_gender != false",
    #                  ns = ns,
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("hochschulform_studierende_einstieg_verlauf_gender_2"),
    #                    choices = "Uni"
    #                  ))

  )
}

#' studium_studienzahl_einstieg_verlauf_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_verlauf_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$genzl, {
      r$genzl <- input$genzl
    })

    observeEvent(input$genz_date, {
      r$genz_date <- input$genz_date
    })


    # observeEvent(input$hochschulform_studierende_einstieg_verlauf_gender_1, {
    #   r$hochschulform_studierende_einstieg_verlauf_gender_1 <- input$hochschulform_studierende_einstieg_verlauf_gender_1
    # })
    #
    #
    # observeEvent(input$hochschulform_studierende_einstieg_verlauf_gender_2, {
    #   r$hochschulform_studierende_einstieg_verlauf_gender_2 <- input$hochschulform_studierende_einstieg_verlauf_gender_2
    # })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_verlauf_gender_ui("studium_studienzahl_einstieg_verlauf_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_verlauf_gender_server("studium_studienzahl_einstieg_verlauf_gender_1")

#' studium_studienzahl_bl_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bl_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wählen Sie ein Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_studienzahl_bl_vergleich"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_studienzahl_bl_vergleich"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Wählen Sie eine Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_vergleich == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform__studium_studienzahl_bl_vergleich1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_vergleich != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_vergleich2"),
                         choices = "Uni"
                       )),
      p("Wählen Sie ein Fach:"),
      shinyWidgets::pickerInput(
        inputId = ns("subject_studium_studienzahl_bl_vergleich"),
        choices = c("MINT (aggregiert)","Mathe", "Ingenieur"),
        selected = "MINT (aggregiert)"
      )
    ),
    p("Wählen Sie den Status der Student*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("level_studium_studienzahl_bl_vergleich"),
      choices = c("Studienanfänger", "Studierende"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
  )
}

#' studium_studienzahl_bl_vergleich Server Functions
#'
#' @noRd
mod_studium_studienzahl_bl_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_studienzahl_bl_vergleich, {
      r$date_studium_studienzahl_bl_vergleich <- input$date_studium_studienzahl_bl_vergleich
    })

    observeEvent(input$nurLehramt_studium_studienzahl_bl_vergleich, {
      r$nurLehramt_studium_studienzahl_bl_vergleich <- input$nurLehramt_studium_studienzahl_bl_vergleich
    })

    observeEvent(input$hochschulform__studium_studienzahl_bl_vergleich1, {
      r$hochschulform__studium_studienzahl_bl_vergleich1 <- input$hochschulform__studium_studienzahl_bl_vergleich1
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_vergleich2, {
      r$hochschulform_studium_studienzahl_bl_vergleich2 <- input$hochschulform_studium_studienzahl_bl_vergleich2
    })

    observeEvent(input$subject_studium_studienzahl_bl_vergleich, {
      r$subject_studium_studienzahl_bl_vergleich <- input$subject_studium_studienzahl_bl_vergleich
    })

    observeEvent(input$level_studium_studienzahl_bl_vergleich, {
      r$level_studium_studienzahl_bl_vergleich <- input$level_studium_studienzahl_bl_vergleich
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_vergleich_ui("studium_studienzahl_bl_vergleich_1")

## To be copied in the server
# mod_studium_studienzahl_bl_vergleich_server("studium_studienzahl_bl_vergleich_1")

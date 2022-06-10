#' studium_studienzahl_bl_vergleich_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bl_vergleich_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_studienzahl_bl_gender_vergleich"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_studienzahl_bl_gender_vergleich"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Wähle eine Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_gender_vergleich == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_gender_vergleich1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_gender_vergleich != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_gender_vergleich2"),
                         choices = "Uni"
                       )),
      p("Wähle ein Fach:"),
      shinyWidgets::pickerInput(
        inputId = ns("subject_studium_studienzahl_bl_gender_vergleich"),
        choices = c("MINT (aggregiert)","Mathe", "Ingenieur"),
        selected = "MINT (aggregiert)"
      )
    )
  )
}

#' studium_studienzahl_bl_vergleich_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_bl_vergleich_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_studienzahl_bl_gender_vergleich, {
      r$date_studium_studienzahl_bl_gender_vergleich <- input$date_studium_studienzahl_bl_gender_vergleich
    })

    observeEvent(input$nurLehramt_studium_studienzahl_bl_gender_vergleich, {
      r$nurLehramt_studium_studienzahl_bl_gender_vergleich <- input$nurLehramt_studium_studienzahl_bl_gender_vergleich
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_gender_vergleich1, {
      r$hochschulform_studium_studienzahl_bl_gender_vergleich1 <- input$hochschulform_studium_studienzahl_bl_gender_vergleich1
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_gender_vergleich2, {
      r$hochschulform_studium_studienzahl_bl_gender_vergleich2 <- input$hochschulform_studium_studienzahl_bl_gender_vergleich2
    })

    observeEvent(input$subject_studium_studienzahl_bl_gender_vergleich, {
      r$subject_studium_studienzahl_bl_gender_vergleich <- input$subject_studium_studienzahl_bl_gender_vergleich
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_vergleich_gender_ui("studium_studienzahl_bl_vergleich_gender_1")

## To be copied in the server
# mod_studium_studienzahl_bl_vergleich_gender_server("studium_studienzahl_bl_vergleich_gender_1")

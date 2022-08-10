#' studium_studienzahl_bl_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bl_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_studienzahl_bl_map"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Nur Lehramt anzeigen:"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_studienzahl_bl_map"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Auswahl der Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_map == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_map1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_map != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_map2"),
                         choices = "Uni"
                       )),
      p("Auswahl des Fachs:"),
      shinyWidgets::pickerInput(
        inputId = ns("subject_studium_studienzahl_bl_map"),
        choices = c("MINT (aggregiert)","Mathematik/Naturwissenschaften", "Ingenieurwissenschaften"),
        selected = "MINT (aggregiert)"
      )
    )
  )
}

#' studium_studienzahl_bl_map Server Functions
#'
#' @noRd
mod_studium_studienzahl_bl_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_studienzahl_bl_map, {
      r$date_studium_studienzahl_bl_map <- input$date_studium_studienzahl_bl_map
    })

    observeEvent(input$nurLehramt_studium_studienzahl_bl_map, {
      r$nurLehramt_studium_studienzahl_bl_map <- input$nurLehramt_studium_studienzahl_bl_map
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_map1, {
      r$hochschulform_studium_studienzahl_bl_map1 <- input$hochschulform_studium_studienzahl_bl_map1
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_map2, {
      r$hochschulform_studium_studienzahl_bl_map2 <- input$hochschulform_studium_studienzahl_bl_map2
    })

    observeEvent(input$subject_studium_studienzahl_bl_map, {
      r$subject_studium_studienzahl_bl_map <- input$subject_studium_studienzahl_bl_map
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_map_ui("studium_studienzahl_bl_map_1")

## To be copied in the server
# mod_studium_studienzahl_bl_map_server("studium_studienzahl_bl_map_1")

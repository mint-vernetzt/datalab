#' studium_studienzahl_bl_map_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bl_map_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_studienzahl_bl_gender_map"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Nur Lehramt anzeigen:"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_studienzahl_bl_gender_map"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Auswahl der Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_gender_map == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_gender_map1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_gender_map != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_gender_map2"),
                         choices = "Uni"
                       )),
      p("Auswahl des Fachs:"),
      shinyWidgets::pickerInput(
        inputId = ns("subject_studium_studienzahl_bl_gender_map"),
        choices = c("MINT (aggregiert)","Mathematik/Naturwissenschaften", "Ingenieurwissenschaften"),
        selected = "MINT (aggregiert)"
      ),
      p("Status der Student*innen:"),
      shinyWidgets::radioGroupButtons(
        inputId = ns("level_studium_choice_gender"),
        choices = c("Studienanfänger", "Studierende"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    )
  )
}

#' studium_studienzahl_bl_map_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_bl_map_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_studienzahl_bl_gender_map, {
      r$date_studium_studienzahl_bl_gender_map <- input$date_studium_studienzahl_bl_gender_map
    })

    observeEvent(input$nurLehramt_studium_studienzahl_bl_gender_map, {
      r$nurLehramt_studium_studienzahl_bl_gender_map <- input$nurLehramt_studium_studienzahl_bl_gender_map
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_gender_map1, {
      r$hochschulform_studium_studienzahl_bl_gender_map1 <- input$hochschulform_studium_studienzahl_bl_gender_map1
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_gender_map2, {
      r$hochschulform_studium_studienzahl_bl_gender_map2 <- input$hochschulform_studium_studienzahl_bl_gender_map2
    })

    observeEvent(input$subject_studium_studienzahl_bl_gender_map, {
      r$subject_studium_studienzahl_bl_gender_map <- input$subject_studium_studienzahl_bl_gender_map
    })

    observeEvent(input$level_studium_choice_gender, {
      r$level_studium_choice_gender <- input$level_studium_choice_gender
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_map_gender_ui("studium_studienzahl_bl_map_gender_1")

## To be copied in the server
# mod_studium_studienzahl_bl_map_gender_server("studium_studienzahl_bl_map_gender_1")

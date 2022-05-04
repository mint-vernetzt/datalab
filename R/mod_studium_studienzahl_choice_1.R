#' studium_studienzahl_choice_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_choice_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitpunkt:"),
      shinyWidgets::sliderTextInput(
        inputId = ns("date_studierende"),
        label = NULL,
        choices = c(2010, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      ),
    p("Wähle den Status der Student*innen:"),
      shinyWidgets::radioGroupButtons(
        inputId = ns("indikator_studierende"),
        choices = c("Studierende", "Studienanfänger"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende"), label = "Nein", inline = TRUE),
      tags$span("Ja")
    ),
      # shinyWidgets::radioGroupButtons(
      #   inputId = ns("nurLehramt_studierende"),
      #   choices = c("Ja", "Nein"),
      #   selected = "Nein"
      #   ),
    p("Wähle eine Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studierende == false",
                       ns = ns,
      shinyWidgets::pickerInput(
        inputId = ns("hochschulform_studierende_1"),
        choices = c("insgesamt", "Uni", "FH")
      )),
      conditionalPanel(condition = "input.nurLehramt_studierende != false",
         ns = ns,
        shinyWidgets::pickerInput(
          inputId = ns("hochschulform_studierende_2"),
          choices = "Uni"
        ))#,
      # shinyWidgets::pickerInput(
      #   inputId = ns("geschlecht_studierende"),
      #   label = "Soll nach Geschlecht aufgeteilt werden?",
      #   choices = c("Ja", "Nein")
      # )
      # shinyWidgets::pickerInput(
      #   inputId = ns("mint_vs_rest_studierende"),
      #   label = "Sollen die MINT-Fachbereiche aggregiert werden?",
      #   choices = c("Ja", "Nein")
      # )
  )
}

#' studium_studienzahl_choice_1 Server Functions
#'
#' @noRd
mod_studium_studienzahl_choice_1_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns

    observeEvent(input$date_studierende, {
      r$date_studierende <- input$date_studierende
    })

    observeEvent(input$indikator_studierende, {
      r$indikator_studierende <- input$indikator_studierende
    })

    observeEvent(input$nurLehramt_studierende, {
      r$nurLehramt_studierende <- input$nurLehramt_studierende
    })

    observeEvent(input$geschlecht_studierende, {
      r$geschlecht_studierende <- input$geschlecht_studierende
    })

    observeEvent(input$hochschulform_studierende_1, {
      r$hochschulform_studierende_1 <- input$hochschulform_studierende_1
    })

    observeEvent(input$hochschulform_studierende_2, {
      r$hochschulform_studierende_2 <- input$hochschulform_studierende_2
    })

    observeEvent(input$mint_vs_rest_studierende, {
      r$mint_vs_rest_studierende <- input$mint_vs_rest_studierende
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_choice_1_ui("studium_studienzahl_choice_1_1")

## To be copied in the server
# mod_studium_studienzahl_choice_1_server("studium_studienzahl_choice_1_1")

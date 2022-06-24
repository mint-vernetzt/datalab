#' studium_choice_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_choice_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wählen Sie ein Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_choice_gender"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_choice_gender"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Wählen Sie eine Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studium_choice_gender == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_choice_gender1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studium_choice_gender != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_choice_gender2"),
                         choices = "Uni"
                       ))
    ),
    p("Wählen Sie den Status der Student*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("level_studium_choice_gender"),
      choices = c("Studienanfänger", "Studierende"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
  )
}

#' studium_choice_gender Server Functions
#'
#' @noRd
mod_studium_choice_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_choice_gender, {
      r$date_studium_choice_gender <- input$date_studium_choice_gender
    })

    observeEvent(input$nurLehramt_studium_choice_gender, {
      r$nurLehramt_studium_choice_gender <- input$nurLehramt_studium_choice_gender
    })

    observeEvent(input$hochschulform_studium_choice_gender1, {
      r$hochschulform_studium_choice_gender1 <- input$hochschulform_studium_choice_gender1
    })

    observeEvent(input$hochschulform_studium_choice_gender2, {
      r$hochschulform_studium_choice_gender2 <- input$hochschulform_studium_choice_gender2
    })

    observeEvent(input$level_studium_choice_gender, {
      r$level_studium_choice_gender <- input$level_studium_choice_gender
    })


  })
}

## To be copied in the UI
# mod_studium_choice_gender_ui("studium_choice_gender_1")

## To be copied in the server
# mod_studium_choice_gender_server("studium_choice_gender_1")

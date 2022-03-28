#' studium_abschluss_choice_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_abschluss_choice_2_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::column(
      width = 6,
      shinyWidgets::prettyRadioButtons(
        inputId = ns("date_abschluss"),
        label = "Wähle ein Jahr:",
        choices = c(2012, 2015, 2018, 2020),
        selected = 2012,
        inline = TRUE,
        fill = TRUE
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns("geschlecht_abschluss"),
        label = "Soll die Aufteilung nach Geschlecht getrennt werden ?",
        choices = c("Ja", "Nein"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    )
  )
}

#' studium_abschluss_choice_2 Server Functions
#'
#' @noRd
mod_studium_abschluss_choice_2_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    #ns <- session$ns

    observeEvent(input$geschlecht_abschluss, {
      r$geschlecht_abschluss <- input$geschlecht_abschluss
    })

    observeEvent(input$date_abschluss, {
      r$date_abschluss <- input$date_abschluss
    })

  })
}

## To be copied in the UI
# mod_studium_abschluss_choice_2_ui("studium_abschluss_choice_2_1")

## To be copied in the server
# mod_studium_abschluss_choice_2_server("studium_abschluss_choice_2_1")

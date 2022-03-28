#' studium_studienzahl_choice_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::column(
      width = 2,
      shinyWidgets::sliderTextInput(
        inputId = ns("date"),
        label = "Wähle ein Zeitraum:",
        choices = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
        selected = c(2012, 2020),
        from_min = 2012,
        from_max = 2014,
        to_min = 2015,
        to_max = 2020
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns("geschlecht"),
        label = "Soll die Aufteilung nach Geschlecht getrennt werden ?",
        choices = c("Ja", "Nein"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
      shinyWidgets::pickerInput(
        inputId = ns("indikator"),
        label = "Wähle ein Indikator:",
        choices = list(
          Insgesamt = c("eingeschrieben"),
          Studienanfänger = c("1hs", "1fs")
        )
      )
    )
  )
}

#' studium_studienzahl_choice_2 Server Functions
#'
#' @noRd
mod_studium_studienzahl_choice_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns

    observeEvent(input$geschlecht, {
      r$geschlecht <- input$geschlecht
    })

    observeEvent(input$date, {
      r$date <- input$date
    })

    observeEvent(input$indikator, {
      r$indikator <- input$indikator
    })



  })
}

## To be copied in the UI
# mod_studium_studienzahl_choice_2_ui("studium_studienzahl_choice_2_1")

## To be copied in the server
# mod_studium_studienzahl_choice_2_server("studium_studienzahl_choice_2_1")

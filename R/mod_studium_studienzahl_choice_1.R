#' studium_studienzahl_choice_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_choice_2_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::column(
      width = 2,
      shinyWidgets::prettyRadioButtons(
        inputId = ns("date_waffle"),
        label = "Wähle ein Jahr:",
        choices = c(2012, 2015, 2018, 2020),
        selected = 2020,
        inline = TRUE,
        fill = TRUE
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns("geschlecht_waffle"),
        label = "Soll die Aufteilung nach Geschlecht getrennt werden ?",
        choices = c("Ja", "Nein"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
      shinyWidgets::pickerInput(
        inputId = ns("indikator_waffle"),
        label = "Wähle ein Indikator:",
        choices = list(
          Insgesamt = c("eingeschrieben"),
          Studienanfänger = c("1hs", "1fs")
        )
      )
    )

  )
}

#' studium_studienzahl_choice_1 Server Functions
#'
#' @noRd
mod_studium_studienzahl_choice_2_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns

    observeEvent(input$geschlecht_waffle, {
      r$geschlecht_waffle <- input$geschlecht_waffle
    })

    observeEvent(input$date_waffle, {
      r$date_waffle <- input$date_waffle
    })

    observeEvent(input$indikator_waffle, {
      r$indikator_waffle <- input$indikator_waffle
    })
  })
}

## To be copied in the UI
# mod_studium_studienzahl_choice_1_ui("studium_studienzahl_choice_1_1")

## To be copied in the server
# mod_studium_studienzahl_choice_1_server("studium_studienzahl_choice_1_1")

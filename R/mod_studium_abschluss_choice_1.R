#' studium_abschluss_choice_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_abschluss_choice_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::column(
      width = 6,
      shinyWidgets::sliderTextInput(
        inputId = ns("date_abschluss_1"),
        label = "Wähle ein Zeitraum:",
        choices = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
        selected = c(2012, 2020),
        from_min = 2012,
        from_max = 2014,
        to_min = 2015,
        to_max = 2020
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns("geschlecht_abschluss_1"),
        label = "Soll die Aufteilung nach Geschlecht getrennt werden ?",
        choices = c("Ja", "Nein"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
      shinyWidgets::awesomeCheckboxGroup(
        inputId = ns("indikator_abschluss_1"),
        label = "Indikator",
        choices = c("bachelor", "fh", "lehramt", "master", "promotion", "uni"),
        selected = "bachelor"
      )

    )

  )
}

#' studium_abschluss_choice_1 Server Functions
#'
#' @noRd
mod_studium_abschluss_choice_1_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    #ns <- session$ns

    observeEvent(input$geschlecht_abschluss_1, {
      r$geschlecht_abschluss_1 <- input$geschlecht_abschluss_1
    })

    observeEvent(input$date_abschluss_1, {
      r$date_abschluss_1 <- input$date_abschluss_1
    })

    observeEvent(input$indikator_abschluss_1, {
      r$indikator_abschluss_1 <- input$indikator_abschluss_1
    })

  })
}


## To be copied in the UI
# mod_studium_abschluss_choice_1_ui("studium_abschluss_choice_1_1")

## To be copied in the server
# mod_studium_abschluss_choice_1_server("studium_abschluss_choice_1_1")

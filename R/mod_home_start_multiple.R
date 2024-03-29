#' home_start_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_multiple"),
      label = NULL,
      choices = c("2013", "2014","2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2015", "2022")
    ),
    p("Bereiche:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_multiple_1"),
      choices = c("Schüler:innen Leistungskurse", "Studierende",
                  "Auszubildende", "Beschäftigte"),
      selected = c("Schüler:innen Leistungskurse", "Beschäftigte"),
      # options =  list(
      #   "max-options" = 3,
      #   "max-options-text" = "Bitte nur maximal 3 Bereiche auswählen"
      # ),
      multiple = T
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_start_multiple"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_alle_mint_2", title="",
                       content = paste0("Die Grafik zeigt, dass der Anteil von MINT über alle Bereiche von 2015 bis 2020 ziemlich konstat bleibt. Bei Schüler*innen, Auszubildenden und Beschäftigten zeigt sich zwischen 2020 und 2021 ein leichter Rückgang des MINT-Anteil um jeweils einen Prozentpunkt. Was dieser Prozentpunkt ausmacht, sieht man, wenn man die absolute Anzahl betrachtet. 2021 gibt es deutschlandweit knapp 400.000 Beschäftigte weniger in MINT als noch 2020."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_mint_2")
  )


}

#' home_start_multiple Server Functions
#'
#' @noRd
mod_home_start_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_start_multiple, {
      r$date_start_multiple <- input$date_start_multiple
    })

    observeEvent(input$abs_zahlen_start_multiple, {
      r$abs_zahlen_start_multiple <- input$abs_zahlen_start_multiple
    })

    observeEvent(input$indikator_start_multiple_1, {
      r$indikator_start_multiple_1 <- input$indikator_start_multiple_1
    })

  })
}

## To be copied in the UI
# mod_home_start_multiple_ui("home_start_multiple_1")

## To be copied in the server
# mod_home_start_multiple_server("home_start_multiple_1")

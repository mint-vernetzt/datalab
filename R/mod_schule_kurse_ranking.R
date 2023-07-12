#' schule_kurse_ranking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_ranking_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_ranking"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
      selected = 2021
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_kurse_ranking"),
      choices = c("Deutschland",
                  "Bayern",
                  "Berlin",
                  "Brandenburg",
                  "Bremen",
                  "Hamburg",
                  "Hessen",
                  "Mecklenburg-Vorpommern",
                  "Niedersachsen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz",
                  "Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Thüringen"
                  ,
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)"
                  ),
       selected = "Mecklenburg-Vorpommern"
      ),
    br(),
    shinyBS::bsPopover(id="ih_schule_frauen_2", title="",
                       content = paste0("Hier zeigen wir einen direkten Vergleich zwischen den Anteilen von Mädchen in Grundkursen und Leistungskursen. Die erste Einstellung zeigt z. B., dass in Mecklenburg-Vorpommern 2021 der Mädchenanteil in Informatik-Leistungskursen bei unter 20 %, in Informatik-Grundkurse dagegen bei 45 % liegt."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_frauen_2")
  )
}

#' schule_kurse_ranking Server Functions
#'
#' @noRd
mod_schule_kurse_ranking_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_ranking, {
      r$date_kurse_ranking <- input$date_kurse_ranking
    })

    observeEvent(input$states_kurse_ranking, {
      r$states_kurse_ranking <- input$states_kurse_ranking
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_ranking_ui("schule_kurse_ranking_1")

## To be copied in the server
# mod_schule_kurse_ranking_server("schule_kurse_ranking_1")

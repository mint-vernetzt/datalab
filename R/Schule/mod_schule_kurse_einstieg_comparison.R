#' schule_kurse_einstieg_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_einstieg_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_einstieg_comparison"),
      label = NULL,
      choices = c("2013","2014","2015","2016","2017", "2018", "2019", "2020", "2021", "2022"),
      selected = "2022"
    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_mint_2", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass im Jahr 2021 in Deutschland 24 % aller gewählten Grundkurse aus dem Bereich MINT sind. Bei Leistungskursen liegt der Anteil im Jahr 2021 bei 33 %."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_2")
  )
}

#' schule_kurse_einstieg_comparison Server Functions
#'
#' @noRd
mod_schule_kurse_einstieg_comparison_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_einstieg_comparison, {
      r$date_kurse_einstieg_comparison <- input$date_kurse_einstieg_comparison
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_einstieg_comparison_ui("schule_kurse_einstieg_comparison_1")

## To be copied in the server
# mod_schule_kurse_einstieg_comparison_server("schule_kurse_einstieg_comparison_1")

#' schule_kurse_comparison_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_comparison_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_comparison_gender"),
      label = NULL,
      choices = c("2013","2014","2015","2016","2017", "2018", "2019", "2020", "2021", "2022"),
      selected = "2021"
    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_frauen_1", title="",
                       content = paste0("Die erste Darstellung zeigt, dass der Anteil von Mädchen bzw. Frauen in allen MINT-Grundkursen in Deutschland 2021 53 % beträgt. In den MINT-Leistungskursen beträgt dieser Anteil 48 %. In den Nicht-MINT-Fächern ist der Anteil an Mädchen bzw. Frauen etwas höher: In Grundkursen machen Frauen 54 % aus, in Leistungskursen sogar 58 %."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_frauen_1")
  )
}

#' schule_kurse_comparison_gender Server Functions
#'
#' @noRd
mod_schule_kurse_comparison_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_comparison_gender, {
      r$date_kurse_comparison_gender <- input$date_kurse_comparison_gender
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_comparison_gender_ui("schule_kurse_comparison_gender_1")

## To be copied in the server
# mod_schule_kurse_comparison_gender_server("schule_kurse_comparison_gender_1")

#' schule_kurse_comparison_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_schule_kurse_verlauf_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),

    shinyWidgets::sliderTextInput(
      inputId = ns("datum_kurse_verlauf_gender"),
      #label = "Jahr:",
      label = NULL,
      choices = 2013:2023,
      selected = c(2017, 2023)
    ),
    p("Region:"),

    shinyWidgets::pickerInput(
      inputId = ns("region_kurse_verlauf_gender"),
   #   label = "Region:",
   label = NULL,
      choices = c("Deutschland",
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)",
                  #"Baden-Württemberg",
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
      ),
      multiple = FALSE,
      selected = c("Deutschland")
    ),
   p("Darstellungsart:"),
   shinyWidgets::radioGroupButtons(
     inputId = ns("abs_rel_kurse_verlauf_gender"),
     choices = c("In Prozent", "Anzahl"),
     justified = TRUE,
     checkIcon = list(yes = icon("ok",
                                 lib = "glyphicon"))
   ),

   br(),
   shinyBS::bsPopover(id="popoverbox3_1", title="",
                      content = paste0("Die erste Darstellung zeigt z.B., dass deutschlandweit der Mädchenanteil an den MINT-Grundkursen in der Oberstufe grundsätzlich höher ist als an den MINT-Leistungskursen."),
                      trigger = "hover"),

   tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="popoverbox3_1")


  )

}

#' schule_kurse_comparison_gender Server Functions
#'
#' @noRd
#'
mod_schule_kurse_verlauf_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){


    observeEvent(input$datum_kurse_verlauf_gender, {
      r$datum_kurse_verlauf_gender <- input$datum_kurse_verlauf_gender
    })

    observeEvent(input$region_kurse_verlauf_gender, {
      r$region_kurse_verlauf_gender <- input$region_kurse_verlauf_gender
    })

    observeEvent(input$abs_rel_kurse_verlauf_gender, {
      r$abs_rel_kurse_verlauf_gender <- input$abs_rel_kurse_verlauf_gender
    })

  })
}

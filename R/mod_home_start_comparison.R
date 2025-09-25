#' home_start_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_comparison_ui <- function(id){
  ns <- NS(id)

  tagList(

    tags$head(
      tags$style(HTML("
        .dropdown-menu .bs-actionsbox .btn-group .btn {
          background-color: #e7f1ff !important;  /* Hellblau für die Alle auswählen/abwählen Buttons */
          color: #000000 !important;
        }
        .dropdown-menu .bs-actionsbox .btn-group .btn:hover {
          background-color: #d0e8ff !important;  /* Etwas dunkleres Blau beim Hover */
          color: #000000 !important;
        }
      "))
    ),

    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_comparison"),
      label = NULL,
      choices = 2013:2024,
      selected = c(2015, 2024)
    ),

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("regio_start_comparison"),
      choices = c("Deutschland",
                  "Baden-Württemberg",
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
                  "Thüringen",
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)"
      ),
      multiple = FALSE,
      selected = c("Deutschland")
    ),

    p("Bereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_comparison"),
      choices = c("Schüler:innen Leistungskurse" = "Leistungskurse",
                  "Studierende",
                  "Auszubildende", "Beschäftigte"),
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      selected = c("Leistungskurse", "Studierende",
                   "Auszubildende",  "Beschäftigte"),
      multiple = TRUE

    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_start_comparison"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_alle_frauen_2", title="",
                       content = paste0("Der Frauenanteil im MINT-Fachbereich hat sich sowohl in Schule, Studium, Ausbildung wie auch Beschäftigung in den 9 Jahren von 2013 bis 2021 nur geringfügig erhöht (1-3 Prozentpunkte)."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_frauen_2")
  )

}

#' home_start_comparison Server Functions
#'
#' @noRd
mod_home_start_comparison_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_start_comparison, {
      r$date_start_comparison <- input$date_start_comparison
    })

    observeEvent(input$regio_start_comparison, {
      r$regio_start_comparison <- input$regio_start_comparison
    })


    observeEvent(input$indikator_start_comparison, {
      r$indikator_start_comparison <- input$indikator_start_comparison
    })

    observeEvent(input$abs_zahlen_start_comparison, {
      r$abs_zahlen_start_comparison <- input$abs_zahlen_start_comparison
    })


  })
}

## To be copied in the UI
# mod_home_start_comparison_ui("home_start_comparison_1")

## To be copied in the server
# mod_home_start_comparison_server("home_start_comparison_1")

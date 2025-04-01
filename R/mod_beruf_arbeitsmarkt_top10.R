#' beruf_arbeitsmarkt_top10 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_beruf_arbeitsmarkt_top10_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_top_beruf"),
      label = NULL,
      choices = c(2017, 2020, 2022, 2024),
      selected = 2022
    ),
    # Region
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_top_beruf"),
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
      selected = "Deutschland"
    ),

    p("Fachbereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("FB_top_beruf"),
      choices = c("MINT (gesamt)",
                  "Informatik",
                  "Mathematik/Naturwissenschaft",
                  "Produktionstechnik",
                  "Bau- und Gebäudetechnik",
                  "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
                  "Gesundheitstechnik" = "Gesundheitstechnik - Fachkräfte"),
      selected = "MINT (gesamt)"
    ),

    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("betr_abs_rel"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_beruf_fach_4", title="",
                       content = paste0("Die Grafik zeigt, dass Ausbildungsberufe mit Schnittstellen zu Nicht-MINT-Bereichen wie Milchwirtschaftlichen Laboranten oder Zahntechnik besonders hohe Frauenanteile unter den neuen Azubis haben. Demgegenüber sind Männer auch in rein technisch fokussierten Ausbildung interessiert. "),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_4")
  )
}

#' beruf_arbeitsmarkt_top10 Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_top10_server <- function(id, r){
  moduleServer( id, function(input, output, session){


    observeEvent(input$date_top_beruf, {
      r$date_top_beruf <- input$date_top_beruf
    })

    observeEvent(input$states_top_beruf, {
      r$states_top_beruf <- input$states_top_beruf
    })

    observeEvent(input$FB_top_beruf, {
      r$FB_top_beruf <- input$FB_top_beruf
    })

    observeEvent(input$betr_abs_rel, {
      r$betr_abs_rel <- input$betr_abs_rel
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_überblick_fächer_ui

## To be copied in the server
# mod_beruf_arbeitsmarkt_überblick_fächer_server

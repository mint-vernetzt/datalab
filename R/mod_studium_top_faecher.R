#' studium_top_faecher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_top_faecher_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_top_faecher"),
      label = NULL,
      choices = c(2018, 2019, 2020, 2021, 2022),
      selected = 2022
    ),
    # Region
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_top_faecher"),
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
      selected = "Bayern"
    ),
    p("Fachbereich:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("subject_top_faecher"),
      choices = c("MINT-Fächer", "Alle Fachbereiche"= "Alle Fächer"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("subject_abs_rel"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_fach_1", title="",
                       content = paste0("In der ersten Einstellung sind die TOP-10-Fächer in Bayern in MINT bezogen auf den Frauen- bzw. Männeranteil zu sehen. Die Fächer mit dem höchsten Frauenanteil in MINT sind Pharmazie (74 % Frauen) und Biologie (65 % Frauen). Die Fächer mit dem höchsten Männeranteil in MINT sind dagegen Verkehrstechnik / Nautik mit 86 % Männern und Elektrotechnik und Informationstechnik mit 84 %."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_1")
  )
}

#' studium_top_faecher Server Functions
#'
#' @noRd
mod_studium_top_faecher_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_top_faecher, {
      r$date_top_faecher <- input$date_top_faecher
    })

    observeEvent(input$states_top_faecher, {
      r$states_top_faecher <- input$states_top_faecher
    })

    observeEvent(input$subject_top_faecher, {
      r$subject_top_faecher <- input$subject_top_faecher
    })

    observeEvent(input$subject_abs_rel, {
      r$subject_abs_rel <- input$subject_abs_rel
    })


  })
}

## To be copied in the UI
# mod_studium_top_faecher_ui("studium_top_faecher_1")

## To be copied in the server
# mod_studium_top_faecher_server("studium_top_faecher_1")

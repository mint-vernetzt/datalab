#' studium_top_faecher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_top_entgelt_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_top_entgelt"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),
    # Region
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_top_entgelt"),
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
      inputId = ns("subject_top_entgelt"),
      choices = c("MINT-Fächer", "Alle Fachbereiche"= "Alle Fächer"),
      selected = "MINT-Fächer",
      multiple = FALSE
    ),

    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("subject_abs_rel_engelt"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_fach_1_ent", title="",
                       content = paste0("In der ersten Einstellung sind die TOP-10-MINT-Fächer in Deutschland 2023 für weibliche bzw. männliche Studierende gezeigt. Die MINT-Fächer mit dem höchsten Frauenanteil sind Pharmazie (73.1 % Frauen) und Biologie (64.5 %). Die MINT-Fächer mit dem höchsten Männeranteil sind Verkehrstechnik/Nautik (86.4 %  Männern) und Elektrotechnik und Informationstechnik (84 %)."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_1_ent")
  )
}

#' studium_top_faecher Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_top_entgelt_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_top_entgelt, {
      r$date_top_entgelt <- input$date_top_entgelt
    })

    observeEvent(input$states_top_entgelt, {
      r$states_top_entgelt <- input$states_top_entgelt
    })

    observeEvent(input$subject_top_entgelt, {
      r$subject_top_entgelt <- input$subject_top_entgelt
    })

    observeEvent(input$subject_abs_rel_engelt, {
      r$subject_abs_rel_engelt <- input$subject_abs_rel_engelt
    })



  })
}

## To be copied in the UI
# mod_studium_top_faecher_ui("studium_top_faecher_1")

#' schule_kurse_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_kurse_gender"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm",
                  "Bundeslandvergleich - Kartendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse"),
      label = NULL,
      choices = 2013:2024,
      selected = 2024
    ),

    p("Kursniveau:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_kurse_gender"),
      choices = c("Grundkurse", "Leistungskurse", "Oberstufenbelegungen"),
      selected = "Grundkurse"
    ),
    p("Kurswahl der Jungen als Vergleich anzeigen?", style = "color: #b16fab;"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("gegenwert_kurse_gender"),
      choices = c("Ja", "Nein"),
      selected = "Nein",
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    conditionalPanel(condition = "input.ansicht_kurse_gender == 'Einzelansicht - Kuchendiagramm'",
                     ns = ns,

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_kurse_gender"),
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
      multiple = F,
      selected = "Deutschland"
    ),

    br(),
    shinyBS::bsPopover(id="ih_schule_mint_4", title="",
                       content = paste0("Wenn man die Kurswahl der Jungen als Vergleich mitanzeigen lässt, ist in der Grafik zu lesen, dass 2023 deutschlandweit sowohl für Mädchen wie Jungen knapp 25 von 100 Grundkursbelegungen in einem MINT-Fach sind. Unterschiede sieht man in den Leistungskursen. Rund 26 von 100 Leistungskursbelegungen von Mädchen, aber 36 % der Leistungskursbelegungen von Jungen sind in einem MINT-Fach."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_4"),

    br()

    ),
    conditionalPanel(
      condition = "input.ansicht_kurse_gender == 'Bundeslandvergleich - Kartendiagramm'",
       ns = ns,
       p("Fach/Fächergruppe:"),
       shinyWidgets::pickerInput(
         inputId = ns("subject_kurse_gender"),
         choices = c("MINT-Fächer (gesamt)",
                     "Mathematik",
                     "Informatik",
                     "Physik",
                     "Chemie",
                     "Biologie",
                     "andere Fächer (gesamt)",
                     "Deutsch",
                     "Fremdsprachen",
                     "Gesellschaftswissenschaften",
                     "Musik/Kunst",
                     "Religion/Ethik",
                     "Sport"),
         selected = "MINT-Fächer (gesamt)"
       ),

      br(),
      shinyBS::bsPopover(id="ih_schule_mint_5", title="",
                         content = paste0("In dieser Grafik ist in der ersten Einstellung zu sehen, wie viele Mädchen in der Oberstufe einen Grundkurs in MINT wählen, aufgeschlüsselt nach Bundesländern. Ein direkter Vergleich zwischen den Bundesländern ist allerdings schwierig, da eingie Länder Besonderheiten in der Fächerwahl aufweisen."),
                         trigger = "hover"),
      tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_5"),
      br(),

    ),

    br(),
    darstellung(id="popoverdarstellung_3"),
    br()
  )
}

#' schule_kurse_multiple Server Functions
#'
#' @noRd
mod_schule_kurse_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_kurse_gender, {
      r$ansicht_kurse_gender <- input$ansicht_kurse_gender
    })

     observeEvent(input$date_kurse, {
      r$date_kurse <- input$date_kurse
    })

    observeEvent(input$region_kurse_gender, {
      r$region_kurse_gender <- input$region_kurse_gender
    })

    observeEvent(input$subject_kurse_gender, {
      r$subject_kurse_gender <- input$subject_kurse_gender
    })

    observeEvent(input$indikator_kurse_gender, {
      r$indikator_kurse_gender <- input$indikator_kurse_gender
    })

    observeEvent(input$gegenwert_kurse_gender, {
      r$gegenwert_kurse_gender <- input$gegenwert_kurse_gender
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_multiple_ui("schule_kurse_multiple_1")

## To be copied in the server
# mod_schule_kurse_multiple_server("schule_kurse_multiple_1")

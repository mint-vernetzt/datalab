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
    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_kurse_comparison_gender"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Gruppenvergleich - Balkendiagramm",
                  "Kursvergleich - Hanteldiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_comparison_gender"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_kurse_comparison_gender"),
      choices = c("Deutschland",
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)",
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
                  "Thüringen"
      ),
      multiple = F,
      selected = "Deutschland"
    ),

    conditionalPanel(condition = sprintf("input['%s'] == 'Einzelansicht - Kuchendiagramm'",
                                         ns("ansicht_kurse_comparison_gender")),

    # "input.ansicht_kurse_comparison_gender == 'Einzelansicht - Kuchendiagramm'
    #                  || input.ansicht_kurse_comparison_gender == Gruppenvergleich - Balkendiagramm",
      # ns = ns,
       p("Kursniveau:"),
       shinyWidgets::pickerInput(
         inputId = ns("indikator_kurse_comparison_gender"),
         choices = c("Grundkurse", "Leistungskurse", "Oberstufenbelegungen"),
         selected = c("Grundkurse"),
         multiple = FALSE
       ),

    p("Nicht-MINT als Vergleich anzeigen?", style = "color: #b16fab;"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("gegenwert_kurse_comparison_gender"),
      choices = c("Ja", "Nein"),
      selected = "Nein",
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
    ),
    conditionalPanel(condition = sprintf("input['%s'] == 'Gruppenvergleich - Balkendiagramm'",
                                         ns("ansicht_kurse_comparison_gender")),
                     p("Nicht-MINT als Vergleich anzeigen?", style = "color: #b16fab;"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("gegenwert_kurse_comparison_gender_balken"),
                       choices = c("Ja", "Nein"),
                       selected = "Nein",
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     )
    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_frauen_1", title="",
                       content = paste0("Die erste Darstellung zeigt, dass der Anteil von Mädchen bzw. Frauen in allen MINT-Grundkursen in Deutschland 2023 54 % beträgt. In den MINT-Leistungskursen beträgt dieser Anteil 46 %. In den Nicht-MINT-Fächern ist der Anteil an Mädchen bzw. Frauen etwas höher: In Grundkursen machen Mädchen 54 % aus, in Leistungskursen sogar 57 %."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_frauen_1"),

    br(),
    br(),
    shinyBS::bsPopover(id="popoverdarstellung_2", title = "",
                       content = paste0("Falls die Grafik abgeschnitten dargestellt wird, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "popoverdarstellung_2"),

    br()
  )
}

#' schule_kurse_comparison_gender Server Functions
#'
#' @noRd
mod_schule_kurse_comparison_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_kurse_comparison_gender, {
      r$ansicht_kurse_comparison_gender <- input$ansicht_kurse_comparison_gender
    })

    observeEvent(input$date_kurse_comparison_gender, {
      r$date_kurse_comparison_gender <- input$date_kurse_comparison_gender
    })

    observeEvent(input$gegenwert_kurse_comparison_gender, {
      r$gegenwert_kurse_comparison_gender <- input$gegenwert_kurse_comparison_gender
    })

    observeEvent(input$gegenwert_kurse_comparison_gender_balken, {
      r$gegenwert_kurse_comparison_gender_balken <- input$gegenwert_kurse_comparison_gender_balken
    })

    observeEvent(input$region_kurse_comparison_gender, {
      r$region_kurse_comparison_gender <- input$region_kurse_comparison_gender
    })

    observeEvent(input$indikator_kurse_comparison_gender, {
      r$indikator_kurse_comparison_gender <- input$indikator_kurse_comparison_gender
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_comparison_gender_ui("schule_kurse_comparison_gender_1")

## To be copied in the server
# mod_schule_kurse_comparison_gender_server("schule_kurse_comparison_gender_1")

#' ausserschulisch_cp_projekte UI Functions
#'
#' @noRd
mod_ausserschulisch_cp_projekte_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Charakteristika:"),
    shinyWidgets::pickerInput(
      inputId = ns("chara_cp_pros"),
      label = NULL,
      choices = c("Zielgruppe", "spezifische Zielgruppe" = "weitere Zielgruppe",
                  "MINT-Disziplin", "weitere Disziplin",
                  "Aktivitätsgebiet" = "Region", "Format", "Finanzierung"),
      selected = "Zielgruppe"
    ),
    conditionalPanel(condition = "input.chara_cp_pros != 'Region'",
                     ns = ns,
                     p("Region:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("regio_cp_pros"),
                       label = NULL,
                       choices = c("Gesamt",
                                   "Bundesweit",
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
                       selected = "Gesamt"
                     )
    ),
    conditionalPanel(condition = "input.chara_cp_pros == 'Region'",
                     ns = ns,
                     p("Differenzierte Anzeige:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bula_cp_pros"),
                       label = NULL,
                       choices = c("Bundesländern zusammen anzeigen",
                                   "Bundesländern einzeln anzeigen"),
                       selected = "Bundesländern zusammen anzeigen"
                     )
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_rel_cp_pros"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_ausserschulisch_cp2", title="",
                       content = paste0("Betrachtet man die Anzahl der Projekte"),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_ausserschulisch_cp2")

  )
}

#' ausserschulisch_cp_projekte Server Functions
#'
#' @noRd
mod_ausserschulisch_cp_projekte_server <-function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$regio_cp_pros, {
      r$regio_cp_pros <- input$regio_cp_pros
    })

    observeEvent(input$chara_cp_pros, {
      r$chara_cp_pros <- input$chara_cp_pros
    })

    observeEvent(input$bula_cp_pros, {
      r$bula_cp_pros <- input$bula_cp_pros
    })

    observeEvent(input$abs_rel_cp_pros, {
      r$abs_rel_cp_pros <- input$abs_rel_cp_pros
    })

  })
}


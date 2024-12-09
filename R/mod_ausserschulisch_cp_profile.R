#' ausserschulisch_cp_profile UI Functions
#'
#' @noRd
mod_ausserschulisch_cp_profile_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Charakteristika:"),
    shinyWidgets::pickerInput(
      inputId = ns("chara_cp_prof"),
      label = NULL,
      choices = c("Region",
                  "Angebote & Gesuche"),
      selected = "Angebote & Gesuche"
    ),
    conditionalPanel(condition = "input.chara_cp_prof != 'Region'",
                     ns = ns,
                     p("Region:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("regio_cp_prof"),
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
                     ),
                     p("Getrennt anzeigen?"),
                     shinyWidgets::pickerInput(
                       inputId = ns("anz_cp_prof"),
                       label = NULL,
                       choices = c("Zusammen anzeigen",
                                   "Nur Gesuche anzeigen",
                                   "Nur Angebote anzeigen"),
                       selected = "Zusammen anzeigen"
                     )
    ),
    conditionalPanel(condition = "input.chara_cp_prof == 'Region'",
                     ns = ns,
                     p("Landkreise differenziert Anzeigen?"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bula_cp_prof"),
                       label = NULL,
                       choices = c("Bundesländern zusammen anzeigen",
                                   "Bundesländern einzeln anzeigen"),
                       selected = "Bundesländern zusammen anzeigen"
                     )
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_rel_cp_prof"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_ausserschulisch_cp3", title="",
                       content = paste0("Text fehlt noch"),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_ausserschulisch_cp3")

  )
}

#' ausserschulisch_cp_profile Server Functions
#'
#' @noRd
mod_ausserschulisch_cp_profile_server <-function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$regio_cp_prof, {
      r$regio_cp_prof <- input$regio_cp_prof
    })

    observeEvent(input$chara_cp_prof, {
      r$chara_cp_prof <- input$chara_cp_prof
    })

    observeEvent(input$anz_cp_prof, {
      r$anz_cp_prof <- input$anz_cp_prof
    })

    observeEvent(input$bula_cp_prof, {
      r$bula_cp_prof <- input$bula_cp_prof
    })

    observeEvent(input$abs_rel_cp_prof, {
      r$abs_rel_cp_prof <- input$abs_rel_cp_prof
    })

  })
}


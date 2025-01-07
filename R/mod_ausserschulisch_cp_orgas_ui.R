#' ausserschulisch_cp_orgas UI Functions
#'
#' @noRd
mod_ausserschulisch_cp_orgas_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Charakteristika:"),
    shinyWidgets::pickerInput(
      inputId = ns("chara_cp_orgas"),
      label = NULL,
      choices = c("Organisationstyp", "Arbeitsfokus" = "Fokus", "Aktivitätsgebiet" = "Region"),
      selected = "Organisationstyp"
    ),
    conditionalPanel(condition = "input.chara_cp_orgas != 'Region'",
                     ns = ns,
                    p("Region:"),
                    shinyWidgets::pickerInput(
                      inputId = ns("regio_cp_orgas"),
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
    conditionalPanel(condition = "input.chara_cp_orgas == 'Region'",
                     ns = ns,
                     p("Differenzierte Anzeige:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bula_cp_orgas"),
                       label = NULL,
                       choices = c("Bundesländern zusammen anzeigen",
                                   "Bundesländern einzeln anzeigen"),
                       selected = "Bundesländern zusammen anzeigen"
                     )
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_rel_cp_orgas"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    inthelp(
      id = "ih_ausserschulisch_cp1",
      content = "Die erste Darstellung zeigt: Ein Drittel der Organisationen sind gemeinnützige Organisationen, etwas mehr als jede fünfte ist eine Bildungseinrichtung. Dabei können Organisationen auch sowohl gemeinnützige Organisation als auch Bildungseinrichtung sein, denn der eigenen Organisation konnten auch mehrere passenden Organisationstypen zugeordnet werden."
    ),


    )
}

#' ausserschulisch_cp_orgas Server Functions
#'
#' @noRd
mod_ausserschulisch_cp_orgas_server <-function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$regio_cp_orgas, {
      r$regio_cp_orgas <- input$regio_cp_orgas
    })

    observeEvent(input$chara_cp_orgas, {
      r$chara_cp_orgas <- input$chara_cp_orgas
    })

    observeEvent(input$bula_cp_orgas, {
      r$bula_cp_orgas <- input$bula_cp_orgas
    })

    observeEvent(input$abs_rel_cp_orgas, {
      r$abs_rel_cp_orgas <- input$abs_rel_cp_orgas
    })

  })
}


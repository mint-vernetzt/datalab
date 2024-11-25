#' ausserschulisch_mvb_akteursbefragung UI Functions
#'
#' @noRd
mod_ausserschulisch_mvb_akteursbefragung_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Charakteristika:"),
    shinyWidgets::pickerInput(
      inputId = ns("chara_mvb_akteur"),
      label = NULL,
      choices = c("Arbeitsverhältnis", "Berufshintergrund", "Sektor",
                  "Kategorie", "Zielgruppen"),
      selected = "Arbeitsverhältnis"
    ),
    conditionalPanel(condition = "input.chara_mvb_akteur == 'Berufshintergrund' |
                     input.chara_mvb_akteur == 'Zielgruppen'",
                     ns = ns,
                     p("Darstellungsart:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("abs_rel_mvb_akteur"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     )
                    ),
    br(),
    shinyBS::bsPopover(id="ih_ausserschulisch_mvb1", title="",
                       content = paste0("Text fehlt noch"),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_ausserschulisch_mvb1")

  )
}

#' mod_ausserschulisch_mvb_akteursbefragung Server Functions
#'
#' @noRd
mod_ausserschulisch_mvb_akteursbefragung_server <-function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$chara_mvb_akteur, {
      r$chara_mvb_akteur <- input$chara_mvb_akteur
    })

    observeEvent(input$abs_rel_mvb_akteur, {
      r$abs_rel_mvb_akteur <- input$abs_rel_mvb_akteur
    })

  })
}


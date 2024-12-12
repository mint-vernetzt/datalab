#' ausserschulisch_mvb_genderbefragung UI Functions
#'
#' @noRd
mod_ausserschulisch_mvb_genderb_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Themenauswahl:"),
    shinyWidgets::pickerInput(
      inputId = ns("thema_wahl_gender"),
      label = NULL,
      choices = c("Vernetzungswunsch & Aktivität" = "Vernetzungswunsch", "Netzwerk"),
      selected = "Vernetzungswunsch"
    ),


    br(),
    shinyBS::bsPopover(id="ih_ausserschulisch_mvb3", title="",
                       content = paste0("Text fehlt noch"),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_ausserschulisch_mvb3")

  )
}

#' ausserschulisch_mvb_stimmungsb Server Functions
#'
#' @noRd
mod_ausserschulisch_mvb_genderb_server <-function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$thema_wahl_gender, {
      r$thema_wahl_gender <- input$thema_wahl_gender
    })


  })
}


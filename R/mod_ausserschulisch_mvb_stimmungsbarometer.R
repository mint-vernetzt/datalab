#' ausserschulisch_mvb_stimmungsbarometer UI Functions
#'
#' @noRd
mod_ausserschulisch_mvb_stimmungsb_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Antworten zum Ganztag zu:"),
    shinyWidgets::pickerInput(
      inputId = ns("frage_mvb_stimmung"),
      label = NULL,
      choices = c("Nutzung des Ganztags", "Abbau von Lernrückständen"),
      selected = "Nutzung des Ganztags"
    ),

   p("Gruppe:"),
   shinyWidgets::pickerInput(
     inputId = ns("gruppe_mvb_stimmung"),
     choices = c("Gesamt", "schulische Akteur:innen" = "Schule", "außerschulische Akteur:innen"),
     selected = "Gesamt"
    ),

    br(),
    shinyBS::bsPopover(id="ih_ausserschulisch_mvb2", title="",
                       content = paste0("Text fehlt noch"),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_ausserschulisch_mvb2")

  )
}

#' ausserschulisch_mvb_stimmungsb Server Functions
#'
#' @noRd
mod_ausserschulisch_mvb_stimmungsb_server <-function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$frage_mvb_stimmung, {
      r$frage_mvb_stimmung <- input$frage_mvb_stimmung
    })

    observeEvent(input$gruppe_mvb_stimmung, {
      r$gruppe_mvb_stimmung <- input$gruppe_mvb_stimmung
    })

  })
}


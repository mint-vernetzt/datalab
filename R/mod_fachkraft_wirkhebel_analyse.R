#' fachkraft_wirkhebel_analyse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_wirkhebel_analyse_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("fachkraft_item_wirkhebel_analyse"),
      label = NULL,
      choices = 2023:2037,
      selected = "2037"
    ),

    br(),
    shinyBS::bsPopover(id="dh_wirkhebel", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_wirkhebel"),
    br(),
    br(),

    # TODO extract into own module, since this is repeated on a lot of modules
    shinyBS::bsPopover(id="ih_fachkraft_item_wirkhebel_analyse", title="",
                       content = paste0("Die Grafikt zeigt in der obersten Zeile, wie viele MINT-Fachkräfte durch ein Zusammenspiel aller betrachteten Wirkhebel gewonnen werden könnten. Darunter werden die Einzeleffekte aller Wirkhebel dargestellt. Z. B. kann eine Förderung in der MINT-Bildung knapp 800.000 Fachkräfte zusätzlich bis 2037 gewinnen."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_item_wirkhebel_analyse")
  )
}

#' fachkraft_wirkhebel_analyse Server Functions
#'
#' @noRd
mod_fachkraft_wirkhebel_analyse_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$fachkraft_item_wirkhebel_analyse, {
      r$fachkraft_item_wirkhebel_analyse <- input$fachkraft_item_wirkhebel_analyse
    })

  })
}

## To be copied in the UI
# mod_fachkraft_wirkhebel_analyse_ui("fachkraft_wirkhebel_analyse_1")

## To be copied in the server
# mod_fachkraft_wirkhebel_analyse_server("fachkraft_wirkhebel_analyse_1")


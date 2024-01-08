#' fachkraft_item_mint UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_mint_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("map_y_fachkraft_arbeit_mint"),
      label = NULL,
      choices = fachkraft_ui_years(),
      selected = "2022"
    ),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_bl_fachkraft_arbeit_mint"),
      choices = fachkraft_ui_berufslevel(),
      selected = c("Gesamt"),
      multiple = FALSE
    ),

    br(),

    # TODO extract into own module, since this is repeated on a lot of modules

    shinyBS::bsPopover(id="dh_fachkraft_epa", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_fachkraft_epa"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_fachkraft_epa", title="",
                       content = paste0("Die linke Karte der ersten Einstellung zeigt, dass die beiden Bundesländer mit dem höchsten Anteil von Informatik-Studierenden Bayern und Schleswig-Holstein mit jeweils 10 % sind."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_epa")


  )
}

#' fachkraft_item_mint Server Functions
#'
#' @noRd
mod_fachkraft_item_mint_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$map_y_fachkraft_arbeit_mint, {
      r$map_y_fachkraft_arbeit_mint <- input$map_y_fachkraft_arbeit_mint
    })

    observeEvent(input$map_bl_fachkraft_arbeit_mint, {
      r$map_bl_fachkraft_arbeit_mint <- input$map_bl_fachkraft_arbeit_mint
    })
  })
}

## To be copied in the UI
# mod_fachkraft_item_mint_ui("fachkraft_item_mint_1")

## To be copied in the server
# mod_fachkraft_item_mint_server("fachkraft_item_mint_1")

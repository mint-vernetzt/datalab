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
      selected = "2023"
    ),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_bl_fachkraft_arbeit_mint"),
      choices = fachkraft_ui_berufslevel(),
      selected = c("Gesamt"),
      multiple = FALSE
    ),

    br(),

    shinyBS::bsPopover(id="ih_fachkraft-berufsgruppen_2", title="",
                       content = paste0("Bei den Engpassberufen ist die Verteilung von MINT- und Nicht-MINT-Berufsgruppen noch 50 : 50.<br> Dagegen verteilen sich die Berufe ohne Fachkräfteengpass auf nur 49 MINT-Berufsgruppen (32%) und 104 Nicht-MINT-Berufsgruppen (68%)."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft-berufsgruppen_2")

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

#' fachkraft_item_epa UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_epa_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("map_y_fachkraft_arbeit_epa"),
      label = NULL,
      choices = fachkraft_ui_years(),
      selected = "2023"
    ),

    p("Fachbereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_f_fachkraft_arbeit_epa"),
      choices = fachkraft_ui_faecher(),
      selected = c("MINT gesamt", "Nicht MINT"),
      multiple = TRUE,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "<span style='color: red;'>Maximal 2 Indikatoren auswählen</span>")
    ),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_bl_fachkraft_arbeit_epa"),
      choices = fachkraft_ui_berufslevel(),
      selected = c("Gesamt"),
      multiple = FALSE
      ),

    br(),

    shinyBS::bsPopover(id="ih_fachkraft-berufsgruppen_1", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass 47% der MINT-Berufe als Engpassberufe gezählt werden. <br>Dagegen liegt nur in 33% der \"Nicht-MINT-Berufe\" ein Fachkräfteengpass vor."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft-berufsgruppen_1")

  )
}

#' fachkraft_item_epa Server Functions
#'
#' @noRd
mod_fachkraft_item_epa_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$map_y_fachkraft_arbeit_epa, {
      r$map_y_fachkraft_arbeit_epa <- input$map_y_fachkraft_arbeit_epa
    })

    observeEvent(input$map_f_fachkraft_arbeit_epa, {
      r$map_f_fachkraft_arbeit_epa <- input$map_f_fachkraft_arbeit_epa
    })

    observeEvent(input$map_bl_fachkraft_arbeit_epa, {
      r$map_bl_fachkraft_arbeit_epa <- input$map_bl_fachkraft_arbeit_epa
    })

  })
}

## To be copied in the UI
# mod_fachkraft_item_epa_ui("fachkraft_item_epa_1")

## To be copied in the server
# mod_fachkraft_item_epa_server("fachkraft_item_epa_1")

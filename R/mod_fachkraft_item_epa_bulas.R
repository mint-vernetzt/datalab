#' fachkraft_item_epa_bulas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_epa_bulas_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("y_fachkraft_epa_bulas"),
      label = NULL,
      choices = fachkraft_ui_years(),
      selected = "2023"
    ),

    p("Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("regio_fachkraft_epa_bulas"),
      choices = c("Baden-Württemberg",
                  "Bayern",
                  "Berlin / Brandenburg",
                  "Brandenburg / Berlin",
                  "Hamburg",
                  "Hessen",
                  "Mecklenburg-Vorpommern",
                  "Niedersachsen / Bremen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz / Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Schleswig-Holstein / Hamburg",
                  "Thüringen"
      ),
      multiple = FALSE,
      selected = c("Mecklenburg-Vorpommern")
    ),

    p("Fachbereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("f_fachkraft_epa_bulas"),
      choices = fachkraft_ui_faecher(),
      selected = c("MINT gesamt", "Nicht MINT"),
      multiple = TRUE,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "<span style='color: red;'>Maximal 2 Indikatoren auswählen</span>")
    ),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("bl_fachkraft_epa_bulas"),
      choices = fachkraft_ui_berufslevel(),
      selected = c("Gesamt"),
      multiple = FALSE
    ),

    br(),

    shinyBS::bsPopover(id="ih_fachkraft-bulas_1", title="",
                       content = paste0(""),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft-bulas_1")

  )
}

#' fachkraft_item_epa Server Functions
#'
#' @noRd
mod_fachkraft_item_epa_bulas_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$y_fachkraft_epa_bulas, {
      r$y_fachkraft_epa_bulas <- input$y_fachkraft_epa_bulas
    })

    observeEvent(input$regio_fachkraft_epa_bulas, {
      r$regio_fachkraft_epa_bulas <- input$regio_fachkraft_epa_bulas
    })

    observeEvent(input$f_fachkraft_epa_bulas, {
      r$f_fachkraft_epa_bulas <- input$f_fachkraft_epa_bulas
    })

    observeEvent(input$bl_fachkraft_epa_bulas, {
      r$bl_fachkraft_epa_bulas <- input$bl_fachkraft_epa_bulas
    })

  })
}

## To be copied in the UI
# mod_fachkraft_item_epa_ui("fachkraft_item_epa_1")

## To be copied in the server
# mod_fachkraft_item_epa_server("fachkraft_item_epa_1")

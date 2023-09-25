#' international_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_map_ui <- function(id) {

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l"),
      choices = c("EU", "OECD", "Weltweit"),
      selected = "EU",
      multiple = FALSE#,
      # options =  list(
      #   "max-options" = 2,
      #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l == 'EU'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_eu"),
                       choices = international_ui_faecher(region = "EU"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE#,
                       # options =  list(
                       #   "max-options" = 2,
                       #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
                     )),
    conditionalPanel(condition = "input.map_l == 'OECD' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_oecd"),
                       label = NULL,
                       choices = international_ui_years(region = "OECD"),
                       selected = "2020"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_oecd"),
                       choices = international_ui_faecher(region = "OECD"),
                       selected = c("Alle"),
                       multiple = FALSE#,
                       # options =  list(
                       #   "max-options" = 2,
                       #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
                     )),

    br(),

    # TODO extract into own module, since this is repeated on a lot of modules

    shinyBS::bsPopover(id="dh_international_map", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_international_map"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_international_map", title="",
                       content = paste0("Die linke Karte der ersten Einstellung zeigt, dass die beiden Bundesländer mit dem höchsten Anteil von Informatik-Studierenden Bayern und Schleswig-Holstein mit jeweils 10 % sind."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_map")
  )

}

#' international_map Server Functions
#'
#' @noRd
mod_international_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$map_l, {
      r$map_l <- input$map_l
      if (input$map_l == "EU") {
        r$map_y <- input$map_y_eu
        r$map_f <- input$map_f_eu
      }
      if (input$map_l == "OECD") {
        r$map_y <- input$map_y_oecd
        r$map_f <- input$map_f_oecd
      }
    })

    observeEvent(input$map_y_oecd, {
      r$map_y <- input$map_y_oecd
    })

    observeEvent(input$map_f_oecd, {
      r$map_f <- input$map_f_oecd
    })

    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$map_y_eu, {
      r$map_y <- input$map_y_eu
    })

    observeEvent(input$map_f_eu, {
      r$map_f <- input$map_f_eu
    })


  })
}

## To be copied in the UI
# mod_international_map_ui("international_map_1")

## To be copied in the server
# mod_international_map_server("international_map_1")

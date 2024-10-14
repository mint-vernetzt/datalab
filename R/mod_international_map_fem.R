#' international_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_map_fem_ui <- function(id) {

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l_f"),
      choices = c("Europa" = "EU", "OECD"),
      selected = "Europa",
      multiple = FALSE#,
      # options =  list(
      #   "max-options" = 2,
      #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l_f == 'EU'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu_f"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_eu_f"),
                       choices = c("MINT" = "Alle MINT-Fächer",
                                   "---Informatik & Kommunikationstechnologie"="Informatik & Kommunikationstechnologie",
                                   "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                   "---Naturwissenschaften, Mathematik und Statistik"="Naturwissenschaften, Mathematik und Statistik"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE
                     ),
                     p("Darstellungsart:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_betr_eu_f"),
                       choices = c("Frauenanteil in MINT" = "Anteil von Frauen an Allen",
                                   "Anteil MINT-Studierende unter Frauen" = "Anteil an Frauen von Frauen"),
                       selected = c("Frauenanteil in MINT"),
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.map_l_f == 'OECD' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_oecd_f"),
                       label = NULL,
                       choices = international_ui_years(region = "OECD"),
                       selected = "2020"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_oecd_f"),
                       choices = c("MINT",
                                   "---Informatik & Kommunikationstechnologie" = "Informatik & Kommunikationstechnologie",
                                   "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" = "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                   "---Naturwissenschaften, Mathematik und Statistik"= "Naturwissenschaften, Mathematik und Statistik"),
                       selected = c("MINT"),
                       multiple = FALSE
                     ),

                     # p("Anforderungsniveau:"),
                     # shinyWidgets::pickerInput(
                     #   inputId = ns("map_lev_oecd_f"),
                     #   choices = c("Bachelor oder vergleichbar (akademisch)",
                     #               "Master oder vergleichbar (akademisch)",
                     #               "Promotion (ISCED 8)"),
                     #   selected = c("Promotion (ISCED 8)"),
                     #   multiple = FALSE),

                     p("Darstellungsart:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_betr_oecd_f"),
                       choices = c("Frauenanteil in MINT" = "Anteil von Frauen an Allen",
                                   "Anteil MINT-Studierende unter Frauen" = "Anteil an Frauen von Frauen"),
                       selected = c("Frauenanteil in MINT"),
                       multiple = FALSE
                     )
    ),

    br(),

    # shinyBS::bsPopover(id="dh_international_map", title = "",
    #                    content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
    #                    placement = "top",
    #                    trigger = "hover"),
    # tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_international_map"),
    # br(),
    # br(),
    shinyBS::bsPopover(id="ih_international_map2", title="",
                       content = paste0("Die Karte zeigt den Frauenanteil unter MINT-Studierenden im Ländervergleich. In der ersten Einstellung sieht man beispielsweise, dass der Frauenanteil in MINT in den baltischen Staaten und Schweden etwas höher liegt als in den Ländern Mitteleuropas."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_map2")
  )


}

#' international_map Server Functions
#'
#' @noRd
mod_international_map_fem_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$map_l_f, {
      r$map_l_f <- input$map_l_f
      if (input$map_l_f == "EU") {
        r$map_y_f <- input$map_y_eu_f
        r$map_f_f <- input$map_f_eu_f
        r$map_le_betr <- input$map_betr_eu_f

      }
      if (input$map_l_f == "OECD") {
        r$map_y_f <- input$map_y_oecd_f
        r$map_f_f <- input$map_f_oecd_f
        r$map_le_f <- input$map_lev_oecd_f
        r$map_le_betr <- input$map_betr_oecd_f
      }
    })

    observeEvent(input$map_y_oecd_f, {
      r$map_y_f <- input$map_y_oecd_f
    })

    observeEvent(input$map_f_oecd_f, {
      r$map_f_f <- input$map_f_oecd_f
    })

    observeEvent(input$map_lev_oecd_f, {
      r$map_le_f <- input$map_lev_oecd_f
    })

    observeEvent(input$map_betr_oecd_f, {
      r$map_le_betr <- input$map_betr_oecd_f
    })

    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$map_y_eu_f, {
      r$map_y_f <- input$map_y_eu_f
    })

    observeEvent(input$map_f_eu_f, {
      r$map_f_f <- input$map_f_eu_f
    })

    observeEvent(input$map_betr_eu_f, {
      r$map_le_betr <- input$map_betr_eu_f
    })



  })
}

## To be copied in the UI
# mod_international_map_ui("international_map_1")

## To be copied in the server
# mod_international_map_server("international_map_1")

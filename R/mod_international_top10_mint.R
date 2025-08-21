#' international_top10_mint UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_top10_mint_ui <- function(id){

  # logger::log_debug("start mod_international_top10_mint_ui")

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l_int_top10"),
      choices = c("Europa"="EU", "OECD", "Weltweit"),
      selected = "Europa",
      multiple = FALSE#,
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l_int_top10 == 'EU'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu_int_top10"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_eu_int_top10"),
                       choices = international_ui_faecher(region = "EU"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE#,

                     )),
    conditionalPanel(condition = "input.map_l_int_top10 == 'OECD' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_oecd_int_top10"),
                       label = NULL,
                       choices = international_ui_years(region = "OECD"),
                       selected = "2020"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_oecd_int_top10"),
                       choices = international_ui_faecher(region = "OECD"),
                       selected = c("MINT"),
                       multiple = FALSE#,

                     )),
    conditionalPanel(condition = "input.map_l_int_top10 == 'Weltweit' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_ww_int_top10"),
                       label = NULL,
                       choices = international_ui_years(region = "Weltweit"),
                       selected = "2020"
                     )),

    p("Durchschnitt anzeigen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("show_avg_top10_mint_line"),
      choices = c("Ja", "Nein"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),


    br(),
    darstellung(id="dh_international_map_3"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_international_map3", title="",
                       content = paste0("Die Darstellung zeigt, dass Deutschland im europäischen Vergleich den höchsten Anteil MINT-Studierender verzeichnet, gefolgt von Serbien und Finnland. Deutschland liegt mit ca. 36 % MINT-Studierenden deutlich über dem Europa-Durchschnitt von ca. 26 %."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_map3")
  )
}

#' international_top10_mint Server Functions
#'
#' @noRd
mod_international_top10_mint_server <- function(id, r){

  # logger::log_debug("start mod_international_top10_mint_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$show_avg_top10_mint_line, {
      r$show_avg_top10_mint_line_m <- input$show_avg_top10_mint_line
    })


    observeEvent(input$map_l_int_top10, {

      r$map_l_m <- input$map_l_int_top10
      r$show_avg_top10_mint_line <- input$show_avg_top10_mint_line

      if (input$map_l_int_top10 == "EU") {
        r$map_y_m <- input$map_y_eu_int_top10
        r$map_f_m <- input$map_f_eu_int_top10
      }
      if (input$map_l_int_top10 == "OECD") {
        r$map_y_m <- input$map_y_oecd_int_top10
        r$map_f_m <- input$map_f_oecd_int_top10

      }
      if (input$map_l_int_top10 == "Weltweit"){
        r$map_y_m <- input$map_y_ww_int_top10
      }
    })

    observeEvent(input$map_y_ww_int_top10, {
      r$map_y_m <- input$map_y_ww_int_top10
    })


    observeEvent(input$map_y_oecd_int_top10, {
      r$map_y_m <- input$map_y_oecd_int_top10
    })

    observeEvent(input$map_f_oecd_int_top10, {
      r$map_f_m_oecd <- input$map_f_oecd_int_top10

    })

    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up

    observeEvent(input$map_y_eu_int_top10, {
      r$map_y_m <- input$map_y_eu_int_top10
    })

    observeEvent(input$map_f_eu_int_top10, {
      r$map_f_m_eu <- input$map_f_eu_int_top10

    })

  })
}

## To be copied in the UI
# mod_international_top10_mint_ui("international_top10_mint_1")

## To be copied in the server
# mod_international_top10_mint_server("international_top10_mint_1")

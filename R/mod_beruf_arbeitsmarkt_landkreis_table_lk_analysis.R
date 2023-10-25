#' beruf_arbeitsmarkt_landkreis_table_lk_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_landkreis_table_lk_analysis_ui <- function(id){

  ns <- NS(id)

  tagList(
    div(id = id,
        fluidRow(
          p(),
          column(4,
                 shinyWidgets::pickerInput(ns("kategorie_beruf_arbeitsmarkt_landkreis_vergleich"),
                                           choices = c("Auszubildende",
                                                       "Beschäftigte"
                                           )
                 )
          ),
          column(4,
                 shinyWidgets::pickerInput(
                   inputId = ns("fachbereich_beruf_arbeitsmarkt_landkreis_vergleich"),
                   choices = c("Gesamt" = "Alle",
                               "in MINT" = "MINT",
                               "im Bereich Mathematik / Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                               "im Bereich Informatik" = "Informatik",
                               "im Bereich Technik" = "Technik (gesamt)",
                               "im Bereich Technik - Landtechnik" = "Landtechnik",
                               "im Bereich Technik - Produktionstechnik" = "Produktionstechnik",
                               "im Bereich Technik - Bau- und Gebäudetechnik" = "Bau- und Gebäudetechnik",
                               "im Bereich Technik - Verkehrs-, Sicherheits- und Veranstaltungstechnik" = "Verkehrs-, Sicherheits- u. Veranstaltungstechnik",
                               "im Bereich Technik - Gesundheitstechnik" = "Gesundheitstechnik"
                   ),
                   selected = "MINT",
                   multiple = FALSE
                 ),
          ),
          column(4,
                 conditionalPanel(condition = "input.kategorie_beruf_arbeitsmarkt_landkreis_vergleich == 'Auszubildende'",
                                  ns = ns,
                                  shinyWidgets::pickerInput(
                                    inputId = ns("indikator1_beruf_arbeitsmarkt_landkreis_vergleich"),
                                    choices = c("Gesamt (alle der Hauptkategorie)" = "Auszubildende",
                                                "weiblich - männlich" = "Frauen",
                                                "ausländisch - deutsch" = "ausländische Auszubildende",
                                                "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)"
                                    ),
                                    selected = "Gesamt (alle der Hauptkategorie)",
                                    multiple = FALSE
                                  )),
                 conditionalPanel(condition = "input.kategorie_beruf_arbeitsmarkt_landkreis_vergleich == 'Beschäftigte'",
                                  ns = ns,
                                  shinyWidgets::pickerInput(
                                    inputId = ns("indikator2_beruf_arbeitsmarkt_landkreis_vergleich"),
                                    choices = c("Gesamt (alle der Hauptkategorie)" = "Beschäftigte",
                                                "weiblich - männlich" = "Frauen",
                                                "ausländisch - deutsch" = "ausländische Beschäftigte",
                                                "u25" = "Beschäftigte u25",
                                                "25-55" = "Beschäftigte 25-55",
                                                "ü55" =  "Beschäftigte ü55"
                                    ),
                                    selected = "Gesamt (alle der Hauptkategorie)",
                                    multiple = FALSE
                                  ))

          )
        )
    )
  )
}

#' beruf_arbeitsmarkt_landkreis_table_lk_analysis Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_landkreis_table_lk_analysis_server <- function(id, btn, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_landkreis_table_lk_analysis_ui("beruf_arbeitsmarkt_landkreis_table_lk_analysis_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_landkreis_table_lk_analysis_server("beruf_arbeitsmarkt_landkreis_table_lk_analysis_1")

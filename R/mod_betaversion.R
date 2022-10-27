#' Unterseite "Betaversion" UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_betaversion_ui <- function(id){
  ns <- NS(id)
  tagList(


  )
}

#' Startseite Server Functions
#'
#' @noRd
mod_betaversion_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


mod_betaversion_ui <- function(id){
  ns <- NS(id)
  tagList(


    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_breiter_Betaversion.jpg',
            class = "img-responsive",
            height = "300px",
            # width = "150px",
            alt = "Banner Betaversion",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

    fluidRow(
        shinydashboard::box(
          width = 9,
          title = "Betaversion",
          p(style = "text-align: justify; font-size = 16px",
            "Das Projekt MINT-DataLab ist 2021 gestartet. Unser Ziel ist es, bis 2025 ein umfassendes MINT-DataLab aufzubauen.
            Dies soll bestehende amtliche Statistiken zu MINT und Studienergebnisse aus den Bereichen Frühkindliche Bildung, Schulbildung, außerschulische Bildung,
            Hochschulbildung, berufliche Ausbildung, Weiterbildung sowie Arbeitsmarkt umfassen.",
            br(), br(),
            "Bei MINTvernetzt erheben wir selbst Daten zum Bereich außerschulische Bildung, die wir
            perspektivisch hier einbinden werden."
          ))),

    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "What's coming next: Welche neuen Datensätze und Services sind gerade in Pipeline?",
        p(style = "text-align: justify; font-size = 16px",
          tags$li("Absolute Zahlen: Bisher zeigen wir nur Prozentangaben. In der nächsten Entwicklungsphase werden wir
                  auch die absoluten Zahlen integrieren."), br(),
          tags$li("Download-Option für Diagramme: Unser Ziel ist es, dass unsere Diagramme bestmöglich weiterverwendet werden können. Deshalb werden wir bald eine Download-Option ergänzen."), br(),
          tags$li("Barrierefreiheit der Grafiken"), br(),
          tags$li("Regionale Daten: Bisher zeigen wir Daten auf Bundes- und Landesebene. Unser Ziel ist es, auch Vergleiche bzw. Aufwertungen auf regionaler Ebene zu ermöglichen.")
        ), br(), br(),

        p(style = "text-align: justify; font-size = 16px",
          span("Wir freuen uns immer über Rückfragen oder Feedback ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
               tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
          )
        )
      )),



     # Footer

   tags$footer(style="text-align: justify;background-color:white",

                      div(style="display: inline-block;position: relative;padding: 1em;",

                          tags$a(href="https://mint-vernetzt.de/",
                                 img(src='www/MINTv_tranparent.png',
                                     class = "img-responsive",
                                     height = "100px", width = "100px",
                                     alt = "Logo MINT", target="_blank",
                                     style="display: inline-block; margin-left: auto; margin-right:10%;"))),

                      div(style="display: inline-block;position: relative;padding: 1em;",

                          p(tags$a("Impressum", href="#shiny-tab-impressum", "data-toggle" = "tab")," | ",
                            tags$a("Kontakt", href="#shiny-tab-kontakt", "data-toggle" = "tab")," | ",
                            tags$a("Datenschutz", href="#shiny-tab-datenschutz", "data-toggle"="tab"),HTML('&nbsp;'),HTML('&nbsp;'),
                            "Copyright © 2022. Alle Rechte vorbehalten Stifterverband")),

                      div(style="display: inline-block;position: relative;padding: 1em;",

                          tags$a(href="https://www.bmbf.de/bmbf/de/home/home_node.html",
                                 img(src='www/BMBF-Logo_transp1.png',

                                     class = "img-responsive",

                                     height = "200px", width = "200px",

                                     alt = "Logo BMBF", target="_blank",

                                     style="display: inline-block; margin-left: auto; margin-right: auto;"))),

                      div(style="display: inline-block;width: 100%;",

                          " ")



      ))}

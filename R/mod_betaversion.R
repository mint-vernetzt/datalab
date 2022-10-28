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
        img(src='www/Banner_BETA.jpg',
            class = "img-responsive",
            height = "300px",
            # width = "150px",
            alt = "Banner Betaversion",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

    fluidRow(
        shinydashboard::box(
          width = 9,
          title = "Wo wir stehen: Betaversion",
          p(style = "text-align: justify; font-size = 16px",
            "Das Projekt MINT-DataLab ist 2021 gestartet. Unser Ziel ist es, bis 2025 mit den uns zur Verfügungen stehenden Ressourcen ein umfassendes MINT-DataLab aufzubauen.
            Dies soll bestehende amtliche Statistiken zu MINT und Studienergebnisse aus den Bereichen Frühkindliche Bildung, Schulbildung, außerschulische Bildung,
            Hochschulbildung, berufliche Ausbildung, Weiterbildung sowie Arbeitsmarkt umfassen.",
            br(), br(),
            "Bei MINTvernetzt erheben wir selbst Daten zum Bereich außerschulische Bildung, die wir
            perspektivisch hier einbinden werden."

          ),
          p(style = "text-align: justify; font-size = 16px",
            "Das MINT-DataLab ist ein Angebot aus dem Projekt ", tags$a(href="https://mint-vernetzt.de/", "MINTvernetzt", target="_blank"), "."
          ))),

    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Was wir vorhaben: Welche neuen Datensätze und Darstellungen sind gerade in Pipeline?",
        p(style = "text-align: justify; font-size = 16px",

          "Neue Datensätze",
          tags$li("Regionale Daten: Bisher zeigen wir Daten auf Bundes- und Landesebene. Unser Ziel ist es, auch Vergleiche bzw. Aufwertungen auf regionaler Ebene zu ermöglichen."),
          tags$li("Mehr Bereiche abdecken: Frühkindliche Bidlung, außerschulsche Bildung und Weiterbildung fehlen noch im MINT-DataLab.
          Je nach Datenverfügbarkeit bemühen wir uns diese Bereiche ebenfalls hier zu beleuchten. Auch allgemeine demografische und sozioökonomische Kennzahlen werden wir aufnehmen."),
          tags$li("Mehr inhaltliche Dimensionen: Bisher zeigen wir nur die Anteile von MINT und den Anteil Frauen in MINT. Besonders für den Schulbereich werden wir noch Kompetenzdaten ergänzen.
                  Außerdem wollen wir unsere Auswertungen um ausländsiche Studierende, Auszubildende und Fachkräfte erweitern."),
          tags$li("Fokus auf Fachkräfte: Die Kennzahlen um Beschäftige werden wir noch um differenziertere Kennzahlen zu Fachkräften erweitern."),

          "Neue Darstellungen", br(),
          tags$li("Absolute Zahlen: Bisher zeigen wir nur Prozentangaben. In der nächsten Entwicklungsphase werden wir
                  auch die absoluten Zahlen integrieren."),
          tags$li("Download-Option für Diagramme: Unser Ziel ist es, dass unsere Diagramme bestmöglich weiterverwendet werden können. Deshalb werden wir bald eine Download-Option ergänzen."),
          tags$li("Barrierefreiheit der Grafiken: Unsere Grafiken sind leider noch nicht barrierefrei. Hier bemühen wir uns um Verbesserungen!"),
          br(), br(),

        p(style = "text-align: justify; font-size = 16px",
          span("Wir freuen uns immer über Rückfragen oder Feedback ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
               tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
          )
        )
      ))),



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

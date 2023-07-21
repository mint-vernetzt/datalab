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
        img(src='www/Banner_Betaversion.jpg',
            class = "img-responsive",
            height = "300px",
            # width = "150px",
            alt = "Banner Betaversion",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

    fluidRow(
        shinydashboard::box(
          width = 12,
          title = "Wo wir stehen: Betaversion",
          p(style = "text-align: justify; font-size = 16px",
            "Update März 2023: Jetzt sind auch die Daten für 2021 verfügbar! Für den Bereich 'Ausbildung und Beruf' stehen jetzt außerdem Daten auf Ebene der Landkreise zur Auswahl.", br(), br(),

            "Das Projekt MINT-DataLab ist 2021 gestartet. Seit Herbst 2022 ist das MINT-DataLab online. Unser Ziel ist es, bis 2025 mit den uns zur Verfügungen stehenden Ressourcen ein umfassendes MINT-DataLab aufzubauen.
            Dies soll bestehende amtliche Statistiken zu MINT und Studienergebnisse aus den Bereichen Frühkindliche Bildung, Schulbildung, außerschulische Bildung,
            Hochschulbildung, berufliche Ausbildung, Weiterbildung sowie Arbeitsmarkt umfassen.",
            br(), br(),

            "Das MINT-DataLab ist ein Angebot aus dem Projekt ", tags$a(href="https://mint-vernetzt.de/", "MINTvernetzt", target="_blank"), "."
          ))),

    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Woran wir arbeiten: Neuen Datensätze und Darstellungen",
        column(8,
          p(style = "text-align: justify; font-size = 18px",
          tags$b(span("Neue Datensätze", style = "color:#154194"))),
          p(style = "text-align: justify; font-size = 16px",
          # tags$b(span("Regionale Daten:")),
          # "Bisher zeigen wir Daten auf Bundes- und Landesebene. Unser Ziel ist es, auch Vergleiche bzw. Auswertungen auf regionaler Ebene zu ermöglichen.",
          # br(),
          tags$b(span("Mehr Bereiche abdecken:")),
          "Frühkindliche Bildung, außerschulsche Bildung und Weiterbildung fehlen noch im MINT-DataLab.
          Je nach Datenverfügbarkeit bemühen wir uns, diese Bereiche ebenfalls hier zu beleuchten. Bei MINTvernetzt erheben wir selbst Daten zum Bereich außerschulische Bildung, die wir
            perspektivisch hier einbinden werden. Auch allgemeine demografische und sozioökonomische Kennzahlen werden wir aufnehmen.",
          br(),
        tags$b(span("Mehr inhaltliche Dimensionen:")),
          "Bisher zeigen wir nur die Anteile von MINT und den Anteil Frauen in MINT. Besonders für den Schulbereich werden wir noch Kompetenzdaten ergänzen.",
        br(),
          tags$b(span("Fokus auf Fachkräfte:")),
          "Die Kennzahlen um Beschäftige werden wir noch um differenziertere Kennzahlen zu Fachkräften erweitern."),

         p(style = "text-align: justify; font-size = 18px",
          tags$b(span("Neue Darstellungen", style = "color:#154194"))),
          p(style = "text-align: justify; font-size = 16px",
          tags$b(span("Absolute Zahlen:")),
                "Bisher zeigen wir nur Prozentangaben. In der nächsten Entwicklungsphase werden wir
                  auch die absoluten Zahlen integrieren.",
          br(),
          tags$b(span("Download-Option für Diagramme:")),
          "Unser Ziel ist es, dass unsere Diagramme bestmöglich weiterverwendet werden können. Deshalb werden wir bald eine Download-Option ergänzen.",
          br(),
          tags$b(span("Barrierefreiheit der Grafiken:")),
          "Unsere Grafiken sind leider noch nicht barrierefrei. Hier bemühen wir uns um Verbesserungen!",
          br(), br()),

        p(style = "text-align: justify; font-size = 18px",
          tags$b(span("Fragen oder Feedback?", style = "color:#154194"))),

        p(style = "text-align: justify; font-size = 16px",
          span("Wir freuen uns immer über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
               tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!")
          )
      ),
      column(4,
             p(br()),
             img(src='www/wordcloud.jpg',
                 class = "img-responsive",
                 #height = "150px", width = "150px",
                 alt = "wordcloud",
                 style="display: block; margin-left: auto; margin-right: auto;"
             ))
      )
      ),funct_footer()





   )}

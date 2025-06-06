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
            "Update Juni 2024: Seit Januar zeigt das MINT-DataLab auch internationale Vergleichsdaten. Eine weitere neue
            Fokusseite behandelt das Thema Fachkräfte. Hier zeigen wir u.a. Zukunftsszenarien der Fachkräfteentwicklung, welche extra
            für MINTvernetzt berechnet wurden. Außerdem gibt es die ersten Grafiken jetzt auch zum Download.",
            br(), br(),

            "Aktuell arbeiten wir an: Dem Ausbau der Downloadfunktion, einer Suchfunktion, der Visualisierung der MINT-Bildungslandschaft und
            der Gehälter in MINT- vs. Nicht-MINT-Berufen.",
            br(), br(),

            "Das Projekt MINT-DataLab ist 2021 gestartet. Seit Herbst 2022 ist das MINT-DataLab online. Unser Ziel ist es, bis 2025 mit den uns zur Verfügung stehenden Ressourcen ein umfassendes MINT-DataLab aufzubauen.
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
          tags$b(span("Mehr Bereiche abdecken:")),
          "Frühkindliche Bildung, außerschulische Bildung und Weiterbildung fehlen noch zum größten Teil im MINT-DataLab.
          Je nach Datenverfügbarkeit bemühen wir uns, diese Bereiche ebenfalls hier zu beleuchten. Bei MINTvernetzt erheben wir selbst Daten zum Bereich außerschulische Bildung.
          Diese werden wir demnächst in das MINT-DataLab integrieren können. Außerdem wollen wir Möglichkeiten ausloten, auch darüber hinaus die MINT-Bildungslandschaft darzustellen.",
          br(),
         tags$b(span("Mehr inhaltliche Dimensionen:")),
          "Zu den Anteilen von MINT und Frauen in MINT sind Daten von internationalen Studierenden und Arbeitskräften, Kompetenzdaten von Schüler:innen sowie erste Arbeitsmarkt-Daten für verschiedenen Altersgruppen hinzugekommen.
         Wir wollen hier noch weiter ausbauen und z. B. Absolvent*innen-Daten und Stundentafeln der MINT-Schulfächer ergänzen.",
         br(),
          ),
         p(style = "text-align: justify; font-size = 18px",
          tags$b(span("Neue Darstellungen", style = "color:#154194"))),
          tags$b(span("Download-Option für Diagramme:")),
          "Unser Ziel ist es, dass unsere Diagramme bestmöglich weiterverwendet werden können. Deshalb werden wir die Download-Option Schrittweise weiter ausbauen.",
          br(),
          tags$b(span("Barrierefreiheit der Grafiken:")),
          "Unsere Grafiken sind leider noch nicht barrierefrei. Hier bemühen wir uns um Verbesserungen!",
          br(),
         tags$b(span("Suchfunktion:")),
         "Mit immer mehr Daten im MINT-DataLab soll dennoch schnell die Grafik gefunden werden, die gerade gesucht wird. Wir wollen deshalb versuchen,
         eine Suchfunktion umzusetzen.",
         br(),br(),

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
                 alt = "wordcloud",
                 style="display: block; margin-left: auto; margin-right: auto;"
             ))
      )
      ),funct_footer()





   )}

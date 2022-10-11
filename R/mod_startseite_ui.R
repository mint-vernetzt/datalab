#' Startseite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_startseite_ui <- function(id){
  ns <- NS(id)
  tagList(


  )
}

#' Startseite Server Functions
#'
#' @noRd
mod_startseite_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


mod_startseite_ui <- function(id){
  ns <- NS(id)
  tagList(


    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Willkommen_ohne_Logo.jpg',
            class = "img-responsive",
            height = "300px",
            # width = "150px",
            alt = "Banner Start",
            style="display: block; margin-left: auto; margin-right: auto;"
        ),
        br(),
        # p(style = "text-align: justify; font-size = 32px",
        #   span("Die wichtigsten Zahlen zu MINT auf einen Blick")
        # ),
        br(),
        p(style = "text-align: justify; font-size = 48px",
          span(tags$b(span("Willkommen in der Beta-Version des MINT-DataLabs von MINTvernetzt!", style = "color:#b16fab")), br(), br(),
          "Hier präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule, Ausbildung und
          Arbeitsmarkt in Deutschland. Unser Ziel ist es, mit dem MINT-DataLab einen zentralen Ort
                für die wichtigsten Statistiken rund um MINT zu schaffen und mittels interaktiver Diagramme einen intuitiven und informativen Zugang zu gewähren.",
             #  br(), br(),
              # "Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir einen ersten Überblick über die Daten. Auf den bereichsspezifischen", tags$b(span("Unterseiten", style = "color:#b16fab")),
              # " gehen wir mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")
        ),


      ))),

    # Dynamische oder statische Infografiken

    # Breitere Option mit Text daneben und dynamischer Infografik
    fluidRow(

      shinydashboard::box(
        title = "Nur eine Minderheit wählt MINT",
        width = 6,
        #   tags$video(src="www/DataLab_Trichter_dyn.mp4", type = "video/mp4", hight = "300px", autoplay = TRUE),
        #video spielt leider nicht ab - könnte daran liegen dass www Pfad nicht auch für Vidoes eingerichtet ist?

        img(src='www/Anteile MINT.JPG',
            class = "img-responsive",
            height = "150px",
            # width = "150px",
            alt = "Infografik Anteile MINT",
            style="display: block; margin-left: auto; margin-right: auto;"
        ),
        br(),
        p(style = "text-align: justify; font-size = 24px",
          tags$b("Anteil MINT entlang der Bildungskette"),
          ),
        p(style = "text-align: justify; font-size = 16px",
          span("Diese Grafik zeigt, wie hoch der Anteil von Schüler:innen, Studierenden und Auszubildenden und
               Beschäftigten ist, die (in Deutschland) einen MINT-Leistungskurs wählen, ein MINT-Fach studieren, eine MINT-Ausbildung
               absolvieren bzw. später einen MINT-Beruf ausüben. In der Oberstufe machen die MINT-Fächer einen Drittel bei der Leistungskurswahl aus.
               Gut ein Drittel (27 %) der Studierenden studiert ein MINT-Fach. Unter den Auszubildenden macht knapp jede:r Vierte (24 %) eine Ausbildung
               im MINT-Bereich. Von allen Beschäftigten in Deutschland geht ein Fünftel (20 %) einem MINT-Beruf nach.",
          )

      )),

      shinydashboard::box(
        title = "MINT-Gender-Gap wächst entlang der Bildungskette",
        width = 6,
       #   tags$video(src="www/DataLab_Trichter_dyn.mp4", type = "video/mp4", hight = "300px", autoplay = TRUE),
        #video spielt leider nicht ab - könnte daran liegen dass www Pfad nicht auch für Vidoes eingerichtet ist?

       img(src='www/Anteile Frauen.JPG',
           class = "img-responsive",
           height = "150px",
           # width = "150px",
           alt = "Infografik Frauen",
           style="display: block; margin-left: auto; margin-right: auto;"
        ),
       br(),
       p(style = "text-align: justify; font-size = 24px",
         tags$b("Anteil Frauen in MINT entlang der Bildungskette"),
         ),
       p(style = "text-align: justify; font-size = 16px",
         span("Diese Grafik zeigt, wie der Anteil an Mädchen bzw. Frauen entlang der MINT-Bildungskette immer weiter abnimmt.
               Während in den MINT-Leistungskurse in der Oberstufe der Anteil an Mädchen und Jungen fast ausgewogen ist, nimmt der Anteil
               an Frauen, die MINT-Studiengänge wählen (32 %), eine Ausbildung in MINT ergreifen (13 %) und später in MINT-Berufen arbeiten (16 %), immer weiter ab.",
              br(),
         ))
        )
          ),



    #Option mit drei Boxen nebeneinander und Beschreibung darunter
    # fluidRow(
    #   shinydashboard::box(
    #     width = 4,
    #    img(src='www/DataLab_Frauenanteil.jpg',
    #         class = "img-responsive",
    #         height = "300px",
    #        # width = "150px",
    #         alt = "Infografik Frauenanteil",
    #         style="display: block; margin-left: auto; margin-right: auto;"
    #         )
    #
    #     ),
    #   shinydashboard::box(
    #     width = 4,
    #     img(src='www/DataLab_Anteil_Studis.jpg',
    #         class = "img-responsive",
    #         height = "300px",
    #        # width = "150px",
    #         alt = "Infografik Anteil Studierende",
    #         style="display: block; margin-left: auto; margin-right: auto;"
    #     )),
    #   shinydashboard::box(
    #     width = 4,
    #     img(src='www/DataLab_Anteil_Azubis.jpg',
    #         #class = "img-responsive",
    #         height = "300px",
    #         #width = "150px",
    #         alt = "Infografik Anteil Auszubildende",
    #         style="display: block; margin-left: auto; margin-right: auto;"
    #     ))
    #
    #
    #   ),
    #
    # # Erläuterung zu den Infografiken
    #
    # fluidRow(
    #   shinydashboard::box(
    #     title = "Anteil Frauen entlang der Bildungskette",
    #     width = 4,
    #     p(style = "text-align: justify; font-size = 16px",
    #       span("Hier ist nocheinmal der Anteil von Frauen im MINT-Bereich entlang der Bildungskette dargestellt.", br(),
    #   ))),
    #   shinydashboard::box(
    #     title = "Wie viel macht MINT - bei Studierenden?",
    #     width = 4,
    #     p(style = "text-align: justify; font-size = 16px",
    #       span("Der Anteil an Studierenden, welche einen Studiengang aus dem MINT-Bereich wählen, liegt bei 34 %", br(),
    #     ))),
    #   shinydashboard::box(
    #     title = "Wie viel macht MINT - bei Auszubildenden?",
    #     width = 4,
    #     p(style = "text-align: justify; font-size = 16px",
    #      span("Bei Auszubildenden wählen weniger einen Beruf aus dem Bildungsbereich. Nur 24 % gehen einer Ausbildung im MINT-Bereich nach.", br(),
    #       )))
    #   ),
    #

    fluidRow(
         shinydashboard::box(
           title = "Orientierungshilfe für das MINT-DataLab: 4 Seiten - immer die gleiche Logik",
            width = 12,
           img(src='www/How to 5.JPG',
                        #class = "img-responsive",
                        height = "800px",
                        # width = "150px",
                        alt = "How to 2",
                        style="display: block; margin-left: auto; margin-right: auto;"
                    ))),



    # fluidRow(
    #   shinydashboard::box(
    #     title = "Orientierungshilfe für das MINT-DataLab: 4 Seiten - immer die gleiche Logik",
    #      width = 12,
    #
    #     column(width = 6,
    #
    #       p(style = "text-align: justify; font-size = 16px",
    #
    #      span("Das DataLab umfasst aktuell 4 Unterseiten:", br(),
    #       tags$b(span(" > Alle Beildunsgbereiche")), br(),
    #       tags$b(span(" > Schule")), br(),
    #       tags$b(span(" > Studium")), br(),
    #       tags$b(span(" > Ausbildung & Beruf")), br(), br(),
    #      "Auf der Seite > Alle Bildungsbereiche < vergleichen wir die Bildunsgbereiche miteinander.
    #       Auf den folgenden, bereichsspezifischen Unterseiten gehen wir je Bildungsbereich mehr ins Detail
    #       und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")), br(), br(),
    #
    #
    #       p(style = "text-align: justify; font-size = 16px",
    #       span("Die vier Unterseiten sind jeweils sehr ähnlich aufgebaut und beantworten folgende Fragen:", br(),
    #            br(),
    #            tags$b(span("#MINT",style = "color:#b16fab")),": Wie hoch ist der Anteil von MINT in den verschiedenen Bildungsbereichen?", br(),br(),
    #            tags$b(span("#Frauen in MINT",style = "color:#b16fab")),": Wie hoch ist der Anteil von Frauen und Mädchen innerhalb von MINT in den verschiedenen Bildungsbereichen?", br(),br(),
    #            tags$b(span("#Fächerwahl von Frauen",style = "color:#b16fab")),": Wie unterscheidet sich die Fächerwahl oder Berufswahl von Frauen und Männern bzw. Mädchen und Jungen?"
    #            )),
    #     ),
    #
    #   column(width = 6,
    #     img(src='www/How to3.png',
    #         #class = "img-responsive",
    #         height = "600px",
    #         # width = "150px",
    #         alt = "How to 2",
    #         style="display: block; margin-left: auto; margin-right: auto;"
    #     )))) ,

    # Footer


    tags$footer(style="text-align: justify;",

                div(style="display: inline-block;position: relative;top: 1.2em;",

                    tags$a(href="https://mint-vernetzt.de/",
                           img(src='www/MINTv_tranparent.png',
                               class = "img-responsive",
                               height = "100px", width = "100px",
                               alt = "Logo MINT", target="_blank",
                               style="display: inline-block; margin-left: auto; margin-right:10%;"))),

                div(style="display: inline-block;position: relative;top: 1.2em;",

                    p(tags$a("Impressum", href="#shiny-tab-impressum", "data-toggle" = "tab")," | ",
                      tags$a("Kontakt", href="#shiny-tab-kontakt", "data-toggle" = "tab")," | ",
                      tags$a("Datenschutz", href="#shiny-tab-datenschutz", "data-toggle"="tab"),HTML('&nbsp;'),HTML('&nbsp;'),
                      "Copyright © 2022. Alle Rechte vorbehalten Stifterverband")),

                div(style="display: inline-block;position: relative;top: 1.2em;",

                    tags$a(href="https://www.bmbf.de/bmbf/de/home/home_node.html",
                           img(src='www/BMBF-Logo_transp1.png',

                               class = "img-responsive",

                               height = "200px", width = "200px",

                               alt = "Logo BMBF", target="_blank",

                               style="display: inline-block; margin-left: auto; margin-right: auto;"))),

                div(style="display: inline-block;width: 100%;",

                    " ")

    ))
  # Taglist zu

}

## To be copied in the UI
# mod_startseite_ui("startseite_1")

## To be copied in the server
# mod_startseite_server("startseite_1")

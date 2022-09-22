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
        img(src='www/Banner_Start.jpg',
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
          "Hier präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
                 Ausbildung und Arbeitsmarkt in Deutschland.",
             #  br(), br(),
              # "Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir einen ersten Überblick über die Daten. Auf den bereichsspezifischen", tags$b(span("Unterseiten", style = "color:#b16fab")),
              # " gehen wir mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")
        ),


      ))),

    # Dynamische oder statische Infografiken

    fluidRow(
      shinydashboard::box(
        width = 4,
        img(src='www/DataLab_Trichter_dyn.mp4',
            #class = "img-responsive",
            height = "300px",
            # width = "150px",
            alt = "Infografik Trichter",
            style="display: block; margin-left: auto; margin-right: auto;"
        )),
      shinydashboard::box(
        width = 4,
        img(src='www/DataLab_Trichter_dyn.mp4',
            #class = "img-responsive",
            height = "300px",
            # width = "150px",
            alt = "Infografik Trichter",
            style="display: block; margin-left: auto; margin-right: auto;"
        )),
      shinydashboard::box(
        width = 4,
        img(src='www/DataLab_Trichter_dyn.mp4',
            #class = "img-responsive",
            height = "300px",
            # width = "150px",
            alt = "Infografik Trichter",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))


      ),

    # Erläuterung zu den Infografiken

    fluidRow(
      shinydashboard::box(
        title = "xx",
        width = 4,
        p(style = "text-align: justify; font-size = 16px",
          span("xx", br(),
      ))),
      shinydashboard::box(
        title = "xx",
        width = 4,
        p(style = "text-align: justify; font-size = 16px",
          span("xx", br(),
        ))),
      shinydashboard::box(
        title = "xx",
        width = 4,
        p(style = "text-align: justify; font-size = 16px",
          span("xx", br(),
          )))),




    fluidRow(
      shinydashboard::box(
          title = "Orientierungshilfe: Wie ist das DataLab aufgebaut?",
         width = 4,
          p(style = "text-align: justify; font-size = 16px",
          span("Die Unterseiten sind jeweils sehr ähnlich aufgebaut und beantworten folgende Fragen:", br(),
               br(),
               tags$b(span("#MINT",style = "color:#b16fab"),": Wie hoch ist der Anteil von MINT in den verschiedenen Bildungsbereichen?"), br(),br(),
               tags$b(span("#Frauen in MINT",style = "color:#b16fab"),": Wie hoch ist der Anteil von Frauen und Mädchen innerhalb von MINT in den verschiedenen Bildungsbereichen?"), br(),br(),
               tags$b(span("#Fächerwahl von Frauen",style = "color:#b16fab"),": Wie unterscheidet sich die Fächerwahl oder Berufswahl von Frauen und Männern bzw. Mädchen und Jungen?")
               ),
              br(), br(),

          span("Auf der Überblicksseite >> Alle Beildunsgbereiche << geben wir einen ersten Einblick in die vorhandenen Daten und vergleichen die Bildunsgbereiche miteinander.
          Auf den folgenden, bereichsspezifischen Unterseiten gehen wir je Bildungsbereich mehr ins Detail
          und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene."),


        # p(style = "text-align: justify; font-size = 16px",
        #          span("In der ersten Box gehen wir der Frage nach: Wie hoch ist der Anteil von MINT in den verschiedenen Bildungsbereichen?
        #                    Auf dieser Seite vergleichen wir die Anteile von MINT zwischen den Bildungsbereichen."
        #          )),
        #  p(style = "text-align: justify; font-size = 16px",
        #          span("In der zweiten Box gehen wir der Frage nach: Wie hoch ist der Anteil von Frauen und Mädchen innerhalb von MINT in den verschiedenen Bildungsbereichen?
        #                    Auf dieser Seite vergleichen wir die Anteile von Frauen und Mädchen in MINT zwischen den Bildungsbereichen."
        #          )),

          )
        ),

      shinydashboard::box(
        width = 8,
        img(src='www/How to.png',
            #class = "img-responsive",
            height = "400px",
            # width = "150px",
            alt = "How to",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),














    ) # Taglist zu

}

## To be copied in the UI
# mod_startseite_ui("startseite_1")

## To be copied in the server
# mod_startseite_server("startseite_1")

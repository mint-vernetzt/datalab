#' Startseite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


mod_startseite_start_ui <- function(id){
  ns <- NS(id)
  tagList(


    fluidRow(
      div(class = "clean-box",
          column(
            width = 12,
        img(src='www/Banner_Willkommen.jpg',
            class = "img-responsive",
            height = "300px",
            alt = "Banner Start",
            style="display: block; margin-left: auto; margin-right: auto;"
        )))),

    # Einleitungstext ----
    fluidRow(
      div(class = "clean-box",
          column(
            width = 8,
            h1("Willkommen im MINT-DataLab von MINTvernetzt!"),
            p(style = "text-align: justify; font-size: 14px;",
              "Hier präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule, Ausbildung und
          Arbeitsmarkt in Deutschland.", br(),
          "Unser Ziel ist es, mit dem MINT-DataLab einen zentralen Ort
          für die wichtigsten Statistiken rund um MINT zu schaffen und mittels interaktiver
          Diagramme einen intuitiven und informativen Zugang zu gewähren. Dabei entwickeln wir das MINT-DataLab stetig weiter."
            ),
            p(),

          # Updates ----
          h2(style = "color: #00a87a;",
             "Was ist Neu?"),
          p(style = "text-align: justify; font-size: 14px;",
            "-  Grafiken und Daten als Download", br(),
            "-  Fachkräftedaten auf Bundesland-Level",br(),
            "-  Neue Kurzanalyse: Fachkräftemangel in den MINT-Disziplinen", br(),
            "-  Daten zu Lehramts-Absolvent:innen verfügbar"
          ),
          h2(style = "color: #00a87a;",
             "Woran wir aktuell arbeiten:"),
          p(style = "text-align: justify; font-size: 14px;",
            "-  Argumentationshilfe mit Erklärungen und Beispielen",br(),
            "-  Optimiertes Laden der Website", br()),
          p(),      p(),
          p("Bei Fragen oder Anregungen, melden Sie sich jederzeit gerne ",
            tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),
            " bei uns."),

          p(),
          hr(),
          p(),

    # Kurzanalysen ----
            h5("Was steckt hinter den MINT-Statistiken?"),
            p(style = "text-align: justify; font-size = 20px",
              "Ausgewählte Statistiken bereiten wir in Kurzanalysen auf.
              Hier schauen wir hinter die Zahlen und ordnen sie ein – erklärend,
              lösungsorientiert und wissenschaftlich fundiert.
           Die Kurzanalysen können auf den verschiedenen Bereichs- und Themenseiten
              im MINT-DataLab heruntergeladen werden.", br(),
              "Auf der MINTvernetzt-Website, auf der Themenseite des MINT-DataLabs,
              finden sich alle Kurzanalyen auf einen Blick.",
              br()),
              tags$a(href = "https://www.mint-vernetzt.de/mint-datalab/#kurzanalysen",
                     target = "_blank", "Alle Kurzanalysen",
                     class = "btn btn-default",
                     style = "margin-bottom: 30px; margin-top: 10px;")
    ),

    column(
      width = 11,
      slickR::slickROutput(ns("slider_output"), width = '800px', height = '500px'),

      # Lernvideo ----

      h5("Lernvideo zu den MINT-Daten",
         style = "margin-top: 40px;"),
      p("Auf dem MINT-Campus haben wir ein Video veröffentlich, in dem wir Statistiken
        zum Thema Frauen in MINT zeigen und einordnen."),
      div(
        style = "display: flex; justify-content: center;",
        tags$iframe(
          width = "800", height = "450",
          src = "https://www.youtube.com/embed/cFd8ZvegIhg",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = NA,
          style = "margin: 40px;"
        )
      )
    ),


    column(
      width = 8,

       # Abbinder ----

      p(),
      hr(),
      h1("Entdecken Sie jetzt die verschiedenen MINT-Bereiche!",
         style = "margin-bottom: 40px; margin-top: 40px;")
      )
    )
  ),


    # Footer
    fluidRow(
      shinydashboard::box(
        style = "margin-top: 20px",
        width = 12,
        funct_footer()
      )
    )

)
}

#' Startseite Server Functions
#'
#' @noRd
mod_startseite_start_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$slider_output <- slickR::renderSlickR({

      slickR::slickR(
        obj =  paste0("inst/app/www/slide/", grep("_slide", list.files("inst/app/www/slide"), value = TRUE)),
        height = 450,
        width = "95%"
      )
    })

  })
}


## To be copied in the UI
# mod_startseite_ui("startseite_1")

## To be copied in the server
# mod_startseite_server("startseite_1")

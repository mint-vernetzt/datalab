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

    # Einleitungstext
    fluidRow(
      div(class = "clean-box",
          column(
            width = 7,
            h1("Willkommen im MINT-DataLab von MINTvernetzt!"),
            p(style = "text-align: justify; font-size: 14px;",
              "Hier präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule, Ausbildung und
          Arbeitsmarkt in Deutschland. Unser Ziel ist es, mit dem MINT-DataLab einen zentralen Ort
          für die wichtigsten Statistiken rund um MINT zu schaffen und mittels interaktiver
          Diagramme einen intuitiven und informativen Zugang zu gewähren. Dabei entwickeln wir das MINT-DataLab stetig weiter."
            )
          ),
          column(width =1),
          column(
            width = 4,
           h1(style = "color: #00a87a; font-size: 20px;",
             "Was ist Neu?"),
            p(style = "text-align: justify; font-size: 14px;",
              "-  Grafiken und Daten als Download", br(),
              "-  Fachkräftedaten auf Bundesland-Level",br(),
              "-  Neue Kurzanalyse: Fachkräftemangel in den MINT-Disziplinen", br(),
              "-  Daten zu Lehramts-Absolvent:innen verfügbar"
            ),
            h1(style = "color: #00a87a; font-size: 20px;",
               "Woran wir aktuell arbeiten:"),
            p(style = "text-align: justify; font-size: 14px;",
              "-  Argumentationshilfe mit Erklärungen und Beispielen",br(),
              "-  Optimiertes Laden der Website", br()
            )
          )
      )
    ),



    # Dynamische Grafiken
    fluidRow(
      div(
        class = "clean-box",
        column(
        width = 7,
        h5("Was steckt hinter den MINT-Statistiken?"),
        p(style = "text-align: justify; font-size = 20px",
            "Ausgewählte Statistiken bereiten wir in Kurzanalysen auf.
           Die Kurzanalysen können auf der MINT-DataLab-Themenseite der MINTvernetzt-Website gelesen und
           zukünftig heruntergeladen werden. ",

          br()),
        tags$a(href="https://www.mint-vernetzt.de/mint-datalab/#kurzanalysen",
               "Link zu den Kurzanalysen",
               target = "_blank"),
        p(br())
        ),

        slickR::slickROutput(ns("slider_output"), width = '900px', height = '500px'),

        p(br(),br(),br(),br(),br(),br())
      )
    ),



    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    fluidRow(
      shinydashboard::box(
        width = 10,
        img(src='www/HowToMINTDataLab1.png',
            class = "img-responsive",
            alt = "How to",
            style="display: block; margin-left: auto; margin-right: auto;
            max-width: 80%;"
        ),
        img(src='www/HowToMINTDataLab2.png',
            class = "img-responsive",
            alt = "How to",
            style="display: block; margin-left: auto; margin-right: auto;
            max-width: 80%;"
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

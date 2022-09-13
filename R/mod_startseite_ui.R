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
        p(style = "text-align: justify; font-size = 24px",
          span(tags$b(span("Willkommen in der Beta-Version des MINT-DataLabs von MINTvernetzt!", style = "color:#b16fab")), " Hier präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
                 Ausbildung und Arbeitsmarkt in Deutschland.",
               br(), br(),
               "Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir einen ersten Überblick über die Daten. Auf den bereichsspezifischen", tags$b(span("Unterseiten", style = "color:#b16fab")),
               " gehen wir mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")
        ),


      )),

    fluidRow(
      shinydashboard::box(
        width = 12,
        tags$h2("xx"),
        p(style = "text-align: justify; font-size = 16px",
          span("xx", br(),
               br(),

          )
        ),

      ))



    )

}

## To be copied in the UI
# mod_startseite_ui("startseite_1")

## To be copied in the server
# mod_startseite_server("startseite_1")

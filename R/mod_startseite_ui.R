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
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Willkommen.jpg',
            class = "img-responsive",
            height = "300px",
            # width = "150px",
            alt = "Banner Start",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

    # Einleitungstext
    fluidRow(
      shinydashboard::box(
        width = 8,
        title = "Willkommen im MINT-DataLab von MINTvernetzt!",
        p(style = "text-align: justify; font-size = 20px",
          "Hier präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule, Ausbildung und
            Arbeitsmarkt in Deutschland. Unser Ziel ist es, mit dem MINT-DataLab einen zentralen Ort
                  für die wichtigsten Statistiken rund um MINT zu schaffen und mittels interaktiver
          Diagramme einen intuitiven und informativen Zugang zu gewähren. Dabei entwickeln wir das MINT-DataLab stetig weiter.",
          br()
      )
    )
    ),

    # Dynamische Grafiken
    fluidRow(
      shinydashboard::box(
        width = 10,
        title = "Was steckt hinter den MINT-Statistiken?",
        p(style = "text-align: justify; font-size = 20px",
            "Ausgewählte Statistiken bereiten wir in Kurzanalysen auf.
           Die Kurzanalysen können auf der MINT-DataLab-Themenseite der MINTvernetzt-Website gelesen und
           zukünftig heruntergeladen werden. ",
          # LINK ergänzen, wenn da (Nike)
          br()),
        tags$a(href="https://www.mint-vernetzt.de/mint-datalab/#kurzanalysen", "Link zu den Kurzanalysen", target = "_blank"),
        p(br()),

        slickR::slickROutput(ns("slider_output"), width = '900px', height = '500px'),

        p(br(),br())
      )
    ),

    # How-To
    # fluidRow(
    #   shinydashboard::box(
    #     width = 10,
    #     img(src='www/How-To_MINT-DataLab_2.png',
    #         class = "img-responsive",
    #         # height = "800px",
    #         #width = "150px",
    #         alt = "How to",
    #         style="display: block; margin-left: auto; margin-right: auto;"
    #     ))
    # ),


    # # How-To
    # fluidRow(
    #      shinydashboard::box(
    #         width = 10,
    #        img(src='www/How-To_MINT-DataLab_2.png',
    #                     class = "img-responsive",
    #                    # height = "800px",
    #                     #width = "150px",
    #                     alt = "How to",
    #                     style="display: block; margin-left: auto; margin-right: auto;"
    #                 ))
    # ),

    fluidRow(
      shinydashboard::box(
        width = 10,
        img(src='www/ht1.png',
            class = "img-responsive",
            # height = "800px",
            #width = "150px",
            alt = "How to",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        width = 10,
        img(src='www/ht2.png',
            class = "img-responsive",
            # height = "800px",
            #width = "150px",
            alt = "How to",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))
    ),


    # Footer
    fluidRow(
      shinydashboard::box(
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
      # +
      #   settings(
      #     autoplay = TRUE
      #   )
    })

  })
}


## To be copied in the UI
# mod_startseite_ui("startseite_1")

## To be copied in the server
# mod_startseite_server("startseite_1")

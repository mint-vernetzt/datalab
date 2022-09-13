#' home_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Start-Banner

      # fluidRow(
      #   shinydashboard::box(
      #     width = 12,
      #     img(src='www/Banner_Start.jpg',
      #         class = "img-responsive",
      #         height = "300px",
      #         # width = "150px",
      #         alt = "Banner Start",
      #         style="display: block; margin-left: auto; margin-right: auto;"
      #     ),
      #     br(),
      #     # p(style = "text-align: justify; font-size = 32px",
      #     #   span("Die wichtigsten Zahlen zu MINT auf einen Blick")
      #     # ),
      #     br(),
      #     p(style = "text-align: justify; font-size = 16px",
      #       span(tags$b(span("Willkommen in der Beta-Version des MINT-DataLabs von MINTvernetzt!", style = "color:#b16fab")), " Hier präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
      #            Ausbildung und Arbeitsmarkt in Deutschland.",
      #            br(), br(),
      #            "Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir einen ersten Überblick über die Daten. Auf den bereichsspezifischen", tags$b(span("Unterseiten", style = "color:#b16fab")),
      #            " gehen wir mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")
      #     ),
      #
      #
      #     )),


      fluidRow(
      shinydashboard::box(
      title = "Überblick alle Bildungsbereiche",
       width = 12,
        p(style = "text-align: justify; font-size = 16px",
        span("Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir einen ersten Überblick über die Daten. Auf den bereichsspezifischen", tags$b(span("Unterseiten", style = "color:#b16fab")),
                " gehen wir mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")
         )


         )),



    fluidRow(
      shinydashboard::box(
        title = "#MINT: Wie hoch ist der Anteil von MINT entlang der Bildungskette?",
        width = 12,
        p("Die folgende interaktive Grafik gibt Antworten auf die Fragen: Wie hoch ist der Anteil von MINT-Fächern in der Schule? Wie hoch ist der Anteil von Studierenden, die MINT studieren?", br(),
          "Wie hoch ist der Anteil von Auszubildenden, die eine Ausbildung in MINT machen? Wie hoch ist der Anteil von Beschäftigten, die im MINT-Bereich arbeiten?"),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),
                      shiny::sidebarPanel(
                        mod_home_start_einstieg_ui("mod_home_start_einstieg_ui_1")),
                      shiny::mainPanel(
                        htmlOutput(ns("plot_mint_rest_einstieg_1")))
                            ),
                    tabPanel("Zeitverlauf", br(),
                        shiny::sidebarPanel(
                          mod_home_start_multiple_ui("mod_home_start_multiple_ui_1")),
                        shiny::mainPanel(
                          highcharter::highchartOutput(ns("plot_mint_1")))
                             ),
                    tabPanel("Überblick", br(),
                             shiny::sidebarPanel(
                               mod_home_start_comparison_mint_ui("mod_home_start_comparison_mint_ui_1")),
                             shiny::mainPanel(highcharter::highchartOutput(ns("plot_comparison_mint"))

         ))))),
    fluidRow(
      shinydashboard::box(
        title = "#Frauen_in_MINT: Wie hoch ist der Anteil von Frauen und Mädchen innerhalb von MINT in den verschiedenen Bildungsbereichen?",
        width = 12,
        p("Die folgende interaktive Grafik gibt Antworten auf die Fragen: Wie hoch ist der Anteil von Mädchen in MINT-Leistungskursen?
          Wie hoch ist der Anteil von Frauen in MINT-Studienfächern? Wie hoch ist der Anteil von Frauen in MINT-Ausbildungsgängen?
          Wie hoch ist der Anteil von Frauen in MINT-Berufen?", br(),
        "Zum Vergleich zeigen wir jeweils auch, wie hoch ist der Anteil von Frauen in den anderen, nicht-MINT-Fächern oder .Berufszweigen ist."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),
                             shiny::sidebarPanel(
                               mod_home_start_einstieg_gender_ui("mod_home_start_einstieg_gender_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_pie_mint_gender")))
                            ),
                    tabPanel("Zeitverlauf", br(),
                        shiny::sidebarPanel(
                          mod_home_start_comparison_ui("mod_home_start_comparison_ui_1")),
                        shiny::mainPanel(
                          highcharter::highchartOutput(ns("plot_verlauf_mint"))
                                        )
                            ),
                    tabPanel("Überblick", br(),
                             shiny::sidebarPanel(
                               mod_home_start_comparison_mint_gender_ui("mod_home_start_comparison_mint_gender_ui_1")),
                             shiny::mainPanel(plotOutput(ns("plot_comparison_gender")))

                             )
                    ))),


  )
}

#' home_start Server Functions
#'
#' @noRd
mod_home_start_server <- function(id, data_zentral, data_ausbildungsvertraege ,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$plot_verlauf_mint <- highcharter::renderHighchart({
      home_comparison_line(data_zentral,r)
    })

    output$plot_mint_rest_einstieg_1 <- renderUI({
      home_einstieg_pie(data_zentral,r)
    })

    output$plot_comparison_gender <- renderPlot({
      home_stacked_comparison_gender(data_zentral, data_ausbildungsvertraege, r)
    })

    output$plot_mint_1 <- highcharter::renderHighchart({
      home_rest_mint_verlauf(data_zentral, r)
    })

    output$plot_comparison_mint <- highcharter::renderHighchart({
      home_stacked_comparison_mint(data_zentral, r)
    })

    output$plot_pie_mint_gender <- renderUI({
      home_einstieg_pie_gender(data_zentral, data_ausbildungsvertraege, r)
    })


  })
}

## To be copied in the UI
# mod_home_start_ui("home_start_1")

## To be copied in the server
# mod_home_start_server("home_start_1")







# Alte Start-Box


# fluidRow(
#   shinydashboard::box(
#     #title = span("Willkommen im MINT-DataLab von MINTvernetzt", style = "color:#154194; font-size: 50px"),
#     width = 12,
#     column(width = 9,
#            tags$h1("WILLKOMMEN IM MINT-DATALAB"),
#            #tags$h1("Willkommen im MINT-DataLab"),
#            #p(style = "color:#154194; font-size: 50px", "Willkommen im MINT-DataLab"),
#            p(style = "text-align: justify; font-size = 16px",
#              "Im MINT-DataLab zeigen wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
#                     Ausbildung und Arbeitsmarkt in Deutschland."
#            ),
#            p(style = "text-align: justify; font-size = 16px",
#              span("Unser", tags$b(span("Datenpool", style = "color:#b16fab")), "besteht aktuell aus Statistiken der Bundesagentur für Arbeit, des
#                          Statistischen Bundesamtes und der Kulturministerkonferenz. Weitere Datenquellen werden im Laufe
#                          der Zeit integriert. (Weitere Infos unter: Quellen & Hinweise)")
#            ),
#
#            p(style = "text-align: justify; font-size = 16px",
#              span("Wir freuen uns über ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject=MINT-Datalab", "Feedback"),"!")
#            ),
#     ),
#     #solidHeader = TRUE,
#     #collapsible = FALSE,
#     #br(),
#     column(width = 3, href = "https://mint-vernetzt.de/", rel="noopener", target="_blank",
#            img(src='www/mint_logo_gross.jpg',
#                class = "img-responsive",
#                height = "180px", width = "180px",
#                alt = "Logo MINT",
#                style="display: block; margin-left: auto; margin-right: auto;"
#
#            ))
#   )
# )













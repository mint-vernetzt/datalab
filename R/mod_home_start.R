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
    fluidRow(
      shinydashboard::box(
        #title = span("Willkommen im interaktiven MINT-DataLab", style = "color:#154194; font-size: 50px"),
        width = 12,
        column(width = 9,
               tags$h1("Willkommen im interaktiven MINT-DataLab von MINTvernetzt"),
               #tags$h1("Willkommen im interaktiven MINT-DataLab"),
               #p(style = "color:#154194; font-size: 50px", "Willkommen im MINT-DataLab"),
                  p(style = "text-align: justify; font-size = 16px",
                    "Das MINT-DataLab zeigt", tags$b(span("statistische Kennzahlen", style = "color:#b16fab")), "rund um MINT in den Bereichen Schule, Hochschule,
                    Ausbildung und Arbeitsmarkt in Deutschland. Wir laden ein, sich mithilfe unserer interaktiven Grafiken zu diesen Bereichen zu informieren und inspirieren zu lassen."
                    ),
                   p(style = "text-align: justify; font-size = 16px",
                    "Diese zwei Fragen leiten durch das DataLab:", tags$b(span("Wie hoch ist der MINT-Anteil in den
                               unterschiedlichen Bereichen?", style = "color:#b16fab")), "und",   tags$b(span("Wie hoch ist der jeweilige Frauenanteil?", style = "color:#b16fab")), br(),
                    "Je nach Verfügbarkeit der Daten kann nach einzelnen MINT-Fächern und -Bereichen, Jahren und Bundesländern gefiltert
                               oder diese vergleichen werden."
                    ),
                  p(style = "text-align: justify; font-size = 16px",
                    span("Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir einen ersten Überblick über die Daten. Auf den bereichsspezifischen",
                    tags$b(span("Unterseiten", style = "color:#b16fab")), " Schule, Studium und Ausbildung & Beruf bieten wir zusätzliche Vergleiche nach Subgruppen, den einzelnen MINT-Fächern und Bundesländern.")
                    ),
               ),
        #solidHeader = TRUE,
        #collapsible = FALSE,
        #br(),
        column(width = 3, href = "https://mint-vernetzt.de/", rel="noopener", target="_blank",
               img(src='www/mint_logo_gross.jpg',
                   class = "img-responsive",
                   height = "180px", width = "180px",
                   alt = "Logo MINT",
                   style="display: block; margin-left: auto; margin-right: auto;"
               ),
              ),
        #a(href = "https://mint-vernetzt.de/", rel="noopener", target="_blank",
        #  img(src='www/mint_logo_gross.jpg',
        #      class = "img-responsive",
        #      #height = "150px", width = "150px",
        #      alt = "Logo MINT",
        #      style="display: block; margin-left: auto; margin-right: auto;"
        #      )
        #  ),
        #a(rel="noopener", target="_blank",
        #  img(src='www/BMBF-Logo.jpg',
        #      class = "img-responsive",
        #      #height = "150px", width = "150px",
        #      alt = "Logo BMBF",
        #      style="display: block; margin-left: auto; margin-right: auto;"
        #      )
        #  )
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Wie hoch ist der Anteil von MINT in den verschiedenen Bereichen?",
        width = 12,
        p("Hier zeigen wir den Anteil von MINT für verschiedene Indikatoren
        aus den  Bereichen Schule, Studium und Arbeitsmarkt. Die Unterseiten zu den einzelnen Bereichen sind genauso aufgebaut.", br(), br(),
        tags$b(span("Methodischer Hinweis:")), "Studierende, Auszubildende und Beschäftigte können anhand ihres Studienfaches, Ausbildungsgangs bzw. Tätigkeit in 'MINT' und 'Nicht-MINT' unterteilt werden.
        Bei Schüler:innen ist diese eindeutige Unterscheidung nicht möglich, deshalb nutzen wir die Belegungszahlen von Grund- und Leistungskursen als Indikator für den Anteil von MINT in der Schule."),
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

                             )
      )))
    ),
    fluidRow(
      shinydashboard::box(
        title = "Wie hoch ist der Anteil von Frauen in MINT in den verschiedenen Bereichen?",
        width = 12,
        p("Hier zeigen wir den Anteil von Frauen in MINT für die Indikatoren
        aus den  Bereichen Schule, Hochschule und Arbeitsmarkt anschauen und vergleichen. Zum Vergleich zeigen wir unter jedem Tortendiagramm auch den Anteil von Frauen ingesamt in dem jeweiligen Bereich."),
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
    fluidRow(
      shinydashboard::box(
        #title = " ",
        width = 12,
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Verwendete Quellen:", style = "color:#b16fab")), br(),
               "Statistisches Bundesamt 2021, Bundesagentur für Arbeit 2021, KMK 2021", br(),
               "Weitere Informationen zu den Quellen gibt es hier (LINK)."
               )
        )
      )
  ),
  fluidRow(
    shinydashboard::box(
      title = "Was ist MINTvernetzt? ",
      width = 12,
      column(width = 9,
      p(style = "text-align: justify; font-size = 16px",
         "MINTvernetzt ist die", tags$b(span("MINT-Vernetzungsstelle", style = "color:#b16fab")), "für MINT-Akteur:innen in Deutschland und Dach für die außerschulische MINT-Bildung in Deutschland.
                  MINTvernetzt wird vom Bundesministerium für Bildung und Forschung gefördert und von Mitarbeitenden der Körber-Stiftung, der matrix gGmbH,
                  dem Nationalen MINTForum e.V., dem Stifterverband und der Universität Regensburg als Verbund gemeinsam umgesetzt."


      ),
      p(style = "text-align: justify; font-size = 16px",
        "Das MINT-DataLab wird kontinuierlich und bedarfsorientiert weiterentwickelt. Über", tags$b(span("Anregungen oder Wünsche", style = "color:#b16fab")), " zu weiteren Datensätzen oder Darstellungen,
                 freuen wir uns!", br(),
            "Gerne per Email an", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject=MINT-Vernetzt Datalab", " Antonia Kröger"),
             " oder über unsere Umfrage zum MINT-DataLab (LINK)."
      )
             ),


    column(
      width = 3,
      href = "https://mint-vernetzt.de/", rel="noopener", target="_blank",
       #    img(src='www/mint_logo_gross.jpg',
       #        class = "img-responsive",
       #        height = "180px", width = "180px",
      #        alt = "Logo MINT",
       #        style="display: block; margin-left: auto; margin-right: auto;"
         #  ),
        #   br(),
         #  br(),
           img(src='www/BMBF-Logo.jpg',
               class = "img-responsive",
               height = "150px", width = "150px",
               alt = "Logo BMBF",
               style="display: block; margin-left: auto; margin-right: auto;"
           ),

       ))
       )
        ) # taglist schließen
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

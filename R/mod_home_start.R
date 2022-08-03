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
        #title = span("Willkommen im MINT-DataLab", style = "color:#154194; font-size: 50px"),
        width = 12,
        column(width = 9,
               tags$h1("WILLKOMMEN IM MINT-DATALAB"),
               #tags$h1("Willkommen im MINT-DataLab"),
               #p(style = "color:#154194; font-size: 50px", "Willkommen im MINT-DataLab"),
                  p(style = "text-align: justify; font-size = 16px",
                    "Im MINT-DataLab bieten wir Ihnen statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
                    Ausbildung und Arbeitsmarkt in Deutschland. Wir laden Sie dazu ein, sich zu diesen Bereichen mithilfe unserer
                    interaktiven Grafiken zu informieren und inspirieren zu lassen."
                    ),
                   p(style = "text-align: justify; font-size = 16px",
                     tags$ul(
                       tags$li("Wie hoch ist der Anteil Studierenden an MINT-Studiengängen?"),
                       tags$li("Wie hoch ist der Anteil weiblicher Auszubildenden in MINT in Hessen?"),
                       tags$li("Wie hat sich der Anteil an Mädchen in Informatik-Unterricht in den letzten 5 Jahren verändert?")
                     )
                     ),
                  p(style = "text-align: justify; font-size = 16px",
                    span("Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir Ihnen einen ersten Überblick. Auf den ", tags$b(span("Unterseiten", style = "color:#b16fab")), " finden
                         Sie weiterführende Informationen zu den einzelnen Bereichen.")
                    ),
                  p(style = "text-align: justify; font-size = 16px",
                    span("Unser", tags$b(span("Datenpool", style = "color:#b16fab")), "besteht aktuell aus Statistiken der Bundesagentur für Arbeit, des
                         Statistischen Bundesamtes und der Kulturministerkonferenz. Weitere Datenquellen werden im Laufe
                         der Zeit integriert. (Siehe Reiter: Quellen & Hinweise)")
                    ),
                   p(style = "text-align: justify; font-size = 16px",
                     span("Haben Sie ", tags$b(span("Anregungen oder Wünsche", style = "color:#b16fab")), " an weitere Datensätze oder Darstellungen,
                 freuen wir uns über Ihr Feedback (Link zur Umfrage ).", br(), "Für Fragen steht Ihnen", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject=MINT-Vernetzt Datalab", " Antonia Kröger"), " gerne zur Verfügung.")
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
               br(),
               br(),
               img(src='www/BMBF-Logo.jpg',
                   class = "img-responsive",
                   height = "150px", width = "150px",
                   alt = "Logo BMBF",
                   style="display: block; margin-left: auto; margin-right: auto;"
               )),
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
        title = "Wie hoch in der Anteil von MINT in den verschiedenen Bildungsbereichen?",
        width = 12,
        p("Hier zeigen wir den Anteil von MINT für verschiedene Personengruppen
        aus den  Bereichen Schule, Studium, Ausbildung und Arbeitsmarkt ."),
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
        title = "Anteil von Frauen an MINT-Bereichen",
        width = 12,
        p("Hier können Sie sich den Anteil von Frauen an MINT und nicht-MINT für die Indikatoren
        aus den  Bereichen Schule, Hochschule und Arbeitsmarkt anschauen und vergleichen."),
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
          span(tags$b(span("Quellen:", style = "color:#b16fab")), " ")
        )
      )
  ),
  fluidRow(
    shinydashboard::box(
      #title = " ",
      width = 12,
      p(style = "text-align: justify; font-size = 16px",
        span(tags$b(span("Was ist MINTvernetzt?", style = "color:#b16fab")), br(),
             tags$b(span("Die Service- und Anlaufstelle für MINT-Akteur:innen in Deutschland", style = "color:#b16fab")),br(),
             "Die MINT-Vernetzungsstelle, kurz MINTvernetzt, ist das Dach für die außerschulische MINT-Bildung in Deutschland.
                  MINTvernetzt wird vom Bundesministerium für Bildung und Forschung gefördert und von Mitarbeitenden der Körber-Stiftung, der matrix gGmbH,
                  dem Nationalen MINTForum e.V., dem Stifterverband und der Universität Regensburg als Verbund gemeinsam umgesetzt."))
    )
  )
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

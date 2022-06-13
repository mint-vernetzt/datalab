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
        title = span("Willkommen im MINT-DataLab", style = "color:#154194; font-size: 50px"),
        width = 12,
      p(style = "text-align: justify; font-size = 16px",
        "Im MINT-DataLab bieten wir Ihnen statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
        Ausbildung und Arbeitsmarkt in Deutschland. Wir laden Sie dazu ein, sich zu diesen Bereichen mithilfe unserer
        interaktiven Grafiken zu informieren und inspirieren zu lassen."),
       p(style = "text-align: justify; font-size = 16px",
         tags$ul(
           tags$li("Wie hoch ist der Anteil Studierenden an MINT-Studiengängen?"),
           tags$li("Wie hoch ist der Anteil weiblicher Auszubildenden in MINT in Hessen?"),
           tags$li("Wie hat sich der Anteil an Mädchen in Informatik-Unterricht in den letzten 5 Jahren verändert?")
         )),
      p(style = "text-align: justify; font-size = 16px",
        span("Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir Ihnen einen ersten Überblick. Auf den Unterseiten finden
             Sie weiterführende Informationen zu den einzelnen Bereichen.")),
      p(style = "text-align: justify; font-size = 16px",
        span("Unser", tags$b(span("Datenpool", style = "color:#b16fab")), "besteht aktuell aus Statistiken der Bundesagentur für Arbeit, des
             Statistischen Bundesamtes und der Kulturministerkonferenz. Weitere Datenquellen werden im Laufe
             der Zeit integriert. (Button: weitere Informationen: Reiter: Quellen)")),
      p(style = "text-align: justify; font-size = 16px",
        span("Haben Sie ", tags$b(span("Anregungen oder Wünsche", style = "color:#b16fab")), " an weitere Datensätze oder Darstellungen,
             freuen wir uns über Ihr Feedback (Link zu Umfrage ). Für Fragen steht Ihnen Antonia Kröger gerne
             zur Verfügung."))
      )),
    fluidRow(
      shinydashboard::box(
        title = "Box 1",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("MINT-Anteile", br(),
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
                    tabPanel("Vergleich", br(),
                             shiny::sidebarPanel(
                               mod_home_start_comparison_mint_ui("mod_home_start_comparison_mint_ui_1")),
                             shiny::mainPanel(highcharter::highchartOutput(ns("plot_comparison_mint"))

                             )
      )))
    ),
    fluidRow(
      shinydashboard::box(
        title = "Box 2",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("MINT-Anteile", br(),
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
                    tabPanel("Vergleich", br(),
                             shiny::sidebarPanel(
                               mod_home_start_comparison_mint_gender_ui("mod_home_start_comparison_mint_gender_ui_1")),
                             shiny::mainPanel(plotOutput(ns("plot_comparison_gender")))

                             )
                    )))
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

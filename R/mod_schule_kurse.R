#' schule_kurse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        tags$h2("MINT in der Schule"),
        p(style = "text-align: justify; font-size = 16px",
          "Auf dieser Seite finden Sie statistische Kennzahlen rund um MINT
          im Bereich Schule. Aktuell bieten wir Ihnen Informationen zu den Kursbelegungen
          von Schülern und Schülerinnen in der Oberstufe. Beispielsweise können Sie sich den Anteil von
          Leistungskursen im Fachbereich Informatik im Zeitverlauf ausgeben lassen.
          Oder Sie interessieren sich für die Kursbelegungen von Schülerinnen im Vergleich zu Schülern
          in MINT. Zudem bieten wir Ihnen die Ergebnisse für einzelne Bundesländer an."),
        #br(),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")), "Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.")),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Methodische Hinweise:", style = "color:#b16fab")),
               " "))
      )),
    fluidRow(
      shinydashboard::box(
        title = "Anteil von MINT-Fächern in der Schule",
        width = 12,
        p("Hier können Sie sich den Anteil von MINT und nicht-MINT für
          Leistungs- und Grundkurse anschauen."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                      tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                      shiny::sidebarPanel(
                        tags$style(".well {background-color:#FFFFFF;}"),
                        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                        mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                      shiny::mainPanel(
                        htmlOutput(ns("plot_einstieg_pie")))
                            ),
                    tabPanel("Zeitverlauf", br(),
                             shiny::sidebarPanel(
                               mod_schule_kurse_einstieg_verlauf_ui("mod_schule_kurse_einstieg_verlauf_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")))
                             ),
                    tabPanel("Überblick", br(),
                             shiny::sidebarPanel(
                               mod_schule_kurse_einstieg_comparison_ui("mod_schule_kurse_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_comparison")))
                    ),
                    tabPanel("Datensatz", br(),

                      tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                               .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                      shiny::sidebarPanel(
                        tags$style(".well {background-color:#FFFFFF;}"),
                        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                        mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                      shiny::mainPanel(
                        div(DT::dataTableOutput(ns("data_table_einstieg")),
                            style = "font-size: 75%; width: 75%"),
                        shiny::downloadButton(ns("download_data_box1"), label = "",
                                              class = "butt",
                                              icon = shiny::icon("download")))
                            )
      ))),
    fluidRow(
      shinydashboard::box(
        title = "Anteil von Schülerinnen an MINT-Fächern",
        width = 12,
        p("Hier können Sie sich den Anteil von Schülerinnen an MINT- und nicht-MINT-Fächern für
          Leistungs- und Grundkurse anschauen."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_pie_gender_ui("mod_schule_kurse_pie_gender_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_pie_gender")))
                    ),
                    tabPanel("Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_verlauf_gender_ui("mod_schule_kurse_verlauf_gender_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_verlauf_gender")))
                    ),
                    tabPanel("Überblick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_comparison_gender_ui("mod_schule_kurse_comparison_gender_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_comparison_gender")))
                    )

        ))),
    fluidRow(
      shinydashboard::box(
        title = "Einzelne MINT-Fächer im Vergleich",
        width = 12,
        p("Hier können Sie sich die Kursbelegungen für die einzelnen Fächer im Vergleich anschauen."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                 shiny::sidebarPanel(
                                   mod_schule_kurse_multiple_mint_ui("mod_schule_kurse_multiple_mint_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_waffle_mint")))
                    ),
                    tabPanel("Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                 shiny::sidebarPanel(
                                   mod_schule_kurse_verlauf_bl_subjects_ui("mod_schule_kurse_verlauf_bl_subjects_ui_1")),
                                 shiny::mainPanel(
                                   highcharter::highchartOutput(ns("plot_verlauf_kurse_bl_subjects"))

                                 )
                    ),
                    tabPanel("Überblick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_schule_kurse_comparison_subjects_ui("mod_schule_kurse_comparison_subjects_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_comparison_subjects"))

                             )
                    )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Anteil von Schülerinnen in einzelnen MINT-Fächern",
        width = 12,
        p("Hier können Sie die Kursbelegungen von Schülern und Schülerinnen vergleichen."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_schule_kurse_multiple_ui("mod_schule_kurse_multiple_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_waffle")))
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_verlauf_bl_ui("mod_schule_kurse_verlauf_bl_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_verlauf_kurse_bl")))
                    ),
                    tabPanel("Überblick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_schule_kurse_ranking_ui("mod_schule_kurse_ranking_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_ranking_2"))
                             )
                  )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Bundesländer im Vergleich",
        width = 12,
        p("Hier finden Sie Ergebnisse für die Fächerbelegung in den Bundesländern im Vergleich."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_map_ui("mod_schule_kurse_map_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_map_kurse")))
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_verlauf_multiple_ui("mod_schule_kurse_verlauf_multiple_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_verlauf_multiple")))
                  ),
                  tabPanel("Überblick", br(),

                           shiny::sidebarPanel(
                             mod_schule_kurse_comparison_bl_ui("mod_schule_kurse_comparison_bl_ui_1")),
                           shiny::mainPanel(
                             plotOutput(ns("plot_comparison_bl")))
                 )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Anteil von Schülerinnen in den Bundesländern",
        width = 12,
        p("Hier finden Sie Ergebnisse für die Fächerbelegung nach Geschlecht in den Bundesländern."),
        tabsetPanel(type = "tabs",
                    tabPanel("Regionaler Vergleich", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_map_gender_ui("mod_schule_kurse_map_gender_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_map_kurse_gender")))
                    ),
                    tabPanel("Jahresvergleich", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_verlauf_ui("mod_schule_kurse_verlauf_ui_1")),
                             shiny::mainPanel(

                                    highcharter::highchartOutput(ns("plot_verlauf_kurse"))

                             )
                   ),
                   tabPanel("Vergleich", br(),

                            tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                            shiny::sidebarPanel(
                              mod_schule_kurse_ranking_gender_ui("mod_schule_kurse_ranking_gender_ui_1")),
                            shiny::mainPanel(
                              plotOutput(ns("plot_ranking_gender"))
                              )

                  )
        ))),
    fluidRow(
      shinydashboard::box(
        width = 12,
    p(style = "text-align: justify; font-size = 16px",
      span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")), "KMK 2021, auf Anfrage.")),
    p(style = "text-align: justify; font-size = 16px",
      span(tags$b(span("Methodische Hinweise:", style = "color:#b16fab")),
           "Anders als Studierende und Auszubildende lassen sich Schülerinnen
               und Schüler der Oberstufe nicht dem Bereich MINT eindeutig
               zuordnen, weil sie mindestens zwei Leistungskurse wählen müssen.
               Um dennoch einen Anteil von MINT versus Nicht-MINT angeben zu
               können, werden die Kursbelegungen der Schülerinnen und Schüler
               gezählt. Auf die Ausweisung absoluter Zahlen verzichten wir, da
               sie nicht der Grundgesamtheit der Schülerinnen und Schüler
               entspricht."))
      )
    )
  )
}

#' schule_kurse Server Functions
#'
#' @noRd
mod_schule_kurse_server <- function(id, data_kurse, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 2
    output$plot_einstieg_pie <- renderUI({
      kurse_einstieg_pie(data_kurse,r)
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      kurse_verlauf_single(data_kurse,r)
    })

    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      kurse_einstieg_comparison(data_kurse,r)
    })

    data_table_einstieg_react <- reactive({
      data_einstieg_kurse(data_kurse, r)
    })

    output$data_table_einstieg <- DT::renderDT({
      data_table_einstieg_react()
    })

    # Box 3
    output$plot_pie_gender <- renderUI({
      kurse_einstieg_pie_gender(data_kurse,r)
    })

    output$plot_verlauf_gender <- highcharter::renderHighchart({
      kurse_verlauf_gender(data_kurse,r)
    })

    output$plot_comparison_gender <- renderPlot({
      kurse_comparison_gender(data_kurse,r)
    })

    # Box 4
    output$plot_waffle_mint <- renderPlot({
      kurse_waffle_mint(data_kurse,r)
    })

    output$plot_verlauf_kurse_bl_subjects <- highcharter::renderHighchart({
      kurse_verlauf_subjects_bl(data_kurse,r)
    })

    output$plot_comparison_subjects <- renderPlot({
      kurse_mint_comparison(data_kurse,r)
    })

    # Box 5
    plot_waffle_react <- reactive({
      kurse_waffle(data_kurse,r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    output$plot_verlauf_kurse_bl <- highcharter::renderHighchart({
      kurse_verlauf_single_bl(data_kurse,r)
    })

    plot_ranking_react <- reactive({
      kurse_ranking(data_kurse,r, type="other")
    })

    output$plot_ranking_2 <- renderPlot({
      plot_ranking_react()
    })

    # Box 6
    output$plot_map_kurse <- renderUI({
      kurse_map(data_kurse,r)
    })

    output$plot_verlauf_multiple <- highcharter::renderHighchart({
      kurse_verlauf_multiple_bl(data_kurse,r)
    })

    output$plot_comparison_bl <- renderPlot({
      kurse_mint_comparison_bl(data_kurse,r)
    })

    # Box 7
    output$plot_map_kurse_gender <- renderUI({
      kurse_map_gender(data_kurse,r)
    })

    output$plot_verlauf_kurse <- highcharter::renderHighchart({
      kurse_verlauf(data_kurse,r)
    })

    output$plot_ranking_gender <- renderPlot({
      kurse_ranking_gender(data_kurse,r)
    })

    # downloader
    output$download_data_box1 <- shiny::downloadHandler(
      filename = function() {
        paste("data_kurse", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_einstieg_react(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_schule_kurse_ui("schule_kurse_1")

## To be copied in the server
# mod_schule_kurse_server("schule_kurse_1")

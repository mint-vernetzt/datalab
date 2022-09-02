#' beruf_arbeitsmarkt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
      fluidRow(
        shinydashboard::box(
          width = 12,
          img(src='www/Banner_Beruf.jpg',
              class = "img-responsive",
              #height = "150px", width = "150px",
              alt = "Banner Beruf",
              style="display: block; margin-left: auto; margin-right: auto;"
          ))),

       fluidRow(
      shinydashboard::box(
        width = 12,
        #tags$h2("MINT auf dem Arbeitsmark"),
        # p(style = "text-align: justify; font-size = 16px",
        #   "Auf dieser Seite finden Sie statistische Kennzahlen rund um MINT im Bereich Arbeitsmarkt.
        #   Dabei unterscheiden wir zwischen Auszubildenden und Beschäftigten sowie nach Geschlecht.
        #   Zudem können die Daten anhand der Anforderungsprofile auf dem Arbeitsmarkt analysiert werden und unten
        #   haben wir alle Ergebnisse auch für einzelne Bundesländer aufbereitet."),
        #br(),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")), "Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.")),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Methodische Hinweise: ",
                           style = "color:#b16fab")),"Die Kategorierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit.
               Beschäftigte werden nur als MINT klassifiziert, wenn sie einer MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z.B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend.")),

      )),
    fluidRow(
      shinydashboard::box(
        title = "#MINT: Wie hoch ist der Anteil von Auszubildenden und Beschäftigten, die einen MINT-Beruf erlernen bzw. ausüben?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen ingesamt bei Auszubildenden und Beschäftigten in Deutschland.",
          br(),
          "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 32 Prozent aller Auszubildenden eine Ausbildung im Bereich MINT absolvieren.
          Bei den Beschäftigten liegt der Anteil bei 24 Prozent in 2020."),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                             ),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_einstieg_pie"))
                             )
                    ),
                    tabPanel("Vergleich 2", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1")
                             ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_arbeitsmarkt_waffle"))
                             )
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1")
                               ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf"))
                               )


                    ),
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1")
                             ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_vergleich"))
                             )
                    ),
                    tabPanel("Datensatz", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                               .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                               ),
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
        title = "#MINT im Detail: Vergleiche nach Bundesländern",
        width = 12,
        p("Hier zeigen wir die Unterschiede nach Bundesländern. Die Aufbereitung nach Fachbereichen steht noch aus."),
        tabsetPanel(type = "tabs",
                    tabPanel("Karte", br(),

                             shiny::sidebarPanel(
                               mod_beruf_arbeitsmarkt_bl_ui("mod_beruf_arbeitsmarkt_bl_ui_1")
                             ),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_arbeitsmarkt_bl"))
                             )
                    ),
                    tabPanel("Vergleich (Bundesländer)", br(),

                             shiny::sidebarPanel(
                               mod_beruf_arbeitsmarkt_bl_verlauf_ui("mod_beruf_arbeitsmarkt_bl_verlauf_ui_1")
                             ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_beruf_arbeitsmarkt_bl_verlauf"))
                             )
                    ),

                    tabPanel("Überblick (Bundesländer)", br(),

                             shiny::sidebarPanel(
                               mod_beruf_arbeitsmarkt_bl_vergleich_ui("beruf_arbeitsmarkt_bl_vergleich_ui_1")
                             ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_arbeitsmarkt_bl_vergleich"))
                             )
                    )
                    #
        ))),
    fluidRow(
      shinydashboard::box(
        title = "#Frauen in MINT: Wie hoch ist der Anteil von Frauen innerhalb der MINT-Berufe?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Frauen und Männern innerhalb der MINT-Berufe an. Zum Vergleich zeigen wir auch den Anteil in den anderen, nicht-MINT-Berufen."),
        br(),
        p("Interpretationshilfe: Der Anteil von Frauen bei MINT-Auszubildenden in Deutschland betrug 13 Prozent in 2020.
          Bei den MINT-Beschäftigten betrug dieser Anteil 16 Prozent."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_gender_ui("mod_beruf_arbeitsmarkt_einstieg_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_einstieg_pie_gender"))
                             )
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf_gender"))
                             )


                    ),
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_einstieg_vergleich_gender"))
                               )
                    )))),
    fluidRow(
      shinydashboard::box(
        title = "#Berufswahl von Frauen",
        width = 12,
        p("Hier zeigen wir die Unterschiede zwischen Frauen und Männern aus einer anderen Perspektive. Welchen Anteil machen MINT-Berufe bei der Berufswahl von Frauen und Männern aus?"),
        br(),
        p("Interpretationshilfe: Weibliche Auszubildende haben 2020 zu 10 Prozent eine MINT-Ausbildung gewählt. Bei den Männern waren es 48 Prozent.
          Bei den Beschäftigten sind die Unterschiede ähntlich: Bei den Frauen machten MINT-Berufe 8 Prozent aus, bei den Männern 38 Prozent."),
        tabsetPanel(type = "tabs",
                    tabPanel("Karte", br(),

                             shiny::sidebarPanel(
                               mod_beruf_arbeitsmarkt_bl_gender_ui("mod_beruf_arbeitsmarkt_bl_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_arbeitsmarkt_bl_gender"))
                             )
                    ),
                    tabPanel("Detaillierter Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_beruf_arbeitsmarkt_anforderungen_gender_ui("mod_beruf_arbeitsmarkt_anforderungen_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_arbeitsmarkt_waffle_gender"))
                             )
                             ),
                    tabPanel("Vergleich (Bundesländer)", br(),

                             shiny::sidebarPanel(
                               mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui("mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui_1")
                             ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_beruf_arbeitsmarkt_bl_gender_verlauf"))
                             )
                    ),

                    tabPanel("Überblick (Bundesländer)", br(),

                                       shiny::sidebarPanel(
                                         mod_beruf_arbeitsmarkt_bl_gender_vergleich_ui("beruf_arbeitsmarkt_bl_gender_vergleich_ui_1")
                                       ),
                                       shiny::mainPanel(
                                         plotOutput(ns("plot_arbeitsmarkt_bl_gender_vergleich")))
                    )
        )))
    # ,
    # fluidRow(
    #   shinydashboard::box(
    #     title = "Reste",
    #     width = 12,
    #     p("Lorem ipsum dolor sit amet"),
    #     tabsetPanel(type = "tabs",
    #                 tabPanel("Überblick (Anforderungsniveau)", br(),
    #
    #                                   tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
    #                                   .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
    #                                   shiny::sidebarPanel(
    #                                     mod_beruf_arbeitsmarkt_anforderungen_vergleich_ui("mod_beruf_arbeitsmarkt_anforderungen_vergleich_ui_1")
    #                                   ),
    #                                   shiny::mainPanel(
    #                                     plotOutput(ns("plot_arbeitsmarkt_vergleich"))
    #                                   )
    #                          ),
    #                 tabPanel("Überblick (Anforderungsniveau)", br(),
    #
    #                          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
    #                          .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
    #                          shiny::sidebarPanel(
    #                            mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_ui("mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_ui_1")
    #                          ),
    #                          shiny::mainPanel(
    #                            plotOutput(ns("plot_arbeitsmarkt_vergleich_gender"))
    #                          )
    #                 ),
    #                 tabPanel("Vergleich (Beschäftigungsform)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_ui("mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_ui_1")
    #                          ),
    #                          shiny::mainPanel(
    #                            highcharter::highchartOutput(ns("plot_arbeitsmarkt_verlauf_gender")))
    #                 ),
    #                 tabPanel("Vergleich (Anforderungsniveaus)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_beruf_arbeitsmarkt_anforderungen_verlauf_ui("mod_beruf_arbeitsmarkt_anforderungen_verlauf_ui_1")
    #                          ),
    #                          shiny::mainPanel(
    #                            highcharter::highchartOutput(ns("plot_arbeitsmarkt_verlauf"))
    #                          )
    #                 )
    #
    # )
  )
}

#' beruf_arbeitsmarkt Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_server <- function(id, data_arbeitsmarkt, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 2
    plot_einstieg_pie_react <- reactive({
      arbeitsmarkt_einstieg_pie(data_arbeitsmarkt,r)

    })

    output$plot_einstieg_pie <- renderUI({
      plot_einstieg_pie_react()

    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      beruf_verlauf_single(data_arbeitsmarkt,r)
    })

    output$plot_einstieg_vergleich <- highcharter::renderHighchart({
      beruf_einstieg_vergleich(data_arbeitsmarkt,r)
    })

    data_table_einstieg_react <- reactive({
      data_einstieg_beruf(data_arbeitsmarkt, r)
    })

    output$data_table_einstieg <- DT::renderDT({
      data_table_einstieg_react()
    })

    # Box 3
    output$plot_einstieg_pie_gender <- renderUI({
      arbeitsmarkt_einstieg_pie_gender(data_arbeitsmarkt,r)
    })

    output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
      arbeitsmarkt_einstieg_verlauf_gender(data_arbeitsmarkt, r)
    })

    output$plot_einstieg_vergleich_gender <- renderPlot({
      arbeitsmarkt_einstieg_vergleich_gender(data_arbeitsmarkt,r)
    })

    # Box 4
    output$plot_arbeitsmarkt_waffle <- renderPlot({
      arbeitsmarkt_anforderungen(data_arbeitsmarkt, r)
    })

    output$plot_arbeitsmarkt_verlauf <- highcharter::renderHighchart({
      arbeitsmarkt_anforderungen_verlauf(data_arbeitsmarkt, r)
    })

    output$plot_arbeitsmarkt_vergleich <- renderPlot({
      arbeitsmarkt_anforderungen_vergleich(data_arbeitsmarkt, r)
    })

    # Box 5
    output$plot_arbeitsmarkt_waffle_gender <- renderPlot({
      arbeitsmarkt_anforderungen_gender(data_arbeitsmarkt, r)
    })

    output$plot_arbeitsmarkt_verlauf_gender <- highcharter::renderHighchart({
      arbeitsmarkt_anforderungen_verlauf_gender(data_arbeitsmarkt, r)
    })

    output$plot_arbeitsmarkt_vergleich_gender <- renderPlot({
      arbeitsmarkt_anforderungen_vergleich_gender(data_arbeitsmarkt, r)
    })

    # Box 6
    output$plot_arbeitsmarkt_bl <- renderUI({
      arbeitsmarkt_bl(data_arbeitsmarkt,r)
    })

    output$plot_beruf_arbeitsmarkt_bl_verlauf <- highcharter::renderHighchart({
      arbeitsmarkt_bl_verlauf(data_arbeitsmarkt,r)
    })

    output$plot_arbeitsmarkt_bl_vergleich <- renderPlot({
      arbeitsmarkt_bl_vergleich(data_arbeitsmarkt,r)
    })

    # Box 7
    output$plot_arbeitsmarkt_bl_gender <- renderUI({
      arbeitsmarkt_bl_gender(data_arbeitsmarkt,r)
    })

    output$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- highcharter::renderHighchart({
      arbeitsmarkt_bl_gender_verlauf(data_arbeitsmarkt,r)
    })

    output$plot_arbeitsmarkt_bl_gender_vergleich <- renderPlot({
      arbeitsmarkt_bl_gender_vergleich(data_arbeitsmarkt,r)
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
# mod_beruf_arbeitsmarkt_ui("beruf_arbeitsmarkt_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_server("beruf_arbeitsmarkt_1")

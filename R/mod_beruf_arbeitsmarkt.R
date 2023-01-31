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
          img(src='www/Banner_Ausbildung.jpg',
              class = "img-responsive",
              #height = "150px", width = "150px",
              alt = "Banner Beruf",
              style="display: block; margin-left: auto; margin-right: auto;"
          ))),

       fluidRow(
      shinydashboard::box(
        width = 9,
        titel = "Ausbildung und Beschäftigung in MINT",
        p(style = "text-align: justify; font-size = 16px",
        span(tags$b(span("Kurzbeschreibung der Seite:", style = "color:#b16fab")),
          "Auf dieser Seite zeigen wir statistische Kennzahlen rund um MINT im Bereich Arbeitsmarkt.
          Dabei unterscheiden wir zwischen Auszubildenden und Beschäftigten. Vergleiche sind zusätzlich nach
          Geschlecht und nach Bundesländern möglich.")),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")), "Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Methodische Hinweise: ",
                           style = "color:#b16fab")),"Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert,
                           wenn sie einer MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Informationen unter \"Datenquellen und Hinweise\".",
               br(),br(),
               "Die Rundung der berechneten Werte kann zu minimalen Abweichungen zwischen den Grafiken führen.",
               br(), br(),
               "Der hier verwendete Indikator 'Beschäftigte' umfasst alle sozialversicherungspflichtigen Beschäftigten. Er umfasst sowohl Fachkräfte als auch Hilfskräfte. Eine Differenzierung erfolgt zeitnah."
               )),

      ),
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 3,
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("#MINT:")),"Wie hoch ist der Anteil von Auszubildenden und Beschäftigten, die einen MINT-Beruf erlernen bzw. ausüben?"
          )),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("#MINT im Detail:")),"Vergleiche der Bundesländer"
          )),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("#Frauen in MINT:")),"Wie hoch ist der Anteil von Frauen innerhalb der MINT-Berufe?"
          )),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("#Berufswahl von Frauen")),"Wie unterscheidet sich die Berufswahl von Männern und Frauen?"
          )),
      )),

    fluidRow(
      shinydashboard::box(
        title = "#MINT: Wie hoch ist der Anteil von Auszubildenden und Beschäftigten, die einen MINT-Beruf erlernen bzw. ausüben?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen insgesamt bei Auszubildenden und Beschäftigten in Deutschland.",
          br(), br(),
          "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 32 % aller Auszubildenden eine Ausbildung im Bereich MINT absolvieren.
          Bei den Beschäftigten liegt der Anteil 2020 bei 24 %."),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_einstieg_pie"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1")
                               ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                               )


                    ),
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_vergleich"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    )
                    # ,
                    # tabPanel("Datensatz", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #            .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            tags$style(".well {background-color:#FFFFFF;}"),
                    #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #            mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                    #            ),
                    #          shiny::mainPanel(
                    #            div(DT::dataTableOutput(ns("data_table_einstieg")),
                    #                style = "font-size: 75%; width: 75%"),
                    #            shiny::downloadButton(ns("download_data_box1"), label = "",
                    #                                  class = "butt",
                    #                                  icon = shiny::icon("download")))
                    # )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "#MINT im Detail: Vergleiche der Bundesländer",
        width = 12,
        p("Hier zeigen wir die Unterschiede nach Bundesländern. Die Aufbereitung nach Fachbereichen steht noch aus.",
          br(), br(),
          "Interpretationshilfe: Auf der interaktiven Deutschlandkarte sieht man beispielsweise, dass 2020 Berlin mit 23 %
          den geringsten Anteil an Auszubildenden im MINT-Bereich aufweist. Das Bundesland mit dem höchsten Anteil an Beschäftigten im MINT-Bereich
          ist Baden-Württemberg mit 29 %."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1")

                             ),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_arbeitsmarkt_waffle")),
                               p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    tabPanel("Karte", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_ui("mod_beruf_arbeitsmarkt_bl_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_arbeitsmarkt_bl"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    tabPanel("Vergleich (Bundesländer)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_verlauf_ui("mod_beruf_arbeitsmarkt_bl_verlauf_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_beruf_arbeitsmarkt_bl_verlauf"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),

                    tabPanel("Überblick (Bundesländer)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_vergleich_ui("beruf_arbeitsmarkt_bl_vergleich_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_arbeitsmarkt_bl_vergleich"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    )
                    #
        ))),
    fluidRow(
      shinydashboard::box(
        title = "#Frauen in MINT: Wie hoch ist der Anteil von Frauen innerhalb der MINT-Berufe?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Frauen und Männern innerhalb der MINT-Berufe in Deutschland an. Zum Vergleich zeigen wir auch den Anteil in den anderen, nicht-MINT-Berufen.",
        br(), br(),
        "Interpretationshilfe: Der Anteil von Frauen an MINT-Auszubildenden in Deutschland beträgt 13 % im Jahr 2020.
        Bei den MINT-Beschäftigten beträgt dieser Anteil 16 %. Dagegen machen Frauen in anderen Berufen mehr als die Hälfte aller
        Auszubildenden und Beschäftigten aus (jeweils 56 %)."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_gender_ui("mod_beruf_arbeitsmarkt_einstieg_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_einstieg_pie_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )


                    ),
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_vergleich_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                               )
                    )))),
    fluidRow(
      shinydashboard::box(
        title = "#Berufswahl von Frauen: Wie unterscheidet sich die Berufswahl von Männern und Frauen?",
        width = 12,
        p("Hier zeigen wir die Unterschiede zwischen Frauen und Männern aus einer anderen Perspektive. Welchen Anteil machen MINT-Berufe bei der Berufswahl von Frauen und Männern in Deutschland aus?",
        br(), br(),
        "Interpretationshilfe: Der detaillierte Vergleich zeigt, dass weibliche Auszubildende 2020 deutschlandweit zu 10 % eine MINT-Ausbildung wählen. Bei den Männern sind es 48 %.
        Bei den Beschäftigten sind die Unterschiede ähnlich: Bei den Frauen machen MINT-Berufe 8 % aus, bei den Männern 38 %."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_anforderungen_gender_ui("mod_beruf_arbeitsmarkt_anforderungen_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_arbeitsmarkt_waffle_gender"))
                               ,p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    tabPanel("Karte", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_gender_ui("mod_beruf_arbeitsmarkt_bl_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_arbeitsmarkt_bl_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    tabPanel("Vergleich (Bundesländer)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui("mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_beruf_arbeitsmarkt_bl_gender_verlauf"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),

                    tabPanel("Überblick (Bundesländer)", br(),

                                       shiny::sidebarPanel(
                                         width = 3,
                                         mod_beruf_arbeitsmarkt_bl_gender_vergleich_ui("beruf_arbeitsmarkt_bl_gender_vergleich_ui_1")
                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         plotOutput(ns("plot_arbeitsmarkt_bl_gender_vergleich"))
                                         ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen."))

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
    ,
    tags$footer(style="text-align: justify;background-color:white",

      div(style="display: inline-block;position: relative;padding: 1em;",

          tags$a(href="https://mint-vernetzt.de/",
                    img(src='www/MINTv_tranparent.png',
                     class = "img-responsive",
                    height = "100px", width = "100px",
                      alt = "Logo MINT", target="_blank",
                      style="display: inline-block; margin-left: auto; margin-right:10%;"))),

      div(style="display: inline-block;position: relative;padding: 1em;",

          p(tags$a("Impressum", href="#shiny-tab-impressum", "data-toggle" = "tab")," | ",
          tags$a("Kontakt", href="#shiny-tab-kontakt", "data-toggle" = "tab")," | ",
          tags$a("Datenschutz", href="#shiny-tab-datenschutz", "data-toggle"="tab"),HTML('&nbsp;'),HTML('&nbsp;'),
          "Copyright © 2022. Alle Rechte vorbehalten Stifterverband")),

      div(style="display: inline-block;position: relative;padding: 1em;",

          tags$a(#href="https://www.bmbf.de/bmbf/de/home/home_node.html",
             img(src='www/BMBF-Logo_transp1.png',

                 class = "img-responsive",

                 height = "200px", width = "200px",

                 alt = "Logo BMBF", target="_blank",

                 style="display: inline-block; margin-left: auto; margin-right: auto;"))),

      div(style="display: inline-block;width: 100%;",

          " ")

      ))


}

#' beruf_arbeitsmarkt Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_server <- function(id, data_arbeitsmarkt,data_arbeitsmarkt_detail, r){
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

    output$plot_einstieg_vergleich_gender <- highcharter::renderHighchart({
      arbeitsmarkt_einstieg_vergleich_gender(data_arbeitsmarkt,r)
    })

    # Box 4
    output$plot_arbeitsmarkt_waffle <- renderPlot({
      arbeitsmarkt_anforderungen(data_arbeitsmarkt_detail, r)
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

    output$plot_arbeitsmarkt_bl_vergleich <- highcharter::renderHighchart({
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

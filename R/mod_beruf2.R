#' beruf2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf2_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' kontakt Server Functions
#'
#' @noRd
mod_beruf2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

mod_beruf2_ui <- function(id){
  ns <- NS(id)
  tagList(

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
          ))

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
      title = "Einstieg MINT",
      width = 12,
      p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen insgesamt bei Auszubildenden und Beschäftigten in Deutschland.",
        br(), br(),
        "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 32 % aller Auszubildenden eine Ausbildung im Bereich MINT absolvieren.
          Bei den Beschäftigten liegt der Anteil 2020 bei 24 %."),

      tabsetPanel(type = "tabs",
                  tabPanel("Vergleich MINT und andere Fachbereiche", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                           ),
                           shiny::mainPanel(
                             htmlOutput(ns("plot_einstieg_pie"))
                             ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  ),
                  tabPanel("Vergelich Bundesländer", br(),

                           tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           shiny::sidebarPanel(
                             mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1")
                             ,p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")

                           ),
                           shiny::mainPanel(
                             plotOutput(ns("plot_arbeitsmarkt_waffle")),
                             p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  ),
                  tabPanel("Vergleich MINT-Fachbereiche", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1")
                           ),
                           shiny::mainPanel(
                             highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                             p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
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
                             ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  )

      ))),


  fluidRow(
    shinydashboard::box(
      title = "Einstieg Frauen",
      width = 12,
      p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen insgesamt bei Auszubildenden und Beschäftigten in Deutschland.",
        br(), br(),
        "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 32 % aller Auszubildenden eine Ausbildung im Bereich MINT absolvieren.
          Bei den Beschäftigten liegt der Anteil 2020 bei 24 %."),

      tabsetPanel(type = "tabs",
                  tabPanel("Welche Fachbereiche wählen die meisten Frauen?", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                           ),
                           shiny::mainPanel(
                             htmlOutput(ns("plot_einstieg_pie"))
                             ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  ),
                  tabPanel("Vergleich Geschlechterverhältnisse", br(),

                           tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           shiny::sidebarPanel(
                             mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1")
                             ,p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")

                           ),
                           shiny::mainPanel(
                             plotOutput(ns("plot_arbeitsmarkt_waffle")),
                             p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  ),
                  tabPanel("Unter Frauen beliebteste Fachbereiche (Top 10)", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1")
                           ),
                           shiny::mainPanel(
                             highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                             p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )


                  ),
                  tabPanel("Bundesländervergleich", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1")
                           ),
                           shiny::mainPanel(
                             highcharter::highchartOutput(ns("plot_einstieg_vergleich"))
                             ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  )

      ))),
#
#   fluidRow(
#     shinydashboard::box(
#       title = "Fokus MINT",
#       width = 12,
#       p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen insgesamt bei Auszubildenden und Beschäftigten in Deutschland.",
#         br(), br(),
#         "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 32 % aller Auszubildenden eine Ausbildung im Bereich MINT absolvieren.
#           Bei den Beschäftigten liegt der Anteil 2020 bei 24 %."),
#
#       tabsetPanel(type = "tabs",
#                   tabPanel("Regionaldaten", br(),
#
#                            shiny::sidebarPanel(
#                              tags$style(".well {background-color:#FFFFFF;}"),
#                              tags$head(tags$style(HTML(".small-box {height: 140px}"))),
#                              mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
#                            ),
#                            shiny::mainPanel(
#                              htmlOutput(ns("plot_einstieg_pie"))
#                              ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
#                            )
#                   ))),
      #             tabPanel("Vergelich Bundesländer", br(),
      #
      #                      tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
      #                                      .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
      #                      shiny::sidebarPanel(
      #                        mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1")
      #                        ,p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
      #
      #                      ),
      #                      shiny::mainPanel(
      #                        plotOutput(ns("plot_arbeitsmarkt_waffle")),
      #                        p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
      #                      )
      #             ),
      #             tabPanel("Vergleich Bundesländer", br(),
      #
      #                      shiny::sidebarPanel(
      #                        tags$style(".well {background-color:#FFFFFF;}"),
      #                        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
      #                        mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1")
      #                      ),
      #                      shiny::mainPanel(
      #                        highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
      #                        p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
      #                      )
      #
      #
      #             ),
      #             tabPanel("Überblick", br(),
      #
      #                      shiny::sidebarPanel(
      #                        tags$style(".well {background-color:#FFFFFF;}"),
      #                        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
      #                        mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1")
      #                      ),
      #                      shiny::mainPanel(
      #                        highcharter::highchartOutput(ns("plot_einstieg_vergleich"))
      #                        ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
      #                      )
      #             )
      #
      # ))),


  fluidRow(
    shinydashboard::box(
      title = "Fokus Regionaldaten",
      width = 12,
      p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen insgesamt bei Auszubildenden und Beschäftigten in Deutschland.",
        br(), br(),
        "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 32 % aller Auszubildenden eine Ausbildung im Bereich MINT absolvieren.
          Bei den Beschäftigten liegt der Anteil 2020 bei 24 %."),

      tabsetPanel(type = "tabs",
                  tabPanel("Meine Region im Vergleich", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                           ),
                           shiny::mainPanel(
                             htmlOutput(ns("plot_einstieg_pie"))
                             ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  ),
                  tabPanel("Vergleich Fachbereiche", br(),

                           tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           shiny::sidebarPanel(
                             mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1"),

                             p(style="font-size:12px;color:grey"

                               , br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")

                           ),
                           shiny::mainPanel(
                            plotOutput(ns("plot_arbeitsmarkt_waffle")),
                             p(style="font-size:12px;color:grey"
                               , br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  ),
                  tabPanel("Vergleich Bundesländer", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1")
                           ),
                           shiny::mainPanel(
                             highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                             p(style="font-size:12px;color:grey", br(),
                            "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )


                  ),
                  tabPanel("Vergleich Qualifizierungsniveaus", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}")))
                              ,
                             mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1")
                           ),
                           shiny::mainPanel(
                             highcharter::highchartOutput(ns("plot_einstieg_vergleich")),
                             p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  )

      ))),

  fluidRow(
    shinydashboard::box(
      title = "Fokus ausländische Fachkräfte",
      width = 12,
      p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen insgesamt bei Auszubildenden und Beschäftigten in Deutschland.",
        br(), br(),
        "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 32 % aller Auszubildenden eine Ausbildung im Bereich MINT absolvieren.
          Bei den Beschäftigten liegt der Anteil 2020 bei 24 %."),

      tabsetPanel(type = "tabs",
                  tabPanel("Bundesländervergleich", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                           ),
                           shiny::mainPanel(
                             htmlOutput(ns("plot_einstieg_pie")),
                             p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  ),
                  tabPanel("Vergleich Qulifizierungsniveaus", br(),

                           tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           shiny::sidebarPanel(
                            mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1"),
                             p(style="font-size:12px;color:grey"
                               , br(),
                              "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")

                           ),
                           shiny::mainPanel(
                             plotOutput(ns("plot_arbeitsmarkt_waffle")),
                             p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )
                  ),
                  tabPanel("lorem", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}")))
                             # ,
                             # mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1")
                           ),
                           shiny::mainPanel(
                             highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                             p(style="font-size:12px;color:grey"
                              , "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           )


                  ),
                  tabPanel("ipsum", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1")
                           ),
                           shiny::mainPanel(
                             highcharter::highchartOutput(ns("plot_einstieg_vergleich"))
                             ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.")
                           ))
                  )

      )),



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










## To be copied in the UI
# mod_kontakt_ui("beruf_2")

## To be copied in the server
# mod_kontakt_server("beruf_2")

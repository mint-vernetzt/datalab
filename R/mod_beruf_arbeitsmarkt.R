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
          img(src='www/Banner_Ausbildung_BB.jpg',
              class = "img-responsive",
              #height = "150px", width = "150px",
              alt = "Banner Beruf",
              style="display: block; margin-left: auto; margin-right: auto;"
          ))),


      # Info-Texte

      fluidRow(
        shinydashboard::box(
          title = "Auf dieser Seite",
          width = 7,
          p(style = "text-align: left; font-size = 16px",
            "Auf dieser Seite zeigen wir statistische Kennzahlen rund um MINT im Bereich Arbeitsmarkt.
           Dabei unterscheiden wir zwischen Auszubildenden und (sozialversicherungspflichtigen) Beschäftigten.  Die Kategorisierung in MINT entspricht der Klassifikation durch die Bundesagentur für Arbeit.
            'Hinweise & Datenquellen'.")
        ),

        shinydashboard::box(
          title = "Fragen oder Feedback?",
          width = 5,
          p(style = "text-align: left; font-size = 16px",
            "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
            tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
          ))
      ),

      fluidRow(
        shinydashboard::box(
          title = "Übersicht Fragestellungen",
          width = 7,
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_mint",
            span(tags$b(span("Berufswahl MINT:")))),"Wie hoch ist der Anteil von Auszubildenden und Beschäftigten in MINT?"
            ),
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_fach",
            span(tags$b(span("M-I-N-T:")))),"Blick auf die einzelnen Fächer und Fachbereiche."

            ),
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_frauen",
            span(tags$b(span("Frauen in MINT:")))),"Wie hoch ist der Anteil von Frauen innerhalb der MINT-Berufe?"
            ),
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_international",
                                                                span(tags$b(span("MINT-Beschäftigung im internationalen Vergleich:")))),
            "Hier können Sie den MINT-Anteil im deutschen Arbeitsmarkt international vergleichen."
          ),
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_regional",
            span(tags$b(span("Regionaler MINT-Steckbrief:")))),"Hier bieten wir die Möglichkeit, den eigenen Landkreis unter die Lupe zu nehmen."
            )),

        shinydashboard::box(
          title = "Datenquellen",
          width = 5,
          p(style = "text-align: left; font-size = 16px",
            "Auszubildenden- und Beschäftigenzahlen in Deutschland: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
          p(style = "text-align: left; font-size = 16px",
            "Studierendenzahlen aus Europa: Eurostat 2023, freier Download, eigene Berechnungen durch MINTvernetzt."),
          p(style = "text-align: left; font-size = 16px",
            "Studierendenzahlen der OECD-Länder: OECD 2023, freier Download, eigene Berechnungen durch MINTvernetzt.")

        )
      ),


      # Box 1

    fluidRow( id="beruf_mint",
      shinydashboard::box(
        title = "Berufswahl MINT: Wie hoch ist der Anteil von Auszubildenden und Beschäftigten in MINT?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen insgesamt bei Auszubildenden und Beschäftigten in Deutschland und Unterschiede in der Berufswahl von Männern und Frauen?"),


        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Anteil MINT bei Beschäftigten und Auszubildenden", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1"),

                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(plotOutput(ns("plot_arbeitsmarkt_waffle")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", br(),
                                 "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_mint_1", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> Durch Rundung der berechneten Werte kann es zu minimalen Abweichungen zwischen den Grafiken kommen."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_1")
                            )
                    ),

                    tabPanel("Vergleich Anteil MINT an Beschäftigten und Auszubildenden im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_mint_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_2"),

                               )


                    ),
                    tabPanel("Alle Beschäftigtengruppen auf einen Blick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_einstieg_vergleich")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
                             )
                    ),

                    tabPanel("Vergleich Anteil MINT bei Frauen und Männern", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_anforderungen_gender_ui("mod_beruf_arbeitsmarkt_anforderungen_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(plotOutput(ns("plot_arbeitsmarkt_waffle_gender")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                               shinyBS::bsPopover(id = "h_beruf_mint_4", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br> Durch Rundung der berechneten Werte kann es zu minimalen Abweichungen zwischen den Grafiken kommen."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_4")
                             )
                    ),

                    tabPanel("Vergleich Anteil von Frauen & Männer (Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_gender_ui("mod_beruf_arbeitsmarkt_bl_gender_ui_1"),
                              ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_bl_gender")),
                                                            color = "#154194"),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_mint_5", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_5")
                             )
                    ),

                    # tabPanel("Pie (RAUS)", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            tags$style(".well {background-color:#FFFFFF;}"),
                    #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #            mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            htmlOutput(ns("plot_einstieg_pie"))
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                    #          )
                    # ),
                    #br(),
                    tabPanel("Vergleich Anteil MINT bei Frauen nach Bundesländern im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui("mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_beruf_arbeitsmarkt_bl_gender_verlauf")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_mint_6", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_6")
                             )
                    ),
                    tabPanel("Vergleich Anteil MINT nach Bundesländern im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_verlauf_ui("mod_beruf_arbeitsmarkt_bl_verlauf_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_beruf_arbeitsmarkt_bl_verlauf")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_mint_7", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_7")
                             )
                    ),

                    # erstmal raus, weil so kompliziert
                    # tabPanel("Berufswahl MINT Frauen, nach Bundesländer (RAUS))", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_beruf_arbeitsmarkt_bl_gender_vergleich_ui("beruf_arbeitsmarkt_bl_gender_vergleich_ui_1")
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            plotOutput(ns("plot_arbeitsmarkt_bl_gender_vergleich"))
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."))
                    #
                    # )


                    # ,
                    # tabPanel("Datensatz", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #            .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            tags$style(".well {background-color:#FFFFFF;}"),
                    #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #            mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                    #            ),
                    #          shiny::mainPanel(
                    #            div(DT::dataTableOutput(ns("data_table_einstieg")),
                    #                style = "font-size: 75%; width: 75%"),
                    #            shiny::downloadButton(ns("download_data_box1"), label = "",
                    #                                  class = "butt",
                    #                                  icon = shiny::icon("download")))
                    # )
        ))),


    # Box 2

    fluidRow(id="beruf_fach",
      shinydashboard::box(
        title = "M-I-N-T: Blick auf die einzelnen Fächer und Fachbereiche",
        width = 12,
        p("Hier zeigen wir die Unterschiede nach MINT-Berufsbereichen Mathematik/ Naturwissenschaft, Informatik und Technik.
        Außerdem können die Top 10 MINT-Ausbildunsberufe von Frauen und Männern verglichen werden. Hierfür betrachten wir die
          neu abgeschlossenen Ausbildungsverträge des jeweiligen Jahres."),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Anteil MINT-Berufsfelder zwischen Auszubildenden und Beschäftigten (Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_ui("mod_beruf_arbeitsmarkt_bl_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_bl")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_fach_1", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_1")
                             )
                    ),
                    tabPanel("Alle Bereiche auf einen Blick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_überblick_fächer_ui("mod_beruf_arbeitsmarkt_überblick_fächer_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_arbeitsmarkt_überblick_fächer")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_fach_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_2")
                             )
                             ),


                    tabPanel("Vergleich Anteil MINT-Bereiche nach Bundesländern", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_vergleich_ui("beruf_arbeitsmarkt_bl_vergleich_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_arbeitsmarkt_bl_vergleich")),
                                                            color = "#154194"),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_fach_3", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_3")
                             )
                    ),

                    tabPanel("Top 10 MINT-Ausbildungsberufe", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_top10_ui("mod_beruf_arbeitsmarkt_top10_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_top10")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_fach_4", title = "",
                                                  content = paste0("Hier gezeigt werden nur neue Auszubildende im Fachbereich MINT des jeweiligen Jahres. Ausbidlungsberufe mit weniger als 10 neuen Vertragsabschlüssen für das betrachtete Jahr wurden ausgeschlossen. <br><br>In manchen Fällen weisen mehr als 10 Berufe einen Männeranteil von 100 % auf. In diesen Fällen sind die 10 Berufe mit 100 % Männeranteil angezeigt, welche die meisten Neu-Auszubildenden haben.", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_4")
                             )
                    )
                    #
        ))),

    # Box 3

    fluidRow(id="beruf_frauen",
      shinydashboard::box(
        title = "Frauen in MINT: Wie hoch ist der Anteil von Frauen innerhalb der MINT-Berufe?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Frauen und Männern innerhalb der MINT-Berufe in Deutschland an. Zum Vergleich zeigen wir auch den Anteil in den anderen, nicht-MINT-Berufen."),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Anteil Frauen bei MINT-Auszubildenden und MINT-Beschäftigen", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_gender_ui("mod_beruf_arbeitsmarkt_einstieg_gender_ui_1")

                             ),

                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_pie_gender")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_frauen_1", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_frauen_1")
                             )
                    ),
                    tabPanel("Vergleich Anteil Frauen in MINT-Berufen im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_einstieg_verlauf_gender")),
                                                            color = "#154194"),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_frauen_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_frauen_2")
                             )


                    ),
                    tabPanel("Anteil Frauen an verschiedenen MINT-Berufsgruppen", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_einstieg_vergleich_gender")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_frauen_3", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_frauen_3")
                               )
                    )
        ))),

    # Box International ----

    fluidRow(id="beruf_international",
             shinydashboard::box(
               title = "MINT-Beschäftigung im internationalen Vergleich: Hier können Sie den MINT-Anteil im deutschen Arbeitsmarkt international vergleichen.",
               width = 12,
               p("Diese Box zeigt eine Übersicht von MINT-Statistiken aus dem Bereich des Arbeitsmarkts für den internationalen Vergleich.
                 Die Grafiken basieren auf öffentlichen Statistiken, die durch die EU und die OECD gesammelt wurden.
                 Zum einen zeigen wir, wie groß der Anteil von MINT-Auszubildenden und Beschäftigten in verschiedenen Ländern ist.
                 Außerdem ist zu sehen, in welchen Ländern der Frauenanteil besonders groß oder klein ist.
                 Darüber hinaus werfen wir einen Blick auf Studiums- bzw. Ausbildungs-Anfänger*innen und Absolvent*innen in MINT
                 im Ländervergleich."),

               tabsetPanel(type = "tabs",
                           tabPanel("Vergleich MINT-Anteil (Karte)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_map_arb_ui("mod_international_map_arb_ui_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_studienzahl_map_arb_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."),
                                      shinyBS::bsPopover(id = "h_beruf_international_1", title = "",
                                                         content = paste0("Ausgebildete umfassen alle Personen mit einem tertiären Bildungsabschluss im Bereich MINT, inkl. Studienabschlüssen.", "<br><br>Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise und Datenquellen&quot.", "<br><br>Da Eurostat den Begriff &quotBeschäftigte in MINT&quot sehr breit fasst (z. B. inkl. des Gesundheitswesens), betrachten wir hier nur die kleinere Gruppe an Naturwissenschaftler*innen und Ingenieur*innen.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_1")
                                    )
                           ),
                           tabPanel("Vergleich Frauen in MINT (Karte)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_map_arb_gender_ui("mod_international_map_arb_gender_ui_1")
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_map_arb_gender_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."),
                                      shinyBS::bsPopover(id = "h_beruf_international_2", title = "",
                                                         content = paste0("Ausgebildete umfassen alle Personen mit einem tertiären Bildungsabschluss im Bereich MINT, inkl. Studienabschlüssen.", "<br><br>Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise und Datenquellen&quot.", "<br><br>Da Eurostat den Begriff &quotBeschäftigte in MINT&quot sehr breit fasst (z. B. inkl. des Gesundheitswesens), betrachten wir hier nur die kleinere Gruppe an Naturwissenschaftler*innen und Ingenieur*innen.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_2")
                                    )
                           ),tabPanel("Top 10 MINT-Länder", br(),

                                      #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                      # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                      shiny::sidebarPanel(
                                        width = 3,
                                        #p("LOREM"),
                                        mod_international_top10_mint_arb_ui("mod_international_top10_mint_arb_ui_1")
                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(htmlOutput(ns("plot_international_top10_mint_arb_1")),
                                                                     color = "#154194"),

                                        p(style="font-size:12px;color:grey",
                                          "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."),
                                        shinyBS::bsPopover(id = "h_beruf_international_3", title = "",
                                                           content = paste0("Ausgebildete umfassen alle Personen mit einem tertiären Bildungsabschluss im Bereich MINT, inkl. Studienabschlüssen.", "<br><br>Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise und Datenquellen&quot.", "<br><br>Da Eurostat den Begriff &quotBeschäftigte in MINT&quot sehr breit fasst (z. B. inkl. des Gesundheitswesens), betrachten wir hier nur die kleinere Gruppe an Naturwissenschaftler*innen und Ingenieur*innen.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet"),
                                                           placement = "top",
                                                           trigger = "hover"),
                                        tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_3")
                                      )
                           ),tabPanel("Top 10 Länder Frauen in MINT", br(),

                                      #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                      # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                      shiny::sidebarPanel(
                                        width = 3,
                                        #p("LOREM"),
                                        mod_international_top10_mint_arb_gender_ui("mod_international_top10_mint_arb_gender_ui_1")
                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(htmlOutput(ns("plot_international_top10_mint_arb_gender_1")),
                                                                     color = "#154194"),

                                        p(style="font-size:12px;color:grey",
                                          "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."),
                                        shinyBS::bsPopover(id = "h_beruf_international_4", title = "",
                                                           content = paste0("Ausgebildete umfassen alle Personen mit einem tertiären Bildungsabschluss im Bereich MINT, inkl. Studienabschlüssen.", "<br><br>Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise und Datenquellen&quot.", "<br><br>Da Eurostat den Begriff &quotBeschäftigte in MINT&quot sehr breit fasst (z. B. inkl. des Gesundheitswesens), betrachten wir hier nur die kleinere Gruppe an Naturwissenschaftler*innen und Ingenieur*innen.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet"),
                                                           placement = "top",
                                                           trigger = "hover"),
                                        tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_4")
                                      )
                           ),tabPanel("MINT-Anfänger:innen und Absolvent:innen", br(),

                                      #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                      # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                      shiny::sidebarPanel(
                                        width = 3,
                                        #p("LOREM"),
                                        mod_international_arbeitsmarkt_vergleich_ui("international_arbeitsmarkt_vergleich_1")

                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(htmlOutput(ns("plot_international_arbeitsmarkt_vergleiche_1")),
                                                                     color = "#154194"),

                                        p(style="font-size:12px;color:grey",
                                          "Quelle der Daten: OECD, 2023, als Download, eigene Berechnungen durch MINTvernetzt."),

                                        # shinyBS::bsPopover(id="h_international_arbeit_3", title="",
                                        #                    content = paste0("POPUP INFO TEXT HERE"),
                                        #                    placement = "top",
                                        #                    trigger = "hover"),
                                        # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_arbeit_3")
                                      )
                           )
               )
             )
    ),

    # Box Regional ----

    fluidRow(id="beruf_regional",
      shinydashboard::box(
        title = "Regionaler MINT-Steckbrief",
        width = 12,
        p("Hier bieten wir die Möglichkeit, den eigenen Landkreis unter die Lupe zu nehmen.",

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Landkreise (als Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_landkreis_map_ui("mod_beruf_arbeitsmarkt_landkreis_map_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_detail_map")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_regional_1", title = "",
                                                  content = paste0("Manche Landkreise sind grau dargestellt oder fehlen in der Darstellung. Das liegt daran, dass die zugrundeliegenden Karten vereinzelt alte oder falsche Landkreiszuordnungen (in Niedersachen, Sachsen-Anhalt) enthalten oder einzelne Regionen gar nicht enthalten (Bremen, in Sachsen). Daten zu den fehlenden Regionen sind in der Darstellung im nächstne Tab zu finden.", "<br> <br> Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br>In die Kategorie &quotAuszubildende mit neuem Lehrvertrag&quot fallen sowohl neue Auszubilndende als auch Auszubildende nach Vertragswechsel."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_regional_1")
                             )
                    ),
                    tabPanel("Vergleich Landkreise, Auflistung aller Landkreise", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_landkreis_vergleich_ui("mod_beruf_arbeitsmarkt_landkreis_vergleich_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_arbeitsmarkt_detail_vergleich"),
                                                                                         height = "1600px"),
                                                            color = "#154194"),

                               shinyBS::bsPopover(id = "h_beruf_regional_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br>In die Kategorie &quotAuszubildende mit neuem Lehrvertrag&quot fallen sowohl neue Auszubilndende als auch Auszubildende nach Vertragswechsel."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_regional_2")
                               )
                             # ,
                             # p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                    ),
                    # Tabelle noch nicht fertig gelayoutet
                    # tabPanel("Tabelle", br(),
                    #
                    #          fluidRow(
                    #            shiny::sidebarPanel(
                    #              width = 3
                    #          ),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_beruf_arbeitsmarkt_landkreis_table_lk_ui("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1")
                    #          ),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_beruf_arbeitsmarkt_landkreis_table_lk_ui("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2")
                    #          ),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_beruf_arbeitsmarkt_landkreis_table_lk_ui("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3")
                    #          )),
                    #          fluidRow(
                    #            shiny::sidebarPanel(
                    #              width = 12,
                    #              p(),
                    #              mod_beruf_arbeitsmarkt_landkreis_table_lk_analysis_ui(ns("var1")),
                    #              h5(""),
                    #              actionButton(ns("insertBtn"), "Weitere Betrachtung hinzufügen"),
                    #              actionButton(ns("runBtn"), "Betrachtungen anzeigen")
                    #
                    #              ),
                    #          shiny::mainPanel(
                    #            width = 12,
                    #            DT::DTOutput(ns("table_lk_analysis"))
                    #          ),
                    #          p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                    # ))
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
    ,funct_footer()
    )


}

#' beruf_arbeitsmarkt Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1
    plot_arbeitsmarkt_waffle_react <- reactive({
      arbeitsmarkt_anforderungen(r)
    })

    output$plot_arbeitsmarkt_waffle <- renderPlot({
      plot_arbeitsmarkt_waffle_react()
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      beruf_verlauf_single(r)
    })

    output$plot_einstieg_vergleich <- highcharter::renderHighchart({
      beruf_einstieg_vergleich(r)
    })

    output$plot_arbeitsmarkt_waffle_gender <- renderPlot({
      arbeitsmarkt_anforderungen_gender(r)
    })

    output$plot_arbeitsmarkt_bl_gender <- renderUI({
      arbeitsmarkt_bl_gender(r)
    })

    output$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- highcharter::renderHighchart({
      arbeitsmarkt_bl_gender_verlauf(r)
    })

    output$plot_beruf_arbeitsmarkt_bl_verlauf <- highcharter::renderHighchart({
      arbeitsmarkt_bl_verlauf(r)
    })


    # Box 2
    output$plot_arbeitsmarkt_bl <- renderUI({
      arbeitsmarkt_bl(r)
    })

    output$plot_arbeitsmarkt_überblick_fächer <- highcharter::renderHighchart({
      arbeitsmarkt_überblick_fächer(r)
    })

    output$plot_arbeitsmarkt_bl_vergleich <- highcharter::renderHighchart({
      arbeitsmarkt_bl_vergleich(r)
    })

    output$plot_arbeitsmarkt_top10 <- renderUI({
      arbeitsmarkt_top10(r)
    })

    # Box3
    output$plot_einstieg_pie_gender <- renderUI({
      arbeitsmarkt_einstieg_pie_gender(r)
    })

    output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
      arbeitsmarkt_einstieg_verlauf_gender( r)
    })

    output$plot_einstieg_vergleich_gender <- highcharter::renderHighchart({
      arbeitsmarkt_einstieg_vergleich_gender(r)
    })

    #Box4
    output$plot_international_studienzahl_map_arb_1 <- renderUI({
      plot_international_map_arb(r)
    })

    output$plot_international_map_arb_gender_1 <- renderUI({
      plot_international_map_arb_gender(r)
    })

    output$plot_international_top10_mint_arb_1 <- renderUI({
      plot_international_top10_mint_arb(r)
    })

    output$plot_international_top10_mint_arb_gender_1 <- renderUI({
      plot_international_top10_mint_arb_gender(r)
    })

    # Box 3 - Arbeitsmarkt (Jakob)
    # output$plot_international_arbeitsmarkt_map_1 <- renderUI({
    #   logger::log_debug("plot_international_arbeitsmarkt_map_1")
    #   plot_international_arbeitsmarkt_map(r)
    # })
    #
    # output$plot_international_arbeitsmakrt_top10_1 <- renderUI({
    #   logger::log_debug("plot_international_arbeitsmakrt_top10_1")
    #   plot_international_arbeitsmakrt_top10(r)
    # })

    output$plot_international_arbeitsmarkt_vergleiche_1 <- renderUI({
      # logger::log_debug("plot_international_arbeitsmarkt_vergleiche_1")
      plot_international_arbeitsmarkt_vergleiche(r)
    })


    # Box5
    output$plot_arbeitsmarkt_detail_map <- renderUI({
      arbeitsmarkt_lk_detail_map(r)
    })

    output$plot_arbeitsmarkt_detail_vergleich <- highcharter::renderHighchart({
      arbeitsmarkt_lk_detail_vergleich(r)

    })


    # Rest
    # plot_einstieg_pie_react <- reactive({
    #   arbeitsmarkt_einstieg_pie(data_arbeitsmarkt,r)
    # })
    #
    # output$plot_einstieg_pie <- renderUI({
    #   plot_einstieg_pie_react()
    # })
    #
    # data_table_einstieg_react <- reactive({
    #   data_einstieg_beruf(data_arbeitsmarkt, r)
    # })
    #
    # output$data_table_einstieg <- DT::renderDT({
    #   data_table_einstieg_react()
    # })
    #
    # output$plot_arbeitsmarkt_verlauf <- highcharter::renderHighchart({
    #   arbeitsmarkt_anforderungen_verlauf(data_arbeitsmarkt, r)
    # })
    #
    # output$plot_arbeitsmarkt_vergleich <- renderPlot({
    #   arbeitsmarkt_anforderungen_vergleich(data_arbeitsmarkt, r)
    # })
    #
    # output$plot_arbeitsmarkt_verlauf_gender <- highcharter::renderHighchart({
    #   arbeitsmarkt_anforderungen_verlauf_gender(data_arbeitsmarkt, r)
    # })
    #
    # output$plot_arbeitsmarkt_vergleich_gender <- renderPlot({
    #   arbeitsmarkt_anforderungen_vergleich_gender(data_arbeitsmarkt, r)
    # })
    #
    # output$plot_arbeitsmarkt_bl_gender_vergleich <- renderPlot({
    #   arbeitsmarkt_bl_gender_vergleich(data_arbeitsmarkt,r)
    # })
    #
    #
    # table_lk_analysis_react <- reactive({
    #   arbeitsmarkt_lk_detail_table(data_arbeitsmarkt_detail, input, r)
    # })
    #
    # observeEvent(input$runBtn, {
    #   output$table_lk_analysis <- DT::renderDT({
    #     DT::datatable(isolate(table_lk_analysis_react()),
    #                   style = "bootstrap",
    #                   selection = "none",
    #                   rownames = FALSE,
    #                   options = list(dom = 't',
    #                                  columnDefs = list(list(className = "dt-center", targets = "_all"))),
    #                   escape = FALSE
    #                   )
    #     })
    # })
    #
    # # downloader
    # output$download_data_box1 <- shiny::downloadHandler(
    #   filename = function() {
    #     paste("data_kurse", "csv", sep = ".")
    #   },
    #   content = function(file){
    #     write.csv(data_table_einstieg_react(), file)
    #   }
    # )
  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_ui("beruf_arbeitsmarkt_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_server("beruf_arbeitsmarkt_1")

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
          img(src='www/Banner_Ausbildung_Beruf.jpg',
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
           Dabei unterscheiden wir zwischen Auszubildenden und (sozialversicherungspflichtigen) Beschäftigten.
           Die Kategorisierung in MINT entspricht der Klassifikation durch die Bundesagentur für Arbeit.
            Weitere Inforamtionen dazu finden Sie auf der Unterseite \"Hinweise & Datenquellen\".")
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


      # Box 1 ----

    fluidRow( id="beruf_mint",
      shinydashboard::box(
        title = "Berufswahl MINT: Wie hoch ist der Anteil von Auszubildenden und Beschäftigten in MINT?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Berufen insgesamt bei Auszubildenden und Beschäftigten in Deutschland und Unterschiede in der Berufswahl von Männern und Frauen?"),


        tabsetPanel(type = "tabs",
                    # Tab 1
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

                    # Tab 2
                    tabPanel("Vergleich Anteil MINT an Beschäftigten und Auszubildenden im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_einstieg_verlauf"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_verlauf")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_mint_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_2"),

                               )


                    ),
                    # Tab 3
                    tabPanel("Alle Beschäftigtengruppen auf einen Blick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_einstieg_vergleich"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_vergleich")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
                             )
                    ),
                    # Tab 4

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
                    # Tab 5

                    tabPanel("Vergleich Anteil von Frauen & Männer (Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_gender_ui("mod_beruf_arbeitsmarkt_bl_gender_ui_1"),
                               # br(),br()
                               # ,
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_arbeitsmarkt_bl_gender_1"),
                               #   label = "Download (links)",
                               #   icon = icon("download")),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_arbeitsmarkt_bl_gender_2"),
                               #   label = "Download (rechts)",
                               #   icon = icon("download")),
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
                    # Tab 6
                    tabPanel("Vergleich Anteil MINT bei Frauen nach Bundesländern im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui("mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_beruf_arbeitsmarkt_bl_gender_verlauf"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_beruf_arbeitsmarkt_bl_gender_verlauf")),
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
                               mod_beruf_arbeitsmarkt_bl_verlauf_ui("mod_beruf_arbeitsmarkt_bl_verlauf_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_beruf_arbeitsmarkt_bl_verlauf"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_beruf_arbeitsmarkt_bl_verlauf")),
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


    # Box 2 ----

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
                               mod_beruf_arbeitsmarkt_bl_ui("mod_beruf_arbeitsmarkt_bl_ui_1"),
                               # br(),br()
                               # ,
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_arbeitsmarkt_bl_1"),
                               #   label = "Download (links)",
                               #   icon = icon("download")),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_arbeitsmarkt_bl_2"),
                               #   label = "Download (rechts)",
                               #   icon = icon("download")),
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
                    # tab 2
                    tabPanel("Alle Bereiche auf einen Blick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_überblick_fächer_ui("mod_beruf_arbeitsmarkt_überblick_fächer_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_arbeitsmarkt_überblick_fächer"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_überblick_fächer")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_fach_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_2")
                             )
                             ),
                    # tab 3
                    tabPanel("Vergleich Anteil MINT-Bereiche nach Bundesländern", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bl_vergleich_ui("beruf_arbeitsmarkt_bl_vergleich_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_arbeitsmarkt_bl_vergleich"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_bl_vergleich")),
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
                               mod_beruf_arbeitsmarkt_top10_ui("mod_beruf_arbeitsmarkt_top10_ui_1"),
                               br(),br()
                               ,
                               downloadButton(
                                 outputId = ns("download_btn_plot_arbeitsmarkt_top10_1"),
                                 label = "Download (links)",
                                 icon = icon("download")),
                               downloadButton(
                                 outputId = ns("download_btn_plot_arbeitsmarkt_top10_2"),
                                 label = "Download (rechts)",
                                 icon = icon("download")),
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

    # Box 3 ----

    fluidRow(id="beruf_frauen",
      shinydashboard::box(
        title = "Frauen in MINT: Wie hoch ist der Anteil von Frauen innerhalb der MINT-Berufe?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Frauen und Männern innerhalb der MINT-Berufe in Deutschland an. Zum Vergleich zeigen wir auch den Anteil in den anderen, nicht-MINT-Berufen."),

        tabsetPanel(type = "tabs",
              # tab 1
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
            # tab 2
                    tabPanel("Vergleich Anteil Frauen in MINT-Berufen im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_einstieg_verlauf_gender"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_verlauf_gender")),
                                                            color = "#154194"),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id = "h_beruf_frauen_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_frauen_2")
                             )


                    ),
            # tab 3
                    tabPanel("Anteil Frauen an verschiedenen MINT-Berufsgruppen", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_einstieg_vergleich_gender"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_vergleich_gender")),
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

    # Box Regional ----

    fluidRow(id="beruf_regional",
      shinydashboard::box(
        title = "Regionaler MINT-Steckbrief",
        width = 12,
        p("Hier bieten wir die Möglichkeit, den eigenen Landkreis unter die Lupe zu nehmen.",

        tabsetPanel(type = "tabs",
              # tab 1
                    tabPanel("Vergleich Landkreise (als Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_landkreis_map_ui("mod_beruf_arbeitsmarkt_landkreis_map_ui_1"),
                               # br(),br()
                               # ,
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_arbeitsmarkt_detail_map_1"),
                               #   label = "Download (links)",
                               #   icon = icon("download")),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_arbeitsmarkt_detail_map_2"),
                               #   label = "Download (rechts)",
                               #   icon = icon("download")),
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
              # tab 2
                    tabPanel("Vergleich Landkreise, Auflistung aller Landkreise", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_landkreis_vergleich_ui("mod_beruf_arbeitsmarkt_landkreis_vergleich_ui_1"),
                               br(),br(),
                               downloadButton(
                                 outputId = ns("download_btn_plot_arbeitsmarkt_detail_vergleich"),
                                 label = "Download",
                                 icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_detail_vergleich"),
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

    # Box 1 ----
    # Tab 1
    plot_arbeitsmarkt_waffle_react <- reactive({
      arbeitsmarkt_anforderungen(r)
    })


    output$plot_arbeitsmarkt_waffle <- renderPlot({
      plot_arbeitsmarkt_waffle_react()
    })

    # Tab 2

    # output$plot_einstieg_verlauf <- highcharter::renderHighchart({
    #   beruf_verlauf_single(r)
    # })

    output$plot_einstieg_verlauf <- renderUI({
      plot_list <- beruf_verlauf_single(r)
      r$plot_einstieg_verlauf <- plot_list

      r$plot_einstieg_verlauf_title <- get_plot_title(
        plot = r$plot_einstieg_verlauf
      )

      plot_list
    })

    output$download_btn_plot_einstieg_verlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_verlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_verlauf,
          filename =  r$plot_einstieg_verlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_verlauf_title, file)
        file.remove(r$plot_einstieg_verlauf_title)
      }
    )


    # Tab 3

    # output$plot_einstieg_vergleich <- highcharter::renderHighchart({
    #   beruf_einstieg_vergleich(r)
    # })

    output$plot_einstieg_vergleich <- renderUI({
      plot_list <- beruf_einstieg_vergleich(r)
      r$plot_einstieg_vergleich <- plot_list

      r$plot_einstieg_vergleich_title <- get_plot_title(
        plot = r$plot_einstieg_vergleich
      )

      plot_list
    })

    output$download_btn_plot_einstieg_vergleich <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_vergleich_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_vergleich,
          filename =  r$plot_einstieg_vergleich_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_vergleich_title, file)
        file.remove(r$plot_einstieg_vergleich_title)
      }
    )

    # Tab 4
    output$plot_arbeitsmarkt_waffle_gender <- renderPlot({
      arbeitsmarkt_anforderungen_gender(r)
    })

    # Tab 5

    output$plot_arbeitsmarkt_bl_gender <- renderUI({
      plot_list <- arbeitsmarkt_bl_gender(r)
      # r$plot_arbeitsmarkt_bl_gender_left <- plot_list[[1]]
      # r$plot_arbeitsmarkt_bl_gender_right <- plot_list[[2]]
      #
      # r$plot_arbeitsmarkt_bl_gender_left_title <- get_plot_title(
      #   plot = r$plot_arbeitsmarkt_bl_gender_left
      # )
      # r$plot_arbeitsmarkt_bl_gender_right_title <- get_plot_title(
      #   plot = r$plot_arbeitsmarkt_bl_gender_right
      # )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    # output$download_btn_plot_arbeitsmarkt_bl_gender_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_arbeitsmarkt_bl_gender_left_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_arbeitsmarkt_bl_gender_left,
    #       filename =  r$plot_arbeitsmarkt_bl_gender_left_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_arbeitsmarkt_bl_gender_left_title, file)
    #     file.remove(r$plot_arbeitsmarkt_bl_gender_left_title)
    #   }
    # )
    #
    # output$download_btn_plot_arbeitsmarkt_bl_gender_2 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_arbeitsmarkt_bl_gender_right_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #     add_caption_and_download(
    #       hc = r$plot_arbeitsmarkt_bl_gender_right,
    #       filename =  r$plot_arbeitsmarkt_bl_gender_right_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_arbeitsmarkt_bl_gender_right_title, file)
    #     file.remove(r$plot_arbeitsmarkt_bl_gender_right_title)
    #   }
    # )

    # Tab 6

    # output$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- highcharter::renderHighchart({
    #   arbeitsmarkt_bl_gender_verlauf(r)

      output$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- renderUI({
        plot_list <- arbeitsmarkt_bl_gender_verlauf(r)
        r$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- plot_list

        r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title <- get_plot_title(
          plot = r$plot_beruf_arbeitsmarkt_bl_gender_verlauf
        )

        plot_list
      })

      output$download_btn_plot_beruf_arbeitsmarkt_bl_gender_verlauf <- downloadHandler(
        contentType = "image/png",
        filename = function() {r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title},
        content = function(file) {
          # creating the file with the screenshot and prepare it to download

          add_caption_and_download(
            hc = r$plot_beruf_arbeitsmarkt_bl_gender_verlauf,
            filename =  r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title,
            width = 700,
            height = 400)

          file.copy(r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title, file)
          file.remove(r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title)
        }
      )




    # Tab 7

    # output$plot_beruf_arbeitsmarkt_bl_verlauf <- highcharter::renderHighchart({
    #   arbeitsmarkt_bl_verlauf(r)
    # })

    output$plot_beruf_arbeitsmarkt_bl_verlauf  <- renderUI({
      plot_list <- arbeitsmarkt_bl_verlauf(r)
      r$plot_beruf_arbeitsmarkt_bl_verlauf <- plot_list

      r$plot_beruf_arbeitsmarkt_bl_verlauf_title <- get_plot_title(
        plot = r$plot_beruf_arbeitsmarkt_bl_verlauf
      )

      plot_list
    })

    output$download_btn_plot_beruf_arbeitsmarkt_bl_verlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_beruf_arbeitsmarkt_bl_verlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_beruf_arbeitsmarkt_bl_verlauf,
          filename =  r$plot_beruf_arbeitsmarkt_bl_verlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_beruf_arbeitsmarkt_bl_verlauf_title, file)
        file.remove(r$plot_beruf_arbeitsmarkt_bl_verlauf_title)

})


    # Box 2 ----

    # Tab 1

    output$plot_arbeitsmarkt_bl <- renderUI({
      plot_list <- arbeitsmarkt_bl(r)
      # r$plot_arbeitsmarkt_bl_left <- plot_list[[1]]
      # r$plot_arbeitsmarkt_bl_right <- plot_list[[2]]
      #
      # r$plot_arbeitsmarkt_bl_left_title <- get_plot_title(
      #   plot = r$plot_arbeitsmarkt_bl_left
      # )
      # r$plot_arbeitsmarkt_bl_right_title <- get_plot_title(
      #   plot = r$plot_arbeitsmarkt_bl_right
      # )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    # output$download_btn_plot_arbeitsmarkt_bl_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_arbeitsmarkt_bl_left_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_arbeitsmarkt_bl_left,
    #       filename =  r$plot_arbeitsmarkt_bl_left_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_arbeitsmarkt_bl_left_title, file)
    #     file.remove(r$plot_arbeitsmarkt_bl_left_title)
    #   }
    # )
    #
    # output$download_btn_plot_arbeitsmarkt_bl_2 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_arbeitsmarkt_bl_right_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #     add_caption_and_download(
    #       hc = r$plot_arbeitsmarkt_bl_right,
    #       filename =  r$plot_arbeitsmarkt_bl_right_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_arbeitsmarkt_bl_right_title, file)
    #     file.remove(r$plot_arbeitsmarkt_bl_right_title)
    #   }
    # )


    # tab 2
    # output$plot_arbeitsmarkt_überblick_fächer <- highcharter::renderHighchart({
    #   arbeitsmarkt_überblick_fächer(r)
    # })

    output$plot_arbeitsmarkt_überblick_fächer  <- renderUI({
      plot_list <- arbeitsmarkt_überblick_fächer(r)
      r$plot_arbeitsmarkt_überblick_fächer <- plot_list

      r$plot_arbeitsmarkt_überblick_fächer_title <- get_plot_title(
        plot = r$plot_arbeitsmarkt_überblick_fächer
      )

      plot_list
    })

    output$download_btn_plot_arbeitsmarkt_überblick_fächer <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_überblick_fächer_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_überblick_fächer,
          filename =  r$plot_arbeitsmarkt_überblick_fächer_title,
          width = 700,
          height = 400)

        file.copy(r$plot_arbeitsmarkt_überblick_fächer_title, file)
        file.remove(r$plot_arbeitsmarkt_überblick_fächer_title)

      })

    # Tab 3

    # output$plot_arbeitsmarkt_bl_vergleich <- highcharter::renderHighchart({
    #   arbeitsmarkt_bl_vergleich(r)
    # })

    output$plot_arbeitsmarkt_bl_vergleich  <- renderUI({
      plot_list <- arbeitsmarkt_bl_vergleich(r)
      r$plot_arbeitsmarkt_bl_vergleich <- plot_list

      r$plot_arbeitsmarkt_bl_vergleich_title <- get_plot_title(
        plot = r$plot_arbeitsmarkt_bl_vergleich
      )

      plot_list
    })

    output$download_btn_plot_arbeitsmarkt_bl_vergleich <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_bl_vergleich_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_bl_vergleich,
          filename =  r$plot_arbeitsmarkt_bl_vergleich_title,
          width = 700,
          height = 400)

        file.copy(r$plot_arbeitsmarkt_bl_vergleich_title, file)
        file.remove(r$plot_arbeitsmarkt_bl_vergleich_title)

      })

    # Tab 4

    # output$plot_arbeitsmarkt_top10 <- renderUI({
    #   arbeitsmarkt_top10(r)
    # })

    output$plot_arbeitsmarkt_top10 <- renderUI({
      plot_list <- arbeitsmarkt_top10(r)
      r$plot_arbeitsmarkt_top10_left <- plot_list[[1]]
      r$plot_arbeitsmarkt_top10_right <- plot_list[[2]]

      r$plot_arbeitsmarkt_top10_left_title <- get_plot_title(
        plot = r$plot_arbeitsmarkt_top10_left
      )
      r$plot_arbeitsmarkt_top10_right_title <- get_plot_title(
        plot = r$plot_arbeitsmarkt_top10_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_arbeitsmarkt_top10_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_top10_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_top10_left,
          filename =  r$plot_arbeitsmarkt_top10_left_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_arbeitsmarkt_top10_left_title, file)
        file.remove(r$plot_arbeitsmarkt_top10_left_title)
      }
    )

    output$download_btn_plot_arbeitsmarkt_top10_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_top10_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_top10_right,
          filename =  r$plot_arbeitsmarkt_top10_right_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_arbeitsmarkt_top10_right_title, file)
        file.remove(r$plot_arbeitsmarkt_top10_right_title)
      }
    )



    # Box3 ----

    # tab 1
    output$plot_einstieg_pie_gender <- renderUI({
      arbeitsmarkt_einstieg_pie_gender(r)
    })


    # tab 2
    # output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
    #   arbeitsmarkt_einstieg_verlauf_gender( r)
    # })

    output$plot_einstieg_verlauf_gender  <- renderUI({
      plot_list <- arbeitsmarkt_einstieg_verlauf_gender(r)
      r$plot_einstieg_verlauf_gender <- plot_list

      r$plot_einstieg_verlauf_gender_title <- get_plot_title(
        plot = r$plot_einstieg_verlauf_gender
      )

      plot_list
    })

    output$download_btn_plot_einstieg_verlauf_gender <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_verlauf_gender_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_verlauf_gender,
          filename =  r$plot_einstieg_verlauf_gender_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_verlauf_gender_title, file)
        file.remove(r$plot_einstieg_verlauf_gender_title)

      })


    # tab 3

    # output$plot_einstieg_vergleich_gender <- highcharter::renderHighchart({
    #   arbeitsmarkt_einstieg_vergleich_gender(r)
    # })

    output$plot_einstieg_vergleich_gender  <- renderUI({
      plot_list <- arbeitsmarkt_einstieg_vergleich_gender(r)
      r$plot_einstieg_vergleich_gender <- plot_list

      r$plot_einstieg_vergleich_gender_title <- get_plot_title(
        plot = r$plot_einstieg_vergleich_gender
      )

      plot_list
    })

    output$download_btn_plot_einstieg_vergleich_gender <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_vergleich_gender_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_vergleich_gender,
          filename =  r$plot_einstieg_vergleich_gender_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_vergleich_gender_title, file)
        file.remove(r$plot_einstieg_vergleich_gender_title)

      })


    # Box Regional ----

    # tab 1

    output$plot_arbeitsmarkt_detail_map <- renderUI({
      plot_list <- arbeitsmarkt_lk_detail_map(r)
      # r$plot_arbeitsmarkt_detail_map_left <- plot_list[[1]]
      # r$plot_arbeitsmarkt_detail_map_right <- plot_list[[2]]
      #
      # r$plot_arbeitsmarkt_detail_map_left_title <- get_plot_title(
      #   plot = r$plot_arbeitsmarkt_detail_map_left
      # )
      # r$plot_arbeitsmarkt_detail_map_right_title <- get_plot_title(
      #   plot = r$plot_arbeitsmarkt_detail_map_right
      # )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    # output$download_btn_plot_arbeitsmarkt_detail_map_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_arbeitsmarkt_detail_map_left_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_arbeitsmarkt_detail_map_left,
    #       filename =  r$plot_arbeitsmarkt_detail_map_left_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_arbeitsmarkt_detail_map_left_title, file)
    #     file.remove(r$plot_arbeitsmarkt_detail_map_left_title)
    #   }
    # )
    #
    # output$download_btn_plot_arbeitsmarkt_detail_map_2 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_arbeitsmarkt_detail_map_right_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #     add_caption_and_download(
    #       hc = r$plot_arbeitsmarkt_detail_map_right,
    #       filename =  r$plot_arbeitsmarkt_detail_map_right_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_arbeitsmarkt_detail_map_right_title, file)
    #     file.remove(r$plot_arbeitsmarkt_detail_map_right_title)
    #   }
    # )



    # tab 2

    output$plot_arbeitsmarkt_detail_vergleich <- highcharter::renderHighchart({
      arbeitsmarkt_lk_detail_vergleich(r)

    })

    output$plot_arbeitsmarkt_detail_vergleich  <- renderUI({
      plot_list <- arbeitsmarkt_einstieg_vergleich_gender(r)
      r$plot_arbeitsmarkt_detail_vergleich <- plot_list

      r$plot_arbeitsmarkt_detail_vergleich_title <- get_plot_title(
        plot = r$plot_arbeitsmarkt_detail_vergleich
      )

      plot_list
    })

    output$download_btn_plot_arbeitsmarkt_detail_vergleich <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_detail_vergleich_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_detail_vergleich,
          filename =  r$plot_arbeitsmarkt_detail_vergleich_title,
          width = 700,
          height = 400)

        file.copy(r$plot_arbeitsmarkt_detail_vergleich_title, file)
        file.remove(r$plot_arbeitsmarkt_detail_vergleich_title)

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

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

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Schule.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Schule",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),



    # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Auf dieser Seite zeigen wir, wie hoch der Anteil von MINT-Fächern an allen Schulfächern - gemessen an allen gewählten Grund- und Leistungskursen - ist.
          Je nach Bundesland wählen alle Oberstufen-Schülerinnen und -Schüler mehrere Grund- und Leistungskurse.
          Anhand dieser Belegungszahlen haben wir den Anteil von MINT-Fächern in der Schule berechnet. ")

              # Text zu viel?
              # Anders als bei Studierenden oder Auszubildenden wählen Schüler:innen mehrere Grund- und Leistungskurse und können entsprechend nicht
              #  eindeutig als \"MINT\" oder \"nicht MINT\" eingruppiert werden. Um dennoch einen Anteil von MINT versus nicht MINT angeben zu können,
              #  nutzen wir die Kursbelegungszahlen der Schüler:innen. Auf die Ausweisung absoluter Zahlen verzichten wir, da aus den Belegungszahlen
              #  nicht die Gesamtzahl aller Schüler:innen abgeleitet werden kann. Der Vergleich auf dieser Seite erfolgt entsprechend den Belegungszahlen
              #  der verschiedenen Kurse."
      ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))


    ),

    fluidRow(
      shinydashboard::box(
        title = "Links zu den Themen dieser Seite",
        width = 7,

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_mint",
                                                              span(tags$b(span("MINT-Anteil:")))),"Ein Drittel der Leistungskursbelegungen sind in MINT."
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_fach",
                                                              span(tags$b(span("M-I-N-T:")))), "70 % der MINT-Leistungskurse sind Mathematik."

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_frauen",
                                                              span(tags$b(span("Frauen in MINT:")))),"MINT-Belegungen unter Mädchen und Jungen ungefähr gleich häufig."),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_kompetenz",
                                                              span(tags$b(span("MINT-Kompetenzen:")))),"MINT-Kompetenzen nehmen weiter ab. IQB-Ergebnisse der letzten Jahre."),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_ausserschulisch",
                                                              span(tags$b(span("Frühkindliche Bildung:")))),"Zahl der MINT-aktiven Einrichtungen bei Stiftung Kinder forschen wächst stetig."),

      ),
      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Schüler:innenzahlen der Oberstufe: Kulturministerkonferenz (KMK) 2023, auf Anfrage"),
        p(style = "text-align: left; font-size = 16px",
          "Kompetenzdaten in Deutschland: Institut zur Qualitätsentwicklung im Bildungswesen (IQB), 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
        p(style = "text-align: left; font-size = 16px",
          "Weitere Statistiken über die Belegung von MINT-Fächern in anderen Klassenstufen liegen uns derzeit nicht vor.")
        )

      ),

  # Box 1 -----

    fluidRow(id="schule_mint",
      shinydashboard::box(
        title = "MINT-Anteil: Ein Drittel der Leistungskursbelegungen sind in MINT.",
        width = 12,
        column(
          width = 8,
        p("In 2022 fallen 24 % der Grundkursbelegungen auf ein MINT-Fach.
        Der MINT-Anteil an Leistungskursbelegungen ist noch einmal etwas höher:
        33 % der Belegungen sind in MINT."),
        p("Wie viel MINT in grundlegendem und gehobenem Leistungsniveau belegt werden kann,
        ist von den Wahlmöglichkeiten in den Bundesländern abhängig. Das ist bei einer Betrachtung
        einzelner Bundesländer zu berücksichtigen."),
        p("In den letzten 10 Jahren hat sich der MINT-Anteil an den gesamten
          Oberstufenbelegungen nicht verändert.
          Dabei hat der Anteil von MINT in Grundkurse leicht zu, und der MINT-Anteil
          in Leistungskursen leicht abgenommen.")
        ),
        column(
          width = 12,

                tabsetPanel(type = "tabs",
                    # TODO das als MINT gesamt bzw. als Pie
                    # tabPanel("Vergleich Grund- und Leistungskurse, Fachbereiche", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #                        .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_schule_kurse_multiple_mint_ui("mod_schule_kurse_multiple_mint_ui_1")
                    #            ),
                    #
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(plotOutput(ns("plot_waffle_mint")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey", br(),
                    #           "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #           shinyBS::bsPopover(id="h_schule_mint_1", title = "",
                    #                              content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen."),
                    #                              placement = "top",
                    #                              trigger = "hover"),
                    #           tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_1")
                    #
                    #          )
                    # ),
                    tabPanel("aktueller MINT-Anteil", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_comparison_ui("mod_schule_kurse_einstieg_comparison_ui_1"),
                               # br(), br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_einstieg_comparison"),
                               #   label = "Download",
                               #   icon = icon("download"))
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_comparison")),
                                                            color = "#154194"),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_mint_2", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_2")
                               )

                             ),

                    # tabPanel("Vergleich Grund- und Leistungskurse, MINT aggregiert", br(),
                    #
                    #   tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #                        .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #   shiny::sidebarPanel(
                    #     width = 3,
                    #     tags$style(".well {background-color:#FFFFFF;}"),
                    #     tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #     mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                    #   shiny::mainPanel(
                    #     width = 9,
                    #     htmlOutput(ns("plot_einstieg_pie")),p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."))
                    #         ),

                    tabPanel("MINT-Anteil im Zeitverlauf", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_verlauf_ui("mod_schule_kurse_einstieg_verlauf_ui_1"),
                               # br(), br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_einstieg_verlauf"),
                               #   label = "Download",
                               #   icon = icon("download"))


                               ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_verlauf")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_mint_3", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_3")
                               )
                             ),

                    tabPanel("Bundeslandvergleich MINT-Anteil", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_mint_map_ui("mod_schule_kurse_mint_map_ui_1"),
                               # br(), br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_mint_map_kurse_item_1"),
                               #   label = "Download (links)",
                               #   icon = icon("download")),
                               # br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_mint_map_kurse_item_2"),
                               #   label = "Download (rechts)",
                               #   icon = icon("download"))
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_mint_map_kurse")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_1")
                             )
                    )
                    # ,
                    # tabPanel("Vergleich Bundesländer im Zeitverlauf", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_schule_kurse_verlauf_mint_ui("mod_schule_kurse_verlauf_mint_ui_1"),
                    #            # br(), br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_verlauf_mint"),
                    #            #   label = "Download",
                    #            #   icon = icon("download"))
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_mint")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_schule_fach_2", title = "",
                    #                               content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_2")
                    #          )
                    # )

                    # tabPanel("Vergleich (Bundesländer)", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            mod_schule_kurse_verlauf_ui("mod_schule_kurse_verlauf_ui_1")),
                    #          shiny::mainPanel(
                    #
                    #            highcharter::highchartOutput(ns("plot_verlauf_kurse")))
                    # ),

                    #,
                    #  tabPanel("Datensatz", br(),
                    #
                    #   tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #            .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #   shiny::sidebarPanel(
                    #     tags$style(".well {background-color:#FFFFFF;}"),
                    #     tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #     mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                    #   shiny::mainPanel(
                    #     div(DT::dataTableOutput(ns("data_table_einstieg")),
                    #         style = "font-size: 75%; width: 75%"),
                    #     shiny::downloadButton(ns("download_data_box1"), label = "",
                    #                           class = "butt",
                    #                           icon = shiny::icon("download")))
                    #         )
      )))),

  # Box 2 ----

    fluidRow(id="schule_fach",
      shinydashboard::box(
        title = "M-I-N-T: 70 % der MINT-Leistungskursbelegungen sind Mathematik.",
        width = 12,
        column(
          width = 8,
        p("Zoomt man auf die MINT-Fächer, zeigt sich: In den Leistungskursen heißt
          MINT zum Großteil Mathematik. 70 % der MINT-Leistungskursbelegungen sind in Mathematik.
          Kaum Oberstufenbelegungen fallen dagegen auf Informatik."),
        p("Das hängt mit den Wahlmöglichkeiten in der Oberstufe zusammen.
          Während Mathematik oft ein Pflichtfach für das Abitur ist und auf gehobenem Leistungsniveau
          angeboten wird, sind Oberstufenangebote in Informatik weniger verbreitet.",
        )
        ),
        column(
          width = 12,
        tabsetPanel(type = "tabs",
                    tabPanel("aktueller Anteil MINT-Fächer", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_mint_ui("mod_schule_kurse_multiple_mint_ui_1")
                             ),

                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(uiOutput(ns("plot_waffle_mint")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", br(),
                                 "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_mint_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_1")

                             )
                    ),
                    # tabPanel("Alle Fächer auf einen Blick", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #                        .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_schule_kurse_comparison_subjects_ui("mod_schule_kurse_comparison_subjects_ui_1"),
                    #            # br(), br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_comparison_subjects"),
                    #            #   label = "Download",
                    #            #   icon = icon("download"))
                    #          ),
                    #
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_comparison_subjects")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_schule_fach_5", title = "",
                    #                               content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_5")
                    #
                    #          )),
                    tabPanel("MINT-Fächer im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_verlauf_bl_subjects_ui("mod_schule_kurse_verlauf_bl_subjects_ui_1"),
                               # br(), br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_verlauf_kurse_bl_subjects"),
                               #   label = "Download",
                               #   icon = icon("download"))
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_kurse_bl_subjects")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_3", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_3")

                             )),

                    tabPanel("Bundeslandvergleich MINT-Fächer", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_map_ui("mod_schule_kurse_map_ui_1"),
                               # br(), br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_map_kurse_item_1"),
                               #   label = "Download (links)",
                               #   icon = icon("download")),
                               # br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_map_kurse_item_2"),
                               #   label = "Download (rechts)",
                               #   icon = icon("download"))
                               ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_map_kurse")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey",
                                  "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_1")
                               )
                    ),
                    # tabPanel("Vergleich Bundesländer im Zeitverlauf", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_schule_kurse_verlauf_multiple_ui("mod_schule_kurse_verlauf_multiple_ui_1"),
                    #            # br(), br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_verlauf_multiple"),
                    #            #   label = "Download",
                    #            #   icon = icon("download"))
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_multiple")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_schule_fach_2", title = "",
                    #                               content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_2")
                    #            )
                    # ),
                    #
                    # # tabPanel("Vergleich Grund- und Leistungskurse nach Bundesländern", br(),
                    # #
                    # #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    # #          .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    # #          shiny::sidebarPanel(
                    # #            width = 3,
                    # #            mod_schule_kurse_ranking_gender_ui("mod_schule_kurse_ranking_gender_ui_1"),
                    # #            ),
                    # #
                    # #          shiny::mainPanel(
                    # #            width = 9,
                    # #            shinycssloaders::withSpinner(plotOutput(ns("plot_ranking_gender")),
                    # #                                         color = "#154194"),
                    # #
                    # #            p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    # #            shinyBS::bsPopover(id="h_schule_fach_4", title = "",
                    # #                               content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt. <br> <br> Baden-Württemberg erfasst das Geschlecht von Schüler*innen nicht und kann deshalb nicht angezeigt werden."),
                    # #                               placement = "top",
                    # #                               trigger = "hover"),
                    # #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_4"))
                    # #
                    # # ),
                    #
                    # tabPanel("Alle Bundesländer auf einen Blick", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_schule_kurse_comparison_bl_ui("mod_schule_kurse_comparison_bl_ui_1"),
                    #            # br(), br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_comparison_bl"),
                    #            #   label = "Download",
                    #            #   icon = icon("download"))
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_comparison_bl")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_schule_fach_6", title = "",
                    #                               content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_6")
                    #            ))
        )

        ))),
  # Box 3 ----
    fluidRow(id="schule_frauen",
      shinydashboard::box(
        title = "Frauen in MINT: MINT-Belegungen unter Mädchen und Jungen ungefähr gleich häufig.",
        width = 12,
        column(
          width = 8,
        p("Der Mädchenanteil in MINT-Oberstufenfächern liegt bei gut der Hälfte.
        In \"Nicht-MINT\"-Fächern machen Mädchen 55 % der Kursteilnehmenden aus.
        Diese Zahlen zeigen auf subtile Weise, dass insgesamt mehr Jungen als Mädchen auf Gymnasien sind,
        und dass Jungen im Verhältnis etwas wahrscheinlicher MINT wählen."),
        p("Diese ähnliche Kurswahl hängt vielleicht auch mit Kursbelegungsvorgaben zusammen,
        nach welchen oft alle mindestens ein oder mehrere MINT-Fächer wählen müssen.
        Was sich unterscheidet, ist, welche MINT-Fächer eher von Jungen oder Mädchen belegt werden.
          Der Mädchenanteil in Biologie-Leistungs- und Grundkursen liegt bei um die 60 %.
          In Physik-Leistungskursen ist der Mädchenanteil dagegen bei knapp einem Viertel,
          in Informatik-Leistungskursen bei rund 16 %.")
        ),
        column(
          width = 12,
        tabsetPanel(type = "tabs",

                    # tabPanel("Frauenanteil in MINT-Kursen", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #                        .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            tags$style(".well {background-color:#FFFFFF;}"),
                    #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #            mod_schule_kurse_pie_gender_ui("mod_schule_kurse_pie_gender_ui_1")),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            htmlOutput(ns("plot_pie_gender"))
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."))
                    # ),

                    tabPanel("aktueller Anteil Mädchen in MINT", br(),


                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_comparison_gender_ui("mod_schule_kurse_comparison_gender_ui_1"),
                               # br(), br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_comparison_gender"),
                               #   label = "Download",
                               #   icon = icon("download"))
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(uiOutput(ns("plot_comparison_gender")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_frauen_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_frauen_1")
                               )
                    ),
                    tabPanel("Zeitverlauf Mädchenanteil in MINT", br(), #kann raus

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_verlauf_gender_ui("mod_schule_kurse_verlauf_gender_ui_1")
                               ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_verlauf_gender")),
                                                            color = "#154194"),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                             )
                    ),

                    # Fehler drin: erstmal raus:
                    # tabPanel("Zeitverlauf", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            mod_schule_kurse_verlauf_bl_ui("mod_schule_kurse_verlauf_bl_ui_1")),
                    #          shiny::mainPanel(
                    #            highcharter::highchartOutput(ns("plot_verlauf_kurse_bl"))
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."))
                    # ),
                    # tabPanel("Vergleich Grund- und Leistungskursen nach einzelnen Fächern", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #          .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_schule_kurse_ranking_ui("mod_schule_kurse_ranking_ui_1"), br(),
                    #           ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(plotOutput(ns("plot_ranking_2")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_schule_frauen_2", title = "",
                    #                               content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden. Baden-Württemberg erfasst das Geschlecht von Schüler*innen nicht und kann deshalb nicht angezeigt werden."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_frauen_2")
                    #            )
                    #          ),
                    tabPanel("MINT-Wahlverhalten nach Geschlecht", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_ui("mod_schule_kurse_multiple_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_wahl")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", br(),"Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_mint_4", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden." , "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_4")
                             )
                    )
                    # ,

                    # tabPanel("Bundeslandvergleich", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_schule_kurse_map_gender_ui("mod_schule_kurse_map_gender_ui_1"),
                    #            # br(), br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_map_kurse_gender_item_1"),
                    #            #   label = "Download (links)",
                    #            #   icon = icon("download")),
                    #            # br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_map_kurse_gender_item_2"),
                    #            #   label = "Download (rechts)",
                    #            #   icon = icon("download"))
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_map_kurse_gender")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey",br(), "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_schule_mint_5", title = "",
                    #                               content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden. Baden-Württemberg erfasst das Geschlecht von Schüler*innen nicht und kann deshalb nicht angezeigt werden." , "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_5")
                    #
                    #          )
                    # )
                        )

                    ))),
  # Box 4 ----

      fluidRow(id="schule_kompetenz",
           shinydashboard::box(
             title = "MINT-Kompetenzen: MINT-Kompetenzen nehmen weiter ab. IQB-Ergebnisse der letzten Jahre.",
             width = 12,
             column(
               width = 8,
             p("Diese interaktiven Diagramme geben einen Einblick in die Mathematik-Kompetenzen von Schüler:innen der 4. und 9. Klassen.
             Die Daten stammen aus der Befragung des Instituts zur Qualitätsentwicklung im Bildungswesen e.V. (IQB), das in regelmäßigen Abständen
             die Leistung von Schüler:innen in verschiedenen Fächern testet. Dafür werden deutschlandweit mehr als 1.300 Schulen und
               über 26.000 Schüler:innen befragt."),
             p("Die Ergebnisse aus 2021 zeigen, jede:r fünfte Viertklässler:in beherrscht die nötigen Grundlagen in der Mathematik nicht.
               In der Befragung 10 Jahre zuvor waren es nur jede:r 10te. Mädchen, sowie Schüler:innen mit Zuwanderungsgeschichte und niedrigem sozialen Status
               schneiden im Mittel schlechter ab. Dabei zeigt ein näherer Blick auf die Unterschiede zwischen Mädchen und Jungen, dass sich Mädchen auch weniger für Mathematik
               interessieren und ihre Fähigkeiten im Fach schlechter einschätzen."),
             p(),
             shinyBS::bsPopover(id="i_schule_kompetenz_1", title = "",
                                content = paste0("Im Bericht des IQB-Bildungstrends 2021 kann man weitere Informationen und eine Einordnung der dargestellten Daten finden. <br> <a>https://www.iqb.hu-berlin.de/bt/BT2021/Bericht/</a> <br><br> Weitere Informationen zum Thema Diversität und soziale Herkunft in der Bildungsweld können in der Diversitätsstudie von MINTvernetzt nachgelesen werden. <br> <a> https://mint-vernetzt.de/data/daten-fakten#mint-studie-diversitaet </a>"),
                                placement = "right",
                                trigger = "click"),
             tags$a(paste0("Interessieren Sie sich für weitere Informationen zum Thema, klicken Sie hier:"), icon("info-circle"), id = "i_schule_kompetenz_1"),
             p()
             ),
             column(
               width = 12,
             tabsetPanel(type = "tabs",

                         tabPanel("Leistungsschwache Schüler:innen in Mathematik", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_standard_zeitverlauf_ui("mod_schule_kurse_iqb_standard_zeitverlauf_ui_1"),
                                    # br(), br(),
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_iqb_standard_zeitverlauf"),
                                    #   label = "Download",
                                    #   icon = icon("download"))
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_iqb_standard_zeitverlauf")),
                                                                 color = "#154194"),

                                    p(style="font-size:12px;color:grey", br(),
                                      "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    shinyBS::bsPopover(id="h_schule_kompetenz_1", title = "",
                                                       content = paste0("Für Mecklenburg-Vorpommern liegen keine Daten vor, da pandemiebedingt nicht genug Testungen realisiert werden konnten.", "<br> <br> Gesamte realisierte Stichprobengröße:", "<br> 2021: 1.464 Schulen mit N = 26.844 Schüler:innen", "<br> 2018: 1.462 Schulen mit N = 44.941 Schüler:innen", "<br> 2016: 1.508 Schulen mit N = 29.259 Schüler:innen", "<br> 2012: 1.326 Schulen mit N = 44.584 Schüler:innen", "<br> 2011: 1.349 Schulen mit N = 27.081 Schüler:innen"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten und Stichprobengröße"), icon("info-circle"), id = "h_schule_kompetenz_1")

                                  )
                         ),
                         tabPanel("Leistung Mathematik im Gruppenvergleich", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_ui("mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_ui_1"),
                                    # br(), br(),
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_iqb_mathe_mittel_zeitverlauf"),
                                    #   label = "Download",
                                    #   icon = icon("download"))
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_iqb_mathe_mittel_zeitverlauf")),
                                                                 color = "#154194"),

                                    p(style="font-size:12px;color:grey",br(),
                                      "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    shinyBS::bsPopover(id="def_schule_kompetenz_2", title = "",
                                                       content = paste0("Mit Zuwanderungsgeschichte = Kinder, deren beider Eltern nach Deutschland zugewandert sind. Zuwanderungsgeschichten 1. Generation (auch Kind ist nach Deutschalnd zugewandert) und 2. Generation (Kind ist in Deutschland geboren) werden zusammengefasst. <br> Ohne Zuwanderungsgeschichte = Kinder, deren beider Eltern in Deutschland geboren wurden.", "<br> <br> Bildungskapital = Ressourcen, Kinder durch (kulturelle) Bildung zu fördern und Indikator für den sozialen Status der Eltern. Erfasst wurde das Bildungskapital durch die Anzahl an Bücher im Haushalt (hoch = mehr als 100 Bücher zuhause).", "<br><br>sozialer Status = die soziale Position der Eltern. Hier wurden Status und Anforderungen der Berufe betrachtet."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Definition der Begriffe"), icon("info-circle"), id = "def_schule_kompetenz_2"),
                                    br(),
                                    br(),
                                    shinyBS::bsPopover(id="h_schule_kompetenz_2", title = "",
                                                       content = paste0("Für Mecklenburg-Vorpommern liegen keine Daten vor, da pandemiebedingt nicht genug Testungen realisiert werden konnten.", "<br><br>Für einzelne Bundesländer liegen in bestimmten Bedingungen oder Zeitpunkten keine Daten vor. In diesen Fällen stehen die betroffenen Bundesländer nicht zur Auswahl oder betroffene Jahre werden nicht angezeigt.", "<br> <br> Gesamte realisierte Stichprobengröße:", "<br> 2021: 1.464 Schulen mit N = 26.844 Schüler:innen", "<br> 2018: 1.462 Schulen mit N = 44.941 Schüler:innen", "<br> 2016: 1.508 Schulen mit N = 29.259 Schüler:innen", "<br> 2012: 1.326 Schulen mit N = 44.584 Schüler:innen", "<br> 2011: 1.349 Schulen mit N = 27.081 Schüler:innen"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten und Stichprobengröße"), icon("info-circle"), id = "h_schule_kompetenz_2")
                                     )
                         ),
                         tabPanel("Interesse und Selbsteinschätzung", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_fragen_ui("mod_schule_kurse_iqb_fragen_ui_1"),
                                    # br(), br(),
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_iqb_fragebogen"),
                                    #   label = "Download",
                                    #   icon = icon("download"))
                                  ),

                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_iqb_fragebogen")),
                                                                 color = "#154194"),

                                    p(style="font-size:12px;color:grey",br(),
                                      "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    shinyBS::bsPopover(id="h_schule_kompetenz_3", title = "",
                                                       content = paste0("Das Interesse und die Einschätzung der eigenen Fähigkeiten (fachspezifisches Selbstkonzept) wurden durch mehrere Fragen in einem Fragebogen erfasst, auf einer Skala von 1 bis 4.", "<br> <br> Gesamte realisierte Stichprobengröße:", "<br> 2021: 1.464 Schulen mit N = 26.844 Schüler:innen", "<br> 2016: 1.508 Schulen mit N = 29.259 Schüler:innen", "<br> 2011: 1.349 Schulen mit N = 27.081 Schüler:innen"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten und Stichprobengröße"), icon("info-circle"), id = "h_schule_kompetenz_3")
                                  )
                         )
             )
             ))),


  # Kurzanalyse-Box ----
  div(class = "content-box",
      div(class = "inner-box",
      p(br(),"KURZANALYSE", br()),
      p(style = "font-size = 24",
        strong("MINT-Kompetenztests wie die IQB-Befragung zeigen systematische Gruppenunterschiede in den MINT-Kompetenzen.
               Zwei dieser Unterschiede ordnen wir in jeweils einer Kurzanalyse ein: Wir schauen darauf, woher die Unterschiede
               zwischen Jungen und Mädchen in Mathematik kommen können und wie man Mädchen hier fördern kann. Außerdem betrachten wir
               in unserer Kurzanalyse zum Thema Teilhabe den Einfluss der sozialen Herkunft auf die Schulzeit und wie MINT-Förderung zu Chancengerechtigkeit
                beitragen kann."),
        br(), br(),
        tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/02/MIN_Kurzanalyse_Gender_final.pdf",
               target = "_blank", "Link zu der Kurzanalyse Schule: Mädchen & MINT"), br(),
        tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/02/MIN_Kurzanalyse_Teilhabe_final.pdf",
               target = "_blank", "Link zu der Kurzanalyse Mit MINT-Förderung zu mehr Chancengerechtigkeit"),
        br(), br())
      )
  ),

  # Box 5----

  fluidRow(id="schule_ausserschulisch",
           shinydashboard::box(
             title = "Frühkindliche Bildung: Zahl der MINT-aktiven Einrichtungen bei Stiftung Kinder forschen wächst stetig.",
             width = 12,
             column(
               width = 8,
             p("In diesem Abschnitt betrachten wir die Entwicklung der außerschulischen, frühkindlichen MINT-Bildung.
               Die interaktiven Grafiken basieren auf den Daten der 'Stiftung Kinder forschen'
               (kurz SKf; früher: 'Haus der kleinen Forscher')."),

            p("Die Anzahl an Kitas, Grundschulen und Horte, die durch die Stiftung Kinder forschen für ihr MINT-Bildungsengagement
               zertifiziert wurden oder deren Personal durch die SKf fortgebildet wurde, wächst. Allerdings hat sich während der Jahre,
               die akut von der Corona-Pandemie betroffen waren, verlangsamt. Das spiegelt sich auch in den Zahlen der neu fortgebildeten Personen wider.
               Während zwischen 2013 und 2018 jährlich zwischen 6.000 und 8.000 Personen dazukamen, sind es im Jahr 2022 nur 2.000."), br(),
             p("Dies sind bislang die einzigen Darstellungen aus dem Bereich der außerschulischen MINT-Bildung. Hier wird in Zukunft noch mehr hinzukommen.")
            ),
            column(
              width = 12,

             tabsetPanel(type = "tabs",

                         tabPanel("SKf-zertifizierte und aktive Einrichtungen", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_ausserschulisch_skf_einrichtungen_ui("mod_ausserschulisch_skf_einrichtungen_ui_1"),
                                    # br(), br(),
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_skf_einrichtungen"),
                                    #   label = "Download",
                                    #   icon = icon("download"))
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_skf_einrichtungen")),
                                                                 color = "#154194"),

                                    p(style="font-size:12px;color:grey",
                                     "Quelle der Daten: Stiftung Kinder forschen, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    shinyBS::bsPopover(id="h_schule_ausserschulisch_1", title = "",
                                                       content = paste0("Zertifizierte Einrichtungen = Einrichtungen, die mindestens einmal als &quotHaus der Kleinen Forscher&quot ausgewiesen wurden.", "<br> <br> Einrichtungen mit SKf-Fortbildung = Einrichtungen, von welchen Fach- oder Lehrkräfte Fortbildungen der SKf besucht haben."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Definition der Begriffe"), icon("info-circle"), id = "h_schule_ausserschulisch_1")

                                  )
                         ),
                         tabPanel("Fach- und Lehrkräfte mit SKf-Fortbildung", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_ausserschulisch_skf_personal_ui("mod_ausserschulisch_skf_personal_ui_1"),
                                    # br(), br(),
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_skf_personal"),
                                    #   label = "Download",
                                    #   icon = icon("download"))
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_skf_personal")),
                                                                 color = "#154194"),

                                    p(style="font-size:12px;color:grey",
                                      "Quelle der Daten: Stiftung Kinder forschen, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    br(),
                                    shinyBS::bsPopover(id="h_schule_ausserschulisch_2", title = "",
                                                       content = paste0("Die Teilnehmendenzahlen sind von der SKf geschätzt und auf 1.000er-Stellen gerundet."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_ausserschulisch_2")
                                  )
                         )
             )
             ))),


    # Footer
  funct_footer())

}

#' schule_kurse Server Functions
#'
#' @noRd
mod_schule_kurse_server <- function(id, r){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1 - Wer wählt MINT ----
    ## Waffle
    output$plot_waffle_mint <- renderUI({
      kurse_waffle_mint(r)
    })

    ## Balkendiagramm

    output$plot_einstieg_comparison <- renderUI({
      plot_list <- kurse_einstieg_comparison(r)
      r$plot_einstieg_comparison <- plot_list

      r$plot_einstieg_comparison_title <- get_plot_title(
        plot = r$plot_einstieg_comparison
      )

      plot_list

    })

    output$download_btn_plot_einstieg_comparison <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_comparison_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_comparison,
          filename =  r$plot_einstieg_comparison_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_comparison_title, file)
        file.remove(r$plot_einstieg_comparison_title)
      }
    )
    # output$plot_einstieg_comparison <- highcharter::renderHighchart({
    #   kurse_einstieg_comparison(r)
    # })

    ## Zeitverlauf
    # output$plot_einstieg_verlauf <- highcharter::renderHighchart({
    #   kurse_verlauf_single(r)
    # })

    output$plot_einstieg_verlauf <- renderUI({
      plot_list <- kurse_verlauf_single(r)
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

    ## Waffle Geschlecht
    output$plot_wahl <- renderUI({
      kurse_wahl(r)
    })

    ## Karte Gender

    output$plot_map_kurse_gender <- renderUI({
      plot_list <- kurse_map_gender(r)
      # r$plot_map_kurse_gender_left <- plot_list[[1]]
      # r$plot_map_kurse_gender_right <- plot_list[[2]]
      #
      # r$plot_map_kurse_gender_left_title <- get_plot_title(
      #   plot = r$plot_map_kurse_gender_left
      # )
      # r$plot_map_kurse_gender_right_title <- get_plot_title(
      #   plot = r$plot_map_kurse_gender_right
      # )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    # output$download_btn_plot_map_kurse_gender_item_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_map_kurse_gender_left_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_map_kurse_gender_left,
    #       filename =  r$plot_map_kurse_gender_left_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_map_kurse_gender_left_title, file)
    #     file.remove(r$plot_map_kurse_gender_left_title)
    #   }
    # )
    #
    # output$download_btn_plot_map_kurse_gender_item_2 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_map_kurse_gender_right_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #     add_caption_and_download(
    #       hc = r$plot_map_kurse_gender_right,
    #       filename =  r$plot_map_kurse_gender_right_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_map_kurse_gender_right_title, file)
    #     file.remove(r$plot_map_kurse_gender_right_title)
    #   }
    # )


    output$plot_mint_map_kurse <- renderUI({
      plot_list <- kurse_mint_map(r)
      # r$plot_mint_map_kurse_left <- plot_list[[1]]
      # r$plot_mint_map_kurse_right <- plot_list[[2]]
      #
      # r$plot_mint_map_kurse_left_title <- get_plot_title(
      #   plot = r$plot_mint_map_kurse_left
      # )
      # r$plot_mint_map_kurse_right_title <- get_plot_title(
      #   plot = r$plot_mint_map_kurse_right
      # )

      # return plots

      plot_list

    })

    # output$download_btn_plot_mint_map_kurse_item_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_mint_map_kurse_left_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_mint_map_kurse_left,
    #       filename =  r$plot_mint_map_kurse_left_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_mint_map_kurse_left_title, file)
    #     file.remove(r$plot_mint_map_kurse_left_title)
    #   }
    # )
    #
    # output$download_btn_plot_mint_map_kurse_item_2 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_mint_map_kurse_right_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #     add_caption_and_download(
    #       hc = r$plot_mint_map_kurse_right,
    #       filename =  r$plot_mint_map_kurse_right_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_map_kurse_right_title, file)
    #     file.remove(r$plot_map_kurse_right_title)
    #   }
    # )


    # Box 2 -  M-I-N-T ----

    output$plot_map_kurse <- renderUI({
      kurse_map(r)
    })

    ## Karte Fächer
    # output$plot_map_kurse <- renderUI({
    #   out <- kurse_map(r)
    #   # r$plot_map_kurse_left <- plot_list[[1]]
    #   # r$plot_map_kurse_right <- plot_list[[2]]
    #   #
    #   # r$plot_map_kurse_left_title <- get_plot_title(
    #   #   plot = r$plot_map_kurse_left
    #   # )
    #   # r$plot_map_kurse_right_title <- get_plot_title(
    #   #   plot = r$plot_map_kurse_right
    #   # )
    #
    #   # return plots
    #
    #   out
    #
    # })

    # output$download_btn_plot_map_kurse_item_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_map_kurse_left_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_map_kurse_left,
    #       filename =  r$plot_map_kurse_left_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_map_kurse_left_title, file)
    #     file.remove(r$plot_map_kurse_left_title)
    #   }
    # )
    #
    # output$download_btn_plot_map_kurse_item_2 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_map_kurse_right_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #     add_caption_and_download(
    #       hc = r$plot_map_kurse_right,
    #       filename =  r$plot_map_kurse_right_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_map_kurse_right_title, file)
    #     file.remove(r$plot_map_kurse_right_title)
    #   }
    # )

    ## Verlauf nach BuLas
    # output$plot_verlauf_multiple <- highcharter::renderHighchart({
    #   kurse_verlauf_multiple_bl(r)
    # })

    output$plot_verlauf_multiple <- renderUI({
      plot_list <- kurse_verlauf_multiple_bl(r)
      r$plot_verlauf_multiple <- plot_list

      r$plot_verlauf_multiple_title <- get_plot_title(
        plot = r$plot_verlauf_multiple
      )

      plot_list

    })

    output$download_btn_plot_verlauf_multiple <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_verlauf_multiple_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_verlauf_multiple,
          filename =  r$plot_verlauf_multiple_title,
          width = 700,
          height = 400)

        file.copy(r$plot_verlauf_multiple_title, file)
        file.remove(r$plot_verlauf_multiple_title)
      }
    )

    ## Zeitverlauf Fächer
    # output$plot_verlauf_kurse_bl_subjects <- highcharter::renderHighchart({
    #   kurse_verlauf_subjects_bl(r)
    # })

    output$plot_verlauf_kurse_bl_subjects <- renderUI({
      plot_list <- kurse_verlauf_subjects_bl(r)
      r$plot_verlauf_kurse_bl_subject <- plot_list

      r$plot_verlauf_kurse_bl_subject_title <- get_plot_title(
        plot = r$plot_verlauf_kurse_bl_subject
      )

      plot_list

    })

    output$download_btn_plot_verlauf_kurse_bl_subjects <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_verlauf_kurse_bl_subject_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_verlauf_kurse_bl_subject,
          filename =  r$plot_verlauf_kurse_bl_subject_title,
          width = 700,
          height = 400)

        file.copy(r$plot_verlauf_kurse_bl_subject_title, file)
        file.remove(r$plot_verlauf_kurse_bl_subject_title)
      }
    )

    ## Dumbbell-Plot Mädchen
    output$plot_ranking_gender <- renderPlot({
      kurse_ranking_gender(r)
    })

    ## Balken Übersicht Fächer
    # output$plot_comparison_subjects <- highcharter::renderHighchart({
    #   kurse_mint_comparison(r)
    # })

    output$plot_comparison_subjects <- renderUI({
      plot_list <- kurse_mint_comparison(r)
      r$plot_comparison_subjects <- plot_list

      r$plot_comparison_subjects_title <- get_plot_title(
        plot = r$plot_comparison_subjects
      )

      plot_list

    })

    output$download_btn_plot_comparison_subjects <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_comparison_subjects_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_comparison_subjects,
          filename =  r$plot_comparison_subjects_title,
          width = 700,
          height = 400)

        file.copy(r$plot_comparison_subjects_title, file)
        file.remove(r$plot_comparison_subjects_title)
      }
    )


    ## Balken Übersicht BuLas
    # output$plot_comparison_bl <- highcharter::renderHighchart({
    #   kurse_mint_comparison_bl(r)
    # })

    output$plot_comparison_bl <- renderUI({
      plot_list <- kurse_mint_comparison_bl(r)
      r$plot_comparison_bl <- plot_list

      r$plot_comparison_bl_title <- get_plot_title(
        plot = r$plot_comparison_bl
      )

      plot_list

    })

    output$download_btn_plot_comparison_bl <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_comparison_bl_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_comparison_bl,
          filename =  r$plot_comparison_bl_title,
          width = 700,
          height = 400)

        file.copy(r$plot_comparison_bl_title, file)
        file.remove(r$plot_comparison_bl_title)
      }
    )


    # Box 3 - Frauen ----

    ## Balken Frauen
    # output$plot_comparison_gender <- highcharter::renderHighchart({
    #   kurse_comparison_gender(r)
    # })

    output$plot_comparison_gender <- renderUI({
      if(r$ansicht_kurse_comparison_gender ==
         "Kursvergleich - Hanteldiagramm"){
        plotOutput(ns("plot_ranking_3"))
      }else {
      htmlOutput(ns("plot_comparison_gender_2"))

      }
    })

    output$plot_comparison_gender_2 <- renderUI({
      plot_list <- kurse_comparison_gender(r)
    # r$plot_comparison_gender <- plot_list
    #
    # r$plot_comparison_gender_title <- get_plot_title(
    #   plot = r$plot_comparison_gender
    # )

    plot_list
    })

    output$plot_ranking_3 <- renderPlot({
     # plot_ranking_react()
      kurse_comparison_gender(r)
    })
    # plot_ranking_react <- reactive({
    #   kurse_ranking(r) # type = "other"
    # })

    output$plot_verlauf_gender <- highcharter::renderHighchart({
      kurse_verlauf_gender(r)
    })

    # output$download_btn_plot_comparison_gender <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_comparison_gender_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_comparison_gender,
    #       filename =  r$plot_comparison_gender_title,
    #       width = 700,
    #       height = 400)
    #
    #     file.copy(r$plot_comparison_gender_title, file)
    #     file.remove(r$plot_comparison_gender_title)
    #   }
    # )

    ## Dumbbell Frauen
    # output$plot_ranking_2 <- renderPlot({
    #   plot_ranking_react()
    # })
    # plot_ranking_react <- reactive({
    #   kurse_ranking(r) # type = "other"
    # })


    # Box 4  Kompetenzdaten / IQB ----

    # Tab 1

    # output$plot_iqb_standard_zeitverlauf <- highcharter::renderHighchart({
    #   iqb_standard_zeitverlauf(r)
    # })
    output$plot_iqb_standard_zeitverlauf <- renderUI({
      plot_list <- iqb_standard_zeitverlauf(r)
      r$plot_iqb_standard_zeitverlauf <- plot_list

      r$plot_iqb_standard_zeitverlauf_title <- get_plot_title(
        plot = r$plot_iqb_standard_zeitverlauf
      )

      plot_list

    })

    output$download_btn_plot_iqb_standard_zeitverlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_iqb_standard_zeitverlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_iqb_standard_zeitverlauf,
          filename =  r$plot_iqb_standard_zeitverlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_iqb_standard_zeitverlauf_title, file)
        file.remove(r$plot_iqb_standard_zeitverlauf_title)
      }
    )
    # Tab 2

    # output$plot_iqb_mathe_mittel_zeitverlauf <- highcharter::renderHighchart({
    #   iqb_mathe_mittel_zeitverlauf(r)
    # })

    output$plot_iqb_mathe_mittel_zeitverlauf <- renderUI({
      plot_list <- iqb_mathe_mittel_zeitverlauf(r)
      r$plot_iqb_mathe_mittel_zeitverlauf <- plot_list

      r$plot_iqb_mathe_mittel_zeitverlauf_title <- get_plot_title(
        plot = r$plot_iqb_mathe_mittel_zeitverlauf
      )

      plot_list

    })

    output$download_btn_plot_iqb_mathe_mittel_zeitverlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_iqb_mathe_mittel_zeitverlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_iqb_mathe_mittel_zeitverlauf,
          filename =  r$plot_iqb_mathe_mittel_zeitverlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_iqb_mathe_mittel_zeitverlauf_title, file)
        file.remove(r$plot_iqb_mathe_mittel_zeitverlauf_title)
      }
    )

    # Tab 3

    # output$plot_iqb_fragebogen <- highcharter::renderHighchart({
    #   iqb_fragebogen(r)
    # })

    output$plot_iqb_fragebogen <- renderUI({
      plot_list <- iqb_fragebogen(r)
      r$plot_iqb_fragebogen <- plot_list

      r$plot_iqb_fragebogen_title <- get_plot_title(
        plot = r$plot_iqb_fragebogen
      )

      plot_list

    })

    output$download_btn_plot_iqb_fragebogen <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_iqb_fragebogen_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_iqb_fragebogen,
          filename =  r$plot_iqb_fragebogen_title,
          width = 700,
          height = 400)

        file.copy(r$plot_iqb_fragebogen_title, file)
        file.remove(r$plot_iqb_fragebogen_title)
      }
    )

    # Box 5 außerschulisch  / SKf ----

    # Tab 1
    # output$plot_skf_einrichtungen <- highcharter::renderHighchart({
    #   skf_einrichtungen(r)
    # })

    output$plot_skf_einrichtungen <- renderUI({
      plot_list <- skf_einrichtungen(r)
      r$plot_skf_einrichtungen <- plot_list

      r$plot_skf_einrichtungen_title <- get_plot_title(
        plot = r$plot_skf_einrichtungen
      )

      plot_list

    })

    output$download_btn_plot_skf_einrichtungen <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_skf_einrichtungen_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_skf_einrichtungen,
          filename =  r$plot_skf_einrichtungen_title,
          width = 700,
          height = 400)

        file.copy(r$plot_skf_einrichtungen_title, file)
        file.remove(r$plot_skf_einrichtungen_title)
      }
    )

    # Tab 2

    # output$plot_skf_personal <- highcharter::renderHighchart({
    #   skf_personal(r)
    # })

    output$plot_skf_personal <- renderUI({
      plot_list <- skf_personal(r)
      r$plot_skf_personal <- plot_list

      r$plot_skf_personal_title <- get_plot_title(
        plot = r$plot_skf_personal
      )

      plot_list

    })

    output$download_btn_plot_skf_personal <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_skf_personal_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_skf_personal,
          filename =  r$plot_skf_personal_title,
          width = 700,
          height = 400)

        file.copy(r$plot_skf_personal_title, file)
        file.remove(r$plot_skf_personal_title)
      }
    )




    ### Rest
#
#     output$plot_einstieg_pie <- renderUI({
#       kurse_einstieg_pie(data_kurse,r)
#     })


    # data_table_einstieg_react <- reactive({
    #   data_einstieg_kurse(data_kurse, r)
    # })

    # output$data_table_einstieg <- DT::renderDT({
    #   data_table_einstieg_react()
    # })
    #
    # # Box 3
    # output$plot_pie_gender <- renderUI({
    #   kurse_einstieg_pie_gender(data_kurse,r)
    # })
    #
    # output$plot_verlauf_gender <- highcharter::renderHighchart({
    #   kurse_verlauf_gender(data_kurse,r)
    # })
    #
    #
    # output$plot_verlauf_kurse_bl <- highcharter::renderHighchart({
    #   kurse_verlauf_single_bl(data_kurse,r)
    # })


    # output$plot_comparison_bl <- renderPlot({
    #   kurse_mint_comparison_bl(data_kurse,r)
    # })

    # Box 7
#
#     output$plot_verlauf_kurse <- highcharter::renderHighchart({
#       kurse_verlauf(data_kurse,r)
#     })


#
#
#     # downloader
#     output$download_data_box1 <- shiny::downloadHandler(
#       filename = function() {
#         paste("data_kurse", "csv", sep = ".")
#       },
#       content = function(file){
#         write.csv(data_table_einstieg_react(), file)
#       }
#     )
#
  })
}

## To be copied in the UI
# mod_schule_kurse_ui("schule_kurse_1")

## To be copied in the server
# mod_schule_kurse_server("schule_kurse_1")

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
        img(src='www/Banner_Schule_BB.jpg',
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
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))


    ),

    fluidRow(
      shinydashboard::box(
        title = "Übersicht Fragestellungen",
        width = 7,

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_mint",
                                                              span(tags$b(span("Fächerwahl MINT:")))),"Wie hoch ist der Anteil von MINT-Fächern in der Oberstufe?"
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_fach",
                                                              span(tags$b(span("M-I-N-T:")))), "Blick auf die einzelnen Fächer und Fachbereiche."

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_frauen",
                                                              span(tags$b(span("Frauen in MINT:")))),"Wie hoch ist der Anteil von Mädchen in den MINT-Fächern?"),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_kompetenz",
                                                              span(tags$b(span("MINT-Kompetenzen in der 4. und 9. Klasse:")))),"Wie entwickelt sich die MINT-Kompetenz und das Interesse in MINT?"),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_international",
                                                              span(tags$b(span("MINT-Kompetenzen im internationalen Vergleich:")))),"Wie schneidet Deutschland im internationalen Vergleich in den MINT-Kompetenztests Pisa und TIMSS ab?"),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_ausserschulisch",
                                                              span(tags$b(span("Außerschulische, frühkindliche MINT-Bildung:")))),"Wie hoch ist die Beteiligung in außerschulische, frühkindliche MINT-Bildung?"),

      ),
      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Schüler:innenzahlen der Oberstufe: Kulturministerkonferenz (KMK) 2022, auf Anfrage"),
        p(style = "text-align: left; font-size = 16px",
          "Kompetenzdaten: Institut zur Qualitätsentwicklung im Bildungswesen (IQB), 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
        p(style = "text-align: left; font-size = 16px",
          "Weitere Statistiken über die Belegung von MINT-Fächern in anderen Klassenstufen liegen uns derzeit nicht vor.")
        )

      ),

  # Box 1

    fluidRow(id="schule_mint",
      shinydashboard::box(
        title = "Fächerwahl MINT: Wie hoch ist der Anteil von MINT-Fächern in der Oberstufe?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir, wie häufig MINT-Fächer im Vergleich zu anderen Fächern in der Oberstufe in Deutschland belegt werden."),
                tabsetPanel(type = "tabs",

                    tabPanel("Vergleich Grund- und Leistungskurse, Fachbereiche", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_mint_ui("mod_schule_kurse_multiple_mint_ui_1")
                               ),

                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle_mint"))
                               ,
                               p(style="font-size:12px;color:grey", br(),
                              "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                              shinyBS::bsPopover(id="h_schule_mint_1", title = "",
                                                 content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen."),
                                                 placement = "top",
                                                 trigger = "hover"),
                              tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_1")

                             )
                    ),
                    tabPanel("Vergleich Grund- und Leistungskurse, MINT aggregiert", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_comparison_ui("mod_schule_kurse_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_comparison"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
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

                    tabPanel("Vergleich Grund- und Leistungskurse im Zeitverlauf", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_verlauf_ui("mod_schule_kurse_einstieg_verlauf_ui_1")


                               ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_mint_3", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_3")
                               )
                             ),



                    tabPanel("Vergleich Mädchen und Jungen", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_ui("mod_schule_kurse_multiple_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle"))
                               ,p(style="font-size:12px;color:grey", br(),"Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_mint_4", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden." , "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_4")
                               )
                    ),

                    tabPanel("Vergleich Mädchen und Jungen nach Bundesländern (Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_map_gender_ui("mod_schule_kurse_map_gender_ui_1")
                               ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_map_kurse_gender")),

                               p(style="font-size:12px;color:grey",br(), "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_mint_5", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden. Baden-Württemberg erfasst das Geschlecht von Schüler*innen nicht und kann deshalb nicht angezeigt werden." , "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_5")

                               )
                    ),

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
      ))),

    fluidRow(id="schule_fach",
      shinydashboard::box(
        title = "M-I-N-T: Blick auf die einzelnen Fächer und Fachbereiche",
        width = 12,
        p("Hier zeigen wir die Anteile einzelner MINT-Fächer in Deutschland. Berechnungsgrundlage sind wieder die Belegungszahlen aller Grund- und Leistungskurse.",
        ),
        tabsetPanel(type = "tabs",

                    tabPanel("Vergleich Grund- und Leistungskurse nach Bundesländern (Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_map_ui("mod_schule_kurse_map_ui_1"),
                               ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_map_kurse"))
                               ,p(style="font-size:12px;color:grey",
                                  "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_1")
                               )
                    ),
                    tabPanel("Vergleich Bundesländer im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_verlauf_multiple_ui("mod_schule_kurse_verlauf_multiple_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_multiple"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_2", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_2")
                               )
                    ),
                    tabPanel("Vergleich Fächer im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_verlauf_bl_subjects_ui("mod_schule_kurse_verlauf_bl_subjects_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_kurse_bl_subjects"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_3", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_3")

                               ))
                    ,


                    tabPanel("Vergleich Grund- und Leistungskurse nach Bundesländern", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_ranking_gender_ui("mod_schule_kurse_ranking_gender_ui_1"),
                               ),

                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_ranking_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_4", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt. <br> <br> Baden-Württemberg erfasst das Geschlecht von Schüler*innen nicht und kann deshalb nicht angezeigt werden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_4"))

                    ),

                    tabPanel("Alle Fächer auf einen Blick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_comparison_subjects_ui("mod_schule_kurse_comparison_subjects_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_subjects"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_5", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_5")

                             )),
                    tabPanel("Alle Bundesländer auf einen Blick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_comparison_bl_ui("mod_schule_kurse_comparison_bl_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_bl"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_fach_6", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturfächer und werden hier als Leistungskurse gezählt."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_6")
                               ))

        ))),
    fluidRow(id="schule_frauen",
      shinydashboard::box(
        title = "Mädchen in MINT: Wie hoch ist der Anteil von Mädchen in den MINT-Fächern?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Mädchen und Jungen innerhalb der MINT-Fächer in Deutschland an. Zum Vergleich
          zeigen wir auch den Anteil in den anderen, nicht-MINT-Fächern. Die verschiedenen Diagramme bieten außerdem
          Fächer- und Bundeslandvergleiche."),

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

                    tabPanel("Vergleich Frauenanteil nach Fächergruppen", br(),


                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_comparison_gender_ui("mod_schule_kurse_comparison_gender_ui_1"),
                               ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_frauen_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_frauen_1")
                               )
                    ),



                    # tabPanel("Zeitverlauf MINT", br(), #kann raus
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #                        .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            tags$style(".well {background-color:#FFFFFF;}"),
                    #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #            mod_schule_kurse_verlauf_gender_ui("mod_schule_kurse_verlauf_gender_ui_1")),
                    #          shiny::mainPanel(
                    #            highcharter::highchartOutput(ns("plot_verlauf_gender")))
                    # ),

                    # Fehler drin: erstmal raus:
                    # tabPanel("Zeitverlauf", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            mod_schule_kurse_verlauf_bl_ui("mod_schule_kurse_verlauf_bl_ui_1")),
                    #          shiny::mainPanel(
                    #            highcharter::highchartOutput(ns("plot_verlauf_kurse_bl"))
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."))
                    # ),
                    tabPanel("Vergleich Grund- und Leistungskursen nach einzelnen Fächern", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_ranking_ui("mod_schule_kurse_ranking_ui_1"), br(),
                              ),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_ranking_2"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_schule_frauen_2", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden. Baden-Württemberg erfasst das Geschlecht von Schüler*innen nicht und kann deshalb nicht angezeigt werden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_frauen_2")
                               )
                             ),


                    ))),

      fluidRow(id="schule_kompetenz",
           shinydashboard::box(
             title = "MINT-Kompetenzen in der 4. und 9. Klasse",
             width = 12,
             p("Diese interaktiven Diagramme geben einen Einblick in die Mathe-Kompetenzen von Schüler:innen der 4. und 9. Klassen.
             Die Daten stammen aus der Befragung des Instituts zur Qualitätsentwicklung im Bildungswesen e.V. (IQB), das in regelmäßigen Abständen
             die Leistung von Schüler:innen in verschiedenen Fächern testet. Dafür werden deutschlandweit mehr als 1.300 Schulen und
               über 26.000 Schüler:innen befragt."),
             p(),
             shinyBS::bsPopover(id="i_schule_kompetenz_1", title = "",
                                content = paste0("Im Bericht des IQB-Bildungstrends 2021 kann man weitere Informationen und eine Einordnung der dargestellten Daten finden. <br> <a>https://www.iqb.hu-berlin.de/bt/BT2021/Bericht/</a> <br><br> Weitere Informationen zum Thema Diversität und soziale Herkunft in der Bildungsweld können in der Diversitätsstudie von MINTvernetzt nachgelesen werden. <br> <a> https://mint-vernetzt.de/data/daten-fakten#mint-studie-diversitaet </a>"),
                                placement = "right",
                                trigger = "click"),
             tags$a(paste0("Interessieren Sie weitere Infos zum Thema, klicken Sie hier:"), icon("info-circle"), id = "i_schule_kompetenz_1"),
             p(),
             tabsetPanel(type = "tabs",

                         tabPanel("Leistungsschwache Schüler:innen in Mathematik", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_standard_zeitverlauf_ui("mod_schule_kurse_iqb_standard_zeitverlauf_ui_1"),

                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_iqb_standard_zeitverlauf"))
                                    ,
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
                                   ),


                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_iqb_mathe_mittel_zeitverlauf"))
                                    ,
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
                                  ),

                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_iqb_fragebogen"))
                                    ,
                                    p(style="font-size:12px;color:grey",br(),
                                      "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    shinyBS::bsPopover(id="h_schule_kompetenz_3", title = "",
                                                       content = paste0("Das Interesse und die Einschätzung der eigenen Fähigkeiten (fachspezifisches Selbstkonzept) wurden durch mehrere Fragen in einem Fragebogen erfasst, auf einer Skala von 1 bis 4.", "<br> <br> Gesamte realisierte Stichprobengröße:", "<br> 2021: 1.464 Schulen mit N = 26.844 Schüler:innen", "<br> 2016: 1.508 Schulen mit N = 29.259 Schüler:innen", "<br> 2011: 1.349 Schulen mit N = 27.081 Schüler:innen"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten und Stichprobengröße"), icon("info-circle"), id = "h_schule_kompetenz_3")
                                  )
                         )
             ))),

  fluidRow(id="schule_international",
           shinydashboard::box(
             title = "SCHULE: MINT-Kompetenzen von Schüler*innen im internationalen Vergleich",
             width = 12,
             p("In dieser Box blicken wir auf die Kompetenzdaten der Pisa-Studie und der TIMSS-Erhebung.
               Bei beiden Erhebungen werden unter anderem Kompetenztests in Mathematik und Naturwissenschaften in 4. und 9. Klassen weltweit durchgeführt.
               X Schüler*innen haben in der letzten Befragung teilgenommen. Außerdem betrachten die Grafiken Kompetenz-Unterschiede zwischen Jungen und Mädchen und Unterschiede
                 in Abhängigkeit der Herkunft der Kinder."),
             tabsetPanel(type = "tabs",
                         tabPanel("MINT-Kompetenz im Ländervergleich", br(),

                                  #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                  # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                  shiny::sidebarPanel(
                                    width = 3,
                                    #p("LOREM"),
                                    #mod_international_map_ui("mod_international_map_ui_1")
                                    mod_international_schule_map_ui("international_schule_map_1")
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    htmlOutput(ns("plot_international_schule_map_1")),
                                    p(style="font-size:12px;color:grey",
                                      "Quelle der Daten: IEA, 2023; OECD, 2023, als Download, eigene Berechnungen durch MINTvernetzt."),

                                    shinyBS::bsPopover(id="h_international_schule_1", title="",
                                                       content = paste0("POPUP INFO TEXT HERE"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_1")
                                  )
                         ),

                         tabPanel("MINT-Kompetenz von Jungen und Mädchen", br(),


                                  #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                  # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                  shiny::sidebarPanel(
                                    width = 3,
                                    #p("LOREM"),

                                    mod_international_schule_item_ui("international_schule_item_1")


                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    htmlOutput(ns("plot_international_schule_item_1")),
                                    p(style="font-size:12px;color:grey",
                                      "Quelle der Daten: IEA, 2023, als Download, eigene Berechnungen durch MINTvernetzt."),

                                    shinyBS::bsPopover(id="h_international_schule_2", title="",
                                                       content = paste0("POPUP INFO TEXT HERE"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_2")
                                  )
                         ),

                         tabPanel("MINT-Kompetenz im Gruppenvergleich", br(),

                                  #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                  # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                  shiny::sidebarPanel(
                                    width = 3,
                                    #p("LOREM"),
                                    mod_international_schule_migration_ui("international_schule_migration_1")

                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    htmlOutput(ns("plot_international_schule_migration_1")),
                                    p(style="font-size:12px;color:grey",
                                      "Quelle der Daten: IEA, 2023; OECD, 2023, als Download, eigene Berechnungen durch MINTvernetzt."),

                                    shinyBS::bsPopover(id="h_international_schule_3", title="",
                                                       content = paste0("POPUP INFO TEXT HERE"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_3")
                                  )
                         )
             )
           )
  ),


  fluidRow(id="schule_ausserschulisch",
           shinydashboard::box(
             title = "Außerschulische, frühkindliche MINT-Bildung",
             width = 12,
             p("In diesem Abschnitt betrachten wir die Entwicklung der außerschulischen, frühkindlichen MINT-Bildung.
               Die interaktiven Grafiken basieren auf den Daten der 'Stiftung Kinder forschen' (SKf, früher Haus der kleinen Forscher).
               Es wird gezeigt, wie die Anzahl an Kitas, Horten und Grundschulen wächst, die in der MINT-Bildung aktiv sind. Außerdem wird
               die Anzahl an Fach- und Lehrkräften dargestellt, die sich in frühkindlicher MINT-Bildung durch die Stiftung Kinder forschen
               fortgebildet haben."), br(),
             p("Dies sind bislang die einzigen Darstellungen aus dem Bereich der ausserschulischen MINT-Bildung. Hier wird in Zukunft noch mehr hinzukommen."),

             tabsetPanel(type = "tabs",

                         tabPanel("SKf-zertifizierte und aktive Einrichtungen", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_ausserschulisch_skf_einrichtungen_ui("mod_ausserschulisch_skf_einrichtungen_ui_1"),
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_skf_einrichtungen")),

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
                                    ),


                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_skf_personal")),

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

    # Box 1 - Wer wählt MINT
    ## Waffle
    output$plot_waffle_mint <- renderPlot({
      kurse_waffle_mint(r)
    })

    ## Balkendiagramm
    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      kurse_einstieg_comparison(r)
    })

    ## Zeitverlauf
    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      kurse_verlauf_single(r)
    })

    ## Waffle Geschlecht
    plot_waffle_react <- reactive({
      kurse_waffle(r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    ## Karte Gender
    output$plot_map_kurse_gender <- renderUI({
      kurse_map_gender(r)
    })


    # Box 2 -  M-I-N-T

    ## Karte Fächer
    output$plot_map_kurse <- renderUI({
      kurse_map(r)
    })

    ## Verlauf nach BuLas
    output$plot_verlauf_multiple <- highcharter::renderHighchart({
      kurse_verlauf_multiple_bl(r)
    })

    ## Zeitverlauf Fächer
    output$plot_verlauf_kurse_bl_subjects <- highcharter::renderHighchart({
      kurse_verlauf_subjects_bl(r)
    })

    ## Dumbbell-Plot Mädchen
    output$plot_ranking_gender <- renderPlot({
      kurse_ranking_gender(r)
    })

    ## Balken Übersicht Fächer
    output$plot_comparison_subjects <- highcharter::renderHighchart({
      kurse_mint_comparison(r)
    })

    ## Balken Übersicht BuLas
    output$plot_comparison_bl <- highcharter::renderHighchart({
      kurse_mint_comparison_bl(r)
    })


    # Box 3 - Frauen

    ## Balken Frauen
    output$plot_comparison_gender <- highcharter::renderHighchart({
      kurse_comparison_gender(r)
    })

    ## Dumbbell Frauen
    output$plot_ranking_2 <- renderPlot({
      plot_ranking_react()
    })
    plot_ranking_react <- reactive({
      kurse_ranking(r) # type = "other"
    })


    # Box 4  Kompetenzdaten / IQB

    output$plot_iqb_standard_zeitverlauf <- highcharter::renderHighchart({
      iqb_standard_zeitverlauf(r)
    })

    output$plot_iqb_mathe_mittel_zeitverlauf <- highcharter::renderHighchart({
      iqb_mathe_mittel_zeitverlauf(r)
    })

    output$plot_iqb_fragebogen <- highcharter::renderHighchart({
      iqb_fragebogen(r)
    })

    # Box internationaler Vergleich PISA/TIMSS
    # Box 1 - Schule
    output$plot_international_schule_map_1 <- renderUI({
      logger::log_debug("plot_international_schule_map")
      plot_international_schule_map(r)
    })

    output$plot_international_schule_item_1 <- renderUI({
      logger::log_debug("plot_international_schule_item")
      plot_international_schule_item(r)
    })

    output$plot_international_schule_migration_1 <- renderUI({
      logger::log_debug("plot_international_schule_migration")
      plot_international_schule_migration(r)
    })


    # Box außerschulisch  / SKf

    output$plot_skf_einrichtungen <- highcharter::renderHighchart({
      skf_einrichtungen(r)
    })

    output$plot_skf_personal <- highcharter::renderHighchart({
      skf_personal(r)
    })




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

#' studium_studienzahl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'

tab1_name <- "Vergleich"

mod_studium_studienzahl_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Studium_BB.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Studium",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),


    # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 3,
        p(style = "text-align: left; font-size = 16px",
          "Auf dieser Seite zeigen wir statistische Kennzahlen zum Thema MINT-Fächer studieren. Wir zeigen, wie hoch der Anteil
           von MINT-Fächern gemessen an allen gewählten Studienfächern ist. Dazu zeigen wir Vergleiche nach männlichen und
           weiblichen Studierenden, einzelnen Fächern und nach Bundesländern.")
        ),

      shinydashboard::box(
        title = "Übersicht Fragestellungen",
        width = 3,
        p(
        style = "text-align: left; font-size = 16px",tags$a(href="#jump1c",
        span(tags$b(span("Fächerwahl MINT:")))), "Wie hoch ist der Anteil von Studierenden, die ein MINT-Fach studieren?"
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump2c",
          span(tags$b(span("M-I-N-T:")))), "Blick auf die einzelnen Fächer und Fachbereiche."

          ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump3c",
          span(tags$b(span("Frauen in MINT:")))), "Wie hoch ist der Anteil von Frauen in den MINT-Fächern?"
          ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump4c",
          span(tags$b(span("Internationale Studierende in MINT:")))), "Wie hoch ist der Anteil von internationalen Studierenden in den MINT-Fächern?"
        )
        ),

      shinydashboard::box(
        title = "Datenquellen",
        width = 3,
        p(style = "text-align: left; font-size = 16px",
          "Studierendenzahlen in Deutschland: Destatis 2022, auf Anfrage")

      ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 3,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        )))
    ,

  # Box 1

    fluidRow( id="jump1c",
      shinydashboard::box(
        title = "Fächerwahl MINT: Wie hoch ist der Anteil von Studierenden, die ein MINT-Fach studieren?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Fächern an allen Studienfächern in Deutschland.
          Dabei betrachten wir sowohl Studienanfänger:innen als auch Studierende allgemein. Darüber hinaus werfen wir ein Schlaglicht auf die Verteilung von Männern und Frauen in MINT"),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Anteil MINT nach Studierendengruppen", br(),

                             shiny::sidebarPanel(width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_test_ui("mod_studium_studienzahl_test_ui_1"),
                               p(style="font-size:12px;color:grey",
                               "Hinweis zur Darstellung: Falls die Karte abgeschnitten dargestellt wird, bitte das gesamte Ansichtsfenster einmal
                                 minimieren und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                               p(style="font-size:12px;color:grey",
                                 "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 38 %
                                 der Studienanfänger:innen (1. FS) ein MINT-Fach wählen, bei den Studierenden ist dieser Anteil
                                 mit 37 % in 2021 etwas geringer, was bedeutet, dass die Abbruchsquote in MINT höher liegt als
                                 in anderen Fachbereichen.")
                               ),
                             shiny::mainPanel(width = 9,
                               htmlOutput(ns("test")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."),
                        )),
                    tabPanel("Vergleich Anteil MINT nach Studierendengruppen II", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))
                    ),

                    tabPanel("Vergleich Anteil MINT nach Studierendengruppen im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_ui("mod_studium_studienzahl_einstieg_verlauf_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))

                    ),

                    tabPanel("Vergleich Anteil MINT nach Bundesländern im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_verlauf_bl_subject_ui("mod_studium_studienzahl_verlauf_bl_subject_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject"))
                               ,p(style="font-size:12px;color:grey",
                                  "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")

                             )
                    ),

                    tabPanel("Vergleich Anteil MINT nach Bundesländer im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_verlauf_ui("mod_studium_studienzahl_bl_verlauf")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    tabPanel("Alle Studierendengruppen auf einen Blick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_comparison_ui("mod_studium_studienzahl_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_comparison")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))
                    ),


                    tabPanel("Vergleich Studienfachwahl zwischen Frauen & Männer", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_choice_gender_ui("mod_studium_studienzahl_choice_gender_ui")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle_choice_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                             )
                    ),

                    tabPanel("Studienfachwahl Frauen im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_verlauf_bl_subject_gender_ui("mod_studium_studienzahl_verlauf_bl_subject_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    # tabPanel("Überblick Frauen und Männer", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #          .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_studium_studienzahl_ranking_bl_subject_gender_ui("mod_studium_studienzahl_ranking_bl_subject_gender_ui_1")
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            #highcharter::highchartOutput(ns("plot_ranking_studienzahl_bl_subject_gender1")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                    #            plotOutput(ns("plot_ranking_studienzahl_bl_subject_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                    #          )
                    # )

                  # ,
                  #
                  #
                  # tabPanel("Datensatz", br(),
                  #
                  #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                  #              .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                  #          shiny::sidebarPanel(
                  #            tags$style(".well {background-color:#FFFFFF;}"),
                  #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                  #            mod_studium_studienzahl_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                  #          shiny::mainPanel(
                  #            div(DT::dataTableOutput(ns("data_table_einstieg")),
                  #                style = "font-size: 75%; width: 75%"),
                  #            shiny::downloadButton(ns("download_data_box1"), label = "",
                  #                                  class = "butt",
                  #                                  icon = shiny::icon("download")))
                  # )
        ))),

    # Box 2

  fluidRow( id="jump2c",
      shinydashboard::box(
        title = "M-I-N-T: Blick auf die einzelnen Fächer und Fachbereiche",
        width = 12,
        p("Hier zeigen wir, wie häufig MINT-Fächer im Vergleich zu anderen Studienfächern in Deutschland gewählt werden.
          Außerdem kann man den Anteil von MINT-Fächern zwischen den Bundesländern vergleichen."),

        tabsetPanel(type = "tabs",


                    tabPanel("TOP-10-Fächer", br(),
                        #tags$head(
                       # tags$style(
                       #   ".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                       #      .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                      shiny::sidebarPanel(
                        width = 3,
                        mod_studium_top_faecher_ui("mod_studium_top_faecher"),
                        p(style = "font-size:12px;color:grey",
                          "Interpretationshilfe: In der ersten Einstellung sind die TOP-10-Fächer in Bayern in MINT bezogen auf den Frauen- bzw.
                          Männeranteil zu sehen. Die Fächer mit dem höchsten Frauenanteil in MINT sind Pharmazie (74 % Frauen) und Biologie (65 % Frauen).
                          Die Fächer mit dem höchsten Männeranteil
                          in MINT sind dagegen Verkehrstechnik / Nautik mit 86 % Männern und Elektrotechnik und Informationstechnik mit 84 %.")
                        ),
                      shiny::mainPanel(
                        width = 9,
                        htmlOutput(ns("plot_top_faecher")),
                        p(style = "font-size:12px;color:grey",
                          "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))
                    ),

                    tabPanel("Vergleich Fächer (Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_map_ui("mod_studium_studienzahl_bl_map"),
                               p(style="font-size:12px;color:grey",
                                 "Hinweis zur Darstellung: Falls die Karten abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_studienzahl_map")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                             )
                    ),


                    # Fehler in der Boxgrösse, muss noch behoben werden

                   # tabPanel("Alle Fächer auf einen Blick", br(),
                   #
                   #           tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                   #           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                   #           shiny::sidebarPanel(
                   #             width = 3,
                   #             mod_studium_studienzahl_ranking_bl_subject_ui("mod_studium_studienzahl_ranking_bl_subject_ui_1")),
                   #           shiny::mainPanel(
                   #             width = 9,
                   #             highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject1")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                   #             #plotOutput(ns("plot_ranking_bl_subject")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                   #           )
                   #  ),

                    tabPanel("Vergleich nach Bundesländern", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_vergleich_ui("studium_studienzahl_bl_vergleich")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_vergleich_bl1")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                               #plotOutput(ns("plot_vergleich_bl")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen.")
                             )
                    )
        ))),

    fluidRow(id="jump3c",
      shinydashboard::box(
        title = "Frauen in MINT: Wie hoch ist der Anteil von Frauen in den MINT-Fächern?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Frauen und Männern in Deutschland innerhalb der MINT-Studienfächer an.
          Zum Vergleich zeigen wir auch den Anteil in den anderen, nicht-MINT-Fächern.
          Die verschiedenen Diagramme bieten außerdem Fächer- und Bundeslandvergleiche."),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Anteil Frauen in MINT zwischen Studienanfänger:innen und Studierenden", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_gender_ui("mod_studium_studienzahl_einstieg_gender_ui_1"),
                               p(style="font-size:12px;color:grey",
                                 "Hinweis zur Darstellung: Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                               p(style="font-size:12px;color:grey",
                               "Interpretationshilfe: In der ersten interaktiven Grafik ist zu sehen,
                                dass deutschlandweit 2021 der Anteil von Frauen unter den Studienanfänger:innen in MINT-Fächern 34 % ausmacht.
                               Unter den Studierenden liegt der Frauenanteil in MINT-Fächern bei 32 % etwas darunter. Dies deutet darauf hin, dass bei weiblichen Studierenden die Abbruchquote in MINT höher ist als bei männlichen Studierenden.")
                               ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_einstieg_pie_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))
                    ),

                    tabPanel("Anteil Frauen im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_gender_ui("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))

                  ),

                  tabPanel("Anteil Frauen nach Bundesländern", br(),

                           shiny::sidebarPanel(
                             width = 3,
                             tags$style(".well  {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_studium_studienzahl_einstieg_comparison_gender_ui("mod_studium_studienzahl_einstieg_comparison_gender_ui_1")),
                           shiny::mainPanel(
                             width = 9,
                             highcharter::highchartOutput(ns("plot_einstieg_comparison_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))
                  )
        ))),

  fluidRow(id="jump4c",
           shinydashboard::box(
             title = "Internationale Studierende in MINT: Wie hoch ist der Anteil internationaler Studierender in den MINT-Fächern?",
             width = 12,
             p("Diese Darstellungen zeigen, wie hoch der Anteil internationale Studierender an allen Studierenden eines Fachbereichs oder eines Studienfachs ist.
               Außerdem zeigen wir hier, wie viele internationale Studierende in welchen Fächern studieren. Als 'internationale Studierende' fassen wir alle
               Studierenden zusammen, welche in Deutschland studieren, aber keine deutsche Staatsbürgerschaft besitzen."),
             tabsetPanel(type = "tabs",
                         tabPanel("Anteil von internationalen Studierenden nach Fächern", br(),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    tags$style(".well {background-color:#FFFFFF;}"),
                                    tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                    mod_studium_studienzahl_ausl_ui("mod_studium_studienzahl_ausl_ui"),
                                    p(style="font-size:12px;color:grey", "Interpretationshilfe: Diese Grafik zeigt z. B., dass im Jahr 2020
                                      in Ostdeutschland ca. 39 % der Elektortechnik- und Informationstechnik-Studierenden internationale
                                      Studierende waren. Wählt man als Betrachtung die Anzahl der Studierenden, sieht man das diese 39 % 5.391 internationale
                                      Studierende in Elektrotechnik und Informationstechnik ausmachen.")),
                                  shiny::mainPanel(
                                    width = 9,
                                    tags$head(tags$style(HTML(".small-box {height: 400px}"))),
                                    highcharter::highchartOutput(ns("plot_auslaender_test"), height = "650px"),
                                    p(style="font-size:12px;color:grey", "Hinweis: In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden nur die vorhandenen Studienfachgruppen angezeigt."),
                                    p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))
                                  )
                         ,

                         tabPanel("Anteil von internationalen Studierenden im Zeitvergleich", br(),

                                  shiny::sidebarPanel(
                                    width = 3,
                                    tags$style(".well  {background-color:#FFFFFF;}"),
                                    tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                    mod_studium_studienzahl_ausl_zeit_ui("mod_studium_studienzahl_ausl_zeit_ui")),
                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_auslaender_zeit")),
                                    p(style="font-size:12px;color:grey", "Hinweis: In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen stehen nur die vorhandenen Studienfachgruppen zur Auswahl."),
                                    p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))

                         # tabPanel("Anteil von internationalen Studierenden im Zeitgergleich", br(),
                         #          shiny::sidebarPanel(
                         #            width = 3,
                         #            tags$style(".well {background-color:#FFFFFF;}"),
                         #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                         #            mod_studium_studienzahl_ausl_zeit_ui("mod_studium_studienzahl_ausl_zeit_ui_1")),
                         #          shiny::mainPanel(
                         #            width = 9,
                         #            tags$head(tags$style(HTML(".small-box {height: 400px}"))),
                         #            highcharter::highchartOutput(ns("plot_auslaender_zeit")), p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen."))
                         ),


             ))),




    #Footer
  funct_footer()
  ) # Tagslist zu



    # fluidRow(
    #   shinydashboard::box(
    #     title = "Nicht zuordbar",
    #     width = 12,
    #     p("Hier finden Sie den Anteil an Belegungen von Frauen und Männern in MINT-Fächern für die Bundesländer im Vergleich. "),
    #     tabsetPanel(type = "tabs",
    #                 tabPanel("Karte", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_map_gender_ui("mod_studium_studienzahl_bl_map_gender")
    #                          ),
    #                          shiny::mainPanel(
    #                            htmlOutput(ns("plot_studienzahl_map_gender"))
    #                          )
    #                 ),
    #                 tabPanel("Vergleich (Bundesländer)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_verlauf_gender_ui("mod_studium_studienzahl_bl_verlauf_gender")
    #                          ),
    #                          shiny::mainPanel(
    #                            highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf_gender"))
    #                          )
    #                 ),
    #
    #                 tabPanel("Überblick (Bundsländer)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_vergleich_gender_ui("mod_studium_studienzahl_bl_vergleich_gender_ui")
    #                          ),
    #                          shiny::mainPanel(
    #                            plotOutput(ns("plot_ranking_studienzahl_bl_vergleich_gender"))
    #                          )
    #                 ),
    #
    #
    #
    #                 # zeigt Anteile MINT und nicht Gender
    #
    #                 tabPanel("Überblick (doppelt)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            tags$style(".well {background-color:#FFFFFF;}"),
    #                            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
    #                            mod_studium_studienzahl_einstieg_comparison_gender_ui("mod_studium_studienzahl_einstieg_comparison_gender_ui_1")),
    #                          shiny::mainPanel(
    #                            plotOutput(ns("plot_einstieg_comparison_gender")))
    #
    #                 ),
    #
    #
    #     ))),


}

#' studium_studienzahl Server Functions
#'
#' @noRd
mod_studium_studienzahl_server <- function(id, data_studierende_neu,
                                           #data_studierende_faecher3,
                                           r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    # Box 2
    output$plot_einstieg_pie <- renderUI({
      studienzahl_einstieg_pie(data_studierende:neu,r)
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      studienzahl_verlauf_single(data_studierende2,r)
    })

    # all_mint_23_react <- reactive({
    #   studienzahl_all_mint_23(data_studierende2, r)
    # })



    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      studienzahl_einstieg_comparison(data_studierende2,r)
    })

    data_table_einstieg_react <- reactive({
      data_einstieg(data_studierende, r)
    })

    # output$data_table_einstieg <- DT::renderDT({
    #   data_table_einstieg_react()
    # })

    # Box 3
    output$plot_einstieg_pie_gender <- renderUI({
      studienzahl_einstieg_pie_gender(data_studierende2,r)
    })

    output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
      studienzahl_verlauf_single_gender(data_studierende2,r)
    })

    output$plot_einstieg_comparison_gender <- highcharter::renderHighchart({
      studienzahl_einstieg_comparison_gender(studierende_faecher_alle_indi,r)
    })

    output$plot_verlauf_studienzahl_bl_subject1 <- highcharter::renderHighchart({
      ranking_bl_subject(studierende_faecher_alle_indi,r)
    })


    output$plot_verlauf_studienzahl_bl1 <- highcharter::renderHighchart({
      ranking_bl_subject(data_studierende,r)
    })

    output$test <- renderUI({
      studienzahl_test(data_studierende_neu, r)
    })

    # Box 4
    plot_waffle_react <- reactive({
      studienzahl_waffle_mint(data_studierende_neu,r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    output$plot_verlauf_studienzahl_bl_subject <- highcharter::renderHighchart({
      studienzahl_verlauf_bl_subject(data_studierende2,r)
    })

    output$plot_ranking_bl_subject <- renderPlot({
      ranking_bl_subject(data_studierende,r)
    })

    # Box 5
    plot_waffle_choice_gender_react <- reactive({
      studienzahl_waffle_choice_gender(data_studierende2,r)
    })

    output$plot_waffle_choice_gender <- renderPlot({
      plot_waffle_choice_gender_react()
    })

    output$plot_verlauf_studienzahl_bl_subject_gender <- highcharter::renderHighchart({
      studierende_verlauf_single_bl_gender(data_studierende2,r)
    })

    plot_ranking_studienzahl_bl_subject_gender_react <- reactive({
      studienfaecher_ranking(data_studierende2, r, type="other")
    })

    output$plot_ranking_studienzahl_bl_subject_gender <- renderPlot({
      plot_ranking_studienzahl_bl_subject_gender_react()
    })

    output$plot_ranking_studienzahl_bl_subject_gender1 <- highcharter::renderHighchart({
      plot_ranking_studienzahl_bl_subject_gender_react(data_studierende,r)
    })



    # Box 6
    output$plot_studienzahl_map <- renderUI({
      studierende_map(studierende_faecher_alle_indi,r)
    })

    output$plot_studienzahl_bl_verlauf <- highcharter::renderHighchart({
      studierende_verlauf_multiple_bl(data_studierende2,r)
    })


     output$plot_vergleich_bl <-  renderPlot({
      studierende_mint_vergleich_bl(data_studierende,r)
    })

     output$plot_vergleich_bl1 <- highcharter::renderHighchart({
       studierende_mint_vergleich_bl(studierende_faecher_alle_indi,r)
     })


    # Box 7
    output$plot_studienzahl_map_gender <- renderUI({
      studierende_map_gender(data_studierende,r)
    })

    output$plot_studienzahl_bl_verlauf_gender <- highcharter::renderHighchart({
      studierende_verlauf_multiple_bl_gender(data_studierende,r)
    })

    plot_ranking_studienzahl_bl_vergleich_gender_react <- reactive({
      bundeslaender_ranking(data_studierende, r, type="other")
    })

    output$plot_ranking_studienzahl_bl_vergleich_gender <- renderPlot({
      plot_ranking_studienzahl_bl_vergleich_gender_react()
    })

    # Box 8
    output$plot_top_faecher <-  renderUI({
      plot_ranking_top_faecher(data_studierende_faecher, r)
    })

    # Box Ausländer

    output$plot_auslaender_test <-  highcharter::renderHighchart({
      plot_auslaender_mint(studierende_faecher_alle_indi, r)
    })

    output$plot_auslaender_zeit <-  highcharter::renderHighchart({
      plot_auslaender_mint_zeit(studierende_faecher_alle_indi, r)
    })


    # downloader
    # output$download_data_box1 <- shiny::downloadHandler(
    #   filename = function() {
    #     paste("data_studium", "csv", sep = ".")
    #   },
    #   content = function(file){
    #     write.csv(data_table_einstieg_react(), file)
    #   }
    # )

  })
}

## To be copied in the UI
# mod_studium_studienzahl_ui("studium_studienzahl_1")

## To be copied in the server
# mod_studium_studienzahl_server("studium_studienzahl_1")

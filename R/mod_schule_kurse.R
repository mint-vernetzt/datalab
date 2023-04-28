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
        width = 3,
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
        title = "Übersicht Fragestellungen",
        width = 3,

        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump1a",
        span(tags$b(span("Fächerwahl MINT:")))),"Wie hoch ist der Anteil von MINT-Fächern in der Oberstufe?"
          ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump2a",
         span(tags$b(span("M-I-N-T:")))), "Blick auf die einzelnen Fächer und Fachbereiche."

          ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump3a",
        span(tags$b(span("Frauen in MINT:")))),"Wie hoch ist der Anteil von Mädchen in den MINT-Fächern?"),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump4a",
        span(tags$b(span("Kompetenzdaten IQB:")))),"MINT-Kompetenzen der 4. Klassen."),

        ),

      shinydashboard::box(
        title = "Datenquellen",
        width = 3,
        p(style = "text-align: left; font-size = 16px",
          "Schüler:innenzahlen der Oberstufe: Kulturministerkonferenz (KMK) 2022, auf Anfrage"),
        p(style = "text-align: left; font-size = 16px",
          "Weitere Statistiken über die Belegung von MINT-Fächern in anderen Klassenstufen liegen uns derzeit nicht vor.")
        ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 3,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))),

  # Box 1

    fluidRow(id="jump1a",
      shinydashboard::box(
        title = "Fächerwahl MINT: Wie hoch ist der Anteil von MINT-Fächern in der Oberstufe?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir, wie häufig MINT-Fächer im Vergleich zu anderen Fächern in der Oberstufe in Deutschland belegt werden."),
        # p("In diesen interaktiven Diagrammen beleuchten wir, wie häufig MINT-Fächer im Vergleich zu anderen Fächern in der Oberstufe in Deutschland belegt werden. Dabei sind die möglichen Belegungen von den Vorgaben der Bundesländer abhängig.", tags$a(icon("question-circle"), id="q3"),),
        # shinyBS::bsPopover(id="q3", title = "",
        #                    content = "In vielen werden unterschiedliche und unterschiedlich viele Leistungskurse angeboten.<br><br>
        #                    In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer
        #                    mit erhöhten Wochenstunden und werden hier als Leistungskurse gezählt. Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden. Entsprechend sind die Anteile der Grundlagenfächer an den Grundkursen sehr gering.",
        #                    trigger = "hover"), #das ist in Box
                tabsetPanel(type = "tabs",

                    tabPanel("Vergleich Grund- und Leistungskurse, Fachbereiche", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_mint_ui("mod_schule_kurse_multiple_mint_ui_1"),
                               p(style="font-size:12px;color:grey",
                                 "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass im Jahr 2021 in Deutschland 24 % aller gewählten Grundkurse aus dem Bereich MINT sind. Bei Leistungskursen liegt der Anteil im Jahr 2021 bei 33 %.")
                               ),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle_mint"))
                               ,
                               p(style="font-size:12px;color:grey", br(),
                              "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                              p(style="font-size:12px;color:grey",
                                "Hinweis: Durch Rundung der berechneten Werte kann es zu minimalen Abweichungen zwischen den Grafiken kommen.")

                               )
                    ),
                    tabPanel("Vergleich Grund- und Leistungskurse, MINT aggregiert", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_comparison_ui("mod_schule_kurse_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_comparison"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."))

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
                    #     htmlOutput(ns("plot_einstieg_pie")),p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."))
                    #         ),

                    tabPanel("Vergleich Grund- und Leistungskurse im Zeitverlauf", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_verlauf_ui("mod_schule_kurse_einstieg_verlauf_ui_1")


                               ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")),p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."))
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
                               ,p(style="font-size:12px;color:grey", br(),"Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."))
                    ),

                    tabPanel("Vergleich Mädchen und Jungen nach Bundesländern (Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               # shinyBS::bsPopover(id="q1", title = "Überschrift, kann auch leer bleiben und dadurch ausgeblendet",
                               #                    content = "Inhalt<br><br><br><i>auch HTML tags möglich</i>",
                               #                    trigger = "hover"), #das ist in Box
                               # tags$a(icon("question-circle"), id="q1"), # das ist was man in App sieht
                               mod_schule_kurse_map_gender_ui("mod_schule_kurse_map_gender_ui_1"),

                               p(style="font-size:12px;color:grey",
                                 "Hinweis zur Darstellung: Falls die Karte abgeschnitten dargestellt wird, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein.")
                               # ,
                               # shinyBS::bsPopover(id="q4", title = "",
                               #                    content = "Falls die Karte abgeschnitten dargestellt wird, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren.
                               #                    Dann stellt sich das Seitenverhältnis des Desktops richtig ein.",
                               #                    trigger = "hover"), #das ist in Box
                               # tags$a(icon("question-circle"), id="q4"),
                               ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_map_kurse_gender"))
                               ,
                               # shinyBS::bsPopover(id="q1", title = "",
                               #                    content = "Für Baden-Würtemberg liegen die Belegungszahlen nur für Mädchen und Jungen aggregiert vor",
                               #                    trigger = "hover", placement = "left"), #das ist in Box
                               # tags$a(p(style="font-size:12px;color:grey","Darum ist Baden-Württemberg ausgegraut"), id="q1"), # das ist was man in App sieht
                               # shinyBS::bsPopover(id="q2", title = "",
                               #                    content = "Für Baden-Würtemberg liegen die Belegungszahlen nur für Mädchen und Jungen aggregiert vor",
                               #                    trigger = "hover"), #das ist in Box
                               # tags$a("Darum ist Baden-Württemberg ausgegraut", id="q2"), # das ist was man in App sieht
                               # shinyBS::bsPopover(id="q6", title = "",
                               #                    content = "Für Baden-Würtemberg liegen die Belegungszahlen nur für Mädchen und Jungen aggregiert vor",
                               #                    trigger = "hover"), #das ist in Box
                               # tags$a(icon("question-circle"), id="q6"),

                               p(style="font-size:12px;color:grey",br(), "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                               p(style="font-size:12px;color:grey", "Hinweis: In Bayern gibt es keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache
                                 sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer und werden hier als Leistungskurse gezählt.
                                 Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden und entsprechend sind
                                 die Anteile der Grundlagenfächer an den Grundkursen sehr gering. Für Baden-Würtemberg liegen die Belegungszahlen nur für Mädchen und Jungen aggregiert vor.")
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

    fluidRow(id="jump2a",
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
                               p(style="font-size:12px;color:grey",
                               "Hinweis zur Darstellung: Falls die Karte abgeschnitten dargestellt wird, bitte das gesamte Ansichtsfenster einmal
                                 verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                               p(style="font-size:12px;color:grey",
                               "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass die MINT-Fächer in Grundkursen den höchsten Anteil in Sachsen haben mit 29 % Prozent.
                               Bei den Leistungskursen ist der Anteil der MINT-Fächer in Sachsen-Anhalt mit 50 % am höchsten. Die Vergleiche zwischen den Bundesländern sind jedoch schwierig,
                               da die Regelungen für die Wahl der Kurse in den Bundesländern sehr unterschiedlich sind.")
                               ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_map_kurse"))
                               ,p(style="font-size:12px;color:grey",
                                  "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                               p(style="font-size:12px;color:grey",
                               "Hinweis: In Bayern gibt keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache
                                 sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer und werden hier als Leistungskurse gezählt.
                                 Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden und entsprechend sind
                                 die Anteile der Grundlagenfächer an den Grundkursen sehr gering."))
                    ),
                    tabPanel("Vergleich Bundesländer im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_verlauf_multiple_ui("mod_schule_kurse_verlauf_multiple_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_multiple"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                               p(style="font-size:12px;color:grey", "Hinweis: In Bayern gibt keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache
                                 sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer und werden hier als Leistungskurse gezählt.
                                 Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden und entsprechend sind
                                 die Anteile der Grundlagenfächer an den Grundkursen sehr gering."))
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
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                               p(style="font-size:12px;color:grey", "Hinweis: In Bayern gibt keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache
                                 sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer und werden hier als Leistungskurse gezählt.
                                 Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden und entsprechend sind
                                 die Anteile der Grundlagenfächer an den Grundkursen sehr gering.")))
                    ,


                    tabPanel("Vergleich Grund- und Leistungskurse nach Bundesländern", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_ranking_gender_ui("mod_schule_kurse_ranking_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_ranking_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                               p(style="font-size:12px;color:grey", "Hinweis: In Bayern gibt keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache
                                 sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer und werden hier als Leistungskurse gezählt.
                                 Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden und entsprechend sind
                                 die Anteile der Grundlagenfächer an den Grundkursen sehr gering."))

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
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                               p(style="font-size:12px;color:grey", "Hinweis: In Bayern gibt keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache
                                 sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer und werden hier als Leistungskurse gezählt.
                                 Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden und entsprechend sind
                                 die Anteile der Grundlagenfächer an den Grundkursen sehr gering.")

                             )),
                    tabPanel("Alle Bundesländer auf einen Blick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_comparison_bl_ui("mod_schule_kurse_comparison_bl_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_bl"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                               p(style="font-size:12px;color:grey", "Hinweis: In Bayern gibt keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache
                                 sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer und werden hier als Leistungskurse gezählt.
                                 Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden und entsprechend sind
                                 die Anteile der Grundlagenfächer an den Grundkursen sehr gering.")
                               ))

        ))),
    fluidRow(id="jump3a",
      shinydashboard::box(
        title = "Mädchen in MINT: Wie hoch ist der Anteil von Mädchen in den MINT-Fächern?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Mädchen und Jungen innerhalb der MINT-Fächer in Deutschland an. Zum Vergleich
          zeigen wir auch den Anteil in den anderen, nicht-MINT-Fächern. Die verschiedenen Diagramme bieten außerdem
          Fächer- und Bundeslandvergleiche."),

        tabsetPanel(type = "tabs",

                    tabPanel("Vergleich Fächergruppen", br(),


                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_comparison_gender_ui("mod_schule_kurse_comparison_gender_ui_1"),
                               p(style="font-size:12px;color:grey",
                               "Interpretationshilfe: Die erste Darstellung zeigt, dass der Anteil von Mädchen bzw. Frauen in allen MINT-Grundkursen
                                in Deutschland 2021 53 % beträgt. In den MINT-Leistungskursen beträgt dieser Anteil 48 %. In den Nicht-MINT-Fächern
                               ist der Anteil an Mädchen bzw. Frauen etwas höher: In Grundkursen machen Frauen 54 % aus, in Leistungskursen sogar 58 %.")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."))
                    ),


                    # erstmal raus, weil Pies nicht so schön

                    # tabPanel("Vergleich (Pie) ", br(),
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
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."))
                    # ),

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
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."))
                    # ),
                    tabPanel("Vergleich Grund- und Leistungskursen nach einzelnen Fächern", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_ranking_ui("mod_schule_kurse_ranking_ui_1"), br(),
                               p(style="font-size:12px;color:grey",
                                 "Lesehilfe: Hier zeigen wir einen direkten Vergleich zwischen den Anteilen von Mädchen in Grundkursen und Leistungskursen.
                                 Die Grafik kann wie ein Balkendigramm gelesen werden. Die Verbindungslinie zwischen den Punkten verdeutlicht lediglich die Diskrepanz zwischen den Werten.")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_ranking_2"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen."),
                               p(style="font-size:12px;color:grey","Hinweis: Für Baden-Würtemberg liegen die Belegungszahlen nur für Mädchen und Jungen aggregiert vor.")
                               )
                             ),


                    ))),

      fluidRow(id="jump4a",
           shinydashboard::box(
             title = "Kompetenzdaten IQB: MINT-Kompetenzen der 4. Klassen",
             width = 12,
             p("Dieses interaktive Diagramm gibt einen ersten Einblick in die Mathe-Kompetenzen von Schülerinnen und Schüler der 4. Klassen.
             Die Daten stammen aus der Befragung des Instituts zur Qualitätsentwicklung im Bildungswesen e.V. (IQB), das in regelmäßigen Abständen
             die Leistung von Schülerinnen und Schülern in verschiedenen Fächern testet."),
             p(),
             p("Zeitnah werden weitere Darstellungen der Daten aus den IQB-Befragungen in den 4. und 9. Klassen folgen!"),
             tabsetPanel(type = "tabs",

                         tabPanel("Leistungsschwache Schüler:innen in Mathematik", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_standard_zeitverlauf_ui("mod_schule_kurse_iqb_standard_zeitverlauf_ui_1"),
                                    p(style="font-size:12px;color:grey",
                                      "Interpretationshilfe: Während 2011 noch 11.9 % der Schüler und Schülerinnen die Mindestanforderung in Mathe nicht erfüllen,
                                      gilt 2021 ein fast doppelt so großer Anteil an Schüler/Schülerinnnen als leistungsschwach in Mathematik (21.8 %)."),
                                    p(style="font-size:12px;color:grey",
                                      "Hinweis: Für Mecklenburg-Vorpommern liegen keine Daten vor."),
                                  ),


                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_iqb_standard_zeitverlauf"))
                                    ,
                                    p(style="font-size:12px;color:grey", br(),
                                      "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen."),
                                    # p(style="font-size:12px;color:grey",
                                      # "Hinweis: Für Mecklenburg-Vorpommern liegen keine Daten vor.")
                                  )
                         ),
                         tabPanel("Leistung Mathematik im Gruppenvergleich", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_ui("mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_ui_1"),
                                    # p(style="font-size:12px;color:grey",
                                    #   "Interpretationshilfe: Während 2011 noch 11.9 % der Schüler und Schülerinnen die Mindestanforderung in Mathe nicht erfüllen,
                                    #   gilt 2021 ein fast doppelt so großer Anteil an Schüler/Schülerinnnen als leistungsschwach in Mathematik (21.8 %)."),
                                    p(style="font-size:12px;color:grey",
                                      "Hinweis: Für Mecklenburg-Vorpommern liegen keine Daten vor."),
                                  ),


                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_iqb_mathe_mittel_zeitverlauf"))
                                    ,
                                    p(style="font-size:12px;color:grey", br(),
                                      "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen."),
                                    # p(style="font-size:12px;color:grey",
                                    # "Hinweis: Für Mecklenburg-Vorpommern liegen keine Daten vor.")
                                  )
                         )
             ))),


    # Footer

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
                      "Copyright © 2023. Alle Rechte vorbehalten Stifterverband")),

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

#' schule_kurse Server Functions
#'
#' @noRd
mod_schule_kurse_server <- function(id, data_kurse, data_iqb_4klasse, data_iqb_ges, r){
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

    output$plot_comparison_gender <- highcharter::renderHighchart({
      kurse_comparison_gender(data_kurse,r)
    })

    # Box 4
    output$plot_waffle_mint <- renderPlot({
      kurse_waffle_mint(data_kurse,r)
    })

    output$plot_verlauf_kurse_bl_subjects <- highcharter::renderHighchart({
      kurse_verlauf_subjects_bl(data_kurse,r)
    })

    output$plot_comparison_subjects <- highcharter::renderHighchart({
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

    output$plot_comparison_bl <- highcharter::renderHighchart({
      kurse_mint_comparison_bl(data_kurse,r)
    })

    # output$plot_comparison_bl <- renderPlot({
    #   kurse_mint_comparison_bl(data_kurse,r)
    # })

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

    output$plot_iqb_standard_zeitverlauf <- highcharter::renderHighchart({
      iqb_standard_zeitverlauf(data_iqb_4klasse,r)
    })

    output$plot_iqb_mathe_mittel_zeitverlauf <- highcharter::renderHighchart({
      iqb_mathe_mittel_zeitverlauf(data_iqb_ges,r)
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

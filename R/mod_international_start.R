#' home_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_start_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      div(class = "clean-box",
          column(
            width = 12,
            img(src='www/Banner_International.jpg',
                class = "img-responsive",
                #height = "150px", width = "150px",
                alt = "Banner Fokus: MINT international",
                style="display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"
            )))),

    # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Auf dieser Fokus-Seite blicken wir über Deutschland hinaus. Hier können Sie in den Bereichen ''Schule'',
          ''Studium'' und ''Beruf' internationale Vergleiche anstellen. Betrachten Sie zum Beispiel, in welchen Ländern
          der MINT-Anteil besonders hoch ist oder wo ein verhältnismäßig hoher Frauenanteil in MINT erreicht wird.
          Wenn Sie sich für den MINT-Nachwuchs interessieren, können Sie die Ergebnisse der internationalen MINT-Kompetenzerhebungen PISA und TIMSS betrachten.")),
      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich?", br(),
          "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über die Teilnahme an unserer kurzen",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        title = "Links zu den Themen dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_international",
                                                              span(tags$b(span("MINT-Kompetenzen im internationalen Vergleich:")))),
          "Wie schneidet Deutschland im internationalen Vergleich in den MINT-Kompetenztests PISA und TIMSS ab?"
          ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_international",
                                                              span(tags$b(span("MINT-Studierende im internationalen Vergleich:")))),
          "Hier können Sie den MINT-Anteil an deutschen Hochschulen international vergleichen."
          ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_international",
                                                              span(tags$b(span("MINT-Beschäftigung im internationalen Vergleich:")))),
          "Hier können Sie den MINT-Anteil im deutschen Arbeitsmarkt international vergleichen."
        )

      ),


      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          span(tags$b(span("Schüler:innen-Kompetenz-Daten: OECD, 2023, freier Download (PISA); IEA, 2024, freier Download (TIMSS)."))),
               "Die Ergebnisse der PISA-Erhebung 2025 werden Ende 2026 erwartet, die Ergebnisse von TIMSS 2027 Ende 2028."),
        p( span(tags$b(span("Vergleichszahlen europaweit: Eurostat, 2023, freier Download.")))),
        p(span(tags$b(span("Vergleichszahlen der OECD-Staaten: OECD, 2023, freier Download.")))),
        p(span(tags$b(span("Vergleichszahlen weltweit: UNESCO, 2023, freier Download.")))),
        p("Diese Themenseite wird demnächst überarbeitet, dabei werden die amtlichen
          Statistiken der internationalen Studierenden- und Beschäftigtenzahlen aktualisiert.")

      )

    ),

    # Schule international ----
    fluidRow(id="schule_international",
             shinydashboard::box(
               title = "MINT-Kompetenzen im internationalen Vergleich: Wie schneidet Deutschland im internationalen Vergleich in den MINT-Kompetenztests Pisa und TIMSS ab?",
               width = 12,
               column(
                 width = 8,
               p("In dieser Box blicken wir auf die Kompetenzdaten der PISA-Studie und der TIMSS-Erhebung.
               Bei beiden Erhebungen werden unter anderem Kompetenztests in Mathematik und Naturwissenschaften in 4. und 9. Klassen weltweit durchgeführt.
               Außerdem betrachten die Grafiken Kompetenzunterschiede zwischen Jungen und Mädchen und Unterschiede in Abhängigkeit der sozialen Herkunft der Kinder."),
               p(),
               p(),
               shinyBS::bsPopover(id="i_schule_international_1", title = "",
                                  content = paste0("Eine Einordnung sowie weitere Ergebnisse der TIMSS-Erhebung finden Sie hier: <br> <a>https://timss2019.org/reports/</a> <br><br> Weitere Informationen zur PISA-Studie und ihren Ergebnissen finden Sie hier:. <br> <a> https://www.oecd.org/pisa/ </a>"),
                                  placement = "right",
                                  trigger = "click"),
               tags$a(paste0("Um zu den Original-Ergebnisberichten von TIMSS und PISA zu kommen, klicken Sie hier:"), icon("info-circle"), id = "i_schule_international_1"),
               p(),
               p()
               ),
               column(
                 width = 12,
               tabsetPanel(type = "tabs",
        # tab 1
                           tabPanel("MINT-Kompetenz im Ländervergleich", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      mod_international_schule_map_ui("international_schule_map_1"),
                                      # br(),br(),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_schule_map_1"),
                                      #   label = "Download",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_schule_map_1")),
                                                                   color = "#154194"),

                                      shinyBS::bsPopover(id="h_international_schule_1", title="",
                                                         content = paste0("Regionen, die nicht als unabhängige Staaten anerkannt werden (z. B. Taiwan, Hongkong), können aus technischen Gründen nicht in den Karten dargestellt werden. Daten dieser Regionen sind in den weiteren Grafiken enthalten."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_1")
                                    )
                           ),
        # tab 2

                           tabPanel("MINT-Kompetenz von Jungen und Mädchen", br(),


                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),

                                      mod_international_schule_item_ui("international_schule_item_1"),
                                      br(),br(),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_schule_item_1"),
                                      #   label = "Download",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_schule_item_1")),
                                                                   color = "#154194"),


                                      shinyBS::bsPopover(id="h_international_schule_2", title="",
                                                         content = paste0("Test-Leistungen der Schüler:innen werden nur dann als unterschiedlich dargestellt, wenn das mittlere Ergebnis der Mädchen im Vergleich zu den Jungen signifikant unterschiedlich ist.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_2")
                                    )
                           ),
        # tab 3

                           tabPanel("MINT-Kompetenz im Gruppenvergleich", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_schule_migration_ui("international_schule_migration_1"),
                                      # br(),br(),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_schule_migration_1"),
                                      #   label = "Download",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_schule_migration_1")),
                                                                   color = "#154194"),


                                      shinyBS::bsPopover(id="h_international_schule_3", title="",
                                                         content = paste0("Das Bildungskapital wurde über die Anzahl der Bücher im Zuhause der Kinder operationalisiert. Zur einfacheren Darstellung wird in dieser Abbildung nur eine Auswahl der vorliegenden Untergruppen betrachtet."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_3")
                                    )
                           )
               )
               )
             )
    ),

    # Studium international ----

    fluidRow(id="studium_international",
             shinydashboard::box(

               title = "MINT-Studierende im internationalen Vergleich: Hier können Sie den MINT-Anteil an deutschen Hochschulen international vergleichen.",

               width = 12,
               column(
                 width = 8,
               p("Diese Box zeigt eine Übersicht über MINT-Statistiken aus dem Bereich Studium für den internationalen Vergleich.
               Die Grafiken basieren auf öffentlichen Statistiken, die durch die EU, die OECD oder die UNESCO gesammelt wurden.
                 Zum einen zeigen wir, wie groß der Anteil von MINT-Studierenden an allen Studierenden in verschiedenen Ländern ist. Außerdem ist zu sehen,
                 in welchen Ländern der Frauenanteil oder der Anteil an internationalen Studierenden in MINT-Studiengängen besonders groß oder klein ist.")
               ),
               column(
                 width = 12,
               tabsetPanel(type = "tabs",

      # tab 1
                           tabPanel("Vergleich MINT-Anteil (Karte)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      mod_international_map_ui("mod_international_map_ui_1"),
                                      # br(),br(),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_studienzahl_map_1"),
                                      #   label = "Download",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_studienzahl_map_1")),
                                                                   color = "#154194"),

                                      shinyBS::bsPopover(id="h_international_vergl_1", title="",
                                                         content = paste0("In den Europa-Daten sowie den &quotWeltweit&quot-Daten der UNESCO wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung)."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_1") )
                           ),
# tab 2
                           tabPanel("Vergleich Frauen in MINT (Karte)", br(),
                                    shiny::sidebarPanel(
                                      width = 3,
                                      mod_international_map_fem_ui("international_map_fem_ui_1"),
                                      # br(),br(),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_map_fem_1"),
                                      #   label = "Download",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_map_fem_1")),
                                                                   color = "#154194"),

                                      shinyBS::bsPopover(id="h_international_vergl_2", title="",
                                                         content = paste0("In den Europa-Daten wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung)."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_2") )

                           ),
# tab 3

                           tabPanel("Top 10 der MINT-Länder", br(),
                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_top10_mint_ui("international_top10_mint_1"),
                                      # br(),br()
                                      # ,
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_top10_mint_1_1"),
                                      #   label = "Download (links)",
                                      #   icon = icon("download")),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_top10_mint_1_2"),
                                      #   label = "Download (rechts)",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_top10_mint_1")),
                                                                   color = "#154194"),

                                      shinyBS::bsPopover(id="h_international_vergl_3", title="",
                                                         content = paste0("In den Europa-Daten wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung).", "<br><br>Lichtenstein und San Marino sind in dieser Betrachtung ausgeschlossen, da in beiden Ländern nur eine Hochschule für MINT-Fächer ansässig ist und das die Vergleichbarkeit verzerren könnte."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_3")

                                    )
                           ),
# tab 4

                           tabPanel("Top 10 der Länder Frauen in MINT", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_top10_mint_gender_ui("international_top10_mint_gender_1"),
                                      # br(),br()
                                      # ,
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_top10_mint_gender_1_1"),
                                      #   label = "Download (links)",
                                      #   icon = icon("download")),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_top10_mint_gender_1_2"),
                                      #   label = "Download (rechts)",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_top10_mint_gender_1")),
                                                                   color = "#154194"),

                                     shinyBS::bsPopover(id="h_international_vergl_4", title="",
                                                         content = paste0("In den Europa-Daten wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung).", "<br><br>Liechtenstein und San Marino sind in dieser Betrachtung ausgeschlossen, da in beiden Ländern nur eine Hochschule für MINT-Fächer ansässig ist und das die Vergleichbarkeit verzerren könnte."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_4") )
                           ),
# tab 5
                           tabPanel("Vergleich internationale Studierende", br(),
                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_top10_mint_int_ui("mod_international_top10_mint_int_ui_1"),
                                    # br(),br()
                                    # ,
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_international_mint_top_10_1_1"),
                                    #   label = "Download (links)",
                                    #   icon = icon("download")),
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_international_mint_top_10_1_2"),
                                    #   label = "Download (rechts)",
                                    #   icon = icon("download"))
                                    ),

                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_mint_top_10_1")),
                                                                   color = "#154194"),
                                shinyBS::bsPopover(id="h_international_vergl_5", title="",
                                                         content = paste0("In den Europa-Daten wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung)."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_5")
                                    )
                            )
                      )
)
                )
             ),

    # Arbeitsmarkt International ----

    fluidRow(id="beruf_international",
             shinydashboard::box(
               title = "MINT-Beschäftigung im internationalen Vergleich: Hier können Sie den MINT-Anteil im deutschen Arbeitsmarkt international vergleichen.",
               width = 12,
               column(
                 width = 8,
               p("Diese Box zeigt eine Übersicht über MINT-Statistiken aus dem Bereich des Arbeitsmarkts für den internationalen Vergleich.
                 Die Grafiken basieren auf öffentlichen Statistiken, die durch die EU und die OECD gesammelt wurden.
                 Zum einen zeigen wir, wie groß der Anteil von MINT-Auszubildenden und Beschäftigten in verschiedenen Ländern ist.
                 Außerdem ist zu sehen, in welchen Ländern der Frauenanteil besonders groß oder klein ist.
                 Darüber hinaus werfen wir einen Blick auf Studiums- bzw. Ausbildungs-Anfänger:innen und Absolvent:innen in MINT
                 im Ländervergleich.")
               ),
               column(
                 width = 12,

               tabsetPanel(type = "tabs",
      # tab 1
                           tabPanel("Vergleich MINT-Anteil (Karte)", br(),
                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_map_arb_ui("mod_international_map_arb_ui_1"),
                                      # br(),br(),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_studienzahl_map_arb_1"),
                                      #   label = "Download",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_studienzahl_map_arb_1")),
                                                                   color = "#154194"),

                                      shinyBS::bsPopover(id = "h_beruf_international_1", title = "",
                                                         content = paste0("Ausgebildete umfassen alle Personen mit einem tertiären Bildungsabschluss im Bereich MINT, inkl. Studienabschlüssen.", "<br><br>Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise & Datenquellen&quot.", "<br><br>Da Eurostat den Begriff &quotBeschäftigte in MINT&quot sehr breit fasst (z. B. inkl. des Gesundheitswesens), betrachten wir hier nur die kleinere Gruppe an Naturwissenschaftler:innen und Ingenieur:innen.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_1")
                                    )
                           ),
                  # tab 2
                           tabPanel("Vergleich Frauen in MINT (Karte)", br(),
                                    shiny::sidebarPanel(
                                      width = 3,
                                      mod_international_map_arb_gender_ui("mod_international_map_arb_gender_ui_1"),
                                      # br(),br(),
                                      # downloadButton(
                                      #   outputId = ns("download_btn_plot_international_map_arb_gender_1"),
                                      #   label = "Download",
                                      #   icon = icon("download")),
                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_map_arb_gender_1")),
                                                                   color = "#154194"),

                                      shinyBS::bsPopover(id = "h_beruf_international_2", title = "",
                                                         content = paste0("Ausgebildete umfassen alle Personen mit einem tertiären Bildungsabschluss im Bereich MINT, inkl. Studienabschlüssen.", "<br><br>Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise und Datenquellen&quot.", "<br><br>Da Eurostat den Begriff &quotBeschäftigte in MINT&quot sehr breit fasst (z. B. inkl. des Gesundheitswesens), betrachten wir hier nur die kleinere Gruppe an Naturwissenschaftler*innen und Ingenieur*innen.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_2")
                                    )

                       ),
      # Tab 3
      tabPanel("Top 10 der MINT-Länder", br(),


                                      shiny::sidebarPanel(
                                        width = 3,
                                        #p("LOREM"),
                                        mod_international_top10_mint_arb_ui("mod_international_top10_mint_arb_ui_1"),
                                        # br(),br()
                                        # ,
                                        # downloadButton(
                                        #   outputId = ns("download_btn_plot_international_top10_mint_arb_1_1"),
                                        #   label = "Download (links)",
                                        #   icon = icon("download")),
                                        # downloadButton(
                                        #   outputId = ns("download_btn_plot_international_top10_mint_arb_1_2"),
                                        #   label = "Download (rechts)",
                                        #   icon = icon("download"))
                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(htmlOutput(ns("plot_international_top10_mint_arb_1")),
                                                                     color = "#154194"),


                                        shinyBS::bsPopover(id = "h_beruf_international_3", title = "",
                                                           content = paste0("Ausgebildete umfassen alle Personen mit einem tertiären Bildungsabschluss im Bereich MINT, inkl. Studienabschlüssen.", "<br><br>Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise und Datenquellen&quot.", "<br><br>Da Eurostat den Begriff &quotBeschäftigte in MINT&quot sehr breit fasst (z. B. inkl. des Gesundheitswesens), betrachten wir hier nur die kleinere Gruppe an Naturwissenschaftler*innen und Ingenieur*innen.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet"),
                                                           placement = "top",
                                                           trigger = "hover"),
                                        tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_3")
                                      )
                           ),tabPanel("Top 10 der Länder Frauen in MINT", br(),


                                      shiny::sidebarPanel(
                                        width = 3,
                                        #p("LOREM"),
                                        mod_international_top10_mint_arb_gender_ui("mod_international_top10_mint_arb_gender_ui_1"),
                                        # br(),br()
                                        # ,
                                        # downloadButton(
                                        #   outputId = ns("download_btn_plot_international_top10_mint_arb_gender_1_1"),
                                        #   label = "Download (links)",
                                        #   icon = icon("download")),
                                        # downloadButton(
                                        #   outputId = ns("download_btn_plot_international_top10_mint_arb_gender_1_2"),
                                        #   label = "Download (rechts)",
                                        #   icon = icon("download"))

                                        ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(htmlOutput(ns("plot_international_top10_mint_arb_gender_1")),
                                                                     color = "#154194"),


                                        shinyBS::bsPopover(id = "h_beruf_international_4", title = "",
                                                           content = paste0("Ausgebildete umfassen alle Personen mit einem tertiären Bildungsabschluss im Bereich MINT, inkl. Studienabschlüssen.", "<br><br>Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise und Datenquellen&quot.", "<br><br>Da Eurostat den Begriff &quotBeschäftigte in MINT&quot sehr breit fasst (z. B. inkl. des Gesundheitswesens), betrachten wir hier nur die kleinere Gruppe an Naturwissenschaftler*innen und Ingenieur*innen.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet"),
                                                           placement = "top",
                                                           trigger = "hover"),
                                        tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_4")
                                      )
                           ),tabPanel("MINT-Anfänger:innen und -Absolvent:innen", br(),


                                      shiny::sidebarPanel(
                                        width = 3,
                                        #p("LOREM"),
                                        mod_international_arbeitsmarkt_vergleich_ui("international_arbeitsmarkt_vergleich_1"),
                                        # br(),br()
                                        # ,
                                        # downloadButton(
                                        #   outputId = ns("download_btn_plot_international_arbeitsmarkt_vergleiche_1"),
                                        #   label = "Download",
                                        #   icon = icon("download")),
                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(htmlOutput(ns("plot_international_arbeitsmarkt_vergleiche_1")),
                                                                     color = "#154194"),

                                        shinyBS::bsPopover(id = "h_beruf_international_ho", title = "",
                                                           content = paste0("Aufgrund unterschiedlicher Definitionen von &quotMINT&quot zwischen den datengebenden Organisationen können die Zahlen voneinander abweichen. Näheres dazu unter &quotHinweise und Datenquellen&quot.", "<br><br>In den OECD-Daten ist für die Zuordnung zu &quotMINT&quot ausschließlich eine Ausbildung oder ein Studium in MINT entscheidend. Der tatsächlich ausgeübte Beruf wird nicht betrachtet"),
                                                           placement = "top",
                                                           trigger = "hover"),
                                        tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_international_ho")

                                      )
                           )
               )
               )
             )
    ),

    # NOCH NICHT FERTIG - INTERNATIONALE TABELLE ----
    # fluidRow(
    #   id="international_table_box",
    #   shinydashboard::box(
    #     title = "INTERNATIONAL - TABLLE",
    #     width = 12,
    #     p("LOREM IPSUM INFO"),
    #     tabsetPanel(
    #       type = "tabs",
    #       tabPanel(
    #         title = "Tabelle", br(),
    #
    #         shiny::sidebarPanel(
    #           width = 12,
    #           mod_international_table_input_ui("international_table_input_1"),
    #         ),
    #         shiny::mainPanel(
    #           width = 12,
    #           DT::dataTableOutput(outputId = ns("international_table_1")),
    #           br(),
    #           downloadButton(
    #             outputId = ns("download_btn_png_international_table_1"),
    #             label = "Download Tabelle (png)",
    #             icon = icon("download")),
    #           downloadButton(
    #             outputId = ns("download_btn_csv_international_table_1"),
    #             label = "Download Daten (csv)",
    #             icon = icon("download")),
    #           # quellen sind schon in der Tabelle enthalten
    #           # p(style="font-size:12px;color:grey",
    #           #   "hier Quellen"),
    #           # shinyBS::bsPopover(
    #           #   id="h_fachkraft_arbeitsmarkt_1", title="",
    #           #   content = paste0("POPUP INFO TEXT HERE"),
    #           #   placement = "top",
    #           #   trigger = "hover"),
    #           # tags$a(paste0("Hinweis zu den Daten"),
    #           #        icon("info-circle"),
    #           #        id = "h_fachkraft_arbeitsmarkt_1")
    #         )
    #       )
    #     )
    #   )
    # ),


    #Footer
    funct_footer()

  )
}


#' schule_kurse Server Functions
#'
#' @noRd
mod_international_start_server <- function(id, r){

  moduleServer( id, function(input, output, session){


    # Box 1 - Schule international ----

    # tab 1

    output$plot_international_schule_map_1 <- renderUI({
      plot_list <- plot_international_schule_map(r)


      plot_list
    })


    # tab 2

    output$plot_international_schule_item_1 <- renderUI({
      plot_list <- plot_international_schule_item(r)
      r$plot_international_schule_item_1 <- plot_list

      r$plot_international_schule_item_1_title <- get_plot_title(
        plot = r$plot_international_schule_item_1
      )

      plot_list
    })



    # tab 3

    output$plot_international_schule_migration_1 <- renderUI({
      plot_list <- plot_international_schule_migration(r)


      plot_list
    })




    # Box 2 - Studium international ----
    # tab 1

    output$plot_international_studienzahl_map_1 <- renderUI({
      plot_list <- plot_international_map(r)


      plot_list
    })




    # tab 2


    output$plot_international_map_fem_1 <- renderUI({
      plot_list <- plot_international_map_fem(r)


      plot_list
    })




    # tab 3

    output$plot_international_top10_mint_1 <- renderUI({
      plot_list <- plot_international_top10(r)
      r$plot_international_top10_mint_1_left <- plot_list[[1]]
      r$plot_international_top10_mint_1_right <- plot_list[[2]]

      r$plot_international_top10_min_1_left_title <- get_plot_title(
        plot = r$plot_international_top10_mint_1_left
      )
      r$plot_international_top10_mint_1_right_title <- get_plot_title(
        plot = r$plot_international_top10_mint_1_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_international_top10_mint_1_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_top10_mint_1_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_international_top10_mint_1_left,
          filename =  r$plot_international_top10_mint_1_left_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_top10_mint_1_left_title, file)
        file.remove(r$plot_international_top10_mint_1_left_title)
      }
    )

    output$download_btn_plot_international_top10_mint_1_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_top10_mint_1_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_international_top10_mint_1_right,
          filename =  r$plot_international_top10_mint_1_right_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_top10_mint_1_right_title, file)
        file.remove(r$plot_international_top10_mint_1_right_title)
      }
    )



    # tab 4

    output$plot_international_top10_mint_gender_1  <- renderUI({
      plot_list <- plot_international_top10_gender(r)
      r$plot_international_top10_mint_gender_1_left <- plot_list[[1]]
      r$plot_international_top10_mint_gender_1_right <- plot_list[[2]]

      r$plot_international_top10_mint_gender_1_left_title <- get_plot_title(
        plot = r$plot_international_top10_mint_gender_1_left
      )
      r$plot_international_top10_mint_gender_1_right_title <- get_plot_title(
        plot = r$plot_international_top10_mint_gender_1_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_international_top10_mint_gender_1_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_top10_mint_gender_1_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_international_top10_mint_gender_1_left,
          filename =  r$plot_international_top10_mint_gender_1_left_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_top10_mint_gender_1_left_title, file)
        file.remove(r$plot_international_top10_mint_gender_1_left_title)
      }
    )

    output$download_btn_plot_international_top10_mint_gender_1_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_top10_mint_gender_1_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_international_top10_mint_gender_1_right,
          filename =  r$plot_international_top10_mint_gender_1_right_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_top10_mint_gender_1_right_title, file)
        file.remove(r$plot_international_top10_mint_gender_1_right_title)
      }
    )




    # tab 5

    output$plot_international_mint_top_10_1  <- renderUI({
      plot_list <- plot_international_mint_top_10(r)
      r$plot_international_mint_top_10_1_left <- plot_list[[1]]
      r$plot_international_mint_top_10_1_right <- plot_list[[2]]

      r$plot_international_mint_top_10_1_left_title <- get_plot_title(
        plot = r$plot_international_mint_top_10_1_left
      )
      r$plot_international_mint_top_10_1_right_title <- get_plot_title(
        plot = r$plot_international_mint_top_10_1_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_international_mint_top_10_1_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_mint_top_10_1_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_international_mint_top_10_1_left,
          filename =  r$plot_international_mint_top_10_1_left_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_mint_top_10_1_left_title, file)
        file.remove(r$plot_international_mint_top_10_1_left_title)
      }
    )

    output$download_btn_plot_international_mint_top_10_1_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_mint_top_10_1_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_international_mint_top_10_1_right,
          filename =  r$plot_international_mint_top_10_1_right_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_mint_top_10_1_right_title, file)
        file.remove(r$plot_international_mint_top_10_1_right_title)
      }
    )




    # Box 3 - Arbeitsmarkt internatnional ----

    # Tab 1

    output$plot_international_studienzahl_map_arb_1 <- renderUI({
      plot_list <- plot_international_map_arb(r)


      plot_list
    })




    # tab 2

    output$plot_international_map_arb_gender_1 <- renderUI({
      plot_list <- plot_international_map_arb_gender(r)


      plot_list
    })



    # tab 3

    output$plot_international_top10_mint_arb_1  <- renderUI({
      plot_list <- plot_international_top10_mint_arb(r)
      r$plot_international_top10_mint_arb_1_left <- plot_list[[1]]
      r$plot_international_top10_mint_arb_1_right <- plot_list[[2]]

      r$plot_international_top10_mint_arb_1_left_title <- get_plot_title(
        plot = r$plot_international_top10_mint_arb_1_left
      )
      r$plot_international_top10_mint_arb_1_right_title <- get_plot_title(
        plot = r$plot_international_top10_mint_arb_1_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_international_top10_mint_arb_1_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_mint_top_10_1_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_international_mint_top_10_1_left,
          filename =  r$plot_international_mint_top_10_1_left_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_mint_top_10_1_left_title, file)
        file.remove(r$plot_international_mint_top_10_1_left_title)
      }
    )

    output$download_btn_plot_international_top10_mint_arb_1_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_mint_top_10_1_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_international_mint_top_10_1_right,
          filename =  r$plot_international_mint_top_10_1_right_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_mint_top_10_1_right_title, file)
        file.remove(r$plot_international_mint_top_10_1_right_title)
      }
    )

    # tab 4

    output$plot_international_top10_mint_arb_gender_1 <- renderUI({
      plot_list <- plot_international_top10_mint_arb_gender(r)
      r$plot_international_top10_mint_arb_gender_left <- plot_list[[1]]
      r$plot_international_top10_mint_arb_gender_right <- plot_list[[2]]

      r$plot_international_top10_mint_arb_gender_left_title <- get_plot_title(
        plot = r$plot_international_top10_mint_arb_gender_left
      )
      r$plot_international_top10_mint_arb_gender_right_title <- get_plot_title(
        plot = r$plot_international_top10_mint_arb_gender_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_international_top10_mint_arb_gender_1_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_top10_mint_arb_gender_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_international_top10_mint_arb_gender_left,
          filename =  r$plot_international_top10_mint_arb_gender_left_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_top10_mint_arb_gender_left_title, file)
        file.remove(r$plot_international_top10_mint_arb_gender_left_title)
      }
    )

    output$download_btn_plot_international_top10_mint_arb_gender_1_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_top10_mint_arb_gender_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_international_top10_mint_arb_gender_right,
          filename =  r$plot_international_top10_mint_arb_gender_right_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_top10_mint_arb_gender_right_title, file)
        file.remove(r$plot_international_top10_mint_arb_gender_right_title)
      }
    )

    # tab 5
    output$plot_international_arbeitsmarkt_vergleiche_1 <- renderUI({
      plot <- plot_international_arbeitsmarkt_vergleiche(r)

      r$plot_international_arbeitsmarkt_vergleiche_1 <- plot
      r$plot_international_arbeitsmarkt_vergleiche_1_title <- get_plot_title(
        plot = r$plot_international_arbeitsmarkt_vergleiche_1
      )

      plot
    })

    output$download_btn_plot_international_arbeitsmarkt_vergleiche_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_international_arbeitsmarkt_vergleiche_1_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_international_arbeitsmarkt_vergleiche_1,
          filename =  r$plot_international_arbeitsmarkt_vergleiche_1_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_international_arbeitsmarkt_vergleiche_1_title, file)
        file.remove(r$plot_international_arbeitsmarkt_vergleiche_1_title)
      }
    )

   # Box 4 - International Table ----
    #
    # output$international_table_1 <- DT::renderDataTable({
    #   r$int_table_DT <- DT::datatable(
    #     data = r$int_table,
    #     # filter = list(position = "top"),
    #     rownames = FALSE,
    #     colnames = stringr::str_to_title(names(r$int_table)),
    #     escape = FALSE,
    #     options = list(
    #       dom = "t"),
    #     # add logo and source
    #     caption = htmltools::tags$caption(
    #       style = 'caption-side: bottom; text-align: right;',
    #       htmltools::div(
    #         style = "display: flex; justify-content: space-between;",
    #         htmltools::p(paste0("Quellen: ", r$int_table_source)),
    #         htmltools::img(
    #           src="https://raw.githubusercontent.com/mint-vernetzt/datalab/main/inst/app/www/MINTvernetztLogo_klein.png",
    #           alt="MINT vernetzt Logo",
    #           width="30",height="30",
    #           style = "align-self: center;"
    #         )
    #       )
    #     )
    #   )
    #
    #   r$int_table_DT
    # })
    #
    # output$download_btn_png_international_table_1 <- downloadHandler(
    #   contentType = "text/csv",
    #   filename = function() {"International_data_custom_table.png"},
    #   content = function(file) {
    #     logger::log_info("Donwload png custom table with international data")
    #     download_table(table = r$int_table_DT,
    #                    filename = "International_data_custom_table.png",
    #                    width = 1000,
    #                    height = 300)
    #
    #     file.copy("International_data_custom_table.png", file)
    #     file.remove("International_data_custom_table.png")
    #   }
    # )
    #
    # output$download_btn_csv_international_table_1 <- downloadHandler(
    #   contentType = "text/csv",
    #   filename = function() {"International_data_custom_table.csv"},
    #   content = function(file) {
    #     logger::log_info("Donwload csv custom table with international data")
    #
    #     write.csv2(x = prep_download_data(r$int_table_csv),
    #                file = file,
    #                row.names = FALSE)
    #
    #   }
    # )


  })
}

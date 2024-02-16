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
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_alle.jpg',
            class = "img-responsive",
            #height = "300px",
            #width = "150px",
            alt = "Banner MINT entlang der Bildungskette",
            style="display: block; margin-left: auto; margin-right: auto;"
        )
      )),

    # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Auf dieser Fokus-Seite Blicken wir über Deutschland hinaus. Hier können Sie in den Bereichen 'Schule',
          'Studium' und 'Beruf' internationale Vergleiche anstellen. Betrachten Sie zum Beispiel, in welchen Ländern
          der MINT-Anteil besonders hoch ist oder wo ein verhältnismäßig hoher Frauenanteil in MINT erreicht wird.
          Wenn Sie sich für den MINT-Nachwuchs interessieren, können Sie die Ergebnisse der internationalen MINT-Kompetenzerhebungen Pisa und TIMSS betrachten.")),
      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich?", br(),
          "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        title = "Übersicht Fragestellungen",
        width = 7,
        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_international",
                                                              span(tags$b(span("MINT-Kompetenzen im internationalen Vergleich:")))),
          "Wie schneidet Deutschland im internationalen Vergleich in den MINT-Kompetenztests Pisa und TIMSS ab?"
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
          "Schüler:innen-Kompetenz-Daten: OECD 2023, freier Download (Pisa); IEA 2023, freier Download (TIMSS)."),
        p("Vergleichszahlen europaweit: Eurostat 2023, freier Download, eigene Berechnungen durch MINTvernetzt."),
        p("Vergleichszahlen der OECD-Staaten: OECD 2023, freier Download, eigene Berechnungen durch MINTvernetzt."),
        p("Vergleichszahlen weltweit: UNESCO 2023, freier Download, eigene Berechnungen durch MINTvernetzt.")

      )

    ),

    # Schule international ----
    fluidRow(id="schule_international",
             shinydashboard::box(
               title = "MINT-Kompetenzen im internationalen Vergleich: Wie schneidet Deutschland im internationalen Vergleich in den MINT-Kompetenztests Pisa und TIMSS ab?",
               width = 12,
               p("In dieser Box blicken wir auf die Kompetenzdaten der PISA-Studie und der TIMSS-Erhebung.
               Bei beiden Erhebungen werden unter anderem Kompetenztests in Mathematik und Naturwissenschaften in 4. und 9. Klassen weltweit durchgeführt.
               Außerdem betrachten die Grafiken Kompetenz-Unterschiede zwischen Jungen und Mädchen und Unterschiede in Abhängigkeit der sozialen Herkunft der Kinder."),
               p(),
               p(),
               shinyBS::bsPopover(id="i_schule_international_1", title = "",
                                  content = paste0("Eine Einordnung sowie weitere Ergebnisse der TIMSS-Erhebung finden Sie hier: <br> <a>https://timss2019.org/reports/</a> <br><br> Weitere Informationen zur PISA-Studie und ihren Ergebnissen finden Sie hier:. <br> <a> https://www.oecd.org/pisa/ </a>"),
                                  placement = "right",
                                  trigger = "click"),
               tags$a(paste0("Um zu den original Ergebnisberichten von TIMSS und PISA zu kommen, klicken Sie hier:"), icon("info-circle"), id = "i_schule_international_1"),
               p(),
               p(),
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
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_schule_map_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: IEA, 2023; OECD, 2023, feier Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_schule_1", title="",
                                                         content = paste0("Regionen, die nicht als unabhängige Staaten anerkannt werden (z. B. Taiwan, Hongkong) können aus technischen Gründen nicht in den Karten dargestellt werden. Daten dieser Regionen sind in den weiteren Grafiken enthalten."),
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
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_schule_item_1")),
                                                                   color = "#154194"),

                                      br(),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: IEA, 2023; OECD, 2023, feier Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_schule_2", title="",
                                                         content = paste0("Test-Leistungen der Schüler:innen werden nur dann als unterschiedlich dargestellt, wenn das mittlere Ergebnis der Mädchen im Vergelich zu den Jungen signifikant unterschiedlich ist.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
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
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_schule_migration_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: IEA, 2023; OECD, 2023, feier Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_schule_3", title="",
                                                         content = paste0("Das Bildungskapital wurde über die Anzahl der Bücher im Zuhause der Kinder operationalisiert. Zur einfacheren Darstellung wird in dieser Abbildung nur eine Auswahl der vorliegenden Untergruppen betrachtet."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_3")
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
               p("Diese Box zeigt eine Übersicht von MINT-Statistiken aus dem Bereich Studium für den internationalen Vergleich.
               Die Grafiken basieren auf öffentlichen Statistiken, die durch die EU, die OECD oder die UNESCO gesammelt wurden.
                 Zum einen zeigen wir, wie groß der Anteil von MINT-Studierenden an allen Studierenden in verschiedenen Ländern ist. Außerdem ist zu sehen,
                 in welchen Ländern der Frauenanteil oder der Anteil an internationalen Studierenden in MINT-Studiengängen besonders groß oder klein ist."),
               tabsetPanel(type = "tabs",
                           tabPanel("Vergleich MINT-Anteil (Karte)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_map_ui("mod_international_map_ui_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_studienzahl_map_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",

                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_vergl_1", title="",
                                                         content = paste0("In den Europa-Daten sowie den &quotweltweit&quot-Daten der UNESCO wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung)."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_1") )
                           ),

                           tabPanel("Vergleich Frauen in MINT (Karte)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_map_fem_ui("international_map_fem_ui_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_map_fem_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; feier Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_vergl_2", title="",
                                                         content = paste0("In den Europa-Daten wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung)."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_2") )

                           ),

                           tabPanel("Top 10 MINT-Länder", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_top10_mint_ui("international_top10_mint_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_top10_mint_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",

                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_vergl_3", title="",
                                                         content = paste0("In den Europa-Daten wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung).", "<br><br>Lichtenstein und San Marino sind in dieser Betrachtung ausgeschlossen, da in beiden Ländern nur eine Hochschule für MINT-Fächer ansässig ist und das die Vergleichbarkeit verzerren könnte."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_3")

                                    )
                           ),

                           tabPanel("Top 10 Länder Frauen in MINT", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_top10_mint_gender_ui("international_top10_mint_gender_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_top10_mint_gender_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",

                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; freies Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_vergl_4", title="",
                                                         content = paste0("In den Europa-Daten wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung).", "<br><br>Lichtenstein und San Marino sind in dieser Betrachtung ausgeschlossen, da in beiden Ländern nur eine Hochschule für MINT-Fächer ansässig ist und das die Vergleichbarkeit verzerren könnte."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_4") )
                           ),
                           tabPanel("Vergleich internationale Studierende", br(),
                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      #p("LOREM"),
                                      mod_international_top10_mint_intl_ui("mod_international_top10_mint_intl_ui_1")),


                                    shiny::mainPanel(
                                      width = 9,
                                      shinycssloaders::withSpinner(htmlOutput(ns("plot_international_mint_top_10_1")),
                                                                   color = "#154194"),

                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; freies Download, eigene Berechnungen durch MINTvernetzt."),


                                      shinyBS::bsPopover(id="h_international_vergl_5", title="",
                                                         content = paste0("In den Europa-Daten wird der ganze tertiäre Bildungsbereich betrachtet, also nicht nur die akademische Bildung (Bachelor, Master, Promotion), sondern auch vertiefende berufsorientierte Bildung (z. B. eine Technikerausbildung)."),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_vergl_5")
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
    output$plot_international_schule_map_1 <- renderUI({
      plot_international_schule_map(r)
    })

    output$plot_international_schule_item_1 <- renderUI({
      plot_international_schule_item(r)
    })

    output$plot_international_schule_migration_1 <- renderUI({
      plot_international_schule_migration(r)
    })


    # Box 2 - Studium international ----
    output$plot_international_studienzahl_map_1 <- renderUI({
      plot_international_map(r)
    })

    output$plot_international_top10_mint_1 <- renderUI({
      plot_international_top10(r)
    })

    output$plot_international_top10_mint_gender_1 <- renderUI({
      plot_international_top10_gender(r)
    })

    output$plot_international_map_fem_1 <- renderUI({
      plot_international_map_fem(r)
    })

    output$plot_international_mint_top_10_1 <- renderUI({
      plot_international_mint_top_10(r)
    })


    # Box 3 - Arbeitsmarkt internatnional ----
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
    output$plot_international_arbeitsmarkt_vergleiche_1 <- renderUI({
      plot_international_arbeitsmarkt_vergleiche(r)
    })


  })
}

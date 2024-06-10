#' international_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_start_ui <- function(id){

  #logger::log_debug("start mod_fachkraft_start_ui")

  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Fachkraefte.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Fokus: MINT-Fachkraefte",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),


    # Info-Texte ----

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Das ist eine unserer Fokus-Seiten. Hier schauen wir auf das Thema MINT-Fachkräfte.
          Basierend auf amtlichen Statistiken und Analysen der Bundesagentur für Arbeit zeigen wir,
          wie es aktuell um Fachkräfte in MINT steht."),
        p(),
        p(style = "text-align: left; font-size = 16px",
          "Außerdem haben wir selbst eine Analyse in Auftrag gegeben,
          mit der wir auf die Zukunft der MINT-Fachkräfte blicken.
          Wir haben Zukunftsszenarien der MINT-Fachkräfteentwicklung berechnen lassen und schauen darauf,
          wie verschiedene Wirkhebel die MINT-Fachkräftezahlen der Zukunft bewegen können."),
        p(),
        p(style = "text-align: left; font-size = 16px",
          "Wir geben hier erste Einblicke in die verschiedenen
          MINT-Fachkräfte-Daten und arbeiten gerade an weiteren Darstellungen.")
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
        p(
          style = "text-align: left; font-size = 16px",tags$a(href="#fachkraft-prognose",
                                                              span(tags$b(span("Fachkräfte-Prognosetool:")))), "Zukunftsszenarien der MINT-Fachkräfteentwicklung"
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#fachkraft-berufsgruppen",
                                                              span(tags$b(span("Berufsgruppen:")))), "Aktueller Fachkräftebedarf in MINT"

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#fachkraft-berufe",
                                                              span(tags$b(span("Berufsebene:")))), "Aktueller Fachkräftebedarf in MINT"
        )
        #,
        # p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_international",
        #                                                       span(tags$b(span("Internationale Studierende in MINT:")))), "Wie hoch ist der Anteil von internationalen Studierenden in den MINT-Fächern?"
        #
        # )
      ),

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Zukunftsszenarien der MINT-Fachkräfteentwicklung: Datengrundlage
          Bundesagentur für Arbeit, 2023; Vorausberechnung druch IW Köln, 2024,
          beauftragt durch MINTvernetzt"),
        p(style = "text-align: left; font-size = 16px",
          "MINT-Fachkräftedaten (Engpassanalyse, Vakanzzeit, Arbeitslosen-Stellen-Relation):
          Bundesagentur für Arbeit 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt")

      )
    ),


    # Box 1 - MINT-Fachkräfte-Prognose ----

    fluidRow(
      id = "fachkraft-prognose",
      shinydashboard::box(
        title = "Fachkräfte-Prognosetool: Zukunftsszenarien der MINT-Fachkräfte",
        width = 12,
        p("Hier können Sie in die Zukunft blicken. Wir haben das Wirtschaftsinstitut IW Köln beauftragt,
          die Entwicklung der MINT-Fachkräfte in den nächsten Jahren zu berechnen. Wir wollen dabei nicht schauen,
          wie viele Personen uns fehlen werden, sondern darauf, wie viele Personen für MINT gewonnen werden könnten.

          Dabei blicken wir auf die Wirkhebel MINT-Bildung fördern, Mädchen in MINT fördern,
          internationale MINT-Fachkräfte integrieren und ältere MINT-Fachkräfte halten.

          Vergleichen Sie hier, wie sich diese Wirkhebel die Fachkräfteentwicklung beeinflussen."),
        br(),
        tabsetPanel(
          type = "tabs",
    tabPanel(
            title = "Zukünftige Fachkräfte-Entwicklung", br(),

            shiny::sidebarPanel(
              width = 3,
              mod_fachkraft_item_prog_ui("fachkraft_item_prog_1"),
              br(),

              # br(),
              # downloadButton(
              #   outputId = ns("download_btn_plot_fachkraft_prog_item_1"),
              #   br(),
              #   label = "Download",
              #   icon = icon("download")),

            ),
            shiny::mainPanel(
              width = 9,
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_fachkraft_prog_item_1"),
                                                                        height = "600px"),
                                                    color = "#154194"),
              p(style="font-size:12px;color:grey",
                "Vorausberechnung durch IW Köln, 2024, beauftragt durch MINTvernetzt"),
              p(),
              tags$a(href = "www/MINT-Fachkraeftezukunftsszenarien_Methodenbericht.pdf", target = "_blank", "Methodenbericht des IW Köln als PDF")
            )
          ),
    tabPanel(

      title = "Im Vergleich: Zukünftige Fachkräfte-Entwicklung", br(),

      shiny::sidebarPanel(
        width = 3,
        mod_fachkraft_item_prog_alle_ui("fachkraft_item_prog_alle_1"),
        br(),
        # br(),
        # downloadButton(
        #   outputId = ns("download_btn_plot_fachkraft_prog_item_1"),
        #   br(),
        #   label = "Download",
        #   icon = icon("download")),
      ),
      shiny::mainPanel(
        width = 9,
        shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_fachkraft_prog_alle_1"),
                                                                  height = "600px"),
                                     color = "#154194"),
        p(style="font-size:12px;color:grey",
          "Vorausberechnung durch IW Köln, 2024, beauftragt durch MINTvernetzt"),
        p(),
        tags$a(href = "www/MINT-Fachkraeftezukunftsszenarien_Methodenbericht.pdf", target = "_blank", "Methodenbericht des IW Köln als PDF")
      )
    ),
    tabPanel(
            "Im Detail: Zukünftige Fachkräfte-Entwicklung", br(),

            shiny::sidebarPanel(
              width = 3,
              mod_fachkraft_item_prog_detail_ui("fachkraft_item_prog_detail_1")
              ,
              br(),

            ),
            shiny::mainPanel(
              width = 9,
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_fachkraft_prog_detail_item_1"),
                                                                        height = "600px"),
                                            color = "#154194"),

              p(style="font-size:12px;color:grey",
                "Vorausberechnung durch IW Köln, 2024, beauftragt durch MINTvernetzt"),
              # shinyBS::bsPopover(id="h_fachkraft_prog_2", title="",
              #                    content = paste0("POPUP INFO TEXT HERE"),
              #                    placement = "top",
              #                    trigger = "hover"),
              # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_prog_2"),
              # p(),
              p(),
              tags$a(href = "www/MINT-Fachkraeftezukunftsszenarien_Methodenbericht.pdf", target = "_blank", "Methodenbericht des IW Köln als PDF")
            )
          )
    ,


    tabPanel(
            "Übersicht Wirkhebel", br(),

            shiny::sidebarPanel(
              width = 3,

              mod_fachkraft_wirkhebel_analyse_ui("fachkraft_item_wirkhebel_analyse_1"),
              br(),# TODO

              # br(),
              # downloadButton(
              #   outputId = ns("download_btn_plot_fachkraft_prog_wirkhebel_analyse_1"),
              #   label = "Download",
              #   icon = icon("download"))

            ),
            shiny::mainPanel(
              width = 9,
              shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot_fachkraft_wirkhebel_analyse_1")),
              color = "#154194"),

              p(style="font-size:12px;color:grey",
              "Vorausberechnung druch IW Köln, 2024, beauftragt durch MINTvernetzt"),
            p(),
            shinyBS::bsPopover(
              id="h_fachkraft-prognosen_3", title="",
              content = paste0("Der Effekt der Förderung von Frauen baut auf den Effekt der MINT-Bildung auf. Das Szenario betrachtet eine erfolgreichere MINT-Bildung mit besonderen Erfolgen bei Mädchen und Frauen."),
              placement = "top",
              trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"),
                   icon("info-circle"),
                   id = "h_fachkraft-prognosen_3"),
            p(),

            tags$a(href = "www/MINT-Fachkraeftezukunftsszenarien_Methodenbericht.pdf", target = "_blank", "Methodenbericht des IW Köln als PDF")
    )
        )))),





  # Box 2 - Fachkräfte auf Berufsgruppen-Level ----
      fluidRow(
        id="fachkraft-berufsgruppen",
        shinydashboard::box(
          title = "Berufsgruppen: Aktueller Fachkräftebedarf in MINT",
          width = 12,
          p("In dieser Box blicken wir auf den Fachkräftebedarf im Bereich MINT.
          Dabei bleiben wir zunächst auf der Ebene der Berufsbereiche MINT bzw. der MINT-Disziplinen
          und Vergleichen diese Bereiche mit \"Nicht-MINT\" bzw. allen Berufen."),
          p("Die hier betrachteten Indikatoren des Fachkräftebedarfs sind die Ergebnisse der
            Engpass-Analyse der Bundesagentur für Arbeit, der Arbeitslosen-Stellen-Relation und der Vakanzzeit.
            Was diese Indikatoren bedeuten, erklären wir in den Infoboxen, die sich bei Klicken auf
            die folgenden Wörter öffnen."),
          br(),
          column(width = 2,
                 shinyBS::bsPopover(id="i_engpass_analyse_def", title = "",

                                    placement = "right",
                                    trigger = "click",
                                    options = list(container = "body",
                                                   content = paste0("Die Bundesagentur für Arbeit hat basierend auf sechs Indikatoren für alle Berufe einen Engpassindikator berechnet. <br><br>Indikatoren: Abgangsrate aus der Arbeitslosigkeit, Arbeitssuchenden-Stellen-Relation, Berufssp. Arbeitslosenquote, Entwicklung der mittleren Entgelte, Vakanzzeit, Veränderung in der Beschäftigung ausländischre Personen. <br><br> Näheres zur Berechnung können Sie hier nachlesen: <br> <a>https://statistik.arbeitsagentur.de/DE/Navigation/Statistiken/Interaktive-Statistiken/Fachkraeftebedarf/Engpassanalyse-Nav.html</a> "),
                                                   delay = list(show = 100, hide = 100),
                                                   template = '<div class="popover" role="tooltip" style="max-width: 100%;"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>')),
                 tags$a(paste0("Engpassanalyse"), icon("info-circle"), id = "i_engpass_analyse_def"),
                 p()
          ),
          column(width = 3,
                 shinyBS::bsPopover(id="i_asr_def", title = "",
                                    content = paste0("Die Arbeitslosen-Stellen-Relation berechnet sich aus folgendem Verhältnis: Jahresdurchschnitt Arbeitslose / Jahresdurchschnitt gemeldete sozialversicherungspflichtige Arbeitsstellen. <br> <br> Eine Arbeitslosen-Stellen-Relation von 3 bedeutet, dass in einem Beruf drei arbeitslose/arbeitssuchende Bewerber auf eine gemeldete Arbeitsstelle kommen. <br><br>Quelle: Bundesagentur für Arbeit"),
                                    placement = "right",
                                    trigger = "click"),
                 tags$a(paste0("   Arbeitslosen-Stellen-Relation"), icon("info-circle"), id = "i_asr_def"),
                 p()
          ),
          column(width = 2,
                 shinyBS::bsPopover(id="i_vakanzzeit_def", title = "",
                                    content = paste0("Wir betrachten hier die abgeschlossene Vakanzzeit. Als abgeschlossene Vakanzzeit versteht man die Dauer, die benötigt wird, bis eine leer stehende Stelle besetzt wir. <br><br>Eine Vakanzzeit von 50 bedeutet, dass eine gemeldete Arbeitsstelle 50 Tage zur Vermittlung gemeldet war, bis sie besetzt werden konnte. <br><br>Quelle: Bundesagentur für Arbeit."),
                                    placement = "right",
                                    trigger = "click"),
                 tags$a(paste0("   Vakanzzeit"), icon("info-circle"), id = "i_vakanzzeit_def"),
                 p()
          ),
          column(width = 12,
                 br(),
          tabsetPanel(

            type = "tabs",

            tabPanel(
            title = "Engpassrisiko im MINT-Bereich", br(),

            # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              mod_fachkraft_item_epa_ui("fachkraft_item_epa_1"),
              br(),
              # br(),
              #
              # downloadButton(
              #   outputId = ns("download_btn_plot_fachkraft_epa_item_1"),
              #   label = "Download (links)",
              #   icon = icon("download")),
              # downloadButton(
              #   outputId = ns("download_btn_plot_fachkraft_epa_item_2"),
              #   label = "Download (rechts)",
              #   icon = icon("download")),
            ),
            shiny::mainPanel(
              width = 9,
              shinycssloaders::withSpinner(htmlOutput(ns("plot_fachkraft_epa_item_1")),
                                           color = "#154194"),


              p(style="font-size:12px;color:grey",
                "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
              shinyBS::bsPopover(
                id="h_fachkraft-berufsgruppen_1", title="",
                content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. <br><br>Informationen zur Berechnung und Bedeutung des Engpassindikators finden Sie in der Infobox zur Engpassanalyse. Diese ist in der Beschreibung über der Grafik verlinkt."),
                placement = "top",
                trigger = "hover"),
              tags$a(paste0("Hinweis zu den Daten"),
                     icon("info-circle"),
                     id = "h_fachkraft-berufsgruppen_1")
            )
          ),
          tabPanel(
            "Verteilung MINT-Bereich nach Engpassrisiko", br(),

            # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              mod_fachkraft_item_mint_ui("fachkraft_item_mint_1")

            ),
            shiny::mainPanel(
              width = 9,
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_fachkraft_mint_item_1")),
                                           color = "#154194"),


              p(style="font-size:12px;color:grey",
                "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
              shinyBS::bsPopover(id="h_fachkraft-berufsgruppen_2", title="",
                                 content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. <br><br>Informationen zur Berechnung und Bedeutung des Engpassindikators finden Sie in der Infobox zur Engpassanalyse. Diese ist in der Beschreibung über der Grafik verlinkt."),
                                 placement = "top",
                                 trigger = "hover"),
              tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft-berufsgruppen_2")
            )
          ),

            tabPanel(
              "Arbeitslosen-Stellen-Relation und Vakanzzeit in MINT", br(),

              # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                mod_fachkraft_bar_vakanz_ui("fachkraft_bar_vakanz_1"),
                br(),

                # br(),
                # downloadButton(
                #   outputId = ns("download_btn_plot_fachkraft_bar_vakanz_1"),
                #   label = "Download",
                #   icon = icon("download"))

              ),
              shiny::mainPanel(
                width = 9,
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("fachkraft_bar_vakanz_1_plot")),
                                             color = "#154194"),

                p(style="font-size:12px;color:grey",
                  "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                shinyBS::bsPopover(id="h_fachkraft_arbeitsmarkt_4", title="",
                                   content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. <br><br>Informationen zur Berechnung und Bedeutung der Vakanzzeit und der Arbeitslosen-Stellen-Relation finden Sie in der jeweiligen Infobox. Diese sind in der Beschreibung über der Grafik verlinkt."),
                                   placement = "top",
                                   trigger = "hover"),
                tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_arbeitsmarkt_4")
              )
            ),
          )
          )
        )
      ),

    # Box 3 - Fachkräfte auf Berufslevel ----

    fluidRow(
      id = "fachkraft-berufe",
      shinydashboard::box(
        title = "Berufsebene: Aktueller Fachkräftebedarf in MINT",
        width = 12,

        p("In dieser Box blicken wir auf den Fachkräftebedarf in einzelnen Berufen.
          Die interaktiven Darstellungen ermöglichen es, den Fachkräftebedarf der einzelner Berufsgattungen,
          z. B. Mechatronik oder Gesundheits- und Krankenpflege, zu betrachten. Außerdem zeigen wir ein Ranking der
          MINT-Berufe, mit dem aktuell höchsten und geringsten Fachkräftebedarf."),
        p("Die hier betrachteten Indikatoren des Fachkräftebedarfs sind die Ergebnisse der
            Engpass-Analyse der Bundesagentur für Arbeit, der Arbeitslosen-Stellen-Relation und der Vakanzzeit.
            Was diese Indikatoren bedeuten, erklären wir in den Infoboxen, die sich bei Klicken auf
            die folgenden Wörter öffnen."),
        br(),
        column(width = 2,
        shinyBS::bsPopover(id="i_engpass_analyse_def_2", title = "",

                           placement = "right",
                           trigger = "click",
                           options = list(container = "body",
                                          content = paste0("Die Bundesagentur für Arbeit hat basierend auf sechs Indikatoren für alle Berufe einen Engpassindikator berechnet. <br><br>Indikatoren: Abgangsrate aus der Arbeitslosigkeit, Arbeitssuchenden-Stellen-Relation, Berufssp. Arbeitslosenquote, Entwicklung der mittleren Entgelte, Vakanzzeit, Veränderung in der Beschäftigung ausländischre Personen. <br><br> Näheres zur Berechnung können Sie hier nachlesen: <br> <a>https://statistik.arbeitsagentur.de/DE/Navigation/Statistiken/Interaktive-Statistiken/Fachkraeftebedarf/Engpassanalyse-Nav.html</a> "),
                                          delay = list(show = 100, hide = 100),
                                          template = '<div class="popover" role="tooltip" style="max-width: 100%;"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>')),
        tags$a(paste0("Engpassanalyse"), icon("info-circle"), id = "i_engpass_analyse_def_2"),
        p()
        ),
        column(width = 3,
        shinyBS::bsPopover(id="i_asr_def_2", title = "",
                           content = paste0("Die Arbeitslosen-Stellen-Relation berechnet sich aus folgendem Verhältnis: Jahresdurchschnitt Arbeitslose / Jahresdurchschnitt gemeldete sozialversicherungspflichtige Arbeitsstellen. <br> <br> Eine Arbeitslosen-Stellen-Relation von 3 bedeutet, dass in einem Beruf drei arbeitslose/arbeitssuchende Bewerber auf eine gemeldete Arbeitsstelle kommen. <br><br>Quelle: Bundesagentur für Arbeit"),
                           placement = "right",
                           trigger = "click"),
        tags$a(paste0("   Arbeitslosen-Stellen-Relation"), icon("info-circle"), id = "i_asr_def_2"),
        p()
        ),
        column(width = 2,
        shinyBS::bsPopover(id="i_vakanzzeit_def_2", title = "",
                           content = paste0("Wir betrachten hier die abgeschlossene Vakanzzeit. Als abgeschlossene Vakanzzeit versteht man die Dauer, die benötigt wird, bis eine leer stehende Stelle besetzt wir. <br><br>Eine Vakanzzeit von 50 bedeutet, dass eine gemeldete Arbeitsstelle 50 Tage zur Vermittlung gemeldet war, bis sie besetzt werden konnte. <br><br>Quelle: Bundesagentur für Arbeit."),
                                            placement = "right",
                                            trigger = "click"),
                           tags$a(paste0("   Vakanzzeit"), icon("info-circle"), id = "i_vakanzzeit_def_2"),
        p()
        ),
        # column(width = 5,
        #        p(" ")
        #        ),

        column(width = 12,
               br(),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Engpassanalyse für MINT-Berufe", br(),
            shiny::sidebarPanel(
              width = 3,
              mod_fachkraft_item_detail_ui("fachkraft_item_detail_1"),
              br(),
              # br(),
              # downloadButton(
              #   outputId = ns("download_btn_plot_fachkraft_item_detail_1"),
              #   label = "Download",
              #   icon = icon("download"))

              ),
            shiny::mainPanel(
              width = 9,
              shinycssloaders::withSpinner(htmlOutput(ns("plot_fachkraft_detail_item_1")),
                                           color = "#154194"),

              p(style="font-size:12px;color:grey",
                "Quelle der Daten: Bundesagentur für Arbeit, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

              shinyBS::bsPopover(id="h_fachkraft_arbeitsmarkt_3", title="",
                                 content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. <br><br>Informationen zur Berechnung und Bedeutung des Engpassindikators finden Sie in der Infobox zur Engpassanalyse. Diese ist in der Beschreibung über der Grafik verlinkt."),
                                 placement = "top",
                                 trigger = "hover"),
              tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_arbeitsmarkt_3")
            )
            )
        )
      )
    ),



  # Footer
  funct_footer()

  )
 )
}

  # Server -------

#' fachkraft_start Server Functions
#'
#' @noRd
mod_fachkraft_start_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  # Box 1 - Fachkraft-Prognose ----

    #ohne download
    output$plot_fachkraft_prog_item_1 <- highcharter::renderHighchart({
      plot_fachkraft_prognose(r)
    })

    # # Plot-List erstellen für Output
    # output$plot_fachkraft_prog_item_1 <- renderUI({
    #   plot_list <- plot_fachkraft_prognose(r)
    #   r$plot_fachkraft_prog_item_1 <- plot_list
    #
    #   r$plot_fachkraft_prog_item_1_title <- get_plot_title(
    #     plot = r$plot_fachkraft_prog_item_1
    #   )
    #
    #   plot_list
    # })
    # # Download erstellen
    # output$download_btn_plot_fachkraft_prog_item_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_fachkraft_prog_item_1_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_fachkraft_prog_item_1,
    #       filename =  r$plot_fachkraft_prog_item_1_title,
    #       width = 700,
    #       height = 400)
    #
    #     file.copy(r$plot_fachkraft_prog_item_1_title, file)
    #     file.remove(r$plot_fachkraft_prog_item_1_title)
    #   }
    # )

    #ohne download
    output$plot_fachkraft_prog_alle_1 <- highcharter::renderHighchart({
      plot_fachkraft_prognose_alle(r)
    })

    # ohne download
    output$plot_fachkraft_prog_detail_item_1 <- highcharter::renderHighchart({
      plot_fachkraft_prognose_detail(r)
      })

    # output$plot_fachkraft_prog_detail_item_1 <- renderUI({
    #
    #   plot_list <- plot_fachkraft_prognose_detail(r)
    #   r$plot_fachkraft_prog_detail_item_1 <- plot_list
    #
    #   r$plot_fachkraft_prog_detail_item_1_title <- get_plot_title(
    #     plot = r$plot_fachkraft_prog_detail_item_1
    #   )
    #
    #   plot_list
    # })

    # ohne Download
    output$plot_fachkraft_wirkhebel_analyse_1 <- plotly::renderPlotly({
      plot_fachkraft_wirkhebel_analyse(r)
      })

    # output$plot_fachkraft_wirkhebel_analyse_1 <- plotly::renderPlotly({
    #
    #   plot_list <- plot_fachkraft_wirkhebel_analyse(r)
    #   r$plot_fachkraft_wirkhebel_analyse_1 <- plot_list
    #
    #   plot_list
    # })



    ## pdf ----
    # output$downloadPDF <- downloadHandler(
    #   # filename = function() {
    #   #   "MINT-Fachkraeftezukunftsszenarien_Methodenbericht.pdf"
    #   # },
    #   content = function(file) {
    #     file.copy("www/MINT-Fachkraeftezukunftsszenarien_Methodenbericht.pdf", file)
    #   }
    # )

  # Box 2 - Fachkraft - Berufsgruppen-Ebene ----

    ## EPA nach MINT

    # Download kurz mal raus
    # output$plot_fachkraft_epa_item_1 <- highcharter::renderHighchart({
    #   plot_list <- plot_fachkraft_epa_item(r)
    #
    #   # return plots
    #   out <- highcharter::hw_grid(
    #     plot_list,
    #     ncol = 2)
    #   out
    #
    # })

    output$plot_fachkraft_epa_item_1 <- renderUI({
      plot_list <- plot_fachkraft_epa_item(r)
      r$plot_fachkraft_epa_item_1_left <- plot_list[[1]]
      r$plot_fachkraft_epa_item_1_right <- plot_list[[2]]

      r$plot_fachkraft_epa_item_1_left_title <- get_plot_title(
        plot = r$plot_fachkraft_epa_item_1_left
      )
      r$plot_fachkraft_epa_item_1_right_title <- get_plot_title(
        plot = r$plot_fachkraft_epa_item_1_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })
    #
    # output$download_btn_plot_fachkraft_epa_item_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_fachkraft_epa_item_1_left_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_fachkraft_epa_item_1_left,
    #       filename =  r$plot_fachkraft_epa_item_1_left_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_fachkraft_epa_item_1_left_title, file)
    #     file.remove(r$plot_fachkraft_epa_item_1_left_title)
    #   }
    # )
    #
    # output$download_btn_plot_fachkraft_epa_item_2 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_fachkraft_epa_item_1_right_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #     add_caption_and_download(
    #       hc = r$plot_fachkraft_epa_item_1_right,
    #       filename =  r$plot_fachkraft_epa_item_1_right_title,
    #       width = 700,
    #       height = 400,
    #       with_labels = FALSE)
    #
    #     file.copy(r$plot_fachkraft_epa_item_1_right_title, file)
    #     file.remove(r$plot_fachkraft_epa_item_1_right_title)
    #   }
    # )

    ## MINT an EPA
    output$plot_fachkraft_mint_item_1 <- highcharter::renderHighchart({
      plot_fachkraft_mint_item(r)

    })

    ## Bar Vakanz

    # Download für JT kurz raus

    output$fachkraft_bar_vakanz_1_plot <- highcharter::renderHighchart({
      plot_fachkraft_bar_vakanz(r)
    })

    #  output$plot_fachkraft_bar_vakanz_1 <- highcharter::renderHighchart({
    #   plot_list <- plot_fachkraft_bar_vakanz(r)
    #   r$plot_fachkraft_bar_vakanz_1 <- plot_list
    #
    #   r$plot_fachkraft_bar_vakanz_1_title <- get_plot_title(
    #     plot = r$plot_fachkraft_bar_vakanz_1
    #   )
    #
    #   plot_list
    # })
    #
    # output$download_btn_plot_fachkraft_bar_vakanz_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_fachkraft_bar_vakanz_1_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_fachkraft_bar_vakanz_1,
    #       filename =  r$plot_fachkraft_bar_vakanz_1_title,
    #       width = 700,
    #       height = 400)
    #
    #     file.copy(r$plot_fachkraft_bar_vakanz_1_title, file)
    #     file.remove(r$plot_fachkraft_bar_vakanz_1_title)
    #   }
    # )

    # Download für JT kurz raus
    output$plot_fachkraft_bar_vakanz_1 <- renderUI({
      plot_list <- plot_fachkraft_bar_vakanz(r)
      plot_list
    })


    ## Detail Berufe


    #Download kurz raus für JT
    # output$plot_fachkraft_detail_item_1 <- highcharter::renderHighchart({
    #   plot_list <- plot_fachkraft_detail_item(r)
    #
    #   highcharter::hw_grid(
    #     plot_list,
    #     ncol = 2)
    # })

    output$plot_fachkraft_detail_item_1 <- renderUI({
      plot_list <- plot_fachkraft_detail_item(r)
      r$plot_fachkraft_detail_item_1_left <- plot_list[[1]]
      r$plot_fachkraft_detail_item_1_right <- plot_list[[2]]

      r$plot_fachkraft_detail_item_1_left_title <- get_plot_title(
        plot = r$plot_fachkraft_detail_item_1_left
      )
      r$plot_fachkraft_detail_item_1_right_title <- get_plot_title(
        plot = r$plot_fachkraft_detail_item_1_right
      )

      highcharter::hw_grid(
        plot_list,
        ncol = 2)
    })
    #
    # output$download_btn_plot_fachkraft_item_detail_1 <- downloadHandler(
    #   contentType = "image/png",
    #   filename = function() {r$plot_fachkraft_detail_item_1_right_title},
    #   content = function(file) {
    #     # creating the file with the screenshot and prepare it to download
    #
    #     add_caption_and_download(
    #       hc = r$plot_fachkraft_detail_item_1_right,
    #       filename =  r$plot_fachkraft_detail_item_1_right_title,
    #       width = 700,
    #       height = 400)
    #
    #     file.copy(r$plot_fachkraft_detail_item_1_right_title, file)
    #     file.remove(r$plot_fachkraft_detail_item_1_right_title)
    #   }
    # )


  })
}

## To be copied in the UI
# mod_international_start_ui("international_start_1")

## To be copied in the server
# mod_international_start_server("international_start_1")

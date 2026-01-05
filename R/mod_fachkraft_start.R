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
      div(class = "clean-box",
          column(
            width = 12,
            img(src='www/Banner_Fachkraft.avif',
                class = "img-responsive",
                #height = "150px", width = "150px",
                alt = "Banner Fokus: MINT-Fachkraefte",
                style="display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"
            )))),


    # Info-Texte ----

    fluidRow(
      shinydashboard::box(
        h2("Auf dieser Seite"),
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Das ist eine unserer Fokus-Seiten. Hier schauen wir auf das Thema MINT-Fachkräfte.
          Auf der Basis von amtlichen Statistiken und Analysen der Bundesagentur für Arbeit zeigen wir,
          wie es aktuell um Fachkräfte in MINT steht."),
        p(),
        p(style = "text-align: left; font-size = 16px",
          "Außerdem haben wir selbst eine Analyse in Auftrag gegeben,
          mit der wir auf die Zukunft der MINT-Fachkräfte blicken.
          Wir haben Zukunftsszenarien der MINT-Fachkräfteentwicklung berechnen lassen und schauen darauf,
          wie verschiedene Wirkhebel die MINT-Fachkräftezahlen der Zukunft bewegen können."),
        p()
      ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über die Teilnahme an unserer kurzen",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        h2("Themenübersicht"),
        width = 7,
        p(
          style = "text-align: left; font-size = 16px",tags$a(href="#fachkraft-zukunft",
                                                              span(tags$b(span("→ Fachkräfte-Zukunftsszenarien:")))), "Zukunftsszenarien der MINT-Fachkräfte"
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#fachkraft-berufsgruppen",
                                                              span(tags$b(span("→ Berufsgruppen:")))), "aktueller Fachkräftebedarf in MINT"

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#fachkraft-berufe",
                                                              span(tags$b(span("→ Berufsebene:")))), "aktueller Fachkräftebedarf in MINT"
        )
      ),

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          tags$b(span("Zukunftsszenarien der MINT-Fachkräfteentwicklung: Datengrundlage
          Bundesagentur für Arbeit, 2023; Vorausberechnung durch IW Köln, 2024,
          beauftragt durch MINTvernetzt"))),
        p(style = "text-align: left; font-size = 16px",
          tags$b(span("Fachkräftedaten: Bundesagentur für Arbeit, 2024, auf Anfrage.")),
        "Daten des Berichtsjahres 2024 ca. ab Juli 2025 verfügbar."),
        p("Die amtlichen Statistiken der Fachkräftedaten
          (Engpassanalyse, Vakanzzeit, Arbeitslosen-Stellen-Relation)
          zeigen das aktuellste verfügbare Berichtsjahr 2023.")

      )
    ),

    # Kurzanalyse ----
    div(class = "content-box",
        div(class = "inner-box",
            p(br(),"KURZANALYSE", br()),
            p(style = "font-size = 24",
              strong("Für die folgenden Zukunftsszenarien hat MINTvernetzt vom IW Köln berechnen lassen,
        wie sich die MINT-Fachkräftezahlen in den nächsten Jahren entwickeln werden.
        Wirken Förderinitiativen für den MINT-Nachwuchs, Frauen in MINT, den Verbleib
        älterer Fachkräfte und für die Zuwanderung von MINT-Fachkräften zusammen, können
       bis 2037 1,4 Mio. MINT-Fachkräfte zusätzlich gewonnen werden. Im Vergleich dazu bewegen wir
        uns aktuell auf rund 100.000 MINT-Fachkräfte weniger zu, in einem Worst Case sogar auf rund 1,1 Mio. weniger."),
              br(), br(),
              tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/07/MINTvernetzt_Kurzanalyse_Zukunftsszenarien_MINT_Fachkraefte.pdf", target = "_blank",
                     "Link zu der Kurzanalyse über die Zukunftsszenarien allgemein"), br(),
              p("Veröffentlichung: 16. Juli 2024",
                br(),
                "Zitiervorschlag: MINTvernetzt (2024). Kurzanalyse. Zukunftsszenarien MINT-Fachkräfte."),
              tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/07/MINTvernetzt_Kurzanalyse_Zukunftsszenarien_Zuwanderung.pdf", target = "_blank",
                     "Link zu der Kurzanalyse über die Zukunftsszenarien der Zuwanderung in MINT"), br(),
              br(),
              p("Veröffentlichung: 16. Juli 2024",
                br(),
                "Zitiervorschlag: MINTvernetzt (2024). Kurzanalyse. Zukunftsszenarien Zuwanderung in MINT."))
        )
    ),

    # Box 1 - MINT-Fachkräfte-Zukunftsszenarien ----

    fluidRow(
      id = "fachkraft-zukunft",
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          const links = document.querySelectorAll('.has-tooltip');
          links.forEach(link => {
            const titleText = link.getAttribute('data-tooltip');
            const tooltip = document.createElement('div');
            tooltip.className = 'tooltip';
            tooltip.innerHTML = titleText;
            link.appendChild(tooltip);

            link.addEventListener('mouseover', function() {
              tooltip.style.display = 'block';
            });

            link.addEventListener('mousemove', function(e) {
              tooltip.style.left = e.pageX + 10 + 'px';
              tooltip.style.top = e.pageY + 10 + 'px';
            });

            link.addEventListener('mouseout', function() {
              tooltip.style.display = 'none';
            });
          });
        });
      ")),
      shinydashboard::box(
        h2("Zukunftsszenarien der MINT-Fachkräfte"),
        width = 12,
        column(
          width = 8,
        p("Hier können Sie in die Zukunft blicken. Wir haben das Wirtschaftsforschungsinstitut IW Köln beauftragt,
          Projektionen möglicher Entwicklungen der MINT-Fachkräftezahlen in den nächsten Jahren zu berechnen. Wir wollen dabei nicht schauen,
          wie viele Personen uns fehlen werden, sondern darauf, wie viele Personen für MINT-Berufe gewonnen werden könnten.

          Dabei blicken wir auf die Wirkhebel MINT-Nachwuchs fördern, Mädchen und Frauen in MINT fördern,
          Zuwanderung von MINT-Fachkräften und Verbleib älterer Fachkräfte in MINT-Berufen.

          Nähere Informationen dazu, welche methodischen Annahmen den Wirkhebeln und Szenarien zugrunde liegen,
          finden Sie in in den Info-Boxen der Grafiken.

          Vergleichen Sie hier, wie sich diese Wirkhebel auf die Fachkräfteentwicklung auswirken."),

        p("Hier finden Sie drei Grafikvarianten zum Herunterladen. Die interaktiven, anpassbaren Grafiken folgen darunter und können ebenfalls über die Grafik-Funktion oben rechts neben der Grafik-Überschrift heruntergeladen werden."),

        tags$a(href = "www/Vergleich_Wirkhebel_MINT-Fachkraefte.png", target = "_blank", "Download Grafik Vergleich Wirkhebel",
               title = "Die Grafik öffnet sich in einem neuen Browserfenster und kann mit Rechtsklick + \"Grafik speichern unter...\" heruntergeladen werden."),
        br(),
        tags$a(href = "www/Gesamteffekt_MINT-Fachkraefte.png", target = "_blank", "Download Grafik Gesamteffekt Wirkhebel",
               title = "Die Grafik öffnet sich in einem neuen Browserfenster und kann mit Rechtsklick + \"Grafik speichern unter...\" heruntergeladen werden."),
        br(),
        tags$a(href = "www/Zukunftsszenarien_MINT-Fachkraefte.png", target = "_blank", "Download Grafik Szenarien des Gesamteffekts",
               title = "Die Grafik öffnet sich in einem neuen Browserfenster und kann mit Rechtsklick + \"Grafik speichern unter...\" heruntergeladen werden."),

        ),
        column(
          width = 4,
          tags$div(
            style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: flex-start;",
            tags$strong(
              "MINT-Fachkräfteszenarien erklärt:",
              style = "margin: 0px 0px 0px 60px"
            ),
            p("Lesen Sie in diesem Foliensatz die zentralen Ergebnisse, Methodik und Interpretation nach.",
              style = "margin: 0px 0px 0px 60px"),
            tags$a(
              href = "www/MINTvernetzt_Foliensatz_MINT-Fachkräfteszenarien_erklärt.pdf",
              target = "_blank",
              tags$img(
                src = "www/Screenshot_Foliensatz_Fachkräfteszenarien.png",
                alt = "Cover Foliensatz MINT-Fachkräfteszenarien",
                style = "max-width: 20%; height: auto; cursor: pointer; margin: 10px 0px 0px 70px;
                  border: 1px solid #EFE8E6;"
              )
            )
          )),
       column(
         width = 12,

       br(),br(),
        tabsetPanel(
          type = "tabs",

          tabPanel(
            "Übersicht Wirkhebel", br(),

            shiny::sidebarPanel(
              width = 3,

              mod_fachkraft_wirkhebel_analyse_ui("fachkraft_item_wirkhebel_analyse_1"),
              br(),# TODO

            ),
            shiny::mainPanel(
              width = 9,
              shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot_fachkraft_wirkhebel_analyse_1")),
                                           color = "#154194"),

              p(),
              shinyBS::bsPopover(
                id="h_fachkraft-prognosen_3", title="",
                content = paste0("Der Effekt der Förderung von Frauen baut auf dem Effekt der MINT-Bildung auf. Das Szenario betrachtet eine erfolgreichere MINT-Bildung mit besonderen Erfolgen bei Mädchen und Frauen."),
                placement = "top",
                trigger = "hover"),
              tags$a(paste0("Hinweis zu den Daten"),
                     icon("info-circle"),
                     id = "h_fachkraft-prognosen_3"),
              p(),

              tags$a(href = "www/MINTvernetzt_Methodenbericht_MINT-Fachkräfteszenarien.pdf", target = "_blank", "Methodenbericht des IW Köln als PDF")
            )
          ),


    tabPanel(
            title = "Zukünftige Fachkräfteentwicklung", br(),

            shiny::sidebarPanel(
              width = 3,
              mod_fachkraft_item_prog_ui("fachkraft_item_prog_1"),
              br(),

            ),
            shiny::mainPanel(
              width = 9,
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_fachkraft_prog_item_1"),
                                                                        height = "600px"),
                                                    color = "#154194"),

              p(),
              tags$a(href = "www/MINTvernetzt_Methodenbericht_MINT-Fachkräfteszenarien.pdf", target = "_blank", "Methodenbericht des IW Köln als PDF")
            )
          ),
    tabPanel(

      title = "Im Vergleich: zukünftige Fachkräfteentwicklung", br(),

      shiny::sidebarPanel(
        width = 3,
        mod_fachkraft_item_prog_alle_ui("fachkraft_item_prog_alle_1"),
        br(),

      ),
      shiny::mainPanel(
        width = 9,
        shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_fachkraft_prog_alle_1"),
                                                                  height = "600px"),
                                     color = "#154194"),

        p(),
        tags$a(href = "www/MINTvernetzt_Methodenbericht_MINT-Fachkräfteszenarien.pdf", target = "_blank", "Methodenbericht des IW Köln als PDF")
      )
    ),
    tabPanel(
            "Im Detail: zukünftige Fachkräfteentwicklung", br(),

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

              p(),
              tags$a(href = "www/MINTvernetzt_Methodenbericht_MINT-Fachkräfteszenarien.pdf", target = "_blank", "Methodenbericht des IW Köln als PDF")
            )
          )
)))),



  # Box 2 - Fachkräfte auf Berufsgruppen-Level ----
      fluidRow(
        id="fachkraft-berufsgruppen",
        shinydashboard::box(
          h2("Berufsgruppen: aktueller Fachkräftebedarf in MINT"),
          width = 12,
          column(
            width = 8,
          p("
          Vergleicht man die Berufsgruppen zeigt sich: Der Fachkräfteengpass in MINT-Berufen ist höher als im Nicht-MINT-Bereich.
            Während 39 % der MINT-Berufe einen Fachkräfteengpass aufweisen, und weiter 34 % Anzeichen für einen Fachkräfteengpass zeigen,
            sind es in den Nicht-MINT-Berufen nur jeweils 32 %. Auch die Indikatoren der Arbeitslosen-Stellen-Relation und
            Vakanzzeit weisen auf einen erhöhten Fachkräftebedarf in den MINT-Berufen hin."),
          p("Die hier betrachteten Indikatoren des Fachkräftebedarfs sind die Ergebnisse der
            Engpassanalyse der Bundesagentur für Arbeit, der Arbeitslosen-Stellen-Relation und der Vakanzzeit.
            Was diese Indikatoren bedeuten, erklären wir in den Infoboxen, die sich beim Klicken auf
            die folgenden Wörter öffnen.")
          ),
          column(
            width = 12,
          br(),
          column(width = 2,
                 shinyBS::bsPopover(id="i_engpass_analyse_def", title = "",

                                    placement = "right",
                                    trigger = "click",
                                    options = list(container = "body",
                                                   content = paste0("Die Bundesagentur für Arbeit hat, basierend auf sechs Indikatoren, für alle Berufe einen Engpassindikator berechnet. <br><br>Indikatoren: Abgangsrate aus der Arbeitslosigkeit, Arbeitssuchenden-Stellen-Relation, Berufssp. Arbeitslosenquote, Entwicklung der mittleren Entgelte, Vakanzzeit, Veränderung in der Beschäftigung ausländischer Personen. <br><br> Näheres zur Berechnung können Sie hier nachlesen: <br> <a>https://statistik.arbeitsagentur.de/DE/Navigation/Statistiken/Interaktive-Statistiken/Fachkraeftebedarf/Engpassanalyse-Nav.html</a> "),
                                                   delay = list(show = 100, hide = 100),
                                                   template = '<div class="popover" role="tooltip" style="max-width: 100%;"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>')),
                 tags$a(paste0("Engpassanalyse, hier klicken: "), icon("info-circle"), id = "i_engpass_analyse_def"),
                 p()
          ),
          column(width = 3,
                 shinyBS::bsPopover(id="i_asr_def", title = "",
                                    content = paste0("Die Arbeitslosen-Stellen-Relation berechnet sich aus folgendem Verhältnis: Jahresdurchschnitt Arbeitslose / Jahresdurchschnitt gemeldete sozialversicherungspflichtige Arbeitsstellen. <br> <br> Eine Arbeitslosen-Stellen-Relation von 3 bedeutet, dass in einem Beruf drei arbeitslose/arbeitssuchende Bewerber:innen auf eine gemeldete Arbeitsstelle kommen. <br><br>Quelle: Bundesagentur für Arbeit"),
                                    placement = "right",
                                    trigger = "click"),
                 tags$a(paste0("   Arbeitslosen-Stellen-Relation, hier klicken: "), icon("info-circle"), id = "i_asr_def"),
                 p()
          ),
          column(width = 2,
                 shinyBS::bsPopover(id="i_vakanzzeit_def", title = "",
                                    content = paste0("Wir betrachten hier die abgeschlossene Vakanzzeit. Als abgeschlossene Vakanzzeit versteht man die Dauer, die benötigt wird, bis eine leer stehende Stelle besetzt wird. <br><br>Eine Vakanzzeit von 50 bedeutet, dass eine gemeldete Arbeitsstelle 50 Tage zur Vermittlung gemeldet war, bis sie besetzt werden konnte. <br><br>Quelle: Bundesagentur für Arbeit."),
                                    placement = "right",
                                    trigger = "click"),
                 tags$a(paste0("   Vakanzzeit, hier klicken: "), icon("info-circle"), id = "i_vakanzzeit_def"),
                 p()
          )
          ),
          column(width = 12,
                 br(),
          tabsetPanel(

            type = "tabs",

            tabPanel(
            title = "Fachkräfteengpass in MINT-Berufen", br(),

            shiny::sidebarPanel(
              width = 3,
              mod_fachkraft_item_epa_ui("fachkraft_item_epa_1"),
              br(),
            ),
            shiny::mainPanel(
              width = 9,
              p("Auf Bundesebene liegen Daten zum Fachkräfteengpass in den einzelnen Berufen bzw.
                genauer Berufsgattungen, z. B. Mechatronik, vor."),
              shinycssloaders::withSpinner(htmlOutput(ns("plot_fachkraft_epa_item_1")),
                                           color = "#154194"),



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
            title = "Fachkräfteengpass der Bundesländer", br(),

            shiny::sidebarPanel(
              width = 3,
              mod_fachkraft_item_epa_bulas_ui("fachkraft_item_epa_bulas"),
              br(),
            ),
            shiny::mainPanel(
              width = 9,
              p("Für die einzelnen Bundesländer liegen weniger detaillierte Daten vor als für
                Deutschland. Hier sehen wir Informationen zum mittleren Fachkräfteengpass in den
                verschiedenen, MINT-dominierten Berufsgruppen, z. B. Mechatronik und Automatisierungstechnik.
                Die Berufsgrattungen werden zu Berufsgruppen zusammengefasst und gemeinsam betrachtet."),
              shinycssloaders::withSpinner(htmlOutput(ns("plot_fachkraft_epa_bulas")),
                                           color = "#154194"),



              shinyBS::bsPopover(
                id="h_fachkraft-berufsgruppen_2", title="",
                content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. <br><br>Berufliche Tätigkeiten können nur auf Ebene der Berufsgattungen trennscharf als MINT klassifiziert werden. Als Annäherung werden hier die von MINT-Berufsgattungen dominierten Berufsgruppen unter MINT und den unterschiedlichen MINT-Bereichen zusammengefasst. <br><br>Informationen zur Berechnung und Bedeutung des Engpassindikators finden Sie in der Infobox zur Engpassanalyse. Diese ist in der Beschreibung über der Grafik verlinkt."),
                placement = "top",
                trigger = "hover"),
              tags$a(paste0("Hinweis zu den Daten"),
                     icon("info-circle"),
                     id = "h_fachkraft-berufsgruppen_2")
            )
          ),

            tabPanel(
              "Arbeitslosen-Stellen-Relation und Vakanzzeit in MINT", br(),

              shiny::sidebarPanel(
                width = 3,
                mod_fachkraft_bar_vakanz_ui("fachkraft_bar_vakanz_1"),
                br(),
              ),
              shiny::mainPanel(
                width = 9,
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("fachkraft_bar_vakanz_1_plot")),
                                             color = "#154194"),


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

# Kurzanalyse ----
div(class = "content-box",
    div(class = "inner-box",
        p(br(),"KURZANALYSE", br()),
        p(style = "font-size = 24",
          strong("Im MINT-Bereich zeigt sich ein erhöhter Fachkräftemangel. Doch dabei unterscheidet sich die
                 Lage der einzelnen MINT-Bereich deutlich voneinander. Vor allem der Bereich Technik hat
                 einen besonders ausgeprägten Fachkräfteengpass. Wie es um Technik und die weitern MINT-Bereiche steht und
                  was wirkvolle Ansätze gegen den Fachkräftemangel sein könnte, fasst diese Kurzanalyse zusammen."),
          br(), br(),
          tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2025/03/Fachkraeftemangel-MINT-Disziplinen.pdf", target = "_blank",
                 "Link zu der Kurzanalyse"), br(),
          br(),
          p("Veröffentlichung: 05. März 2024",
            br(),
            "Zitiervorschlag: MINTvernetzt (2024). Kurzanalyse. Fachkräftemangel in MINT; Eine Frage der Disziplin?"))
    )
),

    # Box 3 - Fachkräfte auf Berufslevel ----

    fluidRow(
      id = "fachkraft-berufe",
      shinydashboard::box(
        h2("Berufsebene: aktueller Fachkräftebedarf in MINT"),
        width = 12,
        column(
          width = 8,
        p("
          Diese interaktive Darstellung ermöglichen es, den Fachkräftebedarf der einzelnen Berufsgattungen,
          z. B. Mechatronik oder Gesundheits- und Krankenpflege, zu betrachten.
          So liegt als ein Beispiel in den Berufen der Sanitär-, Heizungs- und Klimatechnik ein akuter Fachkräfteengpass vor (Wert über 2,0).
          Gründe, die zu dieser Beurteilung führen, sind hohe Vakanzzeiten, eine ungünstige Arbeitslosen-Stellen-Relation,
          die hohe berufsspezifische Arbeitslosenquote sowie niedrige Abgangsrate aus der Arbeitslosigkeit."), #Außerdem zeigen wir ein Ranking der MINT-Berufe mit dem aktuell höchsten und geringsten Fachkräftebedarf.
        p("Die hier betrachteten Indikatoren des Fachkräftebedarfs sind die Ergebnisse der
            Engpassanalyse der Bundesagentur für Arbeit, der Arbeitslosen-Stellen-Relation und der Vakanzzeit.
            Was diese Indikatoren bedeuten, erklären wir in den Infoboxen, die sich beim Klicken auf
            die folgenden Wörter öffnen.")
        ),
        column(
          width = 12,
        br(),
        column(width = 2,
        shinyBS::bsPopover(id="i_engpass_analyse_def_2", title = "",
                           placement = "right",
                           trigger = "click",
                           options = list(container = "body",
                                          content = paste0("Die Bundesagentur für Arbeit hat, basierend auf sechs Indikatoren, für alle Berufe einen Engpassindikator berechnet. <br><br>Indikatoren: Abgangsrate aus der Arbeitslosigkeit, Arbeitssuchenden-Stellen-Relation, Berufssp. Arbeitslosenquote, Entwicklung der mittleren Entgelte, Vakanzzeit, Veränderung in der Beschäftigung ausländischer Personen. <br><br> Näheres zur Berechnung können Sie hier nachlesen: <br> <a>https://statistik.arbeitsagentur.de/DE/Navigation/Statistiken/Interaktive-Statistiken/Fachkraeftebedarf/Engpassanalyse-Nav.html</a> "),
                                          delay = list(show = 100, hide = 100),
                                          template = '<div class="popover" role="tooltip" style="max-width: 100%;"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>')),
        tags$a(paste0("Engpassanalyse, hier klicken: "), icon("info-circle"), id = "i_engpass_analyse_def_2"),
        p()
        ),
        column(width = 3,
        shinyBS::bsPopover(id="i_asr_def_2", title = "",
                           content = paste0("Die Arbeitslosen-Stellen-Relation berechnet sich aus folgendem Verhältnis: Jahresdurchschnitt Arbeitslose / Jahresdurchschnitt gemeldete sozialversicherungspflichtige Arbeitsstellen. <br> <br> Eine Arbeitslosen-Stellen-Relation von 3 bedeutet, dass in einem Beruf drei arbeitslose/arbeitssuchende Bewerber:innen auf eine gemeldete Arbeitsstelle kommen. <br><br>Quelle: Bundesagentur für Arbeit"),
                           placement = "right",
                           trigger = "click"),
        tags$a(paste0("   Arbeitslosen-Stellen-Relation, hier klicken: "), icon("info-circle"), id = "i_asr_def_2"),
        p()
        ),
        column(width = 2,
        shinyBS::bsPopover(id="i_vakanzzeit_def_2", title = "",
                           content = paste0("Wir betrachten hier die abgeschlossene Vakanzzeit. Als abgeschlossene Vakanzzeit versteht man die Dauer, die benötigt wird, bis eine leer stehende Stelle besetzt wird. <br><br>Eine Vakanzzeit von 50 bedeutet, dass eine gemeldete Arbeitsstelle 50 Tage zur Vermittlung gemeldet war, bis sie besetzt werden konnte. <br><br>Quelle: Bundesagentur für Arbeit."),
                                            placement = "right",
                                            trigger = "click"),
                           tags$a(paste0("   Vakanzzeit, hier klicken: "), icon("info-circle"), id = "i_vakanzzeit_def_2"),
        p()
        )
        ),

        column(width = 12,
               br(),
       tabsetPanel(
         type = "tabs",
         tabPanel(
           title = "Ranking der Engpassrisiken", br(),

           shiny::sidebarPanel(
             width = 3,
             mod_fachkraft_ranking_epa_ui("fachkraft_ranking_engpass_epa"),
             br(),
           ),
           shiny::mainPanel(
             width = 9,
             shinycssloaders::withSpinner(htmlOutput(ns("plot_fachkraft_ranking_engpass")),
                                          color = "#154194"),
             shinyBS::bsPopover(
               id="fachkraft_engpaesse_ranks", title="",
               content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. <br><br>Informationen zur Berechnung und Bedeutung des Engpassindikators finden Sie in der Infobox zur Engpassanalyse. Diese ist in der Beschreibung über der Grafik verlinkt. In dieser Grafik werden die Berufsgattungen mit dem höchsten Fachkräfteengpass angezeigt. Da oft mehrere Berufe denselben Engpassindikator vorweisen, variiert die Anzahl der gerankten Berufe. Es werden immer alle Berufsgattungen mit demselben Wert entweder vollständig ein, oder ausgeblendet, um die Ergebnisse nicht zu verzerren."),
               placement = "top",
               trigger = "hover"),
             tags$a(paste0("Hinweis zu den Daten"),
                    icon("info-circle"),
                    id = "fachkraft_engpaesse_ranks")
           )

         ),

         tabPanel(
           "Engpassanalyse für MINT-Berufsgattungen", br(),
           shiny::sidebarPanel(
             width = 3,
             mod_fachkraft_item_detail_ui("fachkraft_item_detail_1"),
             br(),
           ),
           shiny::mainPanel(
             width = 9,
             shinycssloaders::withSpinner(htmlOutput(ns("plot_fachkraft_detail_item_1")),
                                          color = "#154194"),
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

    # output$download_wirkhebel <- shiny::downloadHandler(
    #   filename = function() {
    #     "Vergleich_Wirkhebel_MINT-Fachkräfte.png"
    #   },
    #   content = function(file) {
    #     file.copy("www/Vergleich_Wirkhebel_MINT-Fachkräfte.png", file)
    #   },
    #   contentType = "image/png"
    # )

    #ohne download
    output$plot_fachkraft_prog_item_1 <- highcharter::renderHighchart({
      plot_fachkraft_prognose(r)
    })


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


    # ohne Download
    output$plot_fachkraft_wirkhebel_analyse_1 <- plotly::renderPlotly({
      plot_fachkraft_wirkhebel_analyse(r)
      })




    ## pdf ----
    # output$downloadPDF <- downloadHandler(
    #   # filename = function() {
    #   #   "MINTvernetzt_Methodenbericht_MINT-Fachkräfteszenarien.pdf"
    #   # },
    #   content = function(file) {
    #     file.copy("www/MINTvernetzt_Methodenbericht_MINT-Fachkräfteszenarien.pdf", file)
    #   }
    # )

  # Box 2 - Fachkraft - Berufsgruppen-Ebene ----



    output$plot_fachkraft_epa_item_1 <- renderUI({

      plots <- plot_fachkraft_epa_item(r)
      if(length(plots)==2){
        div(
          style = "width: 1000px;",
          out <- highcharter::hw_grid(
            plots,
            ncol = 2
          )
        )
      }else{
        div(
          style = "width: 500px;",
          plots
        )
      }


    })



    ## Fachkräfteegpass Bulas
    output$plot_fachkraft_epa_bulas <- renderUI({
      plot_fachkraft_epa_bulas(r)
    })

    ## Bar Vakanz

    # Download für JT kurz raus

    output$fachkraft_bar_vakanz_1_plot <- highcharter::renderHighchart({
      plot_fachkraft_bar_vakanz(r)
    })




    # Download für JT kurz raus
    output$plot_fachkraft_bar_vakanz_1 <- renderUI({
      plot_list <- plot_fachkraft_bar_vakanz(r)
      plot_list
    })


    ## Detail Berufe

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


    output$plot_fachkraft_ranking_engpass <- renderUI({
      plot_list <- plot_fachkraft_ranking_epa(r)
    })





  })
}

## To be copied in the UI
# mod_international_start_ui("international_start_1")

## To be copied in the server
# mod_international_start_server("international_start_1")

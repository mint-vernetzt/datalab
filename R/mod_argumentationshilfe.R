
#' Argumentationshilfe UI Function + Server
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# UI Funktion der Seite
mod_argumentation_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      # noch austauschen
      div(class = "clean-box",
          column(
            width = 12,
            img(src='www/Banner_Hinweise.jpg',
                class = "img-responsive",
                height = "300px",
                alt = "Banner Quellen",
                style="display: block; margin-left: auto; margin-right: auto;"
            )))),

    ## Einleitungstext ----
    fluidRow(
      div(class = "clean-box",
          column(
            width = 10,
            h1("Schnellstart: Mit MINT-Daten überzeugend argumentieren"),
            p(style = "text-align: justify; font-size = 20px",
              "Ob für Förderanträge, Kommunikation oder strategische Entscheidungen -
              Daten helfen, überzeugend zu argumentieren.
              Sie wollen Ihre Arbeit mit starken Argumenten unterstreichen, zum Beispiel,
              um in Finanzierungsgesuche zu überzeugen, im Diskurs mit Politiker:innen die Relevanz von MINT-Förderung
              zu bestärken oder das eigene Projekt wirksam ausrichten?"),
            p(style = "text-align: justify; font-size = 20px",
            "Hier finden Sie die Werkzeuge dafür, eine starke Argumentationskette
            für die MINT-Bildungsförderung in Ihrer Region aufzubauen."),
            p("Ein Beispiel, wie eine solche Argumentation aussehen könnte, basierend auf fünf Statistiken
            aus dem MINT-DataLab, finden Sie hier:"),
            tags$a(href = "www/Methodenbericht_MINT-Fachkraefteszenarien.pdf",
                   target = "_blank", tags$span(icon("file", style = "margin-right: 5px;"), " NOCH AUSTAUCHEN Beispiel-Bericht für Hamburg"),
                   class = "btn btn-default",
                   style = "margin-bottom: 20px;"),


            h1("So geht´s"),
            tags$span(icon("1", style = "margin: 10px; font-size: 16px;"),
                      "Wählen Sie Ihre gewünschte Region aus."),
            hr(style="margin-top: 10px;margin-bottom: 10px;"),
            tags$span(icon("2", style = "margin: 10px; font-size: 16px;"),
                      "Laden Sie die Daten als Grundlage Ihrer Argumentation herunter."),
            hr(style="margin-top: 10px;margin-bottom: 10px;"),
            tags$span(icon("3", style = "margin: 10px; font-size: 16px;"),
            "Nutzen Sie den DataLab-GPT, der Ihnen basierend auf den Daten eine
              Argumentation passend zu Ihrer Region erstellt."),
            hr(style="margin-top: 10px;margin-bottom: 10px;"),
            tags$span(icon("4", style = "margin: 10px; font-size: 16px;"),
                      "Laden Sie hier die zugehörigen Grafiken herunter und
                      ergänzen Sie in Ihrem Bericht."),

            br(),
            br(),
            hr(style = "border-top: 2px solid #154194;margin-top: 5px;"),
            br(),

                )
          )
    ),

    column(
      width = 10,

      ## Region-Filter ----


      p(strong(style = "text-align: justify; font-size = 24px",
               "1. Wählen Sie Ihre Region aus:")),

      div(
        style = "margin-bottom: 30px",
        shinyWidgets::pickerInput(
          inputId = ns("region_argumentationshilfe"),
          choices = c("Deutschland",
                      "Baden-Württemberg",
                      "Bayern",
                      "Berlin",
                      "Brandenburg",
                      "Bremen",
                      "Hamburg",
                      "Hessen",
                      "Mecklenburg-Vorpommern",
                      "Niedersachsen",
                      "Nordrhein-Westfalen",
                      "Rheinland-Pfalz",
                      "Saarland",
                      "Sachsen",
                      "Sachsen-Anhalt",
                      "Schleswig-Holstein",
                      "Thüringen",
                      "Westdeutschland (o. Berlin)",
                      "Ostdeutschland (inkl. Berlin)"
          ),
          multiple = FALSE,
          selected = c("Deutschland")
        )
      ),

      ## Download-Button ----

      p(strong(style = "text-align: justify; font-size = 24px",
               "2. Laden Sie hier die Daten als Basis der Argumentationkette herunter:")),
      p("Als ein Beispiel für eine datenbasierte Argumentationskette haben wir fünf Statistiken
        aus dem MINT-DataLab ausgewählt. Die Grafiken werden weiter unten auf dieser Seite dargestellt.
        Die Daten aller Grafiken können hier gesammelt heruntergeladen werden.", br(), br(),
        "Das Format für den Download ist ein Text-Dokument, da im nächsten Schritt eine speziell
        angepasste Version von ChatGPT bei der Erstellung der Argumentationskette assistiert.
        Dieser MINT-DataLab-GPT kann nur im Rahmen des Angebots von ChatGPT bzw. OpenAI genutzt werden.
        Bei einer kostenfreien Nutzung von ChatGPT sind Daten-Uploads allerdings nur eingeschränkt möglich.
        Das löst der Daten-Download hier: Die Daten im Text-Format können einfach in den Chat hineinkopiert werden."),
      div(
        style = "margin-bottom: 30px;",
        downloadButton(ns("download_txt"), " Gesammelte Daten herunterladen"),
      ),

      ## Absprung GPT + Beispielbericht ----

      p(strong(style = "text-align: justify; font-size = 24px",
               "3. Laden sie die Daten im MINT-DataLab-GPT hoch und lassen sich KI-gestützt eine Argumentation erstellen:")),
      p("Dieser speziell für das MINT-DataLab konfigurierte MINT-DataLab-GPT
             führt Sie durch die Erstellung einer Arugmentationskette basierend auf Statistiken des MINT-DataLab und
             Informationen zu Ihrem Projekt. Bei Bedarf kann die Argumentation auch durch eine Online-Recherche ergänzt werden.", br(), br(),
        "Der MINT-DataLab-GPT führt einen Dialog mit Ihnen und wird so einen kurze Bericht mit einer möglichen Argumentation mit Ihnen erstellen.", br(),
        "Öffnen Sie dafür mit folgendem Link den Chat mit dem MINT-DataLab-GPT und folgen Sie den Anweisungen dort."),

      tags$a(href = "https://chatgpt.com/g/g-67e4f41fd91881919a753f4309194bf7-test-mint-datalab-assistent-test",
             tags$span(icon("comments", style = "margin-right: 5px;"),"Zum MINT-DataLab-GPT"), target="_blank",
             class = "btn btn-default",
             style = "margin-bottom: 30px; margin-right: 10px;"),


      ## Grafiken ----
      p(strong(style = "text-align: justify; font-size = 24px;",
               "4. Statistiken und Grafiken als Basis der Argumentationskette:")),
      p("Hier finden Sie eine Auswahl an Statistiken, welchen sich
        für eine Argumentationskette für allgemein mehr MINT-Förderung eignen.
        Die gesammelten Daten der Statistiken können unter Schritt 2 heruntergeladen werden."),
       p( "Alle Statistiken gibt es in ähnlicher Form, mit mehr Anpassungsmöglichkeiten, auf den weiteren
         Unterseiten des MINT-DataLab. Hinweise dazu, welche Darstellungen aus dem MINT-DataLab über diese hinaus für eine starke Argumentationskette
        genutzt werden können, finden sich in den blauen Boxen rechts der Grafiken."),
      p("Die grünen Boxen unter den Grafiken geben beispielhaft Interpretationen, die für eine Argumentation im Sinne von
        MINT-Bildungsförderung hinter den Statistiken stecken."),
      p("Wer noch tiefer eintauchen will, kann eine solche Argumentation
            auch auf allen weiteren Statistiken aus dem MINT-DataLab aufbauen. Nutzen Sie dafür denselben MINT-DataLab-GPT wie
            unter Schritt 3 verlinkt und die Daten aus den Daten-Downloads der jeweiligen Grafiken.",
        style = "margin-bottom: 30px;")

    ),

    ## Box Zeitverlauf MINT----

    fluidRow(
             shinydashboard::box(
               title = "1. Welche Rolle spielt MINT in Ihrer Region",
               width = 12,
               column(
                 width = 9,
                 p("Als Einstieg in eine Argumentation kann ein kurzer Überblick über die MINT-Strukturen
                   der eigenen Region geigenet sein. Hierfür können Sie selbst rechercherien oder den
                   MINT-DataLab-GPT um eine Recherche bitten."),
                 p("Ergänzend dazu kann auf die Entwicklung der Beschäftigten, Studierenden und Auszubildenden
                   in MINT geschaut werden.
                   Das kann ein Indikator dafür sein, wie zentral der MINT-Sektor für die Region ist und ob
                   die Relevanz eher steigt oder der MINT-Bereich eher hinter anderen
                   Bereichen zurück fällt.")
               ),
                 column(
                   width = 9,
                   shiny::mainPanel(
                     width = 12,
                     shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_verlauf")),
                                                  color = "#154194"),
                      shinyBS::bsPopover(id="h_argument_1a", title = "",
                                       content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Weitere Informationen finden Sie unter dem Reiter \"Datenquellen und Hinweise\"."),
                                       placement = "top",
                                       trigger = "hover"),
                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_1a"),
                    br(),
                    shinyBS::bsPopover(id="i_argument_1", title = "",
                                       content = paste0("Die linke Grafik stellt den Zeitverlauf der Beschäftigen dar. Die sind in der ersten Einstellung, d.h. für Gesamtdeutschland im Jahr 2023 auf mehr als 7.8 Mio angestiegen, ein Plus von 500.000 gegenüber 2017. Rechts werden die Studierenden und Auszubildenden dargestellt. Diese waren für Gesamtdeutschland konstant."),
                                       placement = "top",
                                       trigger = "hover"),
                    tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_1")
                   )
                 ),
                 column(
                   width = 3,
                   div(class = "content-box",
                       style = "background-color: #15419430;
                              color: #154194;
                              border: 2px solid #154194;
                              margin-left: 20px;
                              width: 90%;
                              border-radius: 10px;",
                              p("Weiter Statistiken, die hier ergänzt werden könnten:"),
                              p("MINT-Anteil:  \"Alle Bildungsbereiche\", aktueller MINT-Anteil + MINT-Anteil im Zeitverlauf"),
                              p("Bundeslandvergleich: \"Ausbildung & Beruf\", aktueller MINT-Anteil + Bundeslandvergleich")
                   )
                 ),
               column(
                 width = 12,

                 column(
                   width = 3,
                   br(),
                   div(class = "content-box",
                       style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                       p(strong("Wenn die Kurve steigt:")),
                       p("Die Relevanz von MINT für die Region wächst.
                     MINT-Kenntnisse müssen steigen, um steigenden Bedarfe an
                     MINT-Kompetenzen begegnen zu können.
                     In MINT-Angebote zu investieren heißt, in die Zukunft zu investieren.")
                   )
                 ),
                 column(
                   width = 3,
                   br(),
                   div(class = "content-box",
                       style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                       p(strong("Wenn die Kurve gleich bleibt:")),
                       p("Der MINT-Bereich ist konstant eine wichtige Säule der Region.
                     Gleichzeitig werden MINT-Kompetenzen aufgrund von Digitalisierung
                     und Technologisierung immer wichtiger. In MINT-Angebote zu
                       investieren heißt, in die Zukunft zu investieren.")
                   )
                 ),
                 column(
                   width = 3,
                   br(),
                   div(class = "content-box",
                       style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                       p(strong("Wenn die Kurve sinkt:")),
                       p("Der MINT-Bereich der Region wird kleiner, obwohl aufgrund
                    von Digitalisierung und Technologisierung MINT die Zukunft des
                    Wirtschaft- und Bildungsbereichs prägt. Stärker in MINT-Angebote
                      zu investieren ist nötig für die zukünftige Wettbewerbsfähigkeit
                      der Region.")
                   )
               )
               )
             )
            ),

    ## Box Fachkräftemagel ----
    fluidRow(
      shinydashboard::box(
        title = "2. Wo die MINT-Fachkräfte fehlen",
        width = 12,
        column(
          width = 8,
          p("Der Bedarf an MINT-Fachkräften ist bundesweit hoch. Das zeigt z. B. die MINT-Fachkräftelücke
          aus dem MINT-Report des IW Köln. Für 2023 wird diese Lücke deutschlandweit auf 209.000 fehlende MINT-Fachkräfte geschätzt.
          Diese Zahl liegt für die einzelnen Bundesländer so nicht vor.", br(),

          "Dafür kann die Engpassanalyse der Bundesagentur für Arbeit betrachtet werden. Sie zeigt das Ausmaß des
            akuten Fachkräfteengpasses. Das unterstreicht, wie wichtig MINT-Förderung ist, damit der Fachkräftemangel reduziert werden kann.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width=12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_fachkraft")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_2", title = "",
                               content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. Auf Bundesebene gibt es detaillierte Daten zu Fachkräfteengpässen in einzelnen Berufsgattungen, z. B. Mechatronik. Für die Bundesländer liegen nur zusammengefasste Informationen zu MINT-dominierten Berufsgruppen wie Mechatronik und Automatisierungstechnik vor. Mehr Infos dazu finden Sie unter der Seite \"MINT-Fachkräfte\"."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_2"),
            br(),
            shinyBS::bsPopover(id="i_argument_2", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass sowohol in MINT-Berufen als auch in Nicht-MINT-Berufen ca. ein Drittel jeweils als Engpassberufe galten. Schaut man sich die genauen Zahlen mit dem Hover an, dass dies in MINT-Berufen 36 % bzw. 69 Berufe betrifft, bei Nicht-MINT-Berufen auch 36 %, allerdings 122 Berufe."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_2")
          )
        ),
        column(
          width = 3,
          div(class = "content-box",
              style = "background-color: #15419430;
                              color: #154194;
                              border: 2px solid #154194;
                              margin-left: 20px;
                              width: 90%;
                              border-radius: 10px;",
              p("Weiter Statistiken, die hier ergänzt werden könnten:"),
              p("Fachkräfte-Engpass nach MINT-Disziplin: \"Fokusseite MINT-Fachkräfte\",
                unter \"Berufsgruppen: aktueller Fachkräftebedarf in MINT\", Fachkräfteengpass der Bundesländer"),
              p("Anteil und Entwicklung der MINT-Disziplinen: \"Ausbildung & Beruf\", unter M-I-N-T, aktueller Anteil MINT-Disziplinen")

          )
        ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Erhöhter Fachkräftemangel in MINT:")),
                p("Während viele Branchen mit Fachkräftemangel zu kämpfen haben,
                ist die Lage in MINT-Berufen, und insbesondere im Technik-Bereich,
                besonders schlecht. Das unterstreicht, wir brauchen mehr Menschen,
                  die sich für MINT interessieren und MINT-Kompetenzen entwickeln,
                  z. B. durch MINT-Bildungsförderung.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Ähnlich hoher Fachkräftemangel in MINT und nicht MINT:")),
                p("Viele Branchen haben mit Fachkräftemangel zu kämpfen,
                so auch der MINT-Bereich. Damit man dem Fachkräftemangel in Zukunft
                  begegnen kann, braucht es mehr Menschen, die sich für MINT
                  interessieren und MINT-Kompetenzen entwickeln, z. B.
                  durch MINT-Bildungsförderung.")
            )
          )

        )
      )),

    ## Box Demografie ----

    fluidRow(
      shinydashboard::box(
        title = "3. Zukunftstrend: Die Fachkräftelücke wird größer",
        width = 12,
        column(
          width = 8,
          p("Die Grafik zuvor zeigt, schon heute fehlen in vielen Regionen besonders MINT-Fachkräfte.
            Der demografische Wandel wird die Situation weiter verschärfen,
            da ein großer Teil der MINT-Beschäftigten in den nächsten Jahren aus
            der Berufstätigkeit ausscheiden wird.", br(),
            "Das zeigt folgende Grafik, wenn man die Zahl der MINT-Beschäftigten über 55 Jahren, welche
            in den nächsten rund 10 Jahren in die Rente eintreten werden, mit der Anzahl an MINT-Beschäftigten
            unter 25 Jahren, die in den Berufen nachfolgen, vergleicht.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_demografie")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_31", title = "",
                               content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Weitere Informationen finden Sie unter dem Reiter \"Datenquellen und Hinweise\"."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_31"),
            br(),
            shinyBS::bsPopover(id="i_argument_3", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass es 2023 mehr als 7.8 Mio Beschäftigte in MINT in Deutschland gab. Dabei macht die Altersgruppe ü55 1.8 Mio aus (23 % aller Beschäftigten), es kommen aber nur knapp 800.000 der Altersgruppe u25 nach (10 %)."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_3")
          )
        ),
        column(
          width = 3,
          div(class = "content-box",
              style = "background-color: #15419430;
                              color: #154194;
                              border: 2px solid #154194;
                              margin-left: 20px;
                              width: 90%;
                              border-radius: 10px;",
              p("Weiter Statistiken, die hier ergänzt werden könnten:"),
              p("MINT-Anteil nach Gruppen: \"Ausbildung & Beruf\", aktueller MINT-Anteil + Gruppenvergleich – Balkendiagramm, Auswahl unter Berufsgruppen treffen"),

          )
        ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Mehr MINT-Beschäftigte scheiden aus dem Berufsleben aus als nachkommen:")),
                p("Das wird den Fachkräftemangel verschärfen. Jetzt ist der letzte Moment,
                  um mit gezielter MINT-Bildungsförderung junge Menschen für MINT zu interessiere,
                  und die Folgen des demografischen Wandel noch abmildern zu können.")
            )
          )

        )
      )),

    ## Box Nachwuchs ----
    fluidRow(
      shinydashboard::box(
        title = "4. MINT-Nachwuchs stärken: Schlüssel zur Fachkräftesicherung",
        width = 12,
        column(
          width = 8,
          p("Viele Faktoren werden zusammen kommen müssen, um die Fachkräftelage
            in MINT zu stabilisieren. Auch, weil der Bedarf an MINT steigt.
            Ein Schlüssel ist, mehr MINT-Nachwuchs zu gewinnen, doch in vielen
            MINT-Bereichen steht es aktuell nicht gut um Nachwuchs, wie die folgende Grafik zeigt.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_nachwuchs")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_4", title = "",
                               content = paste0("Nachwuchs bezeichnet hier die gemeinsame Betrachtung von Auszubildenden und Studierenden."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_4"),
            br(),
            shinyBS::bsPopover(id="h_argument_41", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland die Anzahl des Nachwuchses in den Ingenieurswissenschaften deutlich über der Informatik und den Mathematik/Naturwissenschaften liegt. Während der Nachwuchs in Informatik deutschlanweit zunimmt (+23,7 % sei 2017), nimmt er in den anderen Disziplinen ab (-6,6 % bzw. 5,7 %)."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "h_argument_41")
          )
        ),
        column(
          width = 3,
            div(class = "content-box",
                style = "background-color: #15419430;
                              color: #154194;
                              border: 2px solid #154194;
                              margin-left: 20px;
                              width: 90%;
                              border-radius: 10px;",
                p("Weiter Statistiken, die hier ergänzt werden könnten:"),
                p("Getrennte Betrachtung von Studierenden und Auszubildenden: \"Ausbildung & Beruf\" bzw. \"Studium\",
                  M-I-N-T, Anteil MINT-Fächer im Zeitverlauf"),
            )
        ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Wenn die Anzahl steigt:")),
                p("Mehr junge Menschen interessieren sich für diese Disziplin.
                Doch in den meisten MINT-Bereichen herrscht bereits Fachkräftemangel,
                welcher sich durch die demografische Entwicklung noch verschärfen wird.
                Deshalb ist Förderung notwendig, um positive Tendenzen zu unterstreichen
                und zu verstärken.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Wenn die Anzahl gleich bleibt:")),
                p("Die Entwicklung des Nachwuchses in diesem MINT-Bereich ist stabil.
                  Doch der Bedarf an Kompetenzen aus dieser Disziplin kann weiter
                  steigen und somit auch der Bedarf an Nachwuchs. Ohne Förderinitiativen
                  kann es auch hier zu einer Verschärfung der Fachkräftelage kommen.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Wenn die Anzahl sinkt")),
                p("Weniger junge Menschen scheinen sich für diesen Bereich zu interessieren.
                  Falls hier schon Fachkräftemangel besteht, wird sich dieser weiter verschärfen.
                  Vor allem hier müssen jungen Menschen ihre Perspektiven gezeigt und Interesse geweckt werden.")
            )
          )

        )
      )),

    ## Box Wirkhebel Förderung ----
    fluidRow(
      shinydashboard::box(
        title = "5. Wege aus der Kriese: So stark wirkt MINT-Nachwuchsförderung",
        width = 12,
        column(
          width = 8,
          p("Um dem Fachkräftemangel im MINT-Bereich wirksam zu begegnen,
            sind verschiedene Maßnahmen nötig. Besonders entscheidend ist dabei
            die Förderung des MINT-Nachwuchses, aber auch
            die gezielte Unterstützung von Frauen.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_wirkhebel")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="erkl_wirkhebel_argument", title="",
                               content = paste0("Gesamteffekt: Wirkung aller Hebel kombiniert.", br(),br(), "MINT-Nachwuchs fördern: Zunahme von MINT-Fachkräften unter 35 zwischen 2012 und 2022 setzt sich so in den nächsten Jahren fort.", br(),br(), "Mädchen- und Frauen-Förderung in MINT: Zunahme von weiblichen MINT-Fachkräften unter 35 zwischen 2012 und 2022 setzt sich so in den nächsten Jahren fort.", br(),br(), "Zuwanderung MINT-Fachkräfte: „Hohe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des Statistischen Bundesamts.", br(),br(), "Verbleib älterer MINT-Fachkräfte: Anteil an erwerbstätigen MINT-Fachkräften unter den 55-59-, 60-64-, und 65-69-Jährigen wächst weiterhin so an wie zwischen 2012-2022."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Das bedeuten die Wirkhebel"), icon("info-circle"), id="erkl_wirkhebel_argument"),
            br(),
            shinyBS::bsPopover(id="h_argument_5", title = "",
                               content = paste0("Weitere Informationen zu den Berechnungen des IW Köln im Auftrag von MINTvernetzt lassen sich auf der Seite \"MINT-Fachkräfte\" nachlesen."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Methodenhinweis"), icon("info-circle"), id = "h_argument_5"),
            br(),
            shinyBS::bsPopover(id="i_argument_5", title = "",
                               content = paste0("Spielen alle Wirkhebel zusammen, können bis 2037 1,4 Mio. zusätzliche MINT-Fachkräfte gewonnen werden. Der stärkste Hebel, mit rund +670.000 MINT-Fachkräften ist die Förderung des MINT-Nachwuchses."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_5")
          )
        ),
        column(
          width = 3,
          div(class = "content-box",
              style = "background-color: #15419430;
                              color: #154194;
                              border: 2px solid #154194;
                              margin-left: 20px;
                              width: 90%;
                              border-radius: 10px;",
              p("Weiter Statistiken, die hier ergänzt werden könnten:"),
              p("Alle Ergebnisse der Zukunftsszenarien für MINT-Fachkräfte: “Fokusseite MINT-Fachkräfte“ Zukunftsszenarien + Wirkhebel"),
          )
        ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("MINT-Nachwuchs Förderung")),
                p("Der größte Wirkhebel, um dem akuten MINT-Fachkräftemangel entgegenzuwirken,
                  sind mehr junge Menschen in MINT. Ohne Nachwuchsförderung, z. B. allein über Zuwanderung,
                  wird die MINT-Lücke nicht schließbar sein. MINT-Bildungsförderung ist die beste Chance, die MINT-Industrie
                  langfristig lebendig zu halten.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Mädchen und Frauen in MINT fördern")),
                p("ist ein starker Hebel, um den Fachkräftemangel in MINT zu reduzieren.
                  Mehr junge Frauen, die sich beruflich für MINT entscheiden,
                  tragen außerdem zu diverseren Perspektiven in MINT bei und so zu
                  einer höheren Qualität in MINT-Forschung und -Entwicklung.")
            )
          )

        )

      ))


    # ,
    #
    # fluidRow(
    #   shinydashboard::box(
    #     title = "Frauen in MINT",
    #     width = 12,
    #     column(
    #       width = 8,
    #       p("Text - Allgemeine Erklärung für Grafik")
    #     ),
    #     column(
    #       width = 12,
    #       shiny::mainPanel(
    #         shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_frauen")),
    #                                      color = "#154194"),
    #         shinyBS::bsPopover(id="h_argument_6", title = "",
    #                            content = paste0("Erklärung, wenn nötig."),
    #                            placement = "top",
    #                            trigger = "hover"),
    #         tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_6")
    #       )
    #     ),
    #     column(
    #       width = 8,
    #       p("Text/Boxen mit Interpretation, je nach Grafik")
    #     )
    #   ))
    )
}


# Argumentation Server

mod_argumentation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r <- reactiveValues()

    observeEvent(input$region_argumentationshilfe, {
      r$region_argumentationshilfe <- input$region_argumentationshilfe
    })

    # Hier Funktionen eingebunden

    output$download_txt <- downloadHandler(
      filename = function() {
        paste0("daten_export_", Sys.Date(), ".txt")
      },
      content = function(file) {
        # Hier holst du die fertigen Daten
        daten <- daten_download(r)

        # Schreibe sie ins file (Shiny gibt 'file' automatisch an den Browser aus)
        writeLines(daten, con = file)
      },
      contentType = "text/plain"
    )

    output$plot_argument_verlauf <- renderUI({
      argument_verlauf(r)
    })

    output$plot_argument_fachkraft <- renderUI({
      argument_fachkraft(r)
    })

    output$plot_argument_demografie <- renderUI({
      argument_demografie(r)
    })

    output$plot_argument_nachwuchs <- renderUI({
      argument_nachwuchs(r)
    })

    output$plot_argument_wirkhebel <- renderUI({
      argument_wirkhebel(r)
    })

    output$plot_argument_frauen <- renderUI({
      argument_frauen(r)
    })

  })
}

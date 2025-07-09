
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
            img(src='www/Banner_KI-Analysehilfe.png',
                class = "img-responsive",
                height = "300px",
                alt = "Banner KI-Analysehilfe",
                style="display: block; margin-left: auto; margin-right: auto;"
            )))),


    ## Einleitungstext ----
    fluidRow(
      div(class = "clean-box",
          column(
            width = 8,
            h1("KI-unterstützt datenbasierte Berichte und Argumentationen erstellen"),
            p(style = "text-align: justify; font-size = 20px",
              "Ob für Förderanträge, Kommunikation oder strategische Entscheidungen -
              Daten helfen dabei, die MINT-Bildungswelt zu verstehen und überzeugend
              zu argumentieren. Sie wollen Ihre Arbeit mit Zahlen und Fakten unterstreichen,
              zum Beispiel, um in Finanzierungsanträgen zu überzeugen, im Diskurs mit
              Politiker:innen die Relevanz von MINT-Förderung zu bestärken oder
              das eigene Projekt wirksam auszurichten?"),
            p(style = "text-align: justify; font-size = 20px",
            "Hier finden Sie die Werkzeuge, um einen aussagestarken MINT-Bericht erstellen
            zu lassen und eine starke Argumentationskette für die MINT-Bildungsförderung
            in Ihrer Region aufzubauen."),
            br()
            ),
          column(
            width = 4,
            tags$div(
              style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: flex-start;",
              tags$strong(
                "Beispielbericht für Hamburg:",
                style = "margin: 20px 0px 0px 60px"
              ),
              tags$a(
                href = "www/MINTvernetzt_Argumentationskette_Hamburg.pdf",
                target = "_blank",
                tags$img(
                  src = "www/Bild_Beispielbericht.png",
                  alt = "Cover Beispielbericht Hamburg",
                  style = "max-width: 20%; height: auto; cursor: pointer; margin: 10px 0px 0px 70px;
                  border: 1px solid #EFE8E6;"
                )
              )
            )
          ),

        ## Infos zu GPT ----

        column(
          width = 8,
          h2("MINT-DataLab-GPT - die KI-Assistenz für Datenanalysen:"),
          p("Der MINT-DataLab-GPT ist von MINTvernetzt für Datenanalysen erstellt
            worden und ist eine spezialisierte Form des Chat-GPT von OpenAI.
            Der MINT-DataLab-GPT nutzt das KI-Modell von OpenAI, für die Nutzung muss
            dort ein (kostenfreier) Account angelegt werden."),
          br(),
          tags$strong("Das sind die Vorteile des MINT-DataLab-GPT:"),

          div(class = "content-box",
              style = "background-color: #ee777530;
               color: #000;
               border: 2px solid #ee7775;
               margin-left: 20px;
               width: 90%;
               border-radius: 10px;
               display: flex;
               align-items: center;
               padding: 10px;",
              div(
                class = "linked-image",
                style = "flex: 0 0 20%;",
                tags$a(
                  href = "https://chatgpt.com/g/g-67e4f41fd91881919a753f4309194bf7-test-mint-datalab-assistent-test",
                  target = "_blank",
                  tags$img(
                    src = "www/Bild_MINT-DataLab-GPT.png",
                    alt = "MINT-DataLab-GPT Symbolbild",
                    style = "max-width: 80%; height: auto; cursor: pointer; margin: 0px; border-radius: 10px;"
                  )
                )
              ),
              div(
                style = "flex: 1; padding-left: 20px; display: flex;
                flex-direction: column; justify-content: center;
                text-align: left;",
                tags$strong("Maßgeschneidert für MINT-Daten und Analysen"),
                p("Unser GPT ist speziell konfiguriert darauf, Datenberichte zu erstellen.
         Es ist ausgerichtet auf unsere Datensätze, und verknüpft diese mit den Fakten
         aus den Kurzanalysen des MINT-DataLabs."),
                tags$strong("Persönliche Assistenz für überzeugende Berichte und Argumente"),
                p("Unser GPT begleitet durch den Prozess und kann Daten nicht nur
         interpretieren, sondern Analysen an Ihrem individuellen Bedarf ausrichten und um Informationen
         anreichern, für starke Aussagen und Argumente."),
                tags$strong("Erhöhte Quellensicherheit"),
                p("Unser GPT nutzt als Daten-Quelle die aus dem MINT-DataLab bereitgestellten
         Statistiken und erstellt automatisch Quellenverzeichnisse. So können
         die KI-Vorteile genutzt werden, bei erhöhter Quellensicherheit.")
              )
          ),

          p("Auf dieser Seite finden Sie einen Einstieg in die Nutzung des MINT-DataLab-GPT. Neben
          einer FAQ-Sektion am Ende der Seite finden Sie hier einer Datenvorauswahl für
          Ihren Schnellstart in die Erstellung datenbasierter Berichte und Argumentationen.
            Folgen Sie dafür den Schritten im nächsten Abschnitt."),
          p("Darüber hinaus können alle Daten des MINT-DataLab für eine Analyse mit dem MINT-DataLab-GPT
            heruntergeladen werden. Die Download-Funktion finden Sie oben rechts an den interkativen Grafiken
            auf den jeweiligen Unterseiten.")
        ),



        ## So geht's ----
        column(
          width = 12,
          h2("So geht's:",
            style = "margin-top: 30px;"),
          hr(style = "border-top: 2px solid #ee7775; margin-top: 15px; margin-bottom: 15px;")
        ),

        column(
          width = 2,
          tags$span(icon("1", style = "margin: 10px; font-size: 17px;"),
                    style = "font-weight: 600;",
                    "Region auswählen."),
          img(src='www/gpt_schritt_1.png',
              class = "img-responsive",
              height = "150px",
              alt = "Symbol Schritt 1 Region wählen",
              style="display: block;
                margin-top: 10px; height: 200px; border: 2px solid #EAECF0;
                border-radius: 15px; text-align: left;")
        ),
        column(
          width = 2,
          tags$span(icon("2", style = "margin: 10px; font-size: 17px;"),
                    style = "font-weight: 600;",
                    "Daten herunterladen."),
          img(src='www/gpt_schritt_2.png',
              class = "img-responsive",
              height = "150px",
              alt = "Symbol Schritt 2 Datendownload",
              style="display: block;
                margin-top: 10px; height: 200px; border: 2px solid #EAECF0;
                border-radius: 15px;")
        ),
        column(
          width = 2,
          tags$span(icon("3", style = "margin: 10px; font-size: 17px;"),
                    style = "font-weight: 600;",
                    "KI-gestützte Analyse erstellen."),
          img(src='www/gpt_schritt_3.png',
              class = "img-responsive",
              height = "150px",
              alt = "Symbol Schritt 3 GPT-Chat",
              style="display: block;
                margin-top: 10px; height: 200px; border: 2px solid #EAECF0;
                border-radius: 15px;")
        ),
        column(
          width = 2,
          tags$span(icon("4", style = "margin: 10px; font-size: 17px;"),
                    style = "font-weight: 600;",
                    "Grafiken ergänzen."),
          img(src='www/gpt_schritt_4.png',
              class = "img-responsive",
              height = "150px",
              alt = "Symbol Schritt 4 Grafiken ergänzen",
              style="display: block;
                margin-top: 10px; height: 200px; border: 2px solid #EAECF0;
                border-radius: 15px;")
          ),
        column(
          width = 12,
          hr(style = "border-top: 2px solid #ee7775; margin-top: 30px; margin-bottom: 15px;")
        ),

      )
    ),

   div(
      style = "margin-top: 40px;",

      ## Region-Filter ----

    column(
      width = 8,
      style = "display: flex; align-items: center;",
      div(
        style = "margin: 0px 25px 50px 0px;",
        img(src='www/gpt_schritt_1.png',
            class = "img-responsive",
            alt = "Bild Schritt 1 klein",
            style="display: block;
                margin-top: 10px; border: 2px solid #EAECF0;
                border-radius: 15px; width: 50px;")
      ),
      div(
        style = "flex: 1; margin-bottom: 15px;",
        p(strong(style = "text-align: justify; font-size: 17px;",
                 "Region auswählen.")),

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
      )
    ),



      ## Daten-Download ----

    column(
      width = 8,
      style = "display: flex; align-items: center;",
       div(
        style = "margin: 0px 25px 130px 0px;",
        img(src='www/gpt_schritt_2.png',
            class = "img-responsive",
            alt = "Bild Schritt 2 klein",
            style="display: block;
                  margin-top: 10px; border: 2px solid #EAECF0;
                  border-radius: 15px; width: 85px;")
      ),
      div(
        style = "margin-bottom: 20px;",

        p(strong(style = "text-align: justify; font-size: 17px;",
                 "Daten herunterladen.")),
        p(style = "font-size : 15px;", "Als Fundament für einen stichhaltigen Datenbericht
        bzw. datenbasierte Argumentation wurden fünf Statistiken aus dem MINT-DataLab ausgewählt.
        Die Grafiken zu den Statistiken werden auf dieser Seite dargestellt.
        Die Daten können hier in einem Dokument gebündelt heruntergeladen werden.",
          br(), br(),
          "Das Format ist ein Text-Dokument, um auch mit einem kostenfreien OpenAI-Account die
          Funktionen des MINT-DataLab-GPT optimal nutzen zu können.", br(),
        "Kopieren Sie folgende Inhalte und fügen Sie sie direkt in den GPT-Chat ein."),

        downloadButton(ns("download_txt"), "   Gesammelte Daten herunterladen"),
      )
    ),

      ## MINT-DataLab-GPT ----

    column(
      width = 8,
      style = "display: flex; align-items: center; margin-bottom: 15px;",
      div(
        style = "margin: 0px 25px 60px 0px;",
        img(src='www/gpt_schritt_3.png',
            class = "img-responsive",
            alt = "Bild Schritt 3 klein",
            style="display: block;
                  margin-top: 10px; border: 2px solid #EAECF0;
                  border-radius: 15px; width: 85px;")
      ),
      div(
        p(strong(style = "text-align: justify; font-size: 17px; margin-bottom: 15px;",
                 "KI-gestützte Analyse erstellen.")),
        p(style = "font-size : 15px;",
          "Der speziell für das MINT-DataLab konfigurierte MINT-DataLab-GPT führt Sie
          durch die Erstellung von datenbasierten Berichten oder Arugmentationsketten,
          basierend auf Statistiken des MINT-DataLabs und auf Informationen zu Ihrem Projekt.
          Bei Bedarf können die Daten durch eine Online-Recherche des GPT ergänzt werden."),
        tags$a(href="https://chatgpt.com/g/g-67e4f41fd91881919a753f4309194bf7-test-mint-datalab-assistent-test",
               "→ Zum MINT-DataLab-GPT", target="_blank",
               style = "color: #b16fab; font-weight: 600")

      )
    ),


      ## Grafiken ----
    column(
      width = 8,
      style = "display: flex; align-items: center; margin-bottom: 20px;",
      div(
        style = "margin: 0px 25px 130px 0px;",
        img(src='www/gpt_schritt_4.png',
            class = "img-responsive",
            alt = "Bild Schritt 4 klein",
            style="display: block;
                  margin-top: 10px; border: 2px solid #EAECF0;
                  border-radius: 15px; width: 85px;")
      ),
      div(
        p(strong(style = "text-align: justify; font-size: 17px;",
                 "Grafiken ergänzen.")),
        p(style = "font-size : 15px;",
        "Auf dieser Seite sind die fünf vorausgewählten Statistiken grafisch dargestellt.
        Bei Bedarf können diese Grafiken heruntergeladen und im Datenbericht oder der Argumentation ergänzt werden.
        Die Download-Option für alle Grafiken des MINT-DataLab findet sich rechts oben an den Grafiken."),
        br(),
         p("Die blauen Boxen rechts neben den Grafiken geben Impulse, welche weiteren
           Statistiken in einem MINT-Bericht ergänzt werden könnten."),
        p("Die grünen Boxen unter den Grafiken zeigen beispielhaft, wie man anhand
          der Statistiken für die MINT-Bildungsförderung argumentieren kann.")
        )
      )
    ),


  column(
    width = 12,
    hr(style = "border-top: 2px solid #ee7775; margin-top: 20px;"),

    h2("So kann eine datenbasierte Argumentation für MINT-Bildung aussehen:",
       style= "margin-bottom: 30px; margin-top: 40px;"),
  ),

    ## Box Zeitverlauf MINT----

    fluidRow(id = "box1",
             shinydashboard::box(
               title = "1. Welche Rolle spielt MINT in Ihrer Region",
               width = 12,
               column(
                 width = 9,
                 p("Als Einstieg in eine Argumentation kann ein kurzer Überblick über die MINT-Strukturen
                   der eigenen Region geigenet sein. Hierfür können Sie selbst rechercherien oder den
                   MINT-DataLab-GPT um eine Recherche bitten."),
                 p("Ergänzend dazu kann auf die Entwicklung der Zahlen von Beschäftigten, Studierenden und Auszubildenden
                   in MINT geschaut werden.
                   Das kann ein Indikator dafür sein, wie zentral der MINT-Sektor für die Region ist und ob
                   die Relevanz eher steigt oder der MINT-Bereich eher hinter anderen
                   Bereichen zurück fällt."),
                 shinyBS::bsPopover(id="anz_argument_1", title = "",
                                    content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, verändern Sie bitte kurz die Fenstergröße, indem Sie die Menü-Übersicht links ein- und wieder ausklappen oder indem Sie die Seite kurz verkleinern und wieder maximieren. Dann stellen sich die Größenverhältnisse der Grafiken korrekt ein."),
                                    placement = "top",
                                    trigger = "hover"),
                 tags$a(paste0("Ist die Grafik abgeschnitten dargestellt?"), icon("question-circle"), id = "anz_argument_1"),
                 br(),br(),
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
                              p("Weitere Statistiken, die hier ergänzt werden könnten:"),
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
                     MINT-Kenntnisse müssen ausgebaut werden, um steigenden Bedarfen an
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
                      zu investieren ist für die zukünftige Wettbewerbsfähigkeit
                      der Region essenziell.")
                   )
               )
               )
             )
            ),

    ## Box Fachkräftemagel ----
    fluidRow(id = "box2",
      shinydashboard::box(
        title = "2. Wo die MINT-Fachkräfte fehlen",
        width = 12,
        column(
          width = 8,
          p("Der Bedarf an MINT-Fachkräften ist bundesweit hoch. Das zeigt z. B. die MINT-Fachkräftelücke
          aus dem MINT-Report des IW Köln. Für 2023 wird diese Lücke deutschlandweit auf 209.000 geschätzt.
          So viele MINT-Fachkräfte fehlen also deutschlandweit.
          Diese Zahl liegt für die einzelnen Bundesländer so nicht vor.", br(),

          "Dafür kann die Engpassanalyse der Bundesagentur für Arbeit betrachtet werden. Sie zeigt das Ausmaß des
            akuten Fachkräfteengpasses. Die Zahlen unterstreichen, wie wichtig MINT-Förderung ist,
          um den Fachkräftemangel zu reduzieren.")
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
              p("Weitere Statistiken, die hier ergänzt werden könnten:"),
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
                besonders schlecht. Das unterstreicht: Wir brauchen mehr Menschen,
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
                p(strong("Ähnlich hoher Fachkräftemangel in MINT und Nicht-MINT:")),
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

    fluidRow(id = "box3",
      shinydashboard::box(
        title = "3. Zukunftstrend: Die Fachkräftelücke wird größer",
        width = 12,
        column(
          width = 8,
          p("Die Grafik zuvor zeigt: Schon heute fehlen in vielen Regionen besonders MINT-Fachkräfte.
            Der demografische Wandel wird die Situation weiter verschärfen,
            da ein großer Teil der MINT-Beschäftigten in den nächsten Jahren aus
            der Berufstätigkeit ausscheiden wird.", br(),
            "Das veranschaulicht die folgende Grafik, wenn man die Zahl der MINT-Beschäftigten über 55 Jahren, welche
            in den nächsten rund 10 Jahren in Rente gehen werden, mit der Anzahl an MINT-Beschäftigten
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
              p("Weitere Statistiken, die hier ergänzt werden könnten:"),
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
                  um mit gezielter MINT-Bildungsförderung junge Menschen für MINT zu interessieren,
                  und die Folgen des demografischen Wandels noch abmildern zu können.")
            )
          )

        )
      )),

    ## Box Nachwuchs ----
    fluidRow(id = "box4",
      shinydashboard::box(
        title = "4. MINT-Nachwuchs stärken: Schlüssel zur Fachkräftesicherung",
        width = 12,
        column(
          width = 8,
          p("Viele Faktoren werden zusammen kommen müssen, um die Fachkräftelage
            in MINT zu stabilisieren. Auch, weil der Bedarf an MINT-Kräften steigt.
            Ein Schlüssel ist, mehr MINT-Nachwuchs zu gewinnen, doch in vielen
            MINT-Bereichen steht es aktuell nicht gut um den Nachwuchs, wie die folgende Grafik zeigt.")
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
                p("Weitere Statistiken, die hier ergänzt werden könnten:"),
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
    fluidRow(id = "box5",
      shinydashboard::box(
        title = "5. Wege aus der Krise: So stark wirkt MINT-Nachwuchsförderung",
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
              p("Weitere Statistiken, die hier ergänzt werden könnten:"),
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
                  wird sich die MINT-Lücke nicht schließen lassen. MINT-Bildungsförderung ist die beste Chance, die MINT-Industrie
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
                p("Auch das ist ein starker Hebel, um den Fachkräftemangel in MINT zu reduzieren.
                  Mehr junge Frauen, die sich beruflich für MINT entscheiden,
                  tragen außerdem zu diverseren Perspektiven in MINT bei und so zu
                  einer höheren Qualität in MINT-Forschung und -Entwicklung.")
            )
          )

        )

      )),

    ## FAQ + Nutzungshinweis ----

    hr(style = "border-top: 2px solid #154194; margin-top: 5px; margin-bottom: 5px;"),



    br(),
    fluidRow(id = "box6",
      column(
        width = 9,
        # h3("Fragen und Antworten"),
        # reactable::reactableOutput(ns("faq_table")),
        h2("Fragen und Antworten"),

        tags$details(
          tags$summary(strong(class = "faq-summary",
                              "Kann ich den MINT-DataLab-GPT auch ohne OpenAI-Konto nutzen?")),
          br(),
          HTML("
    <p><strong>Nein</strong>, die Nutzung des MINT-DataLab-GPT erfordert ein aktives OpenAI-Konto.</p>
    <p>Da es sich um einen individualisierten GPT handelt, läuft der Zugriff über die Infrastruktur von OpenAI. Die Registrierung ist <strong>kostenlos</strong> und in wenigen Schritten möglich.</p>
  ")
        ),

        tags$details(
          tags$summary(strong(class = "faq-summary",
                              "Welche Quellen muss ich angeben, wenn ich die Argumentationshilfe nutze?")),
          br(),
          HTML("
    <p>Beim Herunterladen der Grafiken oder Daten werden die <strong> zugrundeliegenden Datenquellen automatisch</strong> mitgeliefert.</p>
    <p>Der MINT-DataLab-GPT ergänzt zusätzlich Quellen genutzer Kurzanalysen oder Online-Recherchen.</p>
    <p>Je nach Kontext oder Zweck der Nutzung empfehlen wir, aus Gründen der Transparenz
    auf die Unterstützung durch ein KI-Modell und den Einsatz des MINT-DataLab-GPT hinzuweisen.
    <p>Empfohlene Formulierung:</p>
    <blockquote>Erstellt unter Verwendung des angepassten GPT-Sprachmodells von MINTvernetzt (MINT-DataLab-GPT) auf Basis von OpenAI-Technologie.</blockquote>
  ")
        ),

        tags$details(
          tags$summary(class = "faq-summary",
                       "Kann ich auch andere Daten mit dem MINT-DataLab-GPT analysieren?"),
          br(),
          HTML("
    <p>Der MINT-DataLab-GPT ist auf die Daten des MINT-DataLabs spezialisiert, kann jedoch grundsätzlich auch <strong>andere oder eigene Daten</strong> verarbeiten.</p>
    <p>Hinweis: Externe Formate können zu Lesefehlern führen. Bitte prüfen Sie die Korrektheit Ihrer Daten sorgfältig.</p>
  ")
        ),

        tags$details(
          tags$summary(strong(class = "faq-summary",
                              "Wie kann ich das Ergebnis der Analyse exportieren?")),
          br(),
          HTML("
    <p>Der MINT-DataLab-GPT liefert direkt nutzbare <strong>Textbausteine</strong>,
    die sich felxibel in Berichte oder Anträge integrieren lassen. Wie bei der Nutzung anderer
         KI-Chats kann zusätzlich das Format des gewünschten Outputs spezifiziert werden.</p>
    <p>Der Daten-Export für die Nutzung mit dem GPT läuft über eine <code>.txt</code>-Datei. Der Inhalt der Datei kann
    durch \"Copy & Paste\" direkt in den Chat kopiert werden.</p>  ")
          # <p>Beispielhafter Export:</p>
          # <img src='www/beispiel_export.png' alt='Beispiel Export' style='max-width: 100%; border: 1px solid #ccc; border-radius: 8px;'>
        ),

        tags$details(
          tags$summary(strong(class = "faq-summary","Ich habe ein Fehlverhalten festgestellt, wo kann ich das melden?")),
          br(),
          HTML("
    <p>Wir entwickeln den MINT-DataLab-GPT kontinuierlich weiter und auch das zugrundeliegende
    KI-Modell von OpenAI kann sich verändern.</p>
    <p>Falls Ihnen ein Fehlverhalten auffällt oder Sie Feedback haben, wie die Nutzung
    zukünftig noch verbessert werden könnte, schreiben Sie uns geren eine kurze Nachricht per E-Mail an:</p>
    <p><a href='mailto:katharina.brunner@mint-vernetzt.de?subject=Feedback%20Argumentationshilfe'>katharina.brunner@mint-vernetzt.de</a></p>
    <p>Vielen Dank!</p>
  ")
        ),


        br(),
        h2("Nutzungshinweis"),
        p("Der MINT-DataLab-GPT ist eine KI-Anwendung, die auf Technologie von OpenAI basiert.
        Die bereitgestellten Inhalte werden automatisiert generiert und können unvollständig,
        fehlerhaft oder veraltet sein. Die Nutzer:innen sind selbst für eine kritische Prüfung der
        Ausgaben verantwortlich."),
        p("Für den MINT-DataLab-GPT gelten folgende Nutzungsbedingungen, auf die auch
        zu Beginn des Chats hingewiesen wird: ",
          tags$a(href = "www/Nutzungshinweis_Haftungsausschluss_GPT.pdf", target = "_blank", "Nutzungshinweis MINT-DataLab-GPT")),
        p("MINTvernetzt und der Stifterverband übernehmen keine Haftung für Schäden
          oder Nachteile, die aus der Verwendung der bereitgestellten Informationen entstehen.
          Es gelten die allgemeinen Haftungsausschlüsse, wie im Impressum hinterlegt."),
        p(),
        p("Je nach Kontext und Zweck der Nutzung empfehlen wir aus Gründen der Transparenz,
               auf die Unterstützung durch ein KI-Modell und insbesondere auf den Einsatz des
               MINT-DataLab-GPT hinzuweisen. Dafür kann folgende Formulierung genutzt werden:",
          br(),
          "\"Erstellt unter Verwendung des angepassten GPT-Sprachmodells von
               MINTvernetzt (MINT-DataLab-GPT) auf Basis von OpenAI-Technologie.\"")
      )
    ),


    funct_footer()

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

    # #### FAQ Tabelle ----
    # faq_data <- data.frame( #tibble::tibble
    #   Frage   = c(
    #     "Kann ich den MINT-DataLab-GPT auch ohne OpenAI-Konto nutzen?",
    #     "Welche Quellen muss ich angeben, wenn ich die Argumentationshilfe nutze?",
    #     "Kann ich auch andere Daten mit dem MINT-DataLab-GPT analysieren?",
    #     "Wie kann ich das Ergebnis exportieren?",
    #     "Ich habe ein Fehlverhalten festgestellt, wo kann ich das melden?"
    #   ),
    #   Antwort = c(
    #    "Nein, die Nutzung des MINT-DataLab-GPT erfordert ein aktives OpenAI-Konto.
    #     Da es sich um einen individualisierten GPT handelt, läuft der Zugriff
    #     über die Infrastruktur von OpenAI. Die Registrierung ist kostenlos und
    #     in wenigen Schritten möglich.",
    #
    #     "Beim Herunterladen der Grafiken werden die Quellen der zugrunde liegenden
    #     Daten automatisch mit heruntergeladen. Der MINT-DataLab-GPT ergänzt die
    #     Quellenangaben zu den verwendeten Daten oder den durchgeführten Online-Recherchen.
    #    Je nach Kontext und Zweck der Nutzung empfehlen wir aus Gründen der Transparenz,
    #            auf die Unterstützung durch ein KI-Modell und insbesondere auf den Einsatz des
    #            MINT-DataLab-GPT hinzuweisen. Dafür kann folgende Formulierung genutzt werden:
    #            Erstellt unter Verwendung des angepassten GPT-Sprachmodells von
    #            MINTvernetzt (MINT-DataLab-GPT) auf Basis von OpenAI-Technologie.",
    #
    #     "Der MINT-DataLab-GPT ist auf die Daten des MINT-DataLabs spezialisiert und
    #     kann diese fehlerfrei auslesen und interpretieren. Dabei können alle Daten
    #     des MINT-DataLabs, auch diejenigen, die nicht auf der Argumentationsseite
    #     zu finden sind, ohne Probleme genutzt werden. Bei externen Datenquellen
    #     können ggf. Probleme beim Lesen der Datenformate auftreten. Grundsätzlich
    #     ist es aber möglich, eigene oder externe Daten ergänzend mit einzuspielen.
    #     Wir empfehlen, die Korrektheit der übergebenen Daten unbedingt zu prüfen.",
    #
    #    "Der MINT-DataLab-GPT liefert Texte oder Textbausteine, die sich flexibel
    #     in Berichte oder Anträge integrieren lassen. Wie bei der Nutzung anderer
    #     KI-Chats kann zusätzlich das Format des gewünschten Outputs spezifiziert werden,
    #     z. B. Stichpunkte für eine Präsentation.",
    #
    #     "Der MINT-DataLab-GPT ist eine Beta-Version. Mithilfe Ihres Feedbacks können
    #     wir die zukünftige Nutzung weiter verbessern. Bitte schreiben Sie uns dafür,
    #     kurz und informell, eine Nachricht per Mail: ,
    #     <a href='mailto:katharina.brunner@mint-vernetzt.de?subject=Feedback Argumentationshilfe'>
    #     E-Mail schreiben</a>. Vielen Dank!"
    #   ),
    #   stringsAsFactors = FALSE
    # )






    # output$faq_table <- reactable::renderReactable({
    #
    #   faq_data <- faq_data[,-3]
    #   reactable::reactable(
    #   faq_data,
    #   searchable       = FALSE,      # Suchfeld ausblenden (minimalistischer Stil)
    #   pagination       = FALSE,      # keine Pagination – wir zeigen alles auf einer Seite
    #   wrap             = TRUE,
    #   highlight        = TRUE,       # hinterlegt Hover-Zeile leicht
    #   outlined         = FALSE,
    #   bordered         = FALSE,
    #   columnGroups     = NULL,
    #
    #   columns = list(
    #     Frage = reactable::colDef(name = "", minWidth = 300),  # Spaltenkopf ausblenden
    #     Antwort = reactable::colDef(show = FALSE, html = TRUE)              # zunächst nicht anzeigen
    #   ),
    #
    #   details = function(index) {
    #     # wird geöffnet, sobald der User eine Frage anklickt
    #     as.character(
    #       tagList(HTML(faq_data$Antwort[index]))
    #       )
    #     # htmltools::tags$div(class = "faq-answer", faq_data$Antwort[[index]]))
    #     #HTML(paste0('<div class = "faq-answer">', faq_data$Antwort[[index]], '</div>'))
    #   },
    #
    #   class = "faq-table"
    # )
    # })

  })
}


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

    # Einleitungstext
    fluidRow(
      div(class = "clean-box",
          column(
            width = 10,
            h1("Argumentationshilfe - ein Schnellstart in die Nutzung von MINT-Daten"),
            p(style = "text-align: justify; font-size = 20px",
              "Ob für Förderanträge, Kommunikation oder strategische Entscheidungen -
              Daten helfen, überzeugend zu argumentieren.
              Sie wollen Ihre Arbeit mit starken Argumenten unterstreichen, zum Beispiel,
              um Finanzierungsgesuche zu unterstreichen, im Diskurs mit Politiker*innen MINT-Förderung
              zu bestärken oder die Ausrichtung des eigenen Projekt wirksam auszurichten?"),
            p(style = "text-align: justify; font-size = 20px",
            "Hier finden Sie die Werkzeuge dafür, eine starke Argumentationskette
            für die MINT-Bildungsförderung in Ihrer Region aufzubauen.",
              br(),
            br(),
              ),

            h1("So geht´s"),
            tags$span(icon("1", style = "margin: 10px; font-size: 16px;"),
                      "Wählen Sie Ihre gewünschte Region aus."),
            hr(),
            tags$span(icon("2", style = "margin: 10px; font-size: 16px;"),
                      "Laden Sie die Daten als Grundlager Ihrer Argumentation herunter."),
            hr(),
            tags$span(icon("3", style = "margin: 10px; font-size: 16px;"),
            "Nutzen Sie den DataLab-GPT, der Ihnen basierend auf den Daten eine
              Argumentation passend zu Ihrer Region erstellt."),
            hr(),
            tags$span(icon("4", style = "margin: 10px; font-size: 16px;"),
                      "Laden Sie hier die zugehörigen Grafiken herunter und
                      ergänzen Sie in Ihrem Bericht."),

            br(),
            br(),
            hr(style = "border-top: 3px solid #154941; margin-top: 5px;"),
            br(),

            p(style = "text-align: justify; font-size = 20px",
              "Wählen Sie Ihre Region aus:"),

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
          )
    ),

    fluidRow(
             shinydashboard::box(
               title = "1. Warum Ihre Region MINT braucht",
               width = 12,
               column(
                 width = 9,
                 p("Als Einstieg kann ein kurzer Überblick über die MINT-Strukturen
                   der eigenen Region gegeben werden. Ergänzend dazu zeigt die
                   Entwicklung der Beschäftigten, Studierenden und Auszubildenden
                   in MINT, wie zentral der MINT-Sektor für eine Region ist und ob
                   die Relevanz eher steigt oder der MINT-Bereich eher hinter anderen
                   Bereichen zurück fällt.")
               ),
                 column(
                   width = 9,
                   shiny::mainPanel(
                     width = 12,
                     shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_verlauf")),
                                                  color = "#154194"),
                     shinyBS::bsPopover(id="h_argument_11", title = "",
                                        content = paste0(""),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_11"),
                     shinyBS::bsPopover(id="h_argument_1", title = "",
                                        content = paste0("Die linke Grafik stellt den Zeitverlauf der Beschäftigen dar. Die sind in der ersten Einstellung, d.h. für Gesamtdeutschland im Jahr 2023 auf mehr als 7.8 Mio angestiegen, ein Plus von 500.000 gegenüber 2017. Rechts werden die Studierenden und Auszubildenden dargestellt. Diese waren für Gesamtdeutschland konstant."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "h_argument_1")
                   )
                 ),
                 column(
                   width = 3,
                   p("Verweis")
                 ),
               column(
                 width = 12,
                 column(
                   width = 3,
                   br(),
                   div(class = "content-box",
                       style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a50;
                              color: #154194;",
                       p(strong("Wenn die Anzahl steigt:")),
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
                              background-color: #00a87a50;
                              color: #154194;",
                       p(strong("Wenn die Anzahl gleich bleibt:")),
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
                              background-color: #00a87a50;
                              color: #154194;",
                       p(strong("Wenn die Anzahl sinkt:")),
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

    fluidRow(
      shinydashboard::box(
        title = "2. Wo die MINT-Fachkräfte fehlen",
        width = 12,
        column(
          width = 8,
          p("Der Bedarf an MINT-Fachkräften ist bundesweit hoch.
          Das unterstreicht, wie wichtig MINT-Förderung ist.
          Die Engpassanalyse der Bundesagentur für Arbeit zeigt das Ausmaß des
            akuten Fachkräfteengpasses:")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width=12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_fachkraft")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_1", title = "",
                               content = paste0(""),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_21"),
            shinyBS::bsPopover(id="h_argument_2", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass sowohol in MINT-Berufen als auch in Nicht-MINT-Berufen ca. ein Drittel jeweils als Engpassberufe galten. Schaut man sich die genauen Zahlen mit dem Hover an, dass dies in MINT-Berufen 36 % bzw. 69 Berufe betrifft, bei Nicht-MINT-Berufen auch 36 %, allerdings 122 Berufe."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "h_argument_2")
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
                              background-color: #00a87a50;
                              color: #154194;",
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
                              background-color: #00a87a50;
                              color: #154194;",
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

    fluidRow(
      shinydashboard::box(
        title = "3. Zukunftstrend: Die Fachkräftelücke wird größer",
        width = 12,
        column(
          width = 8,
          p("Schon heute fehlen in vielen Regionen im Besonderen MINT-Fachkräfte.
            Der demografische Wandel wird die Situation weiter verschärfen,
            da ein großer Teil der MINT-Beschäftigten in den nächsten Jahren aus
            der Berufstätigkeit ausscheiden wird.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_demografie")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_31", title = "",
                               content = paste0(""),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_31"),
            shinyBS::bsPopover(id="h_argument_3", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass es mehr als 7.8 Mio Beschäftigte im MINT-Bereich in Deutschland gab. Dabei macht die Altersgruppe ü55 1.8 Mio aus, es kommen aber nur knapp 800.000 der Altersgruppe u25 nach."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "h_argument_3")
          )
        ),
        column(
          width = 3,
          p("Text/Boxen Verweisen")
        ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a50;
                              color: #154194;",
                p(strong("Mehr MINT-Beschäftigte scheiden aus dem Berufsleben aus als nachkommen:")),
                p("Das wird den Fachkräftemangel verschärfen. Jetzt ist der letzte Moment,
                  um mit gezielter MINT-Bildungsförderung junge Menschen für MINT zu interessiere,
                  und die Folgen des demografischen Wandel noch abmildern zu können.")
            )
          )

        )
      )),

    fluidRow(
      shinydashboard::box(
        title = "4. MINT-Nachwuchs stärken: Schlüssel zur Fachkräftesicherung",
        width = 12,
        column(
          width = 8,
          p("Viele Faktoren werden zusammen kommen müssen, um die Fachkräftelage
            in MINT zu stabilisieren. Auch, weil der Bedarf an MINT steigt.
            Ein Schlüssel ist, mehr MINT-Nachwuchs zu gewinnen, doch in vielen
            MINT-Bereichen steht es aktuell nicht gut um Nachwuchs.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_nachwuchs")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_4", title = "",
                               content = paste0("Mit Nachwuchs ist die Addition von sowohl Auszubildenden als auch Studierenden gemeint. Ingenieurswissenschaften in Bezug auf Auszubildende meint beispielsweiße Technische Berufe. "),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_4"),
            shinyBS::bsPopover(id="h_argument_41", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland die Anzahl des Nachwuchses in den Ingenieurswissenschaften konsistent deutlich über der Informatik und den Mathematik + Naturwissenschaften liegt. "),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "h_argument_41")
          )
        ),
        column(
          width = 3,
          p("Text/Boxen mit Verweis")
        ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a50;
                              color: #154194;",
                p(strong("Technik")),
                p("Text")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a50;
                              color: #154194;",
                p(strong("Informatik:")),
                p("Text.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a50;
                              color: #154194;",
                p(strong("Mathematik/ Naturwissenschaft")),
                p("Text.")
            )
          )

        )
      )),

    fluidRow(
      shinydashboard::box(
        title = "5. Wege aus der Kriese: So stark wirkt MINT-Nachwuchsförderung",
        width = 12,
        column(
          width = 8,
          p("Um dem Fachkräftemangel im MINT-Bereich wirksam zu begegnen,
            sind verschiedene Maßnahmen nötig. Besonders entscheidend ist dabei
            langfristig betrachtet die Förderung des MINT-Nachwuchses, aber auch
            die gezielte Unterstützung von Frauen.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_wirkhebel")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_5", title = "",
                               content = paste0("Erklärung, wenn nötig."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_5"),
            shinyBS::bsPopover(id="h_argument_51", title = "",
                               content = paste0(""),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "h_argument_51")
          )
        ),
        column(
          width = 3,
          p("Text/Boxen mit Verweisen")
        ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a50;
                              color: #154194;",
                p(strong("Mädchen und Frauen in MINT fördern")),
                p("--> Finale Folgerung")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a50;
                              color: #154194;",
                p(strong("MINT-Nachwuchs Förderung")),
                p("--> Finale Folgerung")
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

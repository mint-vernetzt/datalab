
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
            h1("Argumentationshilfe - ein Schnellstart in die Nutzung von MINT-Daten"),
            p(style = "text-align: justify; font-size = 20px",
              "Ob für Förderanträge, Kommunikation oder strategische Entscheidungen -
              Daten helfen, überzeugend zu argumentieren.
              Sie wollen Ihre Arbeit mit starken Argumenten unterstreichen, zum Beispiel,
              um Finanzierungsgesuche zu unterstreichen, im Diskurs mit Politiker*innen MINT-Förderung
              zu bestärken oder die Ausrichtung des eigenen Projekt wirksam auszurichten?"),
            p(style = "text-align: justify; font-size = 20px",
            "Wie zeigen hier exemplarisch 6 zentrale MINT-Statistiken, und wie sie diese
              in eine Argumentationskette für mehr MINT-Förderung umwandeln.",
              br()
              ),

      ## Region-Filter ----

            p(style = "text-align: justify; font-size = 20px",
              "Wählen Sie bei Bedarf hier Ihre Region aus:"),

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

    ## Download-Button ----

    downloadButton(ns("download_txt"), "Alle Daten als txt Dokument herunterladen"),
    br(),
    br(),

    ## Box Zeitverlauf MINT----

    fluidRow(
             shinydashboard::box(
               title = "MINT im Zeitverlauf",
               width = 12,
               column(
                 width = 8,
                 p("Damit in Deutschland ")
               ),
               column(
                 width = 12,
                  shiny::mainPanel(
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
                 width = 8,
                 br(),
                 p("Text/Boxen mit Interpretation, je nach Grafik")
               )
             )),

    ## Box Fachkräftemagel ----
    fluidRow(
      shinydashboard::box(
        title = "MINT-Fachkräfte.",
        width = 12,
        column(
          width = 8,
          p("Text - Allgemeine Erklärung für Grafik")
        ),
        column(
          width = 12,
          shiny::mainPanel(
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
          width = 8,
          p("Text/Boxen mit Interpretation, je nach Grafik")
        )
      )),

    ## Box Demografie ----

    fluidRow(
      shinydashboard::box(
        title = "Demografische Entwicklung",
        width = 12,
        column(
          width = 8,
          p("Text - Allgemeine Erklärung für Grafik")
        ),
        column(
          width = 12,
          shiny::mainPanel(
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
          width = 8,
          p("Text/Boxen mit Interpretation, je nach Grafik")
        )
      )),

    ## Box Nachwuchs ----
    fluidRow(
      shinydashboard::box(
        title = "MINT-Nachwuchs.",
        width = 12,
        column(
          width = 8,
          p("Text - Allgemeine Erklärung für Grafik")
        ),
        column(
          width = 12,
          shiny::mainPanel(
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
          width = 8,
          p("Text/Boxen mit Interpretation, je nach Grafik")
        )
      )),

    ## Box Wirkhebel Förderung ----
    fluidRow(
      shinydashboard::box(
        title = "Wirkhebel MINT-Förderung",
        width = 12,
        column(
          width = 8,
          p("Diese Wirkhebel beziehen sich auf gesamtdeutsche Zahlen.")
        ),
        column(
          width = 12,
          shiny::mainPanel(
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
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "h_argument_5"),
            br(),
            shinyBS::bsPopover(id="i_argument_5", title = "",
                               content = paste0("Spielen alle Wirkhebel zusammen, können bis 2037 1,4 Mio. zusätzliche MINT-Fachkräfte gewonnen werden. Der stärkste Hebel, mit rund +670.000 MINT-Fachkräften ist die Förderung des MINT-Nachwuchses."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_5")
          )
        ),
        column(
          width = 8,
          p("Text/Boxen mit Interpretation, je nach Grafik")
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

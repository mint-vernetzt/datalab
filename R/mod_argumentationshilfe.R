
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
            "Wie zeigen hier exemplarisch 6 zentrale MINT-Statistiken, und wie sie diese
              in eine Argumentationskette für mehr MINT-Förderung umwandeln.",
              br()
              ),

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
                 width = 8,
                 br(),
                 p("Text/Boxen mit Interpretation, je nach Grafik")
               )
             )),

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
          width = 8,
          p("Text/Boxen mit Interpretation, je nach Grafik")
        )
      )),

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
          width = 8,
          p("Text/Boxen mit Interpretation, je nach Grafik")
        )
      )),

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
          width = 8,
          p("Text/Boxen mit Interpretation, je nach Grafik")
        )
      )),

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

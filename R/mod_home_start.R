#' home_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_ui <- function(id){
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
        width = 3,
        p(style = "text-align: left; font-size = 16px",
           "Auf dieser Überblicksseite geben wir einen ersten Einblick in die vorhandenen Daten und vergleichen die
             Bildungsbereiche miteinander. Auf den folgenden bereichsspezifischen Unterseiten gehen wir je Bildungsbereich
             mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")),



      shinydashboard::box(
        title = "Übersicht Fragestellungen",
        width = 3,
        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump1",
        span(tags$b(span("Fächerwahl MINT:")))),"Wie hoch ist der Anteil von MINT entlang der Bildungskette?"
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#jump2",
        span(tags$b(span("Frauen in MINT:")))),"Anteil von Frauen und Mädchen innerhalb von MINT in den verschiedenen Bildungsbereichen"))

        ,

      shinydashboard::box(
        title = "Datenquellen",
        width = 3,
        p(style = "text-align: left; font-size = 16px",
          "Studierendenzahlen: Destatis 2022, auf Anfrage"),
         p("Schüler:innenzahlen: KMK 2022, auf Anfrage"),
          p("Auszubildenden- und Beschäftigtenzahlen: Bundesagentur für Arbeit 2022, auf Anfrage")
        ),

   shinydashboard::box(
     title = "Fragen oder Feedback?",
     width = 3,
     p(style = "text-align: left; font-size = 16px",
       "Sind alle Zahlen und Grafiken verständlich?", br(),
       "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
            tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
       )))
      ,

   # Box 1

    fluidRow(id="jump1",
      shinydashboard::box(
        title = "Fächerwahl MINT: Wie hoch ist der Anteil von MINT entlang der Bildungskette?",
        width = 12,
        p("Die folgende interaktive Grafik gibt Antworten auf die Fragen: Wie hoch ist der Anteil von MINT-Fächern in der Schule? Wie hoch ist der Anteil von Studierenden, die MINT-Fächer belegen?
          Wie hoch ist der Anteil von Auszubildenden, die eine Ausbildung in MINT machen? Wie hoch ist der Anteil von Beschäftigten, die im MINT-Bereich arbeiten?"),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Bereiche", br(),
                      shiny::sidebarPanel(
                        width = 3,
                        mod_home_start_einstieg_ui("mod_home_start_einstieg_ui_1"),
                        p(style="font-size:12px;color:grey", "Hinweis zur Darstellung: Falls die Grafiken abgeschnitten dargestellt wird, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                        p(style="font-size:12px;color:grey", "Interpretationshilfe: Wenn man beispielsweise Auszubildende und Beschäftigte betrachtet, sieht man, dass sich von allen Auszubildenden deutschlandweit im Jahr 2021
                          31 % dazu entscheiden, eine Ausbildung in einem MINT-Beruf zu machen. Bei den Beschäftigten in Deutschland ist dieser Anteil ein wenig geringer. Im Jahr 2021 arbeiten nur 23 % der Beschäftigten in einem MINT-Beruf.")
                        ),
                      shiny::mainPanel(
                        width = 9,
                        htmlOutput(ns("plot_mint_rest_einstieg_1"))
                        ,p(style="font-size:12px;color:grey",
                           "Quellen: Statistisches Bundesamt,2022; Bundesagentur für Arbeit,2022; KMK, 2022, alle auf Anfrage, eigene Berechnungen."),
                        p(style="font-size:12px;color:grey",
                        "Hinweise: Anders als bei Studierenden oder Auszubildenden wählen Schüler:innen mehrere Grund- und Leistungskurse und können entsprechend nicht
                         eindeutig als \"MINT\" oder \"nicht MINT\" eingruppiert werden. Um dennoch einen Anteil von MINT versus nicht MINT angeben zu können,
                         nutzen wir die Kursbelegungszahlen der Schüler:innen.", br(),
                        "Durch Rundungen der berechneten Werte kann es zu minimalen Abweichungen zwischen den Grafiken kommen.")
                        )
                            ),
                    tabPanel("Vergleich Bereiche im Zeitverlauf", br(),
                        shiny::sidebarPanel(
                          width = 3,
                          mod_home_start_multiple_ui("mod_home_start_multiple_ui_1")),
                        shiny::mainPanel(
                          width = 9,
                          highcharter::highchartOutput(ns("plot_mint_1"))
                          ,p(style="font-size:12px;color:grey", "Quellen: Statistisches Bundesamt,2022; Bundesagentur für Arbeit,2022; KMK, 2022, alle auf Anfrage, eigene Berechnungen."))
                             )
                   ,
                    tabPanel("Überblick", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_comparison_mint_ui("mod_home_start_comparison_mint_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_mint"))
                                              ,p(style="font-size:12px;color:grey", "Quellen: Statistisches Bundesamt,2022; Bundesagentur für Arbeit,2022; KMK, 2022, alle auf Anfrage, eigene Berechnungen.")

         ))
         ))),
    fluidRow(id="jump2",
      shinydashboard::box(
        title = "Frauen in MINT: Anteil von Frauen und Mädchen innerhalb von MINT in den verschiedenen Bildungsbereichen",
        width = 12,
        p("Die folgende interaktive Grafik gibt Antworten auf die Fragen: Wie hoch ist der Anteil von Mädchen in MINT-Leistungskursen?
          Wie hoch ist der Anteil von Frauen in MINT-Studienfächern? Wie hoch ist der Anteil von Frauen in MINT-Ausbildungsgängen?
          Wie hoch ist der Anteil von Frauen in MINT-Berufen?", br(),
        "Zum Vergleich zeigen wir jeweils auch, wie hoch der Anteil von Frauen in den anderen, nicht-MINT-Fächern oder -Berufszweigen ist."),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Bereiche", br(),  # Verlgeich
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_einstieg_gender_ui("mod_home_start_einstieg_gender_ui_1"),
                             p(style="font-size:12px;color:grey",
                               "Hinweis zur Darstellung: Falls die Karte abgeschnitten dargestellt wird, bitte das gesamte Ansichtsfenster einmal minimieren und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                             p(style="font-size:12px;color:grey",
                             "Interpretationshilfe: Betrachtet man hier beispielsweise die MINT-Beschäftigten, sieht man, dass deutschlandweit im Jahr 2021 17 % der in MINT-Beschäftigten
                              Frauen sind. Der Anteil an Frauen in anderen Berufsgruppen ist dagegen weitaus größer (55 %).")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_pie_mint_gender")),
                               p(style="font-size:12px;color:grey",
                                  "Quellen: Statistisches Bundesamt,2022; Bundesagentur für Arbeit,2022; KMK, 2022, alle auf Anfrage, eigene Berechnungen.")
                               )
                            )
                    ,
                    tabPanel("Zeitverlauf", br(),
                        shiny::sidebarPanel(
                          width = 3,
                          mod_home_start_comparison_ui("mod_home_start_comparison_ui_1")),
                        shiny::mainPanel(
                          width = 9,
                          highcharter::highchartOutput(ns("plot_verlauf_mint"))
                          ,p(style="font-size:12px;color:grey", "Quellen: Statistisches Bundesamt,2022; Bundesagentur für Arbeit,2022; KMK, 2022, alle auf Anfrage, eigene Berechnungen.")
                                        )
                            ),
                    tabPanel("Überblick", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_comparison_mint_gender_ui("mod_home_start_comparison_mint_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_gender"))
                                              ,p(style="font-size:12px;color:grey", "Quellen: Statistisches Bundesamt,2022; Bundesagentur für Arbeit,2022; KMK, 2022, alle auf Anfrage, eigene Berechnungen."))

                             )
                    )))


  ,


# Footer
funct_footer())
}

#' home_start Server Functions
#'
#' @noRd
mod_home_start_server <- function(id, data_zentral, data_zentral_neu, data_zentral_alt,data_ausbildungsvertraege ,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$plot_verlauf_mint <- highcharter::renderHighchart({
      home_comparison_line(data_zentral_alt,r)
    })

    output$plot_mint_rest_einstieg_1 <- renderUI({
      home_einstieg_pie(data_zentral_alt,r)
    })

    output$plot_comparison_gender <- highcharter::renderHighchart({
      home_stacked_comparison_gender(data_zentral_alt, data_ausbildungsvertraege, r)
    })

    output$plot_mint_1 <- highcharter::renderHighchart({
      home_rest_mint_verlauf(data_zentral_alt, r)
    })

    output$plot_comparison_mint <- highcharter::renderHighchart({
      home_stacked_comparison_mint(data_zentral_alt, r)
    })

    output$plot_pie_mint_gender <- renderUI({
      home_einstieg_pie_gender(data_zentral_alt, data_ausbildungsvertraege, r)
    })


  })
}

## To be copied in the UI
# mod_home_start_ui("home_start_1")

## To be copied in the server
# mod_home_start_server("home_start_1")







# Alte Start-Box


# fluidRow(
#   shinydashboard::box(
#     #title = span("Willkommen im MINT-DataLab von MINTvernetzt", style = "color:#154194; font-size: 50px"),
#     width = 12,
#     column(width = 9,
#            tags$h1("WILLKOMMEN IM MINT-DATALAB"),
#            #tags$h1("Willkommen im MINT-DataLab"),
#            #p(style = "color:#154194; font-size: 50px", "Willkommen im MINT-DataLab"),
#            p(style = "text-align: justify; font-size = 16px",
#              "Im MINT-DataLab zeigen wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
#                     Ausbildung und Arbeitsmarkt in Deutschland."
#            ),
#            p(style = "text-align: justify; font-size = 16px",
#              span("Unser", tags$b(span("Datenpool", style = "color:#b16fab")), "besteht aktuell aus Statistiken der Bundesagentur für Arbeit, des
#                          Statistischen Bundesamtes und der Kulturministerkonferenz. Weitere Datenquellen werden im Laufe
#                          der Zeit integriert. (Weitere Infos unter: Quellen & Hinweise)")
#            ),
#
#            p(style = "text-align: justify; font-size = 16px",
#              span("Wir freuen uns über ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject=MINT-Datalab", "Feedback"),"!")
#            ),
#     ),
#     #solidHeader = TRUE,
#     #collapsible = FALSE,
#     #br(),
#     column(width = 3, href = "https://mint-vernetzt.de/", rel="noopener", target="_blank",
#            img(src='www/mint_logo_gross.jpg',
#                class = "img-responsive",
#                height = "180px", width = "180px",
#                alt = "Logo MINT",
#                style="display: block; margin-left: auto; margin-right: auto;"
#
#            ))
#   )
# )












